namespace Aardvark.Base.Rendering


open System
open System.Collections.Generic
open System.Threading
open System.Runtime.InteropServices
open Aardvark.Base
open Aardvark.Base.Incremental
open Microsoft.FSharp.NativeInterop

#nowarn "9"

[<AutoOpen>]
module private TypedBuffers =
    type ITypedBuffer =
        inherit IDisposable
        abstract member Write : data : Array * offset : nativeint * size : nativeint -> unit
        abstract member AdjustToCount : nativeint -> unit
        abstract member Buffer : IMod<IBuffer>

    type TypedCBuffer(runtime : IRuntime, release : unit -> unit)  =
        let lockObj = obj()
        let mutable elementTypeAndSize = None
        let mutable sizeInElements = 0n
        let buffer = new cbuffer(0n, fun b -> release())

        member x.AdjustToCount(count : nativeint) =
            lock lockObj (fun () ->
                sizeInElements <- count
                match elementTypeAndSize with
                    | Some(_,s) -> buffer.AdjustToSize(s * count)
                    | _ -> ()
            )

        member x.Write(data : Array, offsetInElements : nativeint, count : nativeint) =
            let elementSize = 
                lock lockObj (fun () ->
                    match elementTypeAndSize with
                        | Some (t,s) ->
                            assert (t = data.GetType().GetElementType())
                            s
                        | None ->
                            let t = data.GetType().GetElementType()
                            let s = nativeint (Marshal.SizeOf t)
                            elementTypeAndSize <- Some (t,s)
                            buffer.AdjustToSize(s * sizeInElements)
                            s
                )

            assert (count <= nativeint data.Length)

            let offsetInBytes = elementSize * offsetInElements
            let sizeInBytes = elementSize * count
            buffer.Write(data, offsetInBytes, sizeInBytes)

        member x.Buffer = buffer :> IAdaptiveBuffer

        member x.Dispose() =
            elementTypeAndSize <- None
            sizeInElements <- 0n
            buffer.Dispose()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface ITypedBuffer with
            member x.Write(data, off, cnt) = x.Write(data, off, cnt)
            member x.AdjustToCount(cnt) = x.AdjustToCount(cnt)
            member x.Buffer = buffer :> IMod<IBuffer>

    type TypedMappedBuffer(runtime : IRuntime, release : unit -> unit) =
        let lockObj = new ReaderWriterLockSlim()

        [<VolatileField>]
        let mutable elementTypeAndSize = None

        [<VolatileField>]
        let mutable sizeInElements = 0n

        let buffer = runtime.CreateMappedBuffer()

        let subscription = buffer.OnDispose.Subscribe release

        member x.AdjustToCount(count : nativeint) =
            if sizeInElements <> count then
                ReaderWriterLock.write lockObj (fun () ->
                    if sizeInElements <> count then
                        match elementTypeAndSize with
                            | Some(_,s) -> buffer.Resize(int (s * count))
                            | _ -> ()
                        sizeInElements <- count
                )

        member x.Write(data : Array, offsetInElements : nativeint, count : nativeint) =

            let elementSize = 
                match elementTypeAndSize with
                    | Some (t,s) ->
                        assert (t = data.GetType().GetElementType())
                        s
                    | None ->
                        ReaderWriterLock.write lockObj (fun () ->
                            match elementTypeAndSize with
                                | None -> 
                                    let t = data.GetType().GetElementType()
                                    let s = nativeint (Marshal.SizeOf t)
                                    buffer.Resize(int <| s * sizeInElements)
                                    elementTypeAndSize <- Some(t,s)
                                    s
                                | Some(_,s) ->
                                    s
                        )

            assert (count <= nativeint data.Length)

            let offsetInBytes = elementSize * offsetInElements
            let sizeInBytes = elementSize * count

            let arr = GCHandle.Alloc(data,GCHandleType.Pinned)
            try 
                ReaderWriterLock.read lockObj (fun () ->   // why is here read? it works
                    buffer.Write(arr.AddrOfPinnedObject(), int offsetInBytes, int sizeInBytes)
                )
            finally 
                arr.Free()

        member x.Buffer = buffer :> IMod<IBuffer>

        member x.Dispose() =
            elementTypeAndSize <- None
            sizeInElements <- 0n
            buffer.Dispose()
            subscription.Dispose()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface ITypedBuffer with
            member x.Write(data, off, cnt) = x.Write(data, off, cnt)
            member x.AdjustToCount(cnt) = x.AdjustToCount(cnt)
            member x.Buffer = buffer :> IMod<IBuffer>

type GeometryPool(runtime : IRuntime, asyncWrite : bool) =
    let mutable manager = MemoryManager.createNop()
    let pointers = Dict<IndexedGeometry, managedptr>()
    let buffers = SymbolDict<ITypedBuffer>()
        
    let pointersRW = new ReaderWriterLockSlim()
    let buffersRW = new ReaderWriterLockSlim()

    let faceVertexCount (g : IndexedGeometry) =
        if isNull g.IndexArray then
            let att = g.IndexedAttributes.Values |> Seq.head
            att.Length
        else
            g.IndexArray.Length

    let write (g : IndexedGeometry) (sem : Symbol) (ptr : managedptr) (buffer : ITypedBuffer) =
        
        match g.IndexedAttributes.TryGetValue sem with
            | (true, array) -> 
                if isNull g.IndexArray then
                    buffer.Write(array, ptr.Offset, nativeint ptr.Size)
                else
                    failwith "[GeometryLayout] indexed geometries are not supported atm."
            | _ -> ()

    member x.GetBuffer(sem : Symbol) =
        let isNew = ref false
            
        let result = 
            ReaderWriterLock.write buffersRW (fun () ->
                buffers.GetOrCreate(sem, fun sem ->
                    isNew := true
                    let destroy() = buffers.TryRemove sem |> ignore
                    if asyncWrite then new TypedMappedBuffer(runtime, destroy) :> ITypedBuffer
                    else new TypedCBuffer(runtime, destroy) :> ITypedBuffer
                )
            )

        if !isNew then
            ReaderWriterLock.read pointersRW (fun () ->
                result.AdjustToCount(nativeint manager.Capacity)
                for (KeyValue(g,ptr)) in pointers do
                    write g sem ptr result
            )
        result.Buffer

    member x.Add(g : IndexedGeometry) =
        //let isNew = ref false
        let ptr = 
            ReaderWriterLock.write pointersRW (fun () ->
                pointers.GetOrCreate(g, fun g ->
                    let count = faceVertexCount g
                    //isNew := true
                    let ptr = manager.Alloc count

                    ReaderWriterLock.read buffersRW (fun () ->
                        for (KeyValue(sem, buffer)) in buffers do
                            buffer.AdjustToCount(nativeint manager.Capacity)
                            write g sem ptr buffer
                    )
                    ptr
                )
            )
        Range.ofPtr ptr

    member x.Remove(g : IndexedGeometry) =
        ReaderWriterLock.write pointersRW (fun () ->
            match pointers.TryRemove g with
                | (true, ptr) -> 
                    let range = Range.ofPtr ptr
                    manager.Free ptr
                    range
                | _ ->
                    Range1i.Invalid
        )

    member x.Contains(g : IndexedGeometry) =
        pointers.ContainsKey g

    member x.Buffers = buffers |> Seq.map (fun (KeyValue(k,v)) -> k,v.Buffer)

    member x.Count = pointers.Count

    member x.Dispose() =
        buffers.Values |> Seq.toList |> List.iter (fun b -> b.Dispose())
        manager.Dispose()
        pointers.Clear()
        buffers.Clear()
        manager <- MemoryManager.createNop()

    interface IDisposable with
        member x.Dispose() = x.Dispose()






module BufferMonster = 
    open System.Reflection
    open System.Collections.Concurrent
    
    [<AutoOpen>]
    module Utils = 
        type ITypedBuffer =
            inherit IDisposable
            abstract member Buffer : IMod<IBuffer>
            abstract member Type : Type
            abstract member Write : source : IBuffer * sourceType : Type * sourceOffset : int64 * targetOffset : int64 * count : int64 -> unit
            abstract member Resize : count : int64 -> unit

        type Buffer<'a when 'a : unmanaged>(r : IRuntime) =
            static let elementSize = sizeof<'a> |> int64

            static let writers = ConcurrentDictionary<Type, IBufferWriter>()

            static let getWriter (inputType : Type) =
                writers.GetOrAdd(inputType, fun inputTpye ->
                    if typeof<'a> = inputType then
                        let writerType = typedefof<BufferWriter<int>>.MakeGenericType [| inputType |]
                        let prop = writerType.GetProperty("Instance", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                        prop.GetValue(null) |> unbox<IBufferWriter>
                    else
                        let writerType = typedefof<BufferWriter<int, int>>.MakeGenericType [| inputType; typeof<'a> |]
                        let prop = writerType.GetProperty("Instance", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                        prop.GetValue(null) |> unbox<IBufferWriter>
                )

            let mutable currentSize = 0L
            let store = r.CreateMappedBuffer()


            member x.Type = typeof<'a>

            member x.Write(source : IBuffer, sourceType : Type, sourceOffset : int64, targetOffset : int64, count : int64) =
                let writer = getWriter sourceType

                match source with
                    | :? INativeBuffer as source ->
                        source.Use (fun ptr ->
                            let ptr = ptr + nativeint (sourceOffset * elementSize)
                            writer.Write(ptr, store, targetOffset, count)
                        )
                    | _ ->
                        failwithf "[Buffer] unsupported input-buffer: %A" source

            member x.Resize(count : int64) =
                let size = elementSize * count
                if Interlocked.Exchange(&currentSize, size) <> size then
                    store.Resize(int size)

            member x.Dispose() =
                if Interlocked.Exchange(&currentSize, 0L) <> 0L then
                    store.Dispose()
            
            member x.Buffer = store :> IMod<_>

            interface ITypedBuffer with
                member x.Buffer = store :> IMod<_>
                member x.Type = x.Type
                member x.Write(s,st,so,t,c) = x.Write(s,st,so,t,c)
                member x.Resize(c) = x.Resize(c)
                member x.Dispose() = x.Dispose()

        and IBufferWriter =
            abstract member Write : source : nativeint * target : IMappedBuffer * targetOffset : int64 * count : int64 -> unit

        and BufferWriter<'a when 'a : unmanaged> private() =
            static let elementSize = sizeof<'a> |> int64
            static let instance = BufferWriter<'a>()
        
            member x.Write(source : nativeint, buffer : IMappedBuffer, targetOffset : int64, count : int64) =
                buffer.Write(source, int (targetOffset * elementSize), int (count * elementSize))

            static member Instance = instance

        and BufferWriter<'i, 't when 'i : unmanaged and 't : unmanaged> private() =
            static let tSize = sizeof<'t> |> nativeint
            static let iSize = sizeof<'i> |> nativeint
            static let conversion = PrimitiveValueConverter.converter<'i, 't>
            static let instance = BufferWriter<'i, 't>()

            member x.Write(source : nativeint, buffer : IMappedBuffer, targetOffset : int64, count : int64) =
                let count = nativeint count
                let offset = tSize * nativeint targetOffset
                let size = tSize * count

                buffer.Use (offset, size, fun target ->
                    let mutable source = source
                    let mutable target = target

                    let se = source + count * iSize

                    let mutable targetOffset = int targetOffset
                    while source < se do
                        let value =
                            source
                                |> NativePtr.ofNativeInt<'i>
                                |> NativePtr.read
                                |> conversion

                        NativePtr.write (NativePtr.ofNativeInt target) value
                        source <- source + iSize
                        target <- target + tSize
                
                )

            interface IBufferWriter with
                member x.Write(s,b,t,cnt) = x.Write(s,b,t,cnt)

            static member Instance = instance


    type BufferRange =
        | BufferRange of min : nativeint * count : int
        | IndexedBufferRange of min : nativeint * count : int * indexMin : nativeint * indexCount : int

    type IBufferPoolSlot =
        inherit IAdaptiveObject
        inherit IDisposable
        abstract member Update : IAdaptiveObject -> unit
        abstract member Range : BufferRange

    type BufferPool(runtime : IRuntime, inputs : SymbolDict<Type>) =
        let layout = MemoryManager.createNop()

        let mutable currentCount = 0
        let buffers = 
            inputs |> SymDict.map (fun sem t ->
                let t = typedefof<Buffer<int>>.MakeGenericType [|t|]
                Activator.CreateInstance(t, runtime) |> unbox<ITypedBuffer>
            )

        let bufferViews =
            buffers |> SymDict.map (fun sem b ->
                BufferView(b.Buffer, inputs.[sem])
            )

        let bufferLock = new ReaderWriterLockSlim()

        let getBufferViews (att : IAttributeProvider) =
            buffers
                |> SymDict.toSeq 
                |> Seq.choose (fun (sem,target) ->
                    match att.TryGetAttribute(sem) with
                        | Some view -> Some(target, view)
                        | None -> None
                   )
                |> Dict.ofSeq

        let getBufferSizeInBytes(b : IBuffer) =
            match b with
                | :? INativeBuffer as b -> b.SizeInBytes
                | _ -> failwith "[BufferPool] cannot get size for gpu buffer atm."

        let checkCounts() =
            let cap = layout.Capacity
            if Interlocked.Exchange(&currentCount, cap) <> cap then
                ReaderWriterLock.write bufferLock (fun () ->
                    for (KeyValue(_,b)) in buffers do
                        b.Resize (int64 cap)
                )

        member x.Add(att : IAttributeProvider) : IBufferPoolSlot =
            let views = getBufferViews att
            let viewArr = Dict.toArray views

            let current : ref<Option<managedptr>> = ref None
            let targetPtr = 
                Mod.custom (fun self ->
                    let mutable maxCount = 0
                    for (_,v) in viewArr do
                        let b = v.Buffer.GetValue self
                        match b with
                            | :? INativeBuffer as b ->
                                let cnt = (b.SizeInBytes - v.Offset) / Marshal.SizeOf v.ElementType
                                maxCount <- max maxCount cnt
                            | _ ->
                                failwith "bad buffer"

                    let ptr = 
                        match !current with
                            | Some old when old.Size = maxCount -> 
                                old

                            | Some o -> 
                                layout.Free o
                                layout.Alloc maxCount

                            | None -> 
                                layout.Alloc maxCount

                    checkCounts()
                    current := Some ptr
                    ptr
                )

            let writers =
                viewArr |> Array.map (fun (target, view) ->
                    Mod.custom (fun self ->
                        let input = view.Buffer.GetValue self
                        let ptr = targetPtr.GetValue self

                        let targetOffset = int64 ptr.Offset
                        let offset = view.Offset
                        target.Write(input, view.ElementType, int64 offset, targetOffset, int64 ptr.Size)

                    )
                )

            new BufferPoolSlot(x, current, att, writers) :> IBufferPoolSlot

        member internal x.Remove(ptr : managedptr) : unit =
            layout.Free ptr

        member x.TryGetAttribute(sem : Symbol) : Option<BufferView> =
            match bufferViews.TryGetValue sem with
                | (true, v) -> v |> Some
                | _ -> None

        member x.All = bufferViews |> SymDict.toSeq

        member x.Dispose() : unit =
            layout.Dispose()
            buffers.Values |> Seq.iter (fun b -> b.Dispose())
            bufferLock.Dispose()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IAttributeProvider with
            member x.TryGetAttribute sem = x.TryGetAttribute sem
            member x.All = bufferViews |> SymDict.toSeq

        new(runtime : IRuntime, surface : IBackendSurface) = 
            new BufferPool(
                runtime, 
                surface.Inputs |> List.map (fun (n,t) -> Symbol.Create n, t) |> SymDict.ofList
            )

    and private BufferPoolSlot(parent : BufferPool, current : ref<Option<managedptr>>, att : IAttributeProvider, writers : IMod<unit>[]) =
        inherit AdaptiveObject()
        member internal x.CurrentPointer = !current

        member x.Update(caller : IAdaptiveObject) =
            x.EvaluateIfNeeded caller () (fun () ->
                for w in writers do
                    w.GetValue x

            )

        member x.Range =
            let ptr = current.Value.Value
            BufferRange(ptr.Offset, ptr.Size)

        member x.Dispose() =
            let old = Interlocked.Exchange(&current.contents, None)
            match old with
                | Some o -> parent.Remove o
                | None -> ()

        interface IBufferPoolSlot with
            member x.Update c = x.Update c
            member x.Range = x.Range
            member x.Dispose() = x.Dispose()


    type IndexedBufferPool(runtime : IRuntime, inputs : SymbolDict<Type>, indexType : Type) =
        let buffers = new BufferPool(runtime, inputs)

        let layout = MemoryManager.createNop()
        let indexBuffer = new Buffer<int>(runtime)
        let mutable currentCount = 0
        let checkCounts() =
            let cap = layout.Capacity
            if Interlocked.Exchange(&currentCount, cap) <> cap then
                indexBuffer.Resize (int64 cap)


        member x.Add(att : IAttributeProvider, index : IMod<IBuffer>) =
            let range = buffers.Add(att)

            let current : ref<Option<managedptr>> = ref None
            let ptr =
                Mod.custom (fun self ->
                    let index = index.GetValue self
                    let maxCount =
                        match index with
                            | :? INativeBuffer as b -> b.SizeInBytes / Marshal.SizeOf indexType
                            | _ -> failwith "bad buffer"

                    let ptr = 
                        match !current with
                            | Some old when old.Size = maxCount -> 
                                old

                            | Some o -> 
                                layout.Free o
                                layout.Alloc maxCount

                            | None -> 
                                layout.Alloc maxCount

                    checkCounts()
                    current := Some ptr
                    ptr
                )

            let writer =
                Mod.custom (fun self ->
                    let ptr = ptr.GetValue self
                    let index = 
                        if isNull index then Array.init ptr.Size id |> ArrayBuffer :> IBuffer
                        else index.GetValue self

                    indexBuffer.Write(index, indexType, 0L, int64 ptr.Offset, int64 ptr.Size)
                    ()
                )

            new IndexedBufferPoolSlot(x, current, unbox range, writer) :> IBufferPoolSlot

        member internal x.Remove(ptr : managedptr) =
            layout.Free ptr

        member x.TryGetAttribute(sem : Symbol) : Option<BufferView> =
            buffers.TryGetAttribute sem

        member x.All = buffers.All

        member x.Dispose() : unit =
            layout.Dispose()
            indexBuffer.Dispose()
            buffers.Dispose()

        member x.IndexBuffer = indexBuffer.Buffer

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IAttributeProvider with
            member x.TryGetAttribute sem = x.TryGetAttribute sem
            member x.All = buffers.All

    and private IndexedBufferPoolSlot(parent : IndexedBufferPool, current : ref<Option<managedptr>>, slot : IBufferPoolSlot, writer : IMod<unit>) =
        inherit AdaptiveObject()

        member x.Update(caller : IAdaptiveObject) =
            x.EvaluateIfNeeded caller () (fun () ->
                slot.Update(x)
                writer.GetValue x
            )

        member x.Range =
            let ptr = current.Value.Value
            match slot.Range with
                | BufferRange(offset, size) ->
                    IndexedBufferRange(offset, size, ptr.Offset, ptr.Size)
                | _ ->
                    failwith "[IndexedBufferPool] expected BufferRange but got IndexedBufferRange"

        member x.Dispose() =
            slot.Dispose()
            let old = Interlocked.Exchange(&current.contents, None)
            match old with
                | Some o -> parent.Remove o
                | None -> ()

        interface IBufferPoolSlot with
            member x.Update c = x.Update c
            member x.Range = x.Range
            member x.Dispose() = x.Dispose()

    

    type AdaptiveIndirectBuffer(runtime : IRuntime, indexed : bool) =
        inherit DirtyTrackingAdaptiveObject<IBufferPoolSlot>()

        let newSlots = HashSet<IBufferPoolSlot>()

        let mutable res = Unchecked.defaultof<IBuffer>
        let set = Dictionary<IBufferPoolSlot, DrawCallInfo>()

        member x.GetValue(caller : IAdaptiveObject) =
            x.EvaluateAlways' caller (fun dirty ->
                if x.OutOfDate then
                    lock set (fun () ->
                        dirty.UnionWith newSlots
                        newSlots.Clear()
                        dirty.IntersectWith set.Keys

                        if dirty.Count > 0 then
                            for d in dirty do 
                                d.Update x
                                match d.Range with
                                    | BufferRange(off, cnt) when not indexed ->
                                        set.[d] <- DrawCallInfo(FirstIndex = int off, FaceVertexCount = cnt, InstanceCount = 1)
                                    | IndexedBufferRange(min,cnt,indexMin,indexCnt) when indexed ->
                                        set.[d] <- DrawCallInfo(FirstIndex = int indexMin, FaceVertexCount = indexCnt, BaseVertex = int min, InstanceCount = 1)
                                    | _ -> 
                                        failwith "[AdaptiveIndirectBuffer] unexpected indexing-mode"

                        res <- set.Values |> Seq.toArray |> ArrayBuffer :> IBuffer
                    )
                    


                res
            )

        member x.Add(i : IBufferPoolSlot) =
            lock set (fun () -> 
                set.Add(i, Unchecked.defaultof<_>)
                newSlots.Add i |> ignore
            )
            transact (fun () -> x.MarkOutdated())

        member x.Remove(i : IBufferPoolSlot) =
            lock set (fun () -> 
                set.Remove(i) |> ignore
                newSlots.Remove i |> ignore
            )
            transact (fun () -> x.MarkOutdated())

        interface IMod with
            member x.IsConstant = false
            member x.GetValue c = x.GetValue c :> obj

        interface IMod<IBuffer> with
            member x.GetValue c = x.GetValue c


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BufferPool =
        let create (r : IRuntime) (types : Map<Symbol, Type>) =
            new BufferPool(r, SymDict.ofMap types)

        let destroy (pool : BufferPool) =
            pool.Dispose()

        let alloc (att : IAttributeProvider) (pool : BufferPool) =
            pool.Add(att)

        let free (slot : IBufferPoolSlot) (pool : BufferPool) =
            slot.Dispose()


module Optimizations = 
    open FShade
    open BufferMonster
    open System.Reflection
    open Aardvark.Base.Monads.Option

    type RenderObjectState =
        {
            Surface             : FShadeSurface
            Indexed             : bool

            RenderPass          : RenderPass
            Mode                : IMod<IndexedGeometryMode>
            DepthTest           : IMod<DepthTestMode>
            CullMode            : IMod<CullMode>
            BlendMode           : IMod<BlendMode>
            FillMode            : IMod<FillMode>
            StencilMode         : IMod<StencilMode>
            WriteBuffers        : Option<Set<Symbol>>

        }

    type PoolRenderObject =
        {
            attributeScope      : Ag.Scope
            drawCallInfos       : IMod<list<DrawCallInfo>>
            vertexAttributes    : IAttributeProvider
            uniforms            : IUniformProvider

            // TODO:
            //instanceAttributes  : IAttributeProvider
            //indexBuffer         : IMod<IBuffer>
        }

    type SingleValueBuffer<'a>(value : 'a) =
        interface IBuffer

        interface INativeBuffer with
            member x.SizeInBytes = sizeof<'a>
            member x.Use f =
                let gc = GCHandle.Alloc(value, GCHandleType.Pinned)
                try f (gc.AddrOfPinnedObject())
                finally gc.Free()

            member x.Pin() = failwith "not implemented"
            member x.Unpin() = failwith "not implemented"


    type PoolCall(uniform : IBufferPoolSlot, vertex : IBufferPoolSlot, calls : IMod<list<DrawCallInfo>>) =
        inherit Mod.AbstractMod<list<DrawCallInfo>>()

        override x.Compute() =
            uniform.Update x
            vertex.Update x

            let vertexOffset =
                match vertex.Range with
                    | BufferRange(off,_) -> off
                    | IndexedBufferRange(off,_,_,_) -> off

            let instanceOffset =
                match uniform.Range with
                    | BufferRange(off,_) -> off
                    | IndexedBufferRange(off,_,_,_) -> off


            let calls = calls.GetValue x

            calls |> List.map (fun c ->
                if c.InstanceCount <> 1 then
                    failwith "[PoolCall] cannot support nested instancing"

                DrawCallInfo(
                    FaceVertexCount = c.FaceVertexCount,
                    FirstIndex = int vertexOffset + c.FirstIndex,
                    FirstInstance = int instanceOffset,
                    InstanceCount = 1
                )
            
            )

        member x.Dispose() =
            uniform.Dispose()
            vertex.Dispose()
            calls.Outputs.Remove x |> ignore


    type UniformProviderTemplate(types : SymbolDict<Type>) =
        let creators = 
            types |> SymDict.map (fun _ t ->
                let meth = typeof<UniformProviderTemplate>.GetMethod("CreateBuffer", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                //let meth = meth.MakeGenericMethod [|t|]

                fun (v : IMod) -> 
                    match v.GetType() with
                        | ModOf t ->
                            let meth = meth.MakeGenericMethod [|t|]
                            meth.Invoke(null, [|v|]) |> unbox<BufferView>
                        | _ -> 
                            failwith ""
            )
        
        static member private CreateBuffer (m : IMod<'a>) =
            let buffer = m |> Mod.map (fun v -> SingleValueBuffer(v) :> IBuffer)
            BufferView(buffer, typeof<'a>)

        member x.Instantiate(scope : Ag.Scope, values : IUniformProvider) =
            let views = 
                creators |> SymDict.map (fun sem c -> 
                    lazy ( match values.TryGetUniform(scope, sem) with
                            | Some m -> m |> c |> Some
                            | None -> None
                    )
                )
            { new IAttributeProvider with
                member x.TryGetAttribute sem = 
                    match views.TryGetValue sem with
                        | (true, v) -> v.Value
                        | _ -> None
                member x.All = views |> SymDict.toSeq |> Seq.choose (fun (k,v) -> match v.Value with | Some v -> Some(k,v) | _ -> None)
                member x.Dispose() = ()
            }

    type RenderObjectPool(runtime : IRuntime, signature : IFramebufferSignature, effect : FShadeEffect) =

        let backendSurface = FShadeSurface.Compile(runtime, signature, effect, fun c -> { c with treatUniformsAsInputs = true })
        let surface = runtime.PrepareSurface(signature, backendSurface)


        let instancedUniformTypes : SymbolDict<Type> =
            surface.Inputs  
                |> List.choose (fun (name, t) ->
                    let name = Symbol.Create name
                    if backendSurface.UniformTypes.ContainsKey name then Some(name, t)
                    else None
                   )
                |> SymDict.ofList

        let uniformTemplate = UniformProviderTemplate(instancedUniformTypes)

        let attributeTypes =
            // TODO: find better source for types
            surface.Inputs
                |> Seq.choose (fun (n, v) -> 
                    let name = Symbol.Create n
                    if backendSurface.UniformTypes.ContainsKey name then None
                    else Some(name, v)
                   )
                |> SymDict.ofSeq

        let instanceAttributes = new BufferPool(runtime, instancedUniformTypes)
        let vertexAttributes = new BufferPool(runtime, attributeTypes)

        let calls = Dict<PoolRenderObject, PoolCall>()

        let indirect =
            Mod.custom (fun self ->
                let list = List<DrawCallInfo>()
                for call in calls.Values do
                    let v = call.GetValue self
                    list.AddRange v

                list.ToArray() |> ArrayBuffer :> IBuffer
            )


        member x.VertexAttributes = vertexAttributes :> IAttributeProvider
        member x.InstanceAttributes = instanceAttributes :> IAttributeProvider
        member x.Indirect = indirect
        member x.Surface = Mod.constant (surface :> ISurface)


        member x.Add(o : PoolRenderObject) : unit =
            if not (calls.Contains o) then
                let uniformSlot =
                    instanceAttributes.Add (uniformTemplate.Instantiate(o.attributeScope, o.uniforms))

                let vertexSlot = 
                    vertexAttributes.Add(o.vertexAttributes)

                let call = 
                    PoolCall(uniformSlot, vertexSlot, o.drawCallInfos)

                calls.Add(o, call)
                transact (fun () -> indirect.MarkOutdated())

        member x.Remove(o : PoolRenderObject) : unit =
            match calls.TryRemove o with
                | (true, call) -> 
                    call.Dispose()
                    transact (fun () -> indirect.MarkOutdated())
                | _ -> ()


    let findIndirectDraws (signature : IFramebufferSignature) (runtime : IRuntime) (input : aset<IRenderObject>) : aset<IRenderObject> =
        let pools = Dict<RenderObjectState, RenderObjectPool * RenderObject>()

        let getPool (uniforms : IUniformProvider) (state : RenderObjectState) : Option<RenderObject> * RenderObjectPool =
            let mutable isNew = false
            let pool, ro = 
                pools.GetOrCreate(state, fun state ->
                    isNew <- true
                    let pool = RenderObjectPool(runtime, signature, state.Surface.Effect)

                    let ro =
                        let o = RenderObject.Create()
                        o.AttributeScope <- Ag.emptyScope
                        o.IsActive <- Mod.constant true
                        o.IndirectBuffer <- pool.Indirect
                        o.Mode <- state.Mode
                        o.Surface <- pool.Surface
                        o.DepthTest <- state.DepthTest
                        o.CullMode <- state.CullMode
                        o.BlendMode <- state.BlendMode
                        o.FillMode <- state.FillMode
                        o.StencilMode <- state.StencilMode
                        o.InstanceAttributes <- pool.InstanceAttributes
                        o.VertexAttributes <- pool.VertexAttributes
                        o.Uniforms <- uniforms
                        o.WriteBuffers <- state.WriteBuffers

                        o

                    pool, ro
                )


            if isNew then
                Some ro, pool
            else
                None, pool

        let tryDecompose (ro : RenderObject) =
            let surface = ro.Surface.GetValue null

            match surface with
                | :? FShadeSurface as surface ->
                    if isNull ro.IndirectBuffer then
                        let state =
                            {
                                Surface             = surface
                                Indexed             = not (isNull ro.Indices)
                                RenderPass          = ro.RenderPass
                                Mode                = ro.Mode
                                DepthTest           = ro.DepthTest
                                CullMode            = ro.CullMode
                                BlendMode           = ro.BlendMode
                                FillMode            = ro.FillMode
                                StencilMode         = ro.StencilMode
                                WriteBuffers        = ro.WriteBuffers
                            }

                        let poolObject =
                            {
                                attributeScope      = ro.AttributeScope
                                drawCallInfos       = ro.DrawCallInfos
                                vertexAttributes    = ro.VertexAttributes
                                uniforms            = ro.Uniforms

                            }

                        Some (state, poolObject)
                    else
                        None
                            
                | _ -> 
                    None

        let reader = input.GetReader()
        ASet.custom (fun self ->
            let deltas = reader.GetDelta self

            let res = List<Delta<IRenderObject>>()

            for d in deltas do
                match d with
                    | Add (:? RenderObject as ro) ->
                        match tryDecompose ro with
                            | Some(state, o) ->
                                let r, pool = getPool o.uniforms state

                                pool.Add(o)

                                // if the pool is new then add it's renderObject
                                match r with
                                    | Some ro -> res.Add(Add (ro :> IRenderObject))
                                    | _ -> ()

                            | None -> res.Add d

                    | Rem (:? RenderObject as ro) ->
                        match tryDecompose ro with
                            | Some(state, o) ->
                                match pools.TryGetValue state with
                                    | (true, (pool, _)) -> pool.Remove o
                                    | _ -> ()

                            | None -> res.Add d
                    | d -> res.Add(d)

            res |> CSharpList.toList
        )



        







[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GeometryPool =
    let inline create runtime = new GeometryPool(runtime, false)
    let inline createAsync runtime = new GeometryPool(runtime, true)


    let inline getBuffer (sem : Symbol) (pool : GeometryPool) =
        pool.GetBuffer sem

    let inline add (g : IndexedGeometry) (pool : GeometryPool) =
        pool.Add g

    let inline remove (g : IndexedGeometry) (pool : GeometryPool) =
        pool.Remove g

    let inline contains (g : IndexedGeometry) (pool : GeometryPool) =
        pool.Contains g