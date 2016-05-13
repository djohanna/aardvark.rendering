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

    type BufferPool(runtime : IRuntime, inputs : SymbolDict<Type>) =
        let layout = MemoryManager.createNop()

        let mutable currentCount = 0
        let buffers = 
            inputs |> SymDict.map (fun sem t ->
                let t = typedefof<Buffer<int>>.MakeGenericType [|t|]
                Activator.CreateInstance(t) |> unbox<ITypedBuffer>
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

        member x.Add(calls : IMod<list<DrawCallInfo>>, att : IAttributeProvider) : BufferPoolSlot =
            let views = getBufferViews att
            let viewArr = Dict.toArray views

            let current : ref<Option<managedptr>> = ref None
            let targetPtr = 
                Mod.custom (fun self ->
                    let calls = calls.GetValue self
                    let count = calls |> List.sumBy (fun c -> c.FaceVertexCount)

                    let ptr = 
                        match !current with
                            | Some old when old.Size = count -> 
                                old

                            | Some o -> 
                                layout.Free o
                                layout.Alloc count

                            | None -> 
                                layout.Alloc count

                    checkCounts()
                    current := Some ptr
                    Range1i.FromMinAndSize(int ptr.Offset, ptr.Size)
                )

            let writers =
                viewArr |> Array.map (fun (target, view) ->
                    Mod.custom (fun self ->
                        let input = view.Buffer.GetValue self
                        let calls = calls.GetValue self
                        let ptr = targetPtr.GetValue self

                        let mutable targetOffset = int64 ptr.Min
                        for c in calls do
                            let offset = c.FirstIndex + view.Offset
                            let count = c.FaceVertexCount
                            target.Write(input, view.ElementType, int64 offset, targetOffset, int64 count)
                            targetOffset <- targetOffset + int64 count

                    )
                )

            BufferPoolSlot(current, calls, att, writers)

        member x.Remove(slot : BufferPoolSlot) : unit =
            match slot.CurrentPointer with
                | Some c -> layout.Free c
                | _ -> ()

        member x.TryGetAttribute(sem : Symbol) : Option<BufferView> =
            match buffers.TryGetValue sem with
                | (true, b) -> BufferView(b.Buffer, inputs.[sem]) |> Some
                | _ -> None

        member x.Dispose() : unit =
            layout.Dispose()
            buffers.Values |> Seq.iter (fun b -> b.Dispose())
            bufferLock.Dispose()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        new(runtime : IRuntime, surface : IBackendSurface) = 
            new BufferPool(
                runtime, 
                surface.Inputs |> List.map (fun (n,t) -> Symbol.Create n, t) |> SymDict.ofList
            )

    and BufferPoolSlot(current : ref<Option<managedptr>>, calls : IMod<list<DrawCallInfo>>, att : IAttributeProvider, writers : IMod<unit>[]) =
        inherit AdaptiveObject()
        let mutable currentRange = Range1i.Invalid

        member internal x.CurrentPointer = !current

        member x.Update(caller : IAdaptiveObject) =
            x.EvaluateIfNeeded caller () (fun () ->
                for w in writers do
                    w.GetValue x

            )

        member x.DrawCallInfo =
            let ptr = current.Value.Value
            DrawCallInfo(
                FaceVertexCount = ptr.Size,
                FirstIndex = int ptr.Offset,
                InstanceCount = 1
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