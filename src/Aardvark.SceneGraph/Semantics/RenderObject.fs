namespace Aardvark.SceneGraph.Semantics

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Base.Rendering

open Aardvark.SceneGraph.Internal

[<AutoOpen>]
module RenderObjectSemantics =

    type ISg with
        member x.RenderObjects() : aset<IRenderObject> = x?RenderObjects()

    module Semantic =
        [<System.Obsolete("renderJobs is deprecated, please use renderObjects instead.")>]        
        let renderJobs (s : ISg) : aset<IRenderObject> = s?RenderObjects()
        let renderObjects (s : ISg) : aset<IRenderObject> = s?RenderObjects()

    [<Semantic>]
    type RenderObjectSem() =

        member x.RenderObjects(a : IApplicator) : aset<IRenderObject> =
            aset {
                let! c = a.Child
                yield! c.RenderObjects()
            }

        member x.RenderObjects(g : IGroup) : aset<IRenderObject> =
            aset {
                for c in g.Children do
                    yield! c.RenderObjects()
            }

        member x.RenderObjects(r : Sg.RenderNode) : aset<IRenderObject> =
            let scope = Ag.getContext()
            let rj = RenderObject.Create()
            
            rj.AttributeScope <- scope 
            rj.Indices <- let index  = r.VertexIndexArray in if index = AttributeSemantics.emptyIndex then null else index 
         
            rj.IsActive <- r.IsActive
            rj.RenderPass <- r.RenderPass
            
            let vertexAttributes = new Providers.AttributeProvider(scope, "VertexAttributes")
            let instanceAttributes =  new Providers.AttributeProvider(scope, "InstanceAttributes")

            rj.Uniforms <- new Providers.UniformProvider(scope, r?Uniforms, 
                                                         [vertexAttributes; instanceAttributes])
            rj.VertexAttributes <- vertexAttributes
            rj.InstanceAttributes <- instanceAttributes
            
            rj.DepthTest <- r.DepthTestMode
            rj.CullMode <- r.CullMode
            rj.FillMode <- r.FillMode
            rj.StencilMode <- r.StencilMode
            rj.BlendMode <- r.BlendMode
              
            rj.Surface <- r.Surface
            
            let callInfo =
                adaptive {
                    let! info = r.DrawCallInfo
                    if info.FaceVertexCount < 0 then
                        let! (count : int) = scope?FaceVertexCount
                        return 
                            [| DrawCallInfo(
                                FirstIndex = info.FirstIndex,
                                FirstInstance = info.FirstInstance,
                                InstanceCount = info.InstanceCount,
                                FaceVertexCount = count,
                                BaseVertex = 0
                               )
                            |]
                    else
                        return [|info|]
                }

            rj.DrawCallInfos <- callInfo
            rj.Mode <- r.Mode
            ASet.single (rj :> IRenderObject)


module GeometrySetRenderObjects =
    open System
    open System.Threading
    open System.Runtime.InteropServices
    open System.Collections.Generic
    open System.Threading.Tasks

    type RangeTree =
        | Node of Range1i * RangeTree * RangeTree
        | Leaf of Range1i
        | Nil with

        member s.Range =
            match s with
                | Node(r,_,_) -> r
                | Leaf r -> r
                | Nil -> Range1i.Invalid
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RangeTree = 

        let empty = Nil

        let rec insert (r : Range1i) (min : int) (max : int) (t : RangeTree) =
            match t with
                | Nil -> 
                    Leaf r

                | Leaf l ->
                    let u = l.Union r
                    if l.Intersects r then Leaf u
                    else 
                        if l.Min < r.Min then Node (u, Leaf l, Leaf r)
                        else Node(u, Leaf r, Leaf l)

                | Node(u, left, right) ->
                    let newU = u.Union r
                    if u.Intersects r then
                        let lr = left.Range
                        let rr = right.Range

                        match r.Contains lr, r.Contains rr with
                            | true, true -> Leaf r
                            | true, false ->

                        match lr.Intersects r, rr.Intersects r with
                            | true, false -> 
                                Node(newU, insert r left, right)
                            | false, true -> 
                                Node(newU, left, insert r right)
                            | false, false -> 
                                Node(newU, left, Node(rr.Union(r), Leaf r, right))
                            | true, true -> 
                                Node(newU, insert r left, insert r right)
                            
                    else
                        if u.Min < r.Min then Node(newU, t, Leaf r)
                        else Node(newU, Leaf r, t)


    module private MergedAttributes = 

        type MergedAttributeLayout(types : Map<Symbol, Type>) =
            let man = MemoryManager.createNop()
            let ptrs = Dictionary<IndexedGeometry, managedptr>()

            let vertexCount (ig : IndexedGeometry) =
                assert (isNull ig.IndexArray)
                match ig.IndexedAttributes.Values |> Seq.tryFind (fun _ -> true) with
                    | Some pos -> pos.Length
                    | _ -> failwithf "could not determine vertexCount for IndexedGeometry (no attributes): %A" ig

            member x.Allocated =
                man.AllocatedBytes

            member x.Capacity =
                man.Capacity

            member x.Content =
                ptrs |> Dictionary.toSeq

            member x.TryGetPointer (ig : IndexedGeometry, [<Out>] ptr : byref<managedptr>) =
                ptrs.TryGetValue(ig, &ptr)

            member x.Alloc(ig : IndexedGeometry) =
                match ptrs.TryGetValue ig with
                    | (true, ptr) -> ptr
                    | _ ->
                        let vc = vertexCount ig
                        let ptr = man.Alloc(vc)
                        ptrs.[ig] <- ptr
                        ptr

            member x.Free(ig : IndexedGeometry) =
                match ptrs.TryGetValue ig with
                    | (true, ptr) -> 
                        ManagedPtr.free ptr
                        ptrs.Remove ig |> ignore
                    | _ -> 
                        failwithf "cannot free unknown geometry storage: %A" ig

            member x.GetDrawCallInfos() =
                let free = man.FreeList |> Seq.toArray
                free.QuickSort(fun (l : managedptr) (r : managedptr) -> compare l.Offset r.Offset)


                let ranges =
                    [|
                        let offset = ref 0n
                        for f in free do
                            let size = f.Offset - !offset |> int

                            if size > 0 then yield (!offset, size)
                            offset := f.Offset + nativeint f.Size

                        if !offset < nativeint man.Capacity then
                            yield (!offset, man.Capacity - int !offset)
                    |]



                ranges
                    |> Array.map (fun (offset, size) ->
                        DrawCallInfo(
                            FaceVertexCount = size,
                            InstanceCount = 1,
                            FirstInstance = 0,
                            BaseVertex = 0,
                            FirstIndex = int offset
                        )
                    )

        type Buffer(elementType : Type, input : IMod<obj>) =
            inherit Mod.AbstractMod<IBuffer>()

            let elementSize = Marshal.SizeOf elementType
            let ops = List<Array * managedptr>()
            let mutable capacity = 0
            let mutable store = 0n

            let write (arr : Array) (target : managedptr) =
                let targetSize = target.Size * elementSize
                let targetPtr =  store + target.Offset * nativeint elementSize

                arr.UnsafeCoercedApply(fun (arr : byte[]) ->
                    Marshal.Copy(arr, 0, targetPtr, min arr.Length targetSize)
                )

            member x.Resize(newSize : int) =
                let newSize = newSize * elementSize
                let oldSize = Interlocked.Exchange(&capacity, newSize)
                
                if newSize <> oldSize then
                    let oldPtr = store
                    let copy = min newSize oldSize
                    let newPtr = Marshal.AllocHGlobal newSize
                    if copy > 0 then Marshal.Copy(oldPtr, newPtr, copy)
                    store <- newPtr
                    Marshal.FreeHGlobal oldPtr

            member x.Emit(newOps : seq<Array * managedptr>) =
                lock ops (fun () ->
                    ops.AddRange newOps
                )

            override x.Compute() =
                // TODO: range updates could be performed directly on
                //       backend buffer

                input.GetValue x |> ignore
                let res = 
                    lock ops (fun () ->
                        let res = ops.ToArray()
                        ops.Clear()
                        res
                    )

                for (arr, target) in res do
                    write arr target

                NativeMemoryBuffer(store, capacity) :> IBuffer

        type MergedAttributeProvider(mode : IndexedGeometryMode, attributeTypes : Map<Symbol, Type>, geometries : aset<IndexedGeometry>) =
            inherit Mod.AbstractMod<obj>()

            let layout = MergedAttributeLayout(attributeTypes)
            let reader = geometries.GetReader()
            let buffers = Dictionary<Symbol, Buffer>()
            let views = Dictionary<Symbol, BufferView>()
            let mutable defragRunning = 0
            let mutable version = 0

            let pushWrite (ig : IndexedGeometry) (ptr : managedptr) =
                for (att, b) in Dictionary.toSeq buffers do
                    match ig.IndexedAttributes.TryGetValue att with
                        | (true, arr) -> b.Emit [arr, ptr]
                        | _ -> ()

            let startDefragment() =
                let startVersion = version

                let task = 
                    Task.Factory.StartNew(fun () ->
                        let newLayout = MergedAttributeLayout(attributeTypes)

                        // TODO: defragmentation
                        ()
                    )
                ()

            let possiblyCreatedHole () =
                let rel = float layout.Allocated / float layout.Capacity

                if rel < 0.333333333 then
                    let wasRunning = Interlocked.Exchange(&defragRunning, 1)
                    if wasRunning = 0 then startDefragment()

            member x.TryGetBuffer(att : Symbol) =
                match buffers.TryGetValue att with
                    | (true, b) -> Some b
                    | _ ->
                        match Map.tryFind att attributeTypes with
                            | Some et ->
                                let b = Buffer(et, x)
                                b.Resize(layout.Capacity)
                                Interlocked.Increment(&version) |> ignore

                                let writes =
                                    layout.Content 
                                        |> Seq.choose (fun (ig, ptr) ->
                                            match ig.IndexedAttributes.TryGetValue att with
                                                | (true, arr) -> Some (arr, ptr)
                                                | _ -> None
                                        )
                                        |> Seq.toList

                                b.Emit writes
                                buffers.[att] <- b

                                Some b

                            | _ ->
                                None

            member x.TryGetAttribute (att : Symbol) =
                match views.TryGetValue att with
                    | (true, view) -> Some view
                    | _ ->
                        match Map.tryFind att attributeTypes, x.TryGetBuffer att with
                            | Some et, Some buffer ->
                                let view = BufferView(buffer, et)
                                views.[att] <- view
                                Some view
                            | _ ->
                                None

            member x.DrawCallInfos =
                Mod.custom (fun self ->
                    x.GetValue self |> ignore
                    layout.GetDrawCallInfos()
                )


            member x.Dispose() =
                // TODO: proper disposal
                ()

            interface IDisposable with
                member x.Dispose() = x.Dispose()

            interface IAttributeProvider with
                member x.TryGetAttribute att = x.TryGetAttribute att
                member x.All = views |> Dictionary.toSeq


            override x.Compute() =
                let deltas = reader.GetDelta x

                if not (List.isEmpty deltas) then
                    Interlocked.Increment(&version) |> ignore

                for d in deltas do
                    match d with
                        | Add g -> 
                            assert (isNull g.IndexArray)
                            let mptr = layout.Alloc g
                            pushWrite g mptr
                        | Rem g -> 
                            layout.Free g
                            // TODO: defragment memory when too many holes

                for b in buffers.Values do
                    b.Resize(layout.Capacity)

                null



    [<Semantic>]
    type GeometrySetRenderObjectSem() =
        member x.RenderObjects(g : Sg.GeometrySet) : aset<IRenderObject> =
            let scope = Ag.getContext()
            let rj = RenderObject.Create()
            
            rj.AttributeScope <- scope 
            rj.IsActive <- g.IsActive
            rj.RenderPass <- g.RenderPass
            rj.DepthTest <- g.DepthTestMode
            rj.CullMode <- g.CullMode
            rj.FillMode <- g.FillMode
            rj.StencilMode <- g.StencilMode
            rj.BlendMode <- g.BlendMode
            rj.Surface <- g.Surface
            rj.Mode <- Mod.constant g.Mode


            let att = new MergedAttributes.MergedAttributeProvider(g.Mode, g.AttributeTypes, g.Geometries)
            rj.Uniforms <- new Providers.UniformProvider(scope, g?Uniforms, [att :> IAttributeProvider])
            rj.VertexAttributes <- att
            rj.InstanceAttributes <- new Providers.AttributeProvider(scope, "InstanceAttributes")
            rj.DrawCallInfos <- att.DrawCallInfos
            rj.Indices <- null

            
            
            
            ASet.single (rj :> IRenderObject)

        member x.LocalBoundingBox(g : Sg.GeometrySet) : IMod<Box3d> =
            failwith "not implemented"