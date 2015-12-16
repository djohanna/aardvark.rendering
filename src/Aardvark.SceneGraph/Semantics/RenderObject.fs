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

    


    module RangeTree =
        
        type private HalfRange =
            struct
                val mutable public Value : int
                val mutable public IsMax : bool

                override x.ToString() =
                    if x.IsMax then sprintf "Leq %d" x.Value
                    else sprintf "Geq %d" x.Value

                new(v,max) = { Value = v; IsMax = max }

            end

        let inline private Leq v = HalfRange(v, true)
        let inline private Geq v = HalfRange(v, false)
        let inline private (|Leq|Geq|) (r : HalfRange) =
            if r.IsMax then Leq r.Value
            else Geq r.Value

        type RangeTree = private { data : HalfRange[] } with
            member x.Item
                with get (i : int) =
                    let min = x.data.[2*i+0].Value
                    let max = x.data.[2*i+1].Value
                    Range1i(min, max)

            member x.Count = x.data.Length / 2

            interface System.Collections.IEnumerable with
                member x.GetEnumerator() = new RangeTreeEnumerator(x.data) :> System.Collections.IEnumerator

            interface IEnumerable<Range1i> with
                member x.GetEnumerator() = new RangeTreeEnumerator(x.data) :> _

        and private RangeTreeEnumerator(data : HalfRange[]) =
            let mutable index = -2

            member x.Current =
                Range1i(data.[index].Value , data.[index + 1].Value)

            interface System.Collections.IEnumerator with
                member x.MoveNext() =
                    index <- index + 2
                    index < data.Length

                member x.Reset() =
                    index <- -2

                member x.Current =
                    x.Current :> obj

            interface IEnumerator<Range1i> with
                member x.Current = x.Current
                member x.Dispose() = index <- -2


        let rec private find (data : HalfRange[]) (l : int) (r : int) (v : int) =
            if l > r then
                false, l
            else
                let m = (l + r) / 2
                let mv = data.[m].Value

                let cmp = compare mv v
                if cmp > 0 then
                    find data l (m-1) v
                elif cmp < 0 then
                    find data (m+1) r v
                else
                    true, m


        let private findNextGreater (tree : RangeTree) (v : int) =
            let (found, id) = find tree.data 0 (tree.data.Length-1) v

            if id < tree.data.Length && tree.data.[id].Value > v then Some(id)
            elif id + 1 < tree.data.Length then Some(id + 1)
            else None

        let private findPrevSmaller (tree : RangeTree) (v : int) =
            let (found, id) = find tree.data 0 (tree.data.Length-1) v

            if id < tree.data.Length && tree.data.[id].Value < v then Some(id)
            elif id > 0 then Some(id - 1)
            else None

        module private Array =
            let take (n : int) (arr : 'a[]) =
                Array.sub arr 0 n

            let skip (n : int) (arr : 'a[]) =
                Array.sub arr n (arr.Length - n)


        let empty = { data = [||] }

        let insert (range : Range1i) (tree : RangeTree) =
            
            let data = tree.data
            let min = findPrevSmaller tree range.Min
            let max = findNextGreater tree range.Max

            

            let newData = 
                match min, max with
                    | Some minId, Some maxId ->
                        match data.[minId], data.[maxId] with
                            | Leq _, Geq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Geq range.Min; Leq range.Max|]
                                    data |> Array.skip maxId
                                ]
                            | Geq _, Leq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    data |> Array.skip maxId
                                ]

                            | Geq _, Geq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Leq range.Max|]
                                    data |> Array.skip maxId
                                ]

                            | Leq _, Leq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Geq range.Min|]
                                    data |> Array.skip maxId
                                ]

                    | Some minId, None ->
                        match data.[minId] with
                            | Leq v ->
                                Array.concat [
                                    data |> Array.take (1 + minId)
                                    [|Geq range.Min; Leq range.Max|]
                                ]
                            | Geq v ->
                                Array.concat [
                                    data |> Array.take (1 + minId)
                                    [|Leq range.Max|]
                                ]
                    | None, Some maxId ->
                        match data.[maxId] with
                            | Geq v ->
                                Array.concat [
                                    [|Geq range.Min; Leq range.Max|]
                                    data |> Array.skip maxId
                                ]
                            | Leq v ->
                                Array.concat [
                                    [|Geq range.Min|]
                                    data |> Array.skip maxId
                                ]
                                
                    | None, None ->
                        [|Geq range.Min; Leq range.Max|]

            { data = newData }

        let remove (range : Range1i) (tree : RangeTree) =
            let data = tree.data
            let min = findPrevSmaller tree range.Min
            let max = findNextGreater tree range.Max

            let newData = 
                match min, max with
                    | Some minId, Some maxId ->
                        match data.[minId], data.[maxId] with
                            | Geq _, Leq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Leq range.Min; Geq range.Max|]
                                    data |> Array.skip maxId
                                ]
                            | Leq _, Geq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    data |> Array.skip maxId
                                ]

                            | Geq _, Geq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Leq range.Min|]
                                    data |> Array.skip maxId
                                ]

                            | Leq _, Leq _ ->
                                Array.concat [
                                    data |> Array.take (minId + 1)
                                    [|Geq range.Max|]
                                    data |> Array.skip maxId
                                ]

                    | Some minId, None ->
                        match data.[minId] with
                            | Leq v ->
                                data |> Array.take (1 + minId)
                            | Geq v ->
                                Array.concat [
                                    data |> Array.take (1 + minId)
                                    [|Leq range.Min|]
                                ]
                    | None, Some maxId ->
                        match data.[maxId] with
                            | Geq v ->
                                data |> Array.skip maxId
                            | Leq v ->
                                Array.concat [
                                    [|Geq range.Max|]
                                    data |> Array.skip maxId
                                ]
                                
                    | None, None ->
                        [||]

            { data = newData }

        let toSeq (tree : RangeTree) =
            seq {
                for i in 0..2..tree.data.Length-1 do
                    let min = tree.data.[i]
                    let max = tree.data.[i+1]
                    yield Range1i(min.Value, max.Value)
            }

        let toList (tree : RangeTree) =
            [
                for i in 0..2..tree.data.Length-1 do
                    let min = tree.data.[i]
                    let max = tree.data.[i+1]
                    yield Range1i(min.Value, max.Value)
            ]

        let toArray (tree : RangeTree) =
            tree |> toSeq |> Seq.toArray

        let ofSeq (ranges : seq<Range1i>) =
            let mutable t = empty
            for r in ranges do
                t <- insert r t

            t

        let ofList (ranges : list<Range1i>) =
            ofSeq ranges

        let ofArray (ranges : Range1i[]) =
            ofSeq ranges

        let count (t : RangeTree) =
            t.Count

        let get (i : int) (t : RangeTree) =
            t.[i]

        let merge (l : RangeTree) (r : RangeTree) =
            if l.Count > r.Count then
                let mutable res = l
                for range in r do
                    res <- insert range res

                res
            else
                let mutable res = r
                for range in l do 
                    res <- insert range l
                res

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