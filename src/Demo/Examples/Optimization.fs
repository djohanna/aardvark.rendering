module Optimization

open System
open System.Threading
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph
open Aardvark.Base.Incremental.Operators
open Aardvark.SceneGraph.Semantics
open Aardvark.Rendering.NanoVg
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

module Persistence =
    open System.IO
    open System.IO.Compression
    open Aardvark.Base.Runtime

    type IBlobStore =
        inherit IDisposable
        abstract member Contains : string -> bool
        abstract member Load : string -> 'a
        abstract member Store : string * 'a -> unit

    type private ArrayCoerce<'a, 'b when 'a : unmanaged and 'b : unmanaged>() =
            
        static let sa = sizeof<'a>
        static let sb = sizeof<'b>

//            static let lengthOffset = sizeof<nativeint> |> nativeint
//            static let typeOffset = 2n * lengthOffset

        static let idb : nativeint =
            let gc = GCHandle.Alloc(Array.zeroCreate<'b> 1, GCHandleType.Pinned)
            try 
                let ptr = gc.AddrOfPinnedObject() |> NativePtr.ofNativeInt
                NativePtr.get ptr -2
            finally
                gc.Free()


        static member Coerce (a : 'a[]) : 'b[] =
            let newLength = (a.Length * sa) / sb |> nativeint
            let gc = GCHandle.Alloc(a, GCHandleType.Pinned)
            try
                let ptr = gc.AddrOfPinnedObject() |> NativePtr.ofNativeInt<nativeint>

                NativePtr.set ptr -1 newLength
                NativePtr.set ptr -2 idb

                a |> unbox<'b[]>

            finally
                gc.Free()

        static member CoercedApply (f : 'b[] -> 'r) (a : 'a[]) : 'r =
            let newLength = (a.Length * sa) / sb |> nativeint
            let gc = GCHandle.Alloc(a, GCHandleType.Pinned)
            try
                let ptr = gc.AddrOfPinnedObject() |> NativePtr.ofNativeInt<nativeint>

                let oldLength = NativePtr.get ptr -1
                let oldType = NativePtr.get ptr -2

                NativePtr.set ptr -1 newLength
                NativePtr.set ptr -2 idb

                try
                    a |> unbox<'b[]> |> f
                finally
                    NativePtr.set ptr -1 oldLength
                    NativePtr.set ptr -2 oldType

            finally
                gc.Free()

    type StreamWriter(stream : Stream) =
        let bin = new BinaryWriter(stream)

        member x.WritePrimitive (v : 'a) =
            let mutable v = v
            let ptr : byte[] = &&v |> NativePtr.cast |> NativePtr.toArray sizeof<'a>
            code { return bin.Write(ptr) }

        interface IWriter with
            member x.WritePrimitive (v : 'a) = x.WritePrimitive v
            member x.WritePrimitiveArray(data : 'a[]) =
                code {
                    if isNull data then
                        do! x.WritePrimitive -1
                    else
                        do! x.WritePrimitive data.Length
                        data |> ArrayCoerce<'a, byte>.CoercedApply bin.Write
                }

            member x.WriteBool(v : bool) =
                code { return bin.Write(v) }

            member x.WriteString (str : string) =
                code { return bin.Write(str) }

        interface IReader with
            member x.ReadPrimitive() = failwith "[StreamWriter] cannot read"
            member x.ReadPrimitiveArray() = failwith "[StreamWriter] cannot read"
            member x.ReadBool() = failwith "[StreamWriter] cannot read"
            member x.ReadString() = failwith "[StreamWriter] cannot read"

        interface ICoder with
            member x.IsReading = false

        interface IDisposable with
            member x.Dispose() = bin.Dispose()

    type StreamReader(stream : Stream) =
        let bin = new BinaryReader(stream)

        member x.ReadPrimitive () : Code<'a> =
            let read() : 'a =
                let arr = bin.ReadBytes sizeof<'a>
                let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
                let res = gc.AddrOfPinnedObject() |> NativePtr.ofNativeInt |> NativePtr.read
                gc.Free()
                res

            code { return read() }
        interface IReader with
            member x.ReadPrimitive () = x.ReadPrimitive()

            member x.ReadPrimitiveArray() : Code<'a[]> =
                code {
                    let! length = x.ReadPrimitive()
                    if length < 0 then
                        return null
                    else
                        let data = bin.ReadBytes(sizeof<'a> * length)
                        return ArrayCoerce<byte, 'a>.Coerce data
                }

            member x.ReadBool() =
                code { return bin.ReadBoolean() }

            member x.ReadString () =
                code { return bin.ReadString() }

        interface IWriter with
            member x.WritePrimitive _ = failwith "[StreamReader] cannot write"
            member x.WritePrimitiveArray _ = failwith "[StreamReader] cannot write"
            member x.WriteBool _ = failwith "[StreamReader] cannot write"
            member x.WriteString _ = failwith "[StreamReader] cannot write"

        interface ICoder with
            member x.IsReading = true

        interface IDisposable with
            member x.Dispose() = bin.Dispose()


    type ZipStore(file : string) =
        let stream = File.Open(file, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        let archive = new ZipArchive(stream, ZipArchiveMode.Update)
        

        member x.Contains (name : string) =
            let e = archive.GetEntry name
            not (isNull e)

        member x.Store(name : string, value : 'a) =
            let e = archive.GetEntry(name)
            let e = 
                if isNull e then archive.CreateEntry(name)
                else e

            use w = new StreamWriter(e.Open())
            w.Write value

        member x.Load(name : string) : 'a =
            let e = archive.GetEntry(name)
            if isNull e then 
                failwithf "[ZipStore] could not get entry: %A" name

            use r = new StreamReader(e.Open())
            r.Read()

        member x.Dispose() =
            stream.Flush()
            archive.Dispose()
            stream.Dispose()

        interface IBlobStore with
            member x.Contains(name) = x.Contains(name)
            member x.Load(name) = x.Load(name)
            member x.Store(name, value) = x.Store(name, value)
            member x.Dispose() = x.Dispose()

module TreeStuff =

    type Value<'a>(create : unit -> 'a) =
        let mutable cache = None

        member x.Value =
            lock x (fun () ->
                match cache with
                    | Some c -> c
                    | None -> 
                        let c = create()
                        cache <- Some c
                        c
            )

        member x.Destroy() =
            lock x (fun () ->
                cache <- None
            )

    module Value =
        let create (f : unit -> 'a) = Value f
        let force (v : Value<'a>) = v.Value

    type ValueBuilder() =
        member x.Bind(v : Value<'a>, f : 'a -> Value<'b>) =
            Value(fun () -> v.Value |> f |> Value.force)

        member x.Return(v : 'a) = Value.create (fun () -> v)

        member x.Delay(f : unit -> Value<'a>) = Value(fun () -> f().Value)
    
    let value = ValueBuilder()


    type Point =
        struct
            val mutable public Position : V3f
            val mutable public Color : C4b

            new(p,c) = { Position = p; Color = c }
        end

    type Cell = CSharpDemo.Cell

    [<AutoOpen>]
    module private Utils =
        let inline div2Floor (v : int64) =
            if v % 2L = 0L then v / 2L
            else (v - 1L) / 2L

        let inline div2Ceil (v : int64) =
            if v % 2L = 0L then v / 2L
            else (v + 1L) / 2L


        let floor3d (v : V3d) =
            V3l(v.X |> floor |> int64, v.Y |> floor |> int64, v.Z |> floor |> int64)

    [<CustomEquality; NoComparison>]
    type GridCell =
        struct
            val mutable public X : int64
            val mutable public Y : int64
            val mutable public Z : int64
            val mutable public Exp : int

            static member Containing (box : Box3d) =
                let s = box.Size
                let e = s.NormMax |> Fun.Log2 |> ceil |> int

                let size = pown 2.0 e

                let shift = 
                    let ehalf = floor (float (e + 1) * 0.5) |> int
                    -((pown 4.0 ehalf) - 1.0) / 3.0

                let i = (box.Min - shift) / size |> floor3d
                let mutable cell = GridCell(i.X, i.Y, i.Z, e)

                while not (cell.Contains box) do
                    cell <- cell.Parent

                cell

            member x.BoundingBox =
                let size = pown 2.0 x.Exp

                let shift = 
                    let ehalf = floor (float (x.Exp + 1) * 0.5) |> int
                    let shift = -((pown 4.0 ehalf) - 1.0) / 3.0
                    shift / size

                Box3d(
                    (shift + float x.X) * size          |> float,
                    (shift + float x.Y) * size          |> float,
                    (shift + float x.Z) * size          |> float,
                    (shift + float x.X + 1.0) * size    |> float,
                    (shift + float x.Y + 1.0) * size    |> float,
                    (shift + float x.Z + 1.0) * size    |> float
                )

            member x.Contains (p : V3d) =
                x.BoundingBox.Contains p

            member x.Contains (p : Box3d) =
                x.BoundingBox.Contains p

            member x.Parent =
                if x.Exp % 2 = 0 then GridCell(div2Ceil x.X, div2Ceil x.Y, div2Ceil x.Z, x.Exp + 1)
                else GridCell(div2Floor x.X, div2Floor x.Y, div2Floor x.Z, x.Exp + 1)

            member x.Children =
                let e = x.Exp - 1
                let z = x.Z * 2L
                let y = x.Y * 2L
                let x = x.X * 2L
                [|
                    GridCell(x,    y,    z,    e)
                    GridCell(x,    y,    z+1L, e)
                    GridCell(x,    y+1L, z,    e)
                    GridCell(x,    y+1L, z+1L, e)
                    GridCell(x+1L, y,    z,    e)
                    GridCell(x+1L, y,    z+1L, e)
                    GridCell(x+1L, y+1L, z,    e)
                    GridCell(x+1L, y+1L, z+1L, e)
                |]

            member x.GetChild (index : int) =
                let xc = (index >>> 2) &&& 0x1 |> int64
                let yc = (index >>> 1) &&& 0x1 |> int64
                let zc = (index >>> 0) &&& 0x1 |> int64
                GridCell(2L * x.X + xc, 2L * x.Y + yc, 2L * x.Z + zc, x.Exp - 1)

            member x.Index = V3l(x.X, x.Y, x.Z)

            override x.ToString() =
                sprintf "{ Index = (%d, %d, %d); Exp = %d }" x.X x.Y x.Z x.Exp

            override x.GetHashCode() =
                HashCode.Combine(x.X.GetHashCode(), x.Y.GetHashCode(), x.Z.GetHashCode(), x.Exp)

            override x.Equals o =
                match o with
                    | :? GridCell as o -> x.X = o.X && x.Y = o.Y && x.Z = o.Z && x.Exp = o.Exp
                    | _ -> false

            new(x,y,z,e) = { X = x; Y = y; Z = z; Exp = e }
            new(x : int,y : int,z : int,e : int) = { X = int64 x; Y = int64 y; Z = int64 z; Exp = e }
        end

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GridCell =
        let inline ofBox (b : Box3d) = GridCell.Containing b
        let inline parent (c : GridCell) = c.Parent
        let inline children (c : GridCell) = c.Children
        let inline child (i : int) (c : GridCell) = c.GetChild i
        let inline bounds (c : GridCell) = c.BoundingBox


    module Check =
        open System

        let parentContains (c : GridCell) =
            c.Parent.BoundingBox.Contains c.BoundingBox

        let isChildOfParent (c : GridCell) =
            c.Parent.Children |> Array.exists (fun ci -> ci = c)

        let isSelfContained (c : GridCell) =
            let bb = c.BoundingBox
            c = GridCell.Containing (bb.ShrunkBy(bb.SizeX * 0.01))

        let childEnumeration (c : GridCell) =
            c.Children |> Array.indexed |> Array.forall (fun (i,ci) -> c.GetChild(i) = ci)


        let printBad (cell : GridCell) =
            let self = cell.BoundingBox
            let parent = cell.Parent.BoundingBox

            printfn "cell:      %A" cell
            printfn "parent:    %A" cell.Parent
            printfn "children:  %A" cell.Parent.Children
            printfn "bself:     %A" self
            printfn "bparent:   %A" parent


        let test() =
            let r = Random()

            let randomIndex() = r.Next(1 <<< 20) - 1 <<< 19
            let randomExp() = r.Next(32) - 16
            let randomCell () = GridCell(randomIndex(), randomIndex(), randomIndex(), randomExp())

            for i in 0..100000 do
                let cell = randomCell()
                if not (parentContains cell) then
                    printBad cell
                    failwith "containment"

                if not (isChildOfParent cell) then
                    printBad cell
                    failwith "relation"

                if not (isSelfContained cell) then
                    printBad cell
                    failwith "self contained"     
                                  
                if not (childEnumeration cell) then
                    printBad cell
                    failwith "enum"    



    type Node =
        | Empty 
        | Leaf of int * Value<Point[]>
        | Node of int * Value<Point[]> * Value<Node>[]

    type Octree = { splitThreshold : int; bounds : Box3d; cell : Cell; root : Node }



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Node =
        
        let count (n : Node) =
            match n with
                | Empty -> 0
                | Leaf(c,_) -> c
                | Node(c,_,_) -> c

        let childBoxes (bounds : Box3d) =
            let l = bounds.Min
            let c = bounds.Center
            let h = bounds.Max
            [|
                Box3d(l.X, l.Y, l.Z, c.X, c.Y, c.Z)
                Box3d(l.X, l.Y, c.Z, c.X, c.Y, h.Z)
                Box3d(l.X, c.Y, l.Z, c.X, h.Y, c.Z)
                Box3d(l.X, c.Y, c.Z, c.X, h.Y, h.Z)
                Box3d(c.X, l.Y, l.Z, h.X, c.Y, c.Z)
                Box3d(c.X, l.Y, c.Z, h.X, c.Y, h.Z)
                Box3d(c.X, c.Y, l.Z, h.X, h.Y, c.Z)
                Box3d(c.X, c.Y, c.Z, h.X, h.Y, h.Z)
            |]

        let rec build (splitThreshold : int) (box : Box3d) (points : Point[]) : Node =
            if points.Length = 0 then
                Empty

            elif points.Length < splitThreshold then
                Leaf(points.Length, value { return points })

            else
                let childBoxes = childBoxes box

                let childData = 
                    childBoxes |> Array.map (fun b ->
                        points |> Array.filter (fun p -> b.Contains (V3d p.Position))
                    )

                let children =
                    Array.init 8 (fun i ->
                        value {
                            return build splitThreshold childBoxes.[i] childData.[i]
                        }
                    )

                Node(points.Length, value { return [||]}, children)

            

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Octree =
        
        let count (t : Octree) =
            t.root |> Node.count
        
        let isEmpty (t : Octree) =
            match t.root with
                | Empty -> true
                | _ -> false

        let empty (splitThreshold : int) = { splitThreshold = splitThreshold; bounds = Unchecked.defaultof<Box3d>; cell = Unchecked.defaultof<Cell>; root = Empty }


        let addContained (points : Point[]) (t : Octree) =
            let rec add (bounds : Box3d) (points : Point[]) (n : Node) =
                match n with
                    | Empty -> 
                        Node.build t.splitThreshold bounds points

                    | Leaf(cnt, data) ->
                        if cnt + points.Length < t.splitThreshold then
                            Leaf (
                                cnt + points.Length,
                                value {
                                    let! data = data
                                    return Array.append points data
                                }
                            )
                        else
                            Node.build t.splitThreshold bounds points

                    | Node(cnt, data, children) ->
                        let childBoxes = Node.childBoxes bounds

                        let childData = 
                            childBoxes |> Array.map (fun b ->
                                points |> Array.filter (fun p -> b.Contains (V3d p.Position))
                            )

                        let newChildren =
                            Array.map3 (fun b node d ->
                                value {
                                    let! node = node
                                    return add b d node
                                }
                            ) childBoxes children childData

                        Node(cnt + points.Length, data, newChildren)

            { t with root = add t.bounds points t.root}

        let add (points : Point[]) (t : Octree) =
            if isEmpty t then
                let bounds = Box3d(points |> Seq.map (fun p -> V3d p.Position))
                let cell = Cell(bounds)
                { t with bounds = bounds; cell = cell; root = Node.build t.splitThreshold cell.BoundingBox points }

            else
                let inside, outside = points |> Array.partition (fun p -> t.bounds.Contains (V3d p.Position))
                let t = addContained inside t

                if outside.Length = 0 then
                    t
                else
                    let bounds = Box3d(points |> Seq.map (fun p -> V3d p.Position))

                    let root = t.root
                    let cnt = Node.count root

                    let rec createNode (cell : Cell) =
                        if cell = t.cell then
                            t.root

                        elif cell.Contains t.cell then
                            let children = t.cell.Children
                            Node(
                                cnt,
                                value { return [||] },
                                Array.init 8 (fun i ->
                                    value { return createNode children.[i] }
                                )
                            )

                        else
                            Empty

                    let newCell = Cell(bounds.Union t.cell.BoundingBox)
                    let newRoot = createNode newCell

                    addContained outside { 
                        t with 
                            cell = newCell
                            bounds = bounds.Union t.bounds
                            root = newRoot 
                        }

                        
            














let run() =
    Aardvark.Init()
        
    Console.CancelKeyPress.Add(fun e ->
        File.writeAllText @"C:\Users\schorsch\Desktop\sepp.txt" "hi sepp"
    )

    AppDomain.CurrentDomain.ProcessExit.AddHandler(EventHandler(fun s e ->
        File.writeAllText @"C:\Users\schorsch\Desktop\sepp.txt" "hi sepp"
    ))

    printfn "done"

    let store = new Persistence.ZipStore(@"C:\Users\schorsch\Desktop\test.zip")

    if not (store.Contains "a") then
        printfn "store"
        store.Store("a", 10)
  
    printfn "load"
    let res : int = store.Load("a")
    printfn "a = %A" res
    store.Dispose()
    Environment.Exit 0

    let app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()


    let quadSg =
        let index = [|0;1;2; 0;2;3|]
        let flatten (a : 'a[]) =
            index |> Array.map (fun i -> a.[i]) :> Array

        let quad =
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions,                  flatten [| V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
                        DefaultSemantic.Normals,                    flatten [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |]
                        DefaultSemantic.DiffuseColorCoordinates,    flatten [| V2f.OO; V2f.IO; V2f.II; V2f.OI |]
                    ]
            )
                
        quad |> Sg.ofIndexedGeometry


    let sg = 
        Sg.group' [
            for x in -3 .. 3 do
                for y in -3 .. 3 do
                    let t = Trafo3d.Translation(3.0 * float x, 3.0 * float y, 0.0)
                    yield quadSg |> Sg.trafo (Mod.constant t)
        ]

    let view = CameraView.LookAt(V3d(-2.0,-2.0,2.0), V3d.Zero, V3d.OOI)
    let proj = 
        win.Sizes 
            |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))


    let view = DefaultCameraController.control win.Mouse win.Keyboard win.Time view


    let sg =
        sg |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.Red |> toEffect
           ]
           |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
           |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)


    let renderJobs = sg.RenderObjects()

    let opt = renderJobs |> Optimizations.findIndirectDraws win.FramebufferSignature app.Runtime

    win.RenderTask <- app.Runtime.CompileRender(win.FramebufferSignature, opt) |> DefaultOverlays.withStatistics
    win.Run()







