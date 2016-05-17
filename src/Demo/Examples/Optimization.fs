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

    let div2Floor (v : int64) =
        if v % 2L = 0L then v / 2L
        else (v - 1L) / 2L

    let div2AZ (v : int64) =
        if v % 2L = 0L then v / 2L
        elif v > 0L then (v + 1L) / 2L
        else (v - 1L) / 2L

    type GridCell =
        struct
            val mutable public X : int64
            val mutable public Y : int64
            val mutable public Z : int64
            val mutable public Exp : int

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


            member x.Parent =
                if x.Exp % 2 = 0 then
                    GridCell(div2Floor (1L + x.X), div2Floor (1L + x.Y), div2Floor (1L + x.Z), x.Exp + 1)
                else
                    GridCell(div2Floor x.X, div2Floor x.Y, div2Floor x.Z, x.Exp + 1)

            override x.ToString() =
                sprintf "GridCell(%dL, %dL, %dL, %d)" x.X x.Y x.Z x.Exp

            new(x,y,z,e) = { X = x; Y = y; Z = z; Exp = e }
            new(x : int,y : int,z : int,e : int) = { X = int64 x; Y = int64 y; Z = int64 z; Exp = e }
        end

    module Check =
        open System

        let parentContains (c : GridCell) =
            c.Parent.BoundingBox.Contains c.BoundingBox

        let printBad (cell : GridCell) =
            let self = cell.BoundingBox
            let parent = cell.Parent.BoundingBox

            printfn "cell:   %A" cell
            printfn "self:   %A" self
            printfn "parent: %A" parent


        let test() =
            let r = Random()

            let randomIndex() = r.Next(1 <<< 16) - 1 <<< 15
            let randomExp() = (r.Next(32) - 16) //- 16

            let randomCell () = GridCell(randomIndex(), randomIndex(), randomIndex(), randomExp())

            for i in 0..1000000 do
                let cell = randomCell()
                if not (parentContains cell) then
                    printBad cell
                    failwith "asdasdasd"


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







