#if INTERACTIVE
#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#else
namespace Examples
#endif


open System
open Aardvark.Base
    
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Interactive

open Default // makes viewTrafo and other tutorial specicific default creators visible

module GeometryHelper =

     let frustum (f : IMod<CameraView>) (proj : IMod<Frustum>) =
        let invViewProj = Mod.map2 (fun v p -> (CameraView.viewTrafo v * Frustum.projTrafo p).Inverse) f proj

        let positions = 
            [|
                V3f(-1.0, -1.0, -1.0)
                V3f(1.0, -1.0, -1.0)
                V3f(1.0, 1.0, -1.0)
                V3f(-1.0, 1.0, -1.0)
                V3f(-1.0, -1.0, 1.0)
                V3f(1.0, -1.0, 1.0)
                V3f(1.0, 1.0, 1.0)
                V3f(-1.0, 1.0, 1.0)
            |]

        let indices =
            [|
                1;2; 2;6; 6;5; 5;1;
                2;3; 3;7; 7;6; 4;5; 
                7;4; 3;0; 0;4; 0;1;
            |]

        let geometry =
            IndexedGeometry(
                Mode = IndexedGeometryMode.LineList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Colors, Array.create indices.Length C4b.Red :> Array
                    ]
            )

        geometry
            |> Sg.ofIndexedGeometry
            |> Sg.trafo invViewProj

module Sphere =
    open System.Collections.Generic


    let generate level =
        let vertices = List<_>()
        let indices = List<_>()

        let cache = Dictionary()
        let mutable index = 0
        
        let addVertex (p:V3f) = 
            vertices.Add <| Vec.normalize p
            index <- index + 1
            index - 1

        let emitTriangle (indices:List<_>) tri =
            indices.Add tri

        let getMiddlePoint p1 p2  =
            let small,great = if p1 < p2 then int64 p1,int64 p2 else int64 p2,int64 p1
            let key = (small <<< 32) + great
            match cache.TryGetValue key with
                | (false,_) -> 
                    let p1 = vertices.[p1]
                    let p2 = vertices.[p2]
                    let m = V3f.op_Multiply(0.5f,p1+p2)
                    let i = addVertex m
                    cache.[key] <- i
                    i
                | (true,v) -> v

        let t = (1.0 + Fun.Sqrt 5.0) / 2.0

        V3f(-1.0,  t, 0.0) |> addVertex |> ignore
        V3f( 1.0,  t, 0.0) |> addVertex |> ignore
        V3f(-1.0, -t, 0.0) |> addVertex |> ignore
        V3f( 1.0, -t, 0.0) |> addVertex |> ignore
       
        V3f( 0.0, -1.0,  t) |> addVertex |> ignore
        V3f( 0.0,  1.0,  t) |> addVertex |> ignore
        V3f( 0.0, -1.0, -t) |> addVertex |> ignore
        V3f( 0.0,  1.0, -t) |> addVertex |> ignore

        V3f(  t, 0.0, -1.0) |> addVertex |> ignore
        V3f(  t, 0.0,  1.0) |> addVertex |> ignore
        V3f( -t, 0.0, -1.0) |> addVertex |> ignore
        V3f( -t, 0.0,  1.0) |> addVertex |> ignore

        emitTriangle indices (0, 11, 5)
        emitTriangle indices (0, 5, 1)
        emitTriangle indices (0, 1, 7)
        emitTriangle indices (0, 7, 10)
        emitTriangle indices (0, 10, 11)
                      
        emitTriangle indices (1, 5, 9)
        emitTriangle indices (5, 11, 4)
        emitTriangle indices (11, 10, 2)
        emitTriangle indices (10, 7, 6)
        emitTriangle indices (7, 1, 8)
                      
        emitTriangle indices (3, 9, 4)
        emitTriangle indices (3, 4, 2)
        emitTriangle indices (3, 2, 6)
        emitTriangle indices (3, 6, 8)
        emitTriangle indices (3, 8, 9)

        emitTriangle indices (4, 9, 5)
        emitTriangle indices (2, 4, 11)
        emitTriangle indices (6, 2, 10)
        emitTriangle indices (8, 6, 7)
        emitTriangle indices (9, 8, 1)

        
        let rec run faces toGo = 
            if toGo = 0 then faces
            else
                let newFaces = List()
                for (v1,v2,v3) in faces do
                  let a = getMiddlePoint v1 v2
                  let b = getMiddlePoint v2 v3
                  let c = getMiddlePoint v3 v1
                  
                  emitTriangle newFaces (v1, a, c)
                  emitTriangle newFaces (v2, b, a)
                  emitTriangle newFaces (v3, c, b)
                  emitTriangle newFaces (a, b, c)
                run newFaces (toGo - 1)

        let indices = run indices level
        indices.ToArray() |> Array.collect (fun (a,b,c) -> [|a;b;c|]), vertices.ToArray()


module QuickTest = 

    
    Aardvark.Rendering.Interactive.FsiSetup.init (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug"])


//    let r = Random()
//    let range = 1.0
//    let offset = V3f(1,1,2)
//    let rv _ = V3f(r.NextDouble()*range,r.NextDouble()*range,r.NextDouble()*range) + offset
//    let randomPoints = Array.init 10000 rv
//
//    let sum = (Array.fold (+) V3f.OOO randomPoints)
//    let center = sum * (V3f (1.0 / float randomPoints.Length,1.0 / float randomPoints.Length,1.0 / float randomPoints.Length))
//    let radius = Vec.length <| (Array.maxBy (fun v -> let v : V3f = (v - center) in v.LengthSquared) randomPoints) - center
    let center = V3f.OOO
    let size = 1.0
    win.Size <- V2i(800,800)

//    let pointSg =
//        let quad =
//            let positions = randomPoints
//
//            IndexedGeometry(IndexedGeometryMode.PointList, null, 
//                SymDict.ofList [DefaultSemantic.Positions, positions :> Array], SymDict.empty)
//
//        // create a scenegraph given the indexedGeometry containing a quad
//        quad |> Sg.ofIndexedGeometry

    let bb = Box3d.FromCenterAndSize(V3d.op_Explicit center,V3d(size,size,size))
    let pointSg2 = Helpers.wireBox C4b.Green bb |> Sg.ofIndexedGeometry

    let sphereSg = 
        let quad =
            let indices,vertices = Sphere.generate 5
            IndexedGeometry(IndexedGeometryMode.PointList, indices, 
                SymDict.ofList [DefaultSemantic.Positions, vertices :> Array], SymDict.empty)
        quad |> Sg.ofIndexedGeometry


    let transparent = C4f(1.0,0.0,0.0,0.3)

    let radius = (float size) * Fun.Sqrt 2
    bb.GetBoundingSphere3d() |> printfn "%A"
    
    let a = bb.GetBoundingSphere3d().Radius
    printfn "%A ra:" a
    let vis =  Sg.trafo (Mod.constant <|  Trafo3d.Scale (1.0) * Trafo3d.Translation (0.0,0.0,0.0)) sphereSg 

    Aardvark.SceneGraph.Semantics.BoundingBoxes.Semantic.localBoundingBox vis |> Mod.force |> printfn "%A"
    Aardvark.SceneGraph.Semantics.BoundingBoxes.Semantic.localBoundingBox pointSg2 |> Mod.force |> printfn "%A"
    let s1 = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect      
                        DefaultSurfaces.constantColor transparent |> toEffect 
                    ]

    let s2 = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect      
                        DefaultSurfaces.constantColor C4f.Blue |> toEffect 
                    ]

    let camLoc = Mod.init V3d.OOO
    let view = viewTrafo   () 
    let rview = Mod.init view

    let cv = 
        camLoc |> Mod.map (fun loc -> 
            CameraView.lookAt loc (V3d.op_Explicit center) V3d.OOI
        )

    let hfov = 80.0
    let copy = 
        win.Sizes |> Mod.map (fun s -> Frustum.perspective hfov 0.1 50.0 (float s.X / float s.Y))
    let frustumVis = GeometryHelper.frustum cv copy

    win.Keyboard.KeyDown(Keys.Space).Values.Subscribe(fun _ ->
        transact (fun () ->     
            let view = Mod.force rview |> Mod.force
            let camPos = view |> CameraView.location
            let center = V3d.op_Explicit center

            let size = a * 2.0
            let aspect = float <| win.Size.X / win.Size.Y
            let hfov = hfov / 57.3;
            let vfov = Fun.Atan(Fun.Tan(hfov / 2.0) / aspect) * 2.0
            let dx = (size / 2.0) / Fun.Tan(vfov / 2.0)
            let dy = (size / 2.0) / Fun.Tan(hfov / 2.0)
            let d = max dx dy
            let p = (camPos - center).Normalized * (d + a/2.0) 
            let newPos = p + center

            printfn "newP :%A" p
            printfn "newCam :%A" newPos
            printfn "oldCam: %A" camPos
            Mod.change camLoc newPos

            let view =  CameraView.LookAt(newPos, center, V3d.OOI)
            let a = DefaultCameraController.control win.Mouse win.Keyboard win.Time view
            Mod.change rview a
        )
    ) |> ignore

    let sg =
            [ (*s1 pointSg2;*) s1 pointSg2; s2 frustumVis ; s2 vis ] 
            |> Sg.group
            |> Sg.depthTest (Mod.init Rendering.DepthTestMode.LessOrEqual)
            //|> Sg.blendMode (Mod.constant Rendering.BlendMode.Blend)
            |> Sg.viewTrafo ((Mod.bind id rview) |> Mod.map CameraView.viewTrafo )
            |> Sg.projTrafo (copy |> Mod.map Frustum.projTrafo    ) 


    let run () =
        Aardvark.Rendering.Interactive.FsiSetup.init (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug"])
        setSg sg
        win.Run()


open QuickTest

#if INTERACTIVE
setSg sg
printfn "Done. Modify sg and call setSg again in order to see the modified rendering result."
#endif