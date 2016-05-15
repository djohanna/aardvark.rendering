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







