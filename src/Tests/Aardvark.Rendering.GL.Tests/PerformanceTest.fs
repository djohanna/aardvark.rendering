namespace PerformanceTests

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.SceneGraph.Semantics
open Aardvark.Application.WinForms


[<AutoOpen>]
module GeometryUtilities =

    open System
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics

    module Helpers =

        let box (color : C4b) (box : Box3d) = 

            let randomColor = color //C4b(rand.Next(255) |> byte, rand.Next(255) |> byte, rand.Next(255) |> byte, 255uy)

            let indices =
                [|
                    1;2;6; 1;6;5
                    2;3;7; 2;7;6
                    4;5;6; 4;6;7
                    3;0;4; 3;4;7
                    0;1;5; 0;5;4
                    0;3;2; 0;2;1
                |]

            let positions = 
                [|
                    V3f(box.Min.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Max.Y, box.Max.Z)
                    V3f(box.Min.X, box.Max.Y, box.Max.Z)
                |]

            let normals = 
                [| 
                    V3f.IOO;
                    V3f.OIO;
                    V3f.OOI;

                    -V3f.IOO;
                    -V3f.OIO;
                    -V3f.OOI;
                |]

            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,

                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                        DefaultSemantic.Colors, indices |> Array.map (fun _ -> randomColor) :> Array
                    ]

            )

module PerformanceTest =


    let run () =

        Aardvark.Init()

        use app = new OpenGlApplication()
        let win = app.CreateGameWindow(1)

        let initialView = CameraView.LookAt(V3d.III, V3d.OOO, V3d.OOI)
        let perspective = 
            win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.01 1000.0 (float s.X / float s.Y))
        let cameraView  = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialView

        let candidates = 
            [| for _ in 0 .. 8 do yield Helpers.box C4b.Red Box3d.Unit |> Sg.ofIndexedGeometry |]

        let scale = 100.0
        let rnd = Random()
        let nextTrafo () =
            let x,y,z = rnd.NextDouble(), rnd.NextDouble(), rnd.NextDouble()
            Trafo3d.Translation(x*scale,y*scale,z*scale) 

        let objects = 
            [ for x in 1 .. 25000 do
                let r = rnd.Next(candidates.Length)
                yield Sg.trafo (nextTrafo () |> Mod.constant) candidates.[r]
            ] |> Sg.group

        //let transparency = RenderPass.after "nam" RenderPassOrder.BackToFront RenderPass.main
       
        let sg =
            objects
                |> Sg.viewTrafo (cameraView  |> Mod.map CameraView.viewTrafo )
                |> Sg.projTrafo (perspective |> Mod.map Frustum.projTrafo    )
                |> Sg.effect [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor (C4f(1.0,1.0,1.0,0.2)) |> toEffect ]
                //|> Sg.pass transparency
                |> Sg.blendMode (Mod.constant BlendMode.None)

        let config = BackendConfiguration.NativeOptimized
        win.RenderTask <- app.Runtime.CompileRender(win.FramebufferSignature, config, sg.RenderObjects()) 

        win.Run()

module RenderTaskPerformance =


    let run () =
        Aardvark.Init()

        use app = new OpenGlApplication()
        let win = app.CreateGameWindow(1)

        let initialView = CameraView.LookAt(V3d.III, V3d.OOO, V3d.OOI)
        let perspective = 
            win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
        let cameraView  = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialView

        let candidates = 
            [| for _ in 0 .. 8 do yield Helpers.box C4b.Red Box3d.Unit |> Sg.ofIndexedGeometry |]

        let scale = 100.0
        let rnd = Random()
        let nextTrafo () =
            let x,y,z = rnd.NextDouble(), rnd.NextDouble(), rnd.NextDouble()
            Trafo3d.Translation(x*scale,y*scale,z*scale) 

        let objects = 
            [ for x in 0 .. 20 do
                let r = rnd.Next(candidates.Length)
                yield Sg.trafo (nextTrafo () |> Mod.constant) candidates.[r]
            ] |> Sg.group

        let renderObjects = Semantics.RenderObjectSemantics.Semantic.renderObjects objects

        //let transparency = RenderPass.after "nam" RenderPassOrder.Arbitrary RenderPass.main

        let renderObjects =
            objects
                |> Sg.viewTrafo (cameraView  |> Mod.map CameraView.viewTrafo )
                |> Sg.projTrafo (perspective |> Mod.map Frustum.projTrafo    )
                |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor (C4f(1.0,1.0,1.0,0.2)) |> toEffect ]
                |> Sg.blendMode (Mod.constant BlendMode.Blend)
                |> Semantics.RenderObjectSemantics.Semantic.renderObjects


        let framebuffers =
            [| for i in 0 .. 6 do 
                let color = app.Runtime.CreateRenderbuffer(win.Sizes.GetValue(),RenderbufferFormat.Rgba8,1) :> IFramebufferOutput
                let depth = app.Runtime.CreateRenderbuffer(win.Sizes.GetValue(),RenderbufferFormat.Depth24Stencil8, 1) :> IFramebufferOutput
                yield
                    app.Runtime.CreateFramebuffer(
                        win.FramebufferSignature, [
                                DefaultSemantic.Colors, color
                                DefaultSemantic.Depth, depth
                            ]
                        )
             |]

        let config = BackendConfiguration.NativeOptimized
        let r = System.Random()
        let renderTasks = 
            [ for i in 0 .. 10 do
                let task = app.Runtime.CompileRender(win.FramebufferSignature, config, renderObjects)
                yield task, framebuffers.[r.Next(framebuffers.Length-1)]
            ]

        let customTask = 
            RenderTask.custom (fun (s,output) ->
                for i in 0 .. 100 do
                    for (r,fbo) in renderTasks do
                        r.Run output.framebuffer |> ignore
                RenderingResult(output.framebuffer, FrameStatistics.Zero)
            )

        win.RenderTask <- customTask

        win.Run()
    
    
(*

good 2.0.0

unscientific approximate numbers on GTX980
25k objects:
managedOptimized 19   fps => 4.7K
nativeoptimized  143  fps => 3.6M
glvm/opt         111  fps => 2.7M
glvm/nopt        20   fps => 0.5M
interpreter      ??? 



bad: 2.2.1

unscientific approximate numbers on GTX980
25k objects:
managedOptimized 12  fps => 300k draw calls
nativeoptimized  30  fps => 750k draw calls
glvm/opt         30  fps => 750k draw calls
glvm/nopt        15  fps => 375k draw calls
interpreter      0.22fps => 5.5k draw calls


5k sorted
30 => 150k

renderTasks:
6 fps, 100 * 10 per frame * 20 objects => 120k draw calls
*)