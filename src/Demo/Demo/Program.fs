
open Demo
open System
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.AgHelpers
open Aardvark.Rendering.GL
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.CSharp
open Aardvark.Base.Rendering
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.SceneGraph.CSharp
open Aardvark.SceneGraph.Semantics

open Aardvark.Application
open Aardvark.Application.WinForms




let quadSg =
    let quad =
        let index = [|0;1;2; 0;2;3|]
        let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]

        IndexedGeometry(IndexedGeometryMode.TriangleList, index, SymDict.ofList [DefaultSemantic.Positions, positions :> Array], SymDict.empty)


    quad |> Sg.ofIndexedGeometry


module Shader =
    open FShade

    let diffuseTex = 
        sampler2d {
            texture uniform?DiffuseColorTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }

    type Vertex = 
        { 
            [<Position>]        pos     : V4d 
            [<Normal>]          n       : V3d
            [<TexCoord>]        tc      : V2d
            [<WorldPosition>]   wp      : V4d
            [<Color>]           color   : V3d
        }

    let vs (v : Vertex)  = 
        vertex {
            let wp = uniform.ModelTrafo * v.pos

            return 
                { v with 
                    pos = uniform.ViewProjTrafo * wp; 
                    n = uniform.NormalMatrix * v.n; 
                    wp = wp 
                }
        }   

    let pvLight (v : Vertex)  = 
        vertex {
            let wp = uniform.ModelTrafo * v.pos

            let id = V3d.III
            return 
                { v with 
                    pos = uniform.ViewProjTrafo * wp; 
                    n = uniform.NormalMatrix * v.n; 
                    wp = wp 
                    color = id * Vec.dot v.n.Normalized id.Normalized
                }
        }   

    let pvFrag (v : Vertex) =
        fragment {
            let color = diffuseTex.Sample(v.tc).XYZ + v.color * 0.001
            return V4d(color, 1.0)
        }
        


    let ps (v : Vertex) =
        fragment {

            let c = uniform.CameraLocation - v.wp.XYZ |> Vec.normalize
            let n = v.n |> Vec.normalize

            let c = abs (Vec.dot c n) * 0.8 + 0.2

            let color = diffuseTex.Sample(v.tc)

            return V4d(c * color.XYZ, color.W)
        }


    let effect = compose [toEffect vs; toEffect ps]

[<AutoOpen>]
module Main =
    type Trafo3d with
        static member ChangeYZ =
            let fw =
                M44d(1.0,0.0,0.0,0.0,
                     0.0,0.0,-1.0,0.0,
                     0.0,1.0,0.0,0.0,
                     0.0,0.0,0.0,1.0)

            Trafo3d(fw, fw)



[<EntryPoint>]
[<STAThread>]
let main args = 
    System.Diagnostics.Debugger.Break()

    Ag.initialize()
    Aardvark.Init()

    let modelPath = match args |> Array.toList with
                      | []     -> printfn "using default eigi model."; System.IO.Path.Combine( __SOURCE_DIRECTORY__, "eigi", "eigi.dae") 
                      | [path] -> printfn "using path: %s" path; path
                      | _      -> failwith "usage: Demo.exe | Demo.exe modelPath"
    
    //let modelPath =  @"C:\Users\Schorsch\Desktop\bench\4000_128_2000_9.dae"

    //let modelPath =  @"E:\Development\VulkanSharp\bin\Release\Sponza_bunt\sponza_cm.obj"

    DynamicLinker.tryUnpackNativeLibrary "Assimp" |> ignore

    use app = new OpenGlApplication()
    let f = app.CreateSimpleRenderWindow(1)
    let ctrl = f //f.Control

    let view = CameraView.LookAt(V3d(2.0,2.0,2.0), V3d.Zero, V3d.OOI)
    let proj = CameraProjectionPerspective(60.0, 0.1, 10000.0, float (ctrl.Sizes.GetValue().X) / float (ctrl.Sizes.GetValue().Y))
    let mode = Mod.init FillMode.Fill

    let sg = Assimp.load modelPath

    let normalizeTo (target : Box3d) (sg : ISg) =
        let source = sg.LocalBoundingBox().GetValue()
        
        let sourceSize = source.Size
        let scale =
            if sourceSize.MajorDim = 0 then target.SizeX / sourceSize.X
            elif sourceSize.MajorDim = 1 then target.SizeY / sourceSize.Y
            else target.SizeZ / sourceSize.Z



        let trafo = Trafo3d.Translation(-source.Center) * Trafo3d.Scale(scale) * Trafo3d.Translation(target.Center)

        sg |> Sg.trafo (Mod.constant trafo)


    let view = DefaultCameraController.control ctrl.Mouse ctrl.Keyboard ctrl.Time view

    let color = Mod.init C4f.Red

    f.Keyboard.KeyDown(Keys.C).Values.Subscribe(fun () ->
        let v = C4f(C3f.White - (Mod.force color).ToC3f())
        transact (fun () ->
            Mod.change color v
        )
    ) |> ignore

    let pointSize = Mod.constant <| V2d(0.06, 0.08)

    let sg =
        sg |> Sg.effect [
                //Shader.pvLight |> toEffect
                //Shader.pvFrag  |> toEffect
                //DefaultSurfaces.trafo |> toEffect
                //DefaultSurfaces.pointSurface pointSize |> toEffect
                //DefaultSurfaces.uniformColor color |> toEffect
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
              ]
           |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
           |> Sg.projTrafo proj.ProjectionTrafos.Mod
           |> Sg.trafo (Mod.constant <| Trafo3d.ChangeYZ)
           //|> Sg.fillMode mode
           //|> Sg.blendMode (Mod.constant BlendMode.Blend)
           |> normalizeTo (Box3d(-V3d.III, V3d.III))
    


    //Demo.AssimpExporter.save @"C:\Users\Schorsch\Desktop\quadScene\eigi.dae" sg

    ctrl.Sizes |> Mod.unsafeRegisterCallbackKeepDisposable (fun s ->
        let aspect = float s.X / float s.Y
        proj.AspectRatio <- aspect
    ) |> ignore

//    let ctx = app.Runtime.Context
//    let fbo = new Aardvark.Rendering.GL.Framebuffer(ctx,(fun _ -> 0),ignore,[],None)
//
//
//
//    let sw = System.Diagnostics.Stopwatch()
//    using ctx.ResourceLock (fun _ ->
//        for i in 0 .. 10000 do
//            printfn "run %d" i
//            sw.Restart()
//            let task = app.Runtime.CompileRender(sg.RenderObjects())
//            task.Run fbo |> ignore
//            sw.Stop ()
//            task.Dispose()
//            app.Runtime.Reset()
//            printfn "%A ms" sw.Elapsed.TotalMilliseconds
//            System.Environment.Exit 0
//    )
// 
 
//    let task = app.Runtime.CompileRender(sg.RenderObjects())
//    using ctx.ResourceLock (fun _ ->
//       task.Run fbo |> ignore
//    )   
 
    let engine = Mod.init BackendConfiguration.NativeOptimized
    let engines = 
        ref [
            BackendConfiguration.UnmanagedOptimized
            BackendConfiguration.UnmanagedRuntime
            BackendConfiguration.UnmanagedUnoptimized
            BackendConfiguration.ManagedOptimized
            BackendConfiguration.NativeOptimized
            BackendConfiguration.NativeUnoptimized
        ]

    ctrl.Keyboard.DownWithRepeats.Values.Subscribe (fun k ->
        if k = Aardvark.Application.Keys.P then
            match !engines with
                | h::r ->
                    transact(fun () -> Mod.change engine h)
                    engines := r @ [h]
                | _ -> ()
        elif k = Aardvark.Application.Keys.G then
            System.GC.AddMemoryPressure(1000000000L)
            System.GC.Collect()
            System.GC.WaitForFullGCApproach() |> ignore
            System.GC.RemoveMemoryPressure(1000000000L)

        ()
    ) |> ignore

    let sg = sg |> Sg.loadAsync


    let task = app.Runtime.CompileRender(engine.GetValue(), sg)

    ctrl.RenderTask <- task |> DefaultOverlays.withStatistics

    f.Run()
    0
