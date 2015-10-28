namespace Demo

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

module Transparency =

    let main args = 

        Aardvark.Init()

        use app = new OpenGlApplication()
        let ctrl = app.CreateSimpleRenderWindow(1)

        let view = CameraView.LookAt(V3d(2.0,2.0,2.0), V3d.Zero, V3d.OOI)
        let proj = CameraProjectionPerspective(60.0, 0.1, 10000.0, float (ctrl.Sizes.GetValue().X) / float (ctrl.Sizes.GetValue().Y))
        

        let view = DefaultCameraController.control ctrl.Mouse ctrl.Keyboard ctrl.Time view
        
        let geometry = 
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexArray = [| 0; 1; 2; 0; 2; 3 |],
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions,                  [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO |] :> Array
                        DefaultSemantic.DiffuseColorCoordinates,    [| V2f.OO; V2f.IO; V2f.II; V2f.OI |] :> Array
                        DefaultSemantic.Normals,                    [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |] :> Array
                    ]
            )

        let sg = Sg.ofIndexedGeometry geometry
        let sg =
            sg |> Sg.effect [
                    //Shader.pvLight |> toEffect
                    //Shader.pvFrag  |> toEffect
                    //DefaultSurfaces.trafo |> toEffect
                    //DefaultSurfaces.pointSurface pointSize |> toEffect
                    //DefaultSurfaces.uniformColor color |> toEffect
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.constantColor C4f.Red |> toEffect
//                    DefaultSurfaces.diffuseTexture |> toEffect
                    DefaultSurfaces.simpleLighting |> toEffect
                  ]
               |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
               |> Sg.projTrafo proj.ProjectionTrafos.Mod

        let task = app.Runtime.CompileRender sg

        ctrl.RenderTask <- task |> DefaultOverlays.withStatistics

        ctrl.Run()
        0