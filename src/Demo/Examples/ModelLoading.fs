namespace Examples

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms

module Shaders =
    open FShade

    let private normalSampler =
        sampler2d {
            texture uniform?NormalMapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let internal normalMap (v : Effects.Vertex) =
        fragment {
            let texColor = normalSampler.Sample(v.tc).XYZ
            let texNormal = (2.0 * texColor - V3d.III) |> Vec.normalize

            let n =
                if uniform?UseNormalMapping then
                    v.n.Normalized * texNormal.Z + v.b.Normalized * texNormal.X + v.t.Normalized * texNormal.Y |> Vec.normalize
                else v.n

            return { v with n = n }
        }

    let  lighting (twoSided : bool) (v : Effects.Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let c = uniform.LightLocation - v.wp.XYZ |> Vec.normalize
            let l = c
            let h = c

            let ambient = 0.1
            let diffuse = 
                if twoSided then Vec.dot l n |> abs
                else Vec.dot l n |> max 0.0

            let s = Vec.dot h n 

            let l = ambient + (1.0 - ambient) * diffuse

            let spec = V3d.III

            return V4d(v.c.XYZ * l + spec * pown s 32, v.c.W)
        }

module ModelLoading =

    let loadEigi (basePath : string) =
        let body =
            Aardvark.SceneGraph.IO.Loader.Assimp.load (Path.combine [basePath; "body.obj"]) 
            |> Sg.adapter
            |> Sg.fileTexture DefaultSemantic.NormalMapTexture (Path.combine [basePath; "EigiBody_NORMAL.jpg"]) true
            |> Sg.diffuseFileTexture' (Path.combine [basePath; "EigiBodyColor.jpg"]) true
        let drool =
            Aardvark.SceneGraph.IO.Loader.Assimp.load (Path.combine [basePath; "drool.obj"]) 
            |> Sg.adapter
            |> Sg.fileTexture DefaultSemantic.NormalMapTexture (Path.combine [basePath; "EigiDrool_NORMAL.jpg"]) true
            |> Sg.diffuseFileTexture' (Path.combine [basePath; "EigiDrool_COLOR.png"]) true
        let eyes =
            Aardvark.SceneGraph.IO.Loader.Assimp.load (Path.combine [basePath; "eyes.obj"]) 
            |> Sg.adapter
            |> Sg.fileTexture DefaultSemantic.NormalMapTexture (Path.combine [basePath; "EigiEye_NORMAL.jpg"]) true
            |> Sg.diffuseFileTexture' (Path.combine [basePath; "EigiEye_COLOR.jpg"]) true
        let lower =
            Aardvark.SceneGraph.IO.Loader.Assimp.load (Path.combine [basePath; "lowerTeeth.obj"]) 
            |> Sg.adapter
            |> Sg.fileTexture DefaultSemantic.NormalMapTexture (Path.combine [basePath; "EigiTeeth_NORMAL.jpg"]) true
            |> Sg.diffuseFileTexture' (Path.combine [basePath; "EigiTeeth_COLOR.jpg"]) true
        let upper =
            Aardvark.SceneGraph.IO.Loader.Assimp.load (Path.combine [basePath; "upperTeeth.obj"]) 
            |> Sg.adapter
            |> Sg.fileTexture DefaultSemantic.NormalMapTexture (Path.combine [basePath; "EigiTeeth_NORMAL.jpg"]) true
            |> Sg.diffuseFileTexture' (Path.combine [basePath; "EigiTeeth_COLOR.jpg"]) true
        Sg.ofSeq [body;eyes;lower;upper]

    let run () =

        // initialize runtime system
        Ag.initialize(); Aardvark.Init()

        // create simple render window
        use app = new OpenGlApplication()
        let win = app.CreateSimpleRenderWindow()
        win.Text <- "Model Loading (aardvark.docs)"

        let useNormalMapping = Mod.init true

        win.Keyboard.KeyDown(Keys.N).Values.Add(fun _ -> 
            transact (fun _ -> useNormalMapping.Value <- not useNormalMapping.Value)
        )


        // view, projection and default camera controllers
        let initialView = CameraView.lookAt (V3d(9.3, 9.9, 8.6)) V3d.Zero V3d.OOI
        let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
        let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.01 1000.0 (float s.X / float s.Y))
    
        let model = 
            Aardvark.SceneGraph.IO.Loader.Assimp.load @"..\..\data\aardvark\aardvark.obj" 
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0,1.0,-1.0))

        let model = loadEigi @"C:\Aardwork\Eigi\"

        let scene = model


        let scene = 
            [
                for x in -5 .. 5 do
                    for y in -5 .. 5 do
                        for z in -5 .. 5 do
                            yield 
                              Sg.translate (float x) (float y) (float z) model
            ] |> Sg.ofSeq

        let trafos =
            [| for x in -5 .. 5 do
                for y in -5 .. 5 do
                    for z in -5 .. 5 do
                        yield Trafo3d.Translation(float x,float y,float z)
            |]

        let unitBox = Aardvark.SceneGraph.SgPrimitives.Primitives.unitBox
        // is just defined as such:
        let unitBox =
            let box = Box3d.Unit
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

            let texcoords =
                [|
                    V2f.OO; V2f.IO; V2f.II;  V2f.OO; V2f.II; V2f.OI
                |]

            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,

                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                        DefaultSemantic.DiffuseColorCoordinates, indices |> Array.mapi (fun ti _ -> texcoords.[ti % 6]) :> Array
                    ]

            )

        let instanced =
            Sg.instancedGeometry (Mod.constant trafos) unitBox

        //let scene = instanced

        let sg =
            model
                |> Sg.effect [
                        //DefaultSurfaces.instanceTrafo |> toEffect
                        DefaultSurfaces.trafo |> toEffect
                        //DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.constantColor C4f.White |> toEffect
                        DefaultSurfaces.diffuseTexture |> toEffect
                        Shaders.normalMap |> toEffect
                        DefaultSurfaces.lighting true |> toEffect
                   ]
                |> Sg.uniform "UseNormalMapping" useNormalMapping
                |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
                |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)

        // specify render task
        let task =
            app.Runtime.CompileRender(win.FramebufferSignature, sg)

        // start
        win.RenderTask <- task
        win.Run()
        
