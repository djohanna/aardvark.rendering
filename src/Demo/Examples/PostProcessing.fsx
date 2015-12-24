﻿(*
PostProcessing.fsx

This example illustrates how to do a very simple PostProcessing on a scene.
For simplicity the scene is just a random set of points but the example easily 
extends to more complicated scenes since it's just using renderTasks for composition.

Here we simply apply a gaussian blur to the rendered image but other effects can be achieved in a very
similar way. (e.g. fluid-rendering things, etc.)

*)

#load "RenderingSetup.fsx"
open RenderingSetup

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Default // makes viewTrafo and other tutorial specicific default creators visible

// let's start by creating our example-scene containing random points.
let pointSize = Mod.init 50.0
let pointCount = 2048

let pointSg = 
    let rand = Random()
    let randomV3f() = V3f(rand.NextDouble(), rand.NextDouble(), rand.NextDouble())
    let randomColor() = C4b(rand.NextDouble(), rand.NextDouble(), rand.NextDouble(), 1.0)

    Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions (Array.init pointCount (fun _ -> randomV3f()) |> Mod.constant)
        |> Sg.vertexAttribute DefaultSemantic.Colors (Array.init pointCount (fun _ -> randomColor()) |> Mod.constant)
        |> Sg.viewTrafo (viewTrafo() |> Mod.map CameraView.viewTrafo)
        |> Sg.projTrafo (perspective() |> Mod.map Frustum.projTrafo)
        |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.pointSprite |> toEffect; DefaultSurfaces.pointSpriteFragment |> toEffect; DefaultSurfaces.vertexColor |> toEffect]
        |> Sg.uniform "PointSize" pointSize
        |> Sg.uniform "ViewportSize" win.Sizes


// we now need to define some shaders performing the per-pixel blur on a given input texture.
// since the gaussian filter is separable we create two shaders performing the vertical and horizontal blur.
module Shaders =
    open FShade

    type Vertex = { [<TexCoord>] tc : V2d }

    let input =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagMipPoint
        }


    // for a given filterSize and sigma calculate the weights CPU-side
    let filterSize = 15
    let sigma = 6.0

    let halfFilterSize = filterSize / 2
    let weights =
        let res = 
            Array.init filterSize (fun i ->
                let x = abs (i - halfFilterSize)
                exp (-float (x*x) / (2.0 * sigma * sigma))
            )

        // normalize the weights
        let sum = Array.sum res
        res |> Array.map (fun v -> v / sum)


    let gaussX (v : Vertex) =
        fragment {
            let mutable color = V4d.Zero
            let off = V2d(1.0 / float uniform.ViewportSize.X, 0.0)
            for x in -halfFilterSize..halfFilterSize do
                let w = weights.[x+halfFilterSize]
                color <- color + w * input.Sample(v.tc + (float x) * off)

            return V4d(color.XYZ, 1.0)
        }

    let gaussY (v : Vertex) =
        fragment {
            let mutable color = V4d.Zero
            let off = V2d(0.0, 1.0 / float uniform.ViewportSize.Y)
            for y in -halfFilterSize..halfFilterSize do
                let w = weights.[y+halfFilterSize]
                color <- color + w * input.Sample(v.tc + (float y) * off)

            return V4d(color.XYZ, 1.0)
        }


// for rendering the filtered image we need a fullscreen quad
let fullscreenQuad =
    Sg.draw IndexedGeometryMode.TriangleStrip
        |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant [|V3f(-1.0,-1.0,0.0); V3f(1.0,-1.0,0.0); V3f(-1.0,1.0,0.0);V3f(1.0,1.0,0.0) |])
        |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])
        |> Sg.depthTest ~~DepthTestMode.None
        |> Sg.uniform "ViewportSize" win.Sizes

// so in a first pass we need to render our pointScene to a color texture which
// is quite simple using the RenderTask utilities provided in Base.Rendering.
// from the rendering we get an IMod<ITexture> which will be outOfDate whenever
// something changes in pointScene and updated whenever subsequent passes need it.
let mainResult =
    pointSg
        |> Sg.compile win.Runtime win.FramebufferSignature 
        |> RenderTask.renderToColor win.Sizes ~~TextureFormat.Rgba8
   
// by taking the texture created above and the fullscreen quad we can now apply
// the first gaussian filter to it and in turn get a new IMod<ITexture>     
let blurredOnlyX =
    fullscreenQuad 
        |> Sg.texture DefaultSemantic.DiffuseColorTexture mainResult
        |> Sg.effect [Shaders.gaussX |> toEffect]
        |> Sg.compile win.Runtime win.FramebufferSignature
        |> RenderTask.renderToColor win.Sizes ~~TextureFormat.Rgba8

// by taking the texture created above and the fullscreen quad we can now apply
// the first gaussian filter to it and in turn get a new IMod<ITexture>     
let blurredOnlyY =
    fullscreenQuad 
        |> Sg.texture DefaultSemantic.DiffuseColorTexture mainResult
        |> Sg.effect [Shaders.gaussY |> toEffect]
        |> Sg.compile win.Runtime win.FramebufferSignature
        |> RenderTask.renderToColor win.Sizes ~~TextureFormat.Rgba8

// we could now render the blurred result to a texutre too but for our example
// we can also render it directly to the screen.
let final =

    let overlayOriginal =
        fullscreenQuad
            |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.diffuseTexture |> toEffect]
            |> Sg.texture DefaultSemantic.DiffuseColorTexture mainResult
            |> Sg.trafo ~~(Trafo3d.Scale(0.3) * Trafo3d.Translation(-0.7, 0.7, 0.0))
            |> Sg.pass 1UL
            |> Sg.blendMode ~~BlendMode.Blend


    let mainResult =
        fullscreenQuad 
            |> Sg.texture DefaultSemantic.DiffuseColorTexture blurredOnlyX
            |> Sg.effect [Shaders.gaussY |> toEffect]
        
    Sg.group' [mainResult; overlayOriginal]

let showTexture t =
    setSg (
        fullscreenQuad 
            |> Sg.texture DefaultSemantic.DiffuseColorTexture t
            |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]
    )




setSg final

// finally we create a simple utility for changing the pointSize
// you can play with it and see the render-result adjust to the given point-size.
let setPointSize (s : float) =
    transact (fun () -> Mod.change pointSize s)


// some other setters showing intermediate result textures
let showMain() = showTexture mainResult
let showOnlyX() = showTexture blurredOnlyX
let showOnlyY() = showTexture blurredOnlyY
let showFinal() = setSg final      

