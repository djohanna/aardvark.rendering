namespace Examples


open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Base.ShaderReflection
open Aardvark.Rendering.Text
open System.Runtime.InteropServices
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO


module Sponza =

    open FShade

    let run() =

        let slow (v : Effects.Vertex) =
            fragment {
                let t : float = uniform?Time
                let mutable h = 0.00001 * (t % 1.0)
                for i in 0 .. 10000 do 
                    h <- h * sin (float i) * cos (float i)
                return v.c * (1.0 + h)
            }
        

        let win = 
            window {
                display Display.Stereo
                samples 8
                backend Backend.Vulkan
                debug false
            }

        let sg = 
            Aardvark.SceneGraph.IO.Loader.Assimp.loadFrom @"C:\Users\steinlechner\Desktop\Sponza bunt\sponza_cm.obj" Loader.Assimp.defaultFlags
                |> Sg.adapter
                |> Sg.scale (0.01)
                |> Sg.transform (Trafo3d.FromBasis(V3d.IOO, V3d.OOI, V3d.OIO, V3d.Zero))
                |> Sg.uniform "Time" (win.Time |> Mod.map (fun t -> float t.Ticks))
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.simpleLighting
                    do! slow
                }


        let sw = System.Diagnostics.Stopwatch()
        let mutable frames = 0
        win.Time |> Mod.unsafeRegisterCallbackKeepDisposable (fun t ->
            frames <- frames + 1
            if frames > 50 then
                sw.Stop() 
                let t = sw.Elapsed.TotalSeconds
                Log.line "fps: %.3f" (float frames / t)
                sw.Restart()
                frames <- 0
            ()
        ) |> ignore

        win.Scene <- sg

        win.Run()
