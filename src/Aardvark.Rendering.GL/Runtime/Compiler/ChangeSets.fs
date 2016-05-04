namespace Aardvark.Rendering.GL.Compiler

open System
open System.Collections.Generic

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL
open Aardvark.Base.Incremental.Telemetry

type InputSet(o : IAdaptiveObject) =
    let l = obj()
    let inputs = ReferenceCountingSet<IAdaptiveObject>()

    member x.Add(m : IAdaptiveObject) = 
        goodLock123 l (fun () ->
            if inputs.Add m then
                goodLock123 m.Outputs (fun () -> m.Outputs.Add o |> ignore)
        )

    member x.Remove (m : IAdaptiveObject) = 
        goodLock123 l (fun () ->
            if inputs.Remove m then
                goodLock123 m.Outputs (fun () -> m.Outputs.Remove o |> ignore)
        )

