namespace Aardvark.SceneGraph

open Aardvark.SceneGraph.Semantics

open System
open System.Runtime.CompilerServices
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Ag
open Aardvark.Base.Incremental

[<AbstractClass; Sealed; Extension>]
type SceneGraphRuntimeExtensions private() =




    [<Extension>]
    static member CompileRender(x : IRuntime, signature : IFramebufferSignature, rjs : aset<IRenderObject>) =
        x.CompileRender(signature, BackendConfiguration.Default, rjs)

    [<Extension>]
    static member CompileRender (x : IRuntime, signature : IFramebufferSignature, engine : BackendConfiguration, s : ISg) =
        let app = Sg.DynamicNode(Mod.constant s)
        app?Runtime <- x
        let jobs : aset<IRenderObject> = app.RenderObjects()
        // TODO: fix overlays
        //let overlays = app.OverlayTasks() |> ASet.sortBy fst |> AList.map snd |> RenderTask.ofAList
        x.CompileRender(signature, engine, jobs)

    [<Extension>]
    static member CompileRender (x : IRuntime, signature : IFramebufferSignature, s : ISg) =
        SceneGraphRuntimeExtensions.CompileRender(x, signature, BackendConfiguration.Default, s)

    [<Extension>]
    static member CompileTransformFeedback(x : IRuntime, surface : ISurface, mode : IndexedGeometryMode, wanted : list<Symbol>, rjs : aset<IRenderObject>) =
        x.CompileTransformFeedback(surface, mode, wanted, BackendConfiguration.Default, rjs)

    [<Extension>]
    static member CompileTransformFeedback(x : IRuntime, surface : ISurface, mode : IndexedGeometryMode, wanted : list<Symbol>, engine : BackendConfiguration, s : ISg) =
        let app = Sg.DynamicNode(Mod.constant s)
        app?Runtime <- x
        let jobs : aset<IRenderObject> = app.RenderObjects()
        x.CompileTransformFeedback(surface, mode, wanted, engine, jobs)

    [<Extension>]
    static member CompileTransformFeedback(x : IRuntime, surface : ISurface, mode : IndexedGeometryMode, wanted : list<Symbol>, s : ISg) =
        SceneGraphRuntimeExtensions.CompileTransformFeedback(x, surface, mode, wanted, BackendConfiguration.Default, s)

[<AutoOpen>]
module RuntimeSgExtensions =
    module Sg =
    
        let compile (runtime : IRuntime) (signature : IFramebufferSignature) (sg : ISg) =
            runtime.CompileRender(signature, sg)

        let compile' (runtime : IRuntime) (signature : IFramebufferSignature) (config : BackendConfiguration) (sg : ISg) =
            runtime.CompileRender(signature, config, sg)


        let compileTransformFeedback (runtime : IRuntime) (surface : ISurface) (mode : IndexedGeometryMode) (wanted : list<Symbol>) (sg : ISg) =
            runtime.CompileTransformFeedback(surface, mode, wanted, sg)

        let compileTransformFeedback' (runtime : IRuntime) (surface : ISurface) (mode : IndexedGeometryMode) (wanted : list<Symbol>) (config : BackendConfiguration) (sg : ISg) =
            runtime.CompileTransformFeedback(surface, mode, wanted, config, sg)