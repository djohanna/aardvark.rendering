﻿namespace Aardvark.Rendering.GL

open System
open System.Threading
open System.Collections.Concurrent
open OpenTK
open OpenTK.Platform
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4
open Aardvark.Base


/// <summary>
/// a handle represents a GL context which can be made current and released
/// for one thread at a time.
/// </summary>
[<AllowNullLiteral>]
type ContextHandle(handle : IGraphicsContext, window : IWindowInfo) =
    static let current = new ThreadLocal<Option<ContextHandle>>(fun () -> None)

    let l = obj()
    let mutable onMakeCurrent : ConcurrentHashSet<unit -> unit> = null

    static member Current
        with get() = 
            match current.Value with
                | Some ctx when ctx.IsCurrent -> Some ctx
                | _ -> None

        and set v = current.Value <- v

    member x.OnMakeCurrent(f : unit -> unit) =
        Interlocked.CompareExchange(&onMakeCurrent, ConcurrentHashSet(), null) |> ignore
        onMakeCurrent.Add f |> ignore

    member x.Lock = l

    member x.WindowInfo = window
    
    member x.Handle = handle

    member x.IsCurrent =
        handle.IsCurrent

    member x.MakeCurrent() =
        match ContextHandle.Current with
            | Some handle -> handle.ReleaseCurrent()
            | _ -> ()

        handle.MakeCurrent(window)
        ContextHandle.Current <- Some x

        let actions = Interlocked.Exchange(&onMakeCurrent, null)
        if actions <> null then
            for a in actions do
                a()

    member x.ReleaseCurrent() =
        if handle.IsCurrent then
            handle.MakeCurrent(null)
        else
            Log.warn "cannot release context which is not current"
        ContextHandle.Current <- None


/// <summary>
/// A module for managing context handles
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ContextHandle =
    
    let private windows = ConcurrentDictionary<ContextHandle, NativeWindow>()

    let mutable primaryContext : ContextHandle = null

    let usePrimaryContext (f : unit -> 'a) =
        if primaryContext = null then f()
        else
            try
                primaryContext.MakeCurrent()
                f()
            finally
                primaryContext.ReleaseCurrent()

    /// <summary>
    /// creates a new context using the default configuration
    /// </summary>
    let create() =
        let window, context =
            if primaryContext <> null then primaryContext.MakeCurrent()
            
            let mode = Graphics.GraphicsMode(ColorFormat(Config.BitsPerPixel), Config.DepthBits, Config.StencilBits, 1, ColorFormat.Empty, Config.Buffers, false)
            let window = new NativeWindow(16, 16, "background", GameWindowFlags.Default, mode, DisplayDevice.Default)
            let context = new GraphicsContext(GraphicsMode.Default, window.WindowInfo, Config.MajorVersion, Config.MinorVersion, Config.ContextFlags);
            context.MakeCurrent(window.WindowInfo)
            let ctx = context |> unbox<IGraphicsContextInternal>
            ctx.LoadAll()
            context.MakeCurrent(null)
            window, context
    
        
        let handle = new ContextHandle(context, window.WindowInfo)
        
        // add the window to the windows-table to save it from being
        // garbage collected.
        if not <| windows.TryAdd(handle, window) then failwith "failed to add new context to live-set"
    
        // store the primary context (if not already existing)
        if primaryContext = null then
            primaryContext <- handle
    
        handle
      
    

    let createContexts resourceContextCount =
        // if there is a current context release it before creating
        // the GameWindow since the GameWindow makes itself curret
        GraphicsContext.ShareContexts <- true;

        let current = ContextHandle.Current
        match current with
            | Some handle -> handle.ReleaseCurrent()
            | None -> ()

        let contexts =
            [ for i in 1..resourceContextCount do
                yield create()
            ]

        // make the old context current again
        match current with
            | Some handle -> handle.MakeCurrent()
            | None -> ()

        contexts

    /// <summary>
    /// deletes the given context also destroying its associated window-info
    /// </summary>
    let delete(ctx : ContextHandle) =
        if ctx.IsCurrent then
            ctx.ReleaseCurrent()

        match windows.TryRemove ctx with
            | (true, w) -> w.Dispose()
            | _ -> ()

    /// <summary>
    /// checks whether the given context is current on the calling thread
    /// </summary>
    let isCurrent (ctx : ContextHandle) = ctx.IsCurrent

    /// <summary>
    /// makes the given context current on the calling thread.
    /// releases any context being current before doing so.
    /// </summary>
    let makeCurrent (ctx : ContextHandle) = ctx.MakeCurrent()

    /// <summary>
    /// releases the given context from the calling thread
    /// </summary>
    let releaseCurrent (ctx : ContextHandle) = ctx.ReleaseCurrent()


