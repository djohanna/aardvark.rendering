namespace Scratch

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Rendering.NanoVg
open Aardvark.Rendering.GL
open System.Windows.Forms

open Fablish
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.Html

open Xilium.CefGlue.Wrapper
open Xilium.CefGlue

module CefTests =

    module Shader =
        open FShade

        let browserSampler =
            sampler2d {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagPoint
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }

        let fullScreen (v : Effects.Vertex) =
            fragment {
                let coord = V2d(0.5 + 0.5 * v.pos.X, 0.5 - 0.5 * v.pos.Y)
                let pixel = V2d uniform.ViewportSize * coord |> V2i
                let textureSize = browserSampler.Size

                if pixel.X < textureSize.X && pixel.Y < textureSize.Y then
                    let color = browserSampler.[pixel]
                    return color
                else
                    return V4d(0.0,0.0,0.0,0.0)
            }


    module FablishApp =
        type Model = unit // nop
        type Action = unit

        let update m a = m
        let view m : DomNode<Action> = 
            div [attribute "class" "container"; Style ["diplay","inline-block"]] [
                div [ attribute "id" "left_renderControl"; Style [ "height", "600px"; "width", "300px"; "float", "left" ; "background-color", "red"]] [text "render control"]
                div [ attribute "id" "right_renderControl"; Style [ "height", "600px"; "width", "300px" ; "float", "right"]] [text "render control"]
                div [] [ button [] [text "abc"]; button [] [text "cde"]]
            ]

    type Priority =
        | Highest = 0
        | High = 1
        | Normal = 2

    [<AutoOpen>]
    module MessagePump =
        type MessagePump() =
            static let cmp = Func<Priority * (unit -> unit), Priority * (unit -> unit), int>(fun (l,_) (r,_) -> compare l r)

            let sem = new SemaphoreSlim(0)
            let queue = Queue<(unit -> unit)>()
            let runner() =
                while true do
                    sem.Wait()
                    try
                        let action = 
                            lock queue (fun () -> 
                                if queue.Count > 0 then 
                                    let action = queue.Dequeue()
                                    action
                                else 
                                    id
                            )

                        action()
                    with e ->
                        Log.warn "exn: %A" e
                
        

            let start = ThreadStart(runner)
            let thread = Thread(start, 1 <<< 26, IsBackground = true)
            do thread.Start()

            member x.Enqueue(action : unit -> unit) =
                lock queue (fun () ->
                    queue.Enqueue(action)
                )
                sem.Release() |> ignore

            member x.Enqueue(priority : Priority, action : unit -> unit) =
                lock queue (fun () ->
                    queue.Enqueue(action)
                )
                sem.Release() |> ignore

    [<AutoOpen>]
    module BrowserInputOutputDevices =
    
        open Aardvark.Application

        type BrowserKeyboard(focus : ref<bool>, flags : ref<CefEventFlags>, host : CefBrowserHost) =
            inherit EventKeyboard()

            let processKey (k : Keys) (down : bool) =
                let op f g =
                    if down then f ||| g
                    else f &&& ~~~g

                match k with
                    | Keys.LeftCtrl | Keys.RightCtrl -> flags := op !flags CefEventFlags.ControlDown
                    | Keys.LeftAlt | Keys.RightAlt -> flags := op !flags CefEventFlags.AltDown
                    | Keys.LeftShift | Keys.RightShift -> flags := op !flags CefEventFlags.ShiftDown
                    | _ -> ()

            member x.Flags = flags

            override x.KeyDown(k : Keys) =
                if !focus then
                    base.KeyDown(k)
                    let e = CefKeyEvent()
                    e.EventType <- CefKeyEventType.KeyDown
                    e.WindowsKeyCode <- KeyConverter.virtualKeyFromKey k
                    e.NativeKeyCode <- KeyConverter.virtualKeyFromKey k
                    e.Modifiers <- !flags
                    host.SendKeyEvent(e)

                    processKey k true

            override x.KeyUp(k : Keys) =
                if !focus then
                    base.KeyUp(k)
                    let e = CefKeyEvent()
                    e.EventType <- CefKeyEventType.KeyUp
                    e.WindowsKeyCode <- KeyConverter.virtualKeyFromKey k
                    e.NativeKeyCode <- KeyConverter.virtualKeyFromKey k
                    e.Modifiers <- !flags
                    host.SendKeyEvent(e)

                    processKey k false

            override x.KeyPress(c : char) =
                if !focus then
                    base.KeyPress(c)
                    let e = CefKeyEvent()
                    e.WindowsKeyCode <- int c
                    e.EventType <- CefKeyEventType.Char
                    e.Character <- c
                    e.Modifiers <- !flags
                    host.SendKeyEvent(e)


        type BrowserMouse(focus : ref<bool>, flags : ref<CefEventFlags>, host : CefBrowserHost) =
            inherit EventMouse(false)

            let processMouse (pp : PixelPosition) (b : MouseButtons) (down : bool) =
                let t =
                    match b with
                        | MouseButtons.Left -> CefMouseButtonType.Left
                        | MouseButtons.Middle -> CefMouseButtonType.Middle
                        | MouseButtons.Right -> CefMouseButtonType.Right
                        | _ -> CefMouseButtonType.Left

                let f =
                    match b with
                        | MouseButtons.Left -> CefEventFlags.LeftMouseButton
                        | MouseButtons.Middle -> CefEventFlags.MiddleMouseButton
                        | MouseButtons.Right -> CefEventFlags.RightMouseButton
                        | _ -> CefEventFlags.None

                let op f g =
                    if down then f ||| g
                    else f &&& ~~~g


                let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                host.SendMouseClickEvent(e, t, not down, 1)
                flags := op !flags f


            override x.Down(pp : PixelPosition, b : MouseButtons) =
                if !focus then
                    base.Down(pp, b)
                    processMouse pp b true

            override x.Up(pp : PixelPosition, b : MouseButtons) =
                if !focus then
                    base.Up(pp, b)
                    processMouse pp b false

            override x.Click(pp : PixelPosition, b : MouseButtons) =
                if !focus then
                    base.Click(pp, b)
                    let t : CefMouseButtonType =
                        match b with
                            | MouseButtons.Left -> CefMouseButtonType.Left
                            | MouseButtons.Middle -> CefMouseButtonType.Middle
                            | MouseButtons.Right -> CefMouseButtonType.Right
                            | _ -> CefMouseButtonType.Left

                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                    host.SendMouseClickEvent(e, t, false, 1)

            override x.DoubleClick(pp : PixelPosition, b : MouseButtons) =
                if !focus then
                    base.DoubleClick(pp, b)
                    let t : CefMouseButtonType =
                        match b with
                            | MouseButtons.Left -> CefMouseButtonType.Left
                            | MouseButtons.Middle -> CefMouseButtonType.Middle
                            | MouseButtons.Right -> CefMouseButtonType.Right
                            | _ -> CefMouseButtonType.Left

                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
            
                    host.SendMouseClickEvent(e, t, true, 2)

            override x.Scroll(pp : PixelPosition, delta : float) =
                if !focus then
                    base.Scroll(pp, delta)
                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                    host.SendMouseWheelEvent(e, 0, int delta)
                    ()

            override x.Enter(pp : PixelPosition) =
                if !focus then
                    base.Enter(pp)
                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                    host.SendMouseMoveEvent(e, false)

            override x.Leave(pp : PixelPosition) =
                if !focus then
                    base.Enter(pp)
                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                    host.SendMouseMoveEvent(e, true)

            override x.Move(pp : PixelPosition) =
                if !focus then
                    base.Move(pp)
                    let e = CefMouseEvent(pp.Position.X, pp.Position.Y, !flags)
                    host.SendMouseMoveEvent(e, false)

    type MyCefApp() =
        inherit CefApp()

    type MyCefLoadHandler(client : MyCefClient) =
        inherit CefLoadHandler()

        override x.OnLoadStart(browser, frame, transitionType) =
            if frame.IsMain then printfn "start: %s" (browser.GetMainFrame().Url)
            base.OnLoadStart(browser,frame,transitionType)
            client.SetBrowser(browser,frame)

        override x.OnLoadEnd(browser, frame, transitionType) =
            if frame.IsMain then printfn "end: %s" (browser.GetMainFrame().Url)
            base.OnLoadEnd(browser,frame,transitionType)

    and MyCefRenderHandler(width : int, height : int, parent : MyCefClient, texture : IStreamingTexture) =
        inherit CefRenderHandler()

        override x.GetScreenInfo(browser : CefBrowser,info : CefScreenInfo) : bool =
            info.Rectangle <- CefRectangle(0,0,width, height)
            info.AvailableRectangle <- CefRectangle(0,0,width, height)
            true

        override x.GetRootScreenRect(browser : CefBrowser, rect : byref<CefRectangle>) =
            rect <- CefRectangle(0,0,4096, 4096)
            true

        override x.GetViewRect(browser : CefBrowser, rect : byref<CefRectangle>) =
            rect <- CefRectangle(0, 0, width,height)
            true

        override x.OnPopupSize(browser : CefBrowser,rect) =
            ()

        override x.OnPaint(browser : CefBrowser,paintElementType,dirtyRects,buffer,w,h) =
            //printfn "%A" buffer
            if paintElementType = CefPaintElementType.View then
                parent.Render(fun () ->
                    let size = V2i(width, height)
                    texture.UpdateAsync(PixFormat.ByteBGRA, V2i(width, height), buffer)
            )

        override x.OnCursorChange(browser : CefBrowser,cursorHandle,cursorType,customCursorInfo) =
            ()

        override x.OnScrollOffsetChanged(browser : CefBrowser,px,py) =
           ()

        override x.OnImeCompositionRangeChanged(browser, selectedRange, characterBounds) =
            ()
         
    and MyCefClient(runtime : IRuntime, wantMipmaps : bool, w : int, h : int) as this =
        inherit CefClient()
        
        let lockObj = obj()

        let texture = runtime.CreateStreamingTexture(wantMipmaps)
        let version = Mod.init 0
        let transactor = MessagePump()

        let browserReady = new ManualResetEventSlim(false)
        let renderHandler = MyCefRenderHandler(w,h,this,texture)
        let loadHandler = MyCefLoadHandler(this)

        let mutable browser = Unchecked.defaultof<_>
        let mutable frame = Unchecked.defaultof<CefFrame>
        let mutable host : CefBrowserHost = null


        let focus = ref false
        let flags = ref CefEventFlags.None
        let mutable keyboard : Option<BrowserKeyboard> = None
        let mutable mouse : Option<BrowserMouse> = None

        let windowInfo = 
            let windowInfo = CefWindowInfo.Create()
            windowInfo.Width <- 1024
            windowInfo.Height <- 768
            windowInfo.SetAsWindowless(IntPtr.Zero,true)
            windowInfo

        let browserSettings = 
            let browserSettings = CefBrowserSettings()
            browserSettings.WindowlessFrameRate <- 60
            browserSettings

        member internal x.Render(f : unit -> Transaction) =
            let t = f()
            version.UnsafeCache <- version.UnsafeCache + 1

            t.Enqueue version
            transactor.Enqueue (fun () -> t.Commit())

        member x.Texture = texture

        override x.GetRenderHandler() =
            renderHandler :> CefRenderHandler

        override x.GetLoadHandler() =
            loadHandler :> CefLoadHandler

        override x.OnProcessMessageReceived(browser,sourceProcess,message) =
            //printfn "message: %A" message.Name
            false

        member x.Init() =
            lock lockObj (fun () -> 
                if isNull browser then
                    CefBrowserHost.CreateBrowser(windowInfo,x :> CefClient,browserSettings, "about:blank")
                    browserReady.Wait()
            )
             
        member x.LoadUrl(url : string) : unit =
            lock lockObj (fun () ->
                x.Init()
                frame.LoadUrl url
            )

        member x.SetBrowser(b,f) =
            browser <- b
            frame <- f
            host <- b.GetHost()
            browserReady.Set()


        member x.SetFocus (v : bool) =
            lock lockObj (fun () ->
                x.Init()
                if !focus <> v then
                    focus := v
                    host.SendFocusEvent(v)
            )

        member x.Keyboard =
            lock lockObj (fun () ->
                x.Init()
                match keyboard with
                    | Some k -> k :> EventKeyboard
                    | _ ->
                        let k = BrowserKeyboard(focus, flags, host)
                        keyboard <- Some k
                        k :> EventKeyboard
            )

        member x.Mouse =
            lock lockObj (fun () ->
                x.Init()
                match mouse with
                    | Some m -> m :> EventMouse
                    | _ ->
                        let m = BrowserMouse(focus, flags, host)
                        mouse <- Some m
                        m :> EventMouse
            )

    let run argv  =
        ChromiumUtilities.unpackCef()

        CefRuntime.Load()

        let settings = CefSettings()
        settings.MultiThreadedMessageLoop <- true//CefRuntime.Platform = CefRuntimePlatform.Windows;
        settings.SingleProcess <- false;
        settings.LogSeverity <- CefLogSeverity.Default;
        settings.LogFile <- "cef.log";
        settings.ResourcesDirPath <- System.IO.Path.GetDirectoryName(Uri(System.Reflection.Assembly.GetEntryAssembly().CodeBase).LocalPath);
        settings.RemoteDebuggingPort <- 1337;
        settings.NoSandbox <- true;
        settings.WindowlessRenderingEnabled <- true
        let args = 
            if CefRuntime.Platform = CefRuntimePlatform.Windows then argv
            else Array.append [|"-"|] argv

        let mainArgs = CefMainArgs(argv)
        let app = MyCefApp()
        let code = CefRuntime.ExecuteProcess(mainArgs,app,IntPtr.Zero)
        if code <> -1 then System.Environment.Exit code

        CefRuntime.Initialize(mainArgs,settings,app,IntPtr.Zero)

        Application.ApplicationExit.Add(fun _ -> 
            CefRuntime.Shutdown()
        )
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> 
            CefRuntime.Shutdown()
        )

        Ag.initialize()
        Aardvark.Init()

        use app = new OpenGlApplication()
        let win = app.CreateSimpleRenderWindow()

        let client = MyCefClient(app.Runtime, false, 1024,768)
        
        client.LoadUrl("file:///C:/Users/steinlechner/Desktop/xilium.cefglue.fork/CefGlue.Demo.WinForms/bin/Debug/transparency.html")
        client.SetFocus true
        client.Mouse.Use(win.Mouse) |> ignore
        client.Keyboard.Use(win.Keyboard) |> ignore

        let fullscreen =
            Sg.fullScreenQuad
                |> Sg.diffuseTexture client.Texture 
                |> Sg.effect [
                    Shader.fullScreen |> toEffect
                   ]
                |> Sg.uniform "ViewportSize" win.Sizes
    
        let cameraView = 
            CameraView.lookAt (V3d(6.0, 6.0, 6.0)) V3d.Zero V3d.OOI
                |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
                |> Mod.map CameraView.viewTrafo

        let projection = 
            win.Sizes 
                |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))
                |> Mod.map Frustum.projTrafo

        let sg =
            Sg.box' C4b.White Box3d.Unit
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect           
                    DefaultSurfaces.constantColor C4f.Red |> toEffect  
                    ]
                |> Sg.viewTrafo cameraView
                |> Sg.projTrafo projection

        let task =
            RenderTask.ofList [
                app.Runtime.CompileClear(win.FramebufferSignature,Mod.constant C4f.DarkGreen)
                app.Runtime.CompileRender(win.FramebufferSignature, fullscreen)
                    |> DefaultOverlays.withStatistics
            ]

        win.RenderTask <- task
        win.Run()

        CefRuntime.Shutdown()
        0





