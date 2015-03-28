﻿namespace Aardvark.Application.WinForms

open System
open System.Windows.Forms
open Aardvark.Base
open Aardvark.Application

type private WinFormsButtons = System.Windows.Forms.MouseButtons

type Mouse(ctrl : Control) =

    let events = EventSource<MouseEvent>()

    let (~%) (m : WinFormsButtons) =
        let mutable buttons = MouseButtons.None

        if m &&& WinFormsButtons.Left <> WinFormsButtons.None then 
            buttons <- buttons ||| MouseButtons.Left

        if m &&& WinFormsButtons.Right <> WinFormsButtons.None then 
            buttons <- buttons ||| MouseButtons.Right

        if m &&& WinFormsButtons.Middle <> WinFormsButtons.None then 
            buttons <- buttons ||| MouseButtons.Middle

        buttons

    let pos (e : MouseEventArgs) =
        PixelPosition(e.X, e.Y, ctrl.ClientSize.Width, ctrl.ClientSize.Height)

    let (~%%) (m : MouseEventArgs) =
        let buttons = %m.Button
        let location = pos m
        { location = location; buttons = buttons }

    let mousePos() =
        let p = ctrl.PointToClient(Control.MousePosition)
        let s = ctrl.ClientSize
        
        let x = clamp 0 (s.Width-1) p.X
        let y = clamp 0 (s.Height-1) p.Y

        PixelPosition(x, y, ctrl.ClientSize.Width, ctrl.ClientSize.Height)

    let onMouseDownHandler = MouseEventHandler(fun s e -> events.Emit (MouseDown %%e))
    let onMouseUpHandler = MouseEventHandler(fun s e -> events.Emit (MouseUp %%e))
    let onMouseMoveHandler = MouseEventHandler(fun s e -> events.Emit (MouseMove (pos e)))
    let onMouseClickHandler = MouseEventHandler(fun s e -> events.Emit (MouseClick %%e))
    let onMouseDoubleClickHandler = MouseEventHandler(fun s e -> events.Emit (MouseDoubleClick %%e))
    let onMouseWheelHandler = MouseEventHandler(fun s e -> events.Emit (MouseScroll(float e.Delta, pos e)))
    let onMouseEnter = EventHandler(fun s e -> events.Emit (MouseEnter <| mousePos()))
    let onMouseLeave = EventHandler(fun s e -> events.Emit (MouseLeave <| mousePos()))

    do ctrl.MouseDown.AddHandler onMouseDownHandler
       ctrl.MouseUp.AddHandler onMouseUpHandler
       ctrl.MouseMove.AddHandler onMouseMoveHandler
       ctrl.MouseClick.AddHandler onMouseClickHandler
       ctrl.MouseDoubleClick.AddHandler onMouseDoubleClickHandler
       ctrl.MouseWheel.AddHandler onMouseWheelHandler
       ctrl.MouseEnter.AddHandler onMouseEnter
       ctrl.MouseLeave.AddHandler onMouseLeave

    member x.Dispose() =
        ctrl.MouseDown.RemoveHandler onMouseDownHandler
        ctrl.MouseUp.RemoveHandler onMouseUpHandler
        ctrl.MouseMove.RemoveHandler onMouseMoveHandler
        ctrl.MouseClick.RemoveHandler onMouseClickHandler
        ctrl.MouseDoubleClick.RemoveHandler onMouseDoubleClickHandler
        ctrl.MouseWheel.RemoveHandler onMouseWheelHandler
        ctrl.MouseEnter.RemoveHandler onMouseEnter
        ctrl.MouseLeave.RemoveHandler onMouseLeave

    member x.Down = events |> Event.choose (function MouseDown p -> Some p | _ -> None)
    member x.Up = events |> Event.choose (function MouseUp p -> Some p | _ -> None)
    member x.Click = events |> Event.choose (function MouseClick p -> Some p | _ -> None)
    member x.DoubleClick = events |> Event.choose (function MouseDoubleClick p -> Some p | _ -> None)
    member x.Move = events |> Event.choose (function MouseMove p -> Some p | _ -> None)
    member x.Scroll = events |> Event.choose (function MouseScroll(delta, p) -> Some(delta,p) | _ -> None)
    member x.Enter = events |> Event.choose (function MouseEnter p -> Some(p) | _ -> None)
    member x.Leave = events |> Event.choose (function MouseLeave p -> Some(p) | _ -> None)

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IMouse with
        member x.Events = events :> IEvent<_>