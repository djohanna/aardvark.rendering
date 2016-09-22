module Pong

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.Application
open Aardvark.Application.WinForms
open FShade
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Monads.State

[<AutoOpen>]
module Utils = 

    let move forward backward left right = 
        (forward   %?  V2d.OI %. V2d.OO) %+
        (backward  %? -V2d.OI %. V2d.OO) %+
        (left      %? -V2d.IO %. V2d.OO) %+ 
        (right     %?  V2d.IO %. V2d.OO)

    let wasd = 
        move (App.Keyboard.IsDown Keys.W) 
             (App.Keyboard.IsDown Keys.S) 
             (App.Keyboard.IsDown Keys.A) 
             (App.Keyboard.IsDown Keys.D)

    let arrows = 
        move (App.Keyboard.IsDown Keys.Up) 
             (App.Keyboard.IsDown Keys.Down) 
             (App.Keyboard.IsDown Keys.Left) 
             (App.Keyboard.IsDown Keys.Right)

type Input = 
    { space : bool
      paddle1 : double
      paddle2 : double
    }

let input =
    adaptive {
        let! move1 = wasd 
        let! move2 = arrows
        let! space = (App.Keyboard.IsDown Keys.Right)
        return { space = space; paddle1 = move1.X; paddle2 = move2.X }
    }

type Ball =   { pos : V2d; speed : V2d }
type Player = { pos : V2d; speed : V2d; score : int }

type State = Play | Pause
type Game = { state : State; ball : Ball; player1 : Player; player2 : Player }

let setPos x = { pos = V2d(x,0.0); speed = V2d.OO; score = 0 }

let bounds = 0.0,1.0

let defaultGame =
    { state = Pause
      ball = { pos = V2d.OO; speed = V2d.II }
      player1 = setPos 0.25
      player2 = setPos 0.75
    }

let near n c m = m >= n-c && m <= n+c

let update =
    controller {
        for i in input do
            do! fun (g : Game) ->
                let newState = 
                    match g.state, i.space with
                        | Pause, true -> Play
                        | Play,  true -> Pause
                        | o,_ -> o
                { g with state = newState }
            ()
    
    }

//let sim =
//    controller {
//        for dt in Proc.dt do
//            do! fun g -> g
//    }

//let total = 
//    Proc.par [
//        update
//        sim
//    ]




[<Demo>]
let APong() =

    

    Sg.box' C4b.Red Box3d.Unit
        |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.Red |> toEffect
            DefaultSurfaces.simpleLighting |> toEffect
        ]