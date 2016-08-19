#if INTERACTIVE
#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#load "FRPAgain.fsx"
#r "Eto.dll"
#r "Aether.dll"
#else
namespace Examples
#endif

module AetherTest =
    open Aether
    open Aether.Operators

    type Urdar = 
        { cnt : int }

        static member cnt_ = (fun a -> a.cnt), (fun n u -> { u with cnt = n } )

    type Test = 
        { name : string; things : list<Urdar>; other : Urdar }

        static member name_  = (fun t -> t.name) ,  (fun n t -> { t with name = n } )
        static member other_ = (fun t -> t.other),  (fun o t -> { t with other = o })
        static member things_ = (fun t -> t.things), (fun n t -> { t with things = n })

    let cnt = Test.other_ >-> Urdar.cnt_
    let name = Test.name_
    let things = Test.things_

    let cnt2 =  (Optic.set cnt 10) << (Optic.set Test.name_ "abc")

    let a = { name = "abc"; things = []; other = { cnt = 0 } }


    let b = Optic.set cnt 10 a
    let b' = a ^. cnt

    let inline ( --> ) a b = (a, b)
    let inline ( => ) a b = (a, b)
    let inline ( =^ ) (target,lens) value = Optic.set lens value target 
    //let inline ( :=) (target,lens) value = Optic.set lens value target 
    let inline ( <-- ) (target,lens) value = Optic.set lens value target 

    let c  = a-->cnt =^ 10

    let d = (a-->cnt =^ 10)-->name =^ "abc"

    let e = a-->cnt  =^ 10
    let g = e-->name =^ "abc"

    let h = g-->things =^  [{ cnt = 00 }]
    let i = Optic.set (things >-> List.pos_ 0) { cnt = -10000 } h

    let j = Optic.map things (fun a -> { cnt = 99 } :: a ) i

    let test = List.pos_ 1 >?> Urdar.cnt_
    let k = Optic.map things (Optic.set test 2000) i
    ()

module Gui = 
    open Eto.Forms
    open Eto.Drawing
    open EventSystem.NewestShit
    open Aether
    open Aether.Operators
   


    type Thing = 
        {
            tname : string
        }
        static member name_ = (fun t -> t.tname), (fun n o -> { o with tname = n })

    type Domain = 
        {
            name   : string
            number : int
            nthings : list<Thing>
        }
        static member number_ = (fun t -> t.number), (fun n o -> { o with number = n })
        static member things_ = (fun t -> t.nthings), (fun n o -> { o with nthings = n })
        static member name_ = (fun t -> t.name), (fun n o -> { o with name = n })

    type Logics = 
        | ChangeName of string
        | IncNumber

    let empty = { number = 0; nthings = []; name = "unknown" }

    type Msg<'a> = ref<'a -> unit>
    type Layout = Horizontal | Vertical
    type UI =
        | Text   of string
        | Input  of string * Msg<string> 
        | Button of string * Msg<unit>
        | Div    of Layout * list<UI>

    type UI<'a> = { UI : UI; mutable Event:'a -> unit }

    let button text msg =
        let ev = ref ignore 
        let ui = { UI=Button(text,ev); Event=ignore }
        ev := fun () -> ui.Event msg
        ui

    let view (d : Domain) =
        button "increase" IncNumber

    let update msg model =
        match msg with
        | IncNumber -> Optic.map Domain.number_ ((+)1) model
        | _ -> failwith ""


    //let ev = EventSystem.NewestShit.EventStream( Clock.absolute, empty )

    let inc = 
        SF.whenever (Pattern.lift (function | IncNumber -> Some () | _ -> None)) (fun a -> 
            SF.once (Optic.map Domain.number_ ((+)1))
        )

    let toNativeView view =
        match view.UI with
            | Button(text,ev) -> 
                let b = new Eto.Forms.Button(Text = text)
                b.KeyDown.Subscribe(fun _ -> (!ev)()) |> ignore
                b
            | _ -> failwith ""


    let run () =
        let app = new Application()

        let mutable dir = 1
        let mutable k = 0

        let f = new Form()
        f.Size <-  Size(640,480)
        let d = new Panel()
        d.Content <- new Eto.Forms.Button(Text="abc")
        f.Content <- d

        f.Show()

        let ev = EventSystem.NewestShit.EventStream( Clock.absolute, empty )
        let step  =
            fun (panel : Panel) (view : Domain -> UI<'a>) sf model -> 
                let currentView = view model
                panel.Content <- toNativeView currentView
                let newModel = ev.Evaluate ()
                newModel

        let mutable current = empty
        d.MouseDown.Add(fun a ->
            () 
            //let m = step d view inc current
            //current <- m
        )

        app.Run()

