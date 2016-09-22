module AnotherUISketch

type Event<'m> = ref<'m -> unit>

type Layout = Horizontal | Vertical

type UI =
    | Label of string * Event<unit>
    | Button of string * Event<unit>
    | SideBySide of Layout * (UI * UI)

type UIUpdate =
    | InsertUI of list<int> * UI
    | UpdateUI of list<int> * UI
    | EventUI of (unit->unit)
    | ReplaceUI of list<int> * UI

type UI<'msg> = { UI : UI; mutable Event : ('msg -> unit) }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UI =

    let button text msg =
        let ev = ref ignore 
        let ui = { UI = Button(text,ev); Event=ignore }
        ev := fun () -> ui.Event msg
        ui

    let label text msg =
        let ev = ref ignore 
        let ui = { UI = Label(text,ev); Event=ignore }
        ev := fun () -> ui.Event msg
        ui

    let sideBySide layout left right =
        let ui = { UI = SideBySide(layout,(left.UI, right.UI)); Event=ignore }
        left.Event <- ui.Event
        right.Event <- ui.Event
        ui

    let diff (a : UI<_>) (b : UI<_>) =
        let update e1 e2 = fun () -> printfn "ajsdf"
        let rec diff a b path index diffs = 
            match a, b with
                | _ when System.Object.ReferenceEquals(a,b) -> diffs
                | Label(t1,e1), Label(t2,e2) -> 
                    if t1 = t2 then EventUI(update e1 e2) :: diffs 
                    else EventUI(update e1 e2) :: UpdateUI(path,b) :: diffs
                | Button(t1,e1), Button(t2,e2) ->
                    if t1 = t2 then EventUI(update e1 e2) :: diffs
                    else EventUI(update e1 e2) :: UpdateUI(path,b) :: diffs
                | SideBySide(l1,_), SideBySide(l2,_) when l1<>l2 -> ReplaceUI(path,b) :: diffs
                | SideBySide(_,(a,b)), SideBySide(_,(c,d)) -> 
                    let l = diff a c (index::path) (index + 0) diffs
                    diff b d (index::path) (index + 1) l
                | _ -> ReplaceUI(path,b) :: diffs
        diff a.UI b.UI [] 0 []

type INativeUI =
    abstract member Update : list<UIUpdate> -> unit

type Application<'msg,'model> =
    {
        model  : 'model
        update : 'msg   -> 'model   -> 'model
        view   : 'model -> UI<'msg>
    }        

module WinFormsUI =

    open System.Windows.Forms

    let rec create ui : Control =
        match ui with
            | UI.Label(s,m) -> 
                let l = new Label(Text = s) 
                l.Click.Add(fun _ -> (!m)())
                l :> Control
            | UI.Button(t,e) -> 
                let b = new Button(Text = t)
                b.Click.Add(fun _ -> (!e)())
                b :> Control
            | UI.SideBySide(layout,(l,r)) -> 
                let stack =  new FlowLayoutPanel()
                stack.FlowDirection <-
                    match layout with
                        | Vertical -> FlowDirection.BottomUp
                        | Horizontal -> FlowDirection.LeftToRight
                stack.Controls.Add(create l)
                stack.Controls.Add(create r)
                stack :> Control

    let findContainer root (path : list<int>) : Control =  
        let rec find (path : list<int>) =
            match path with
                | []    -> root
                | x::xs -> 
                    let r : Control = find xs
                    r.Controls.[x]
        find path

    let update (root : Form) u =
        match u with
            | InsertUI(path,ui) ->
                let p = findContainer root path
                p.
            | UpdateUI(path,ui) -> failwith ""
            | EventUI _ -> ()
            | ReplaceUI(path,ui) -> 
                failwith ""

    type WinFormsUI() =
        interface INativeUI with
            member x.Update(updates) = 
                failwith ""


type Model = { cnt : int }

type Msg = Reset | Inc | Format

let update (msg : Msg) (model : Model) : Model =
    match msg with
        | Reset -> { cnt = 0 }
        | Inc   -> { model with cnt = model.cnt + 1 }
        | Format -> model

let view (model : Model) : UI<Msg> =
    let label = UI.label (sprintf "uuuhaaaa: %d" model.cnt) Format
    let button = UI.button "incmeeee" Inc
    UI.sideBySide Layout.Horizontal label button


let run (native : INativeUI) app =
    let rec handle model ui msg = 
        let newModel = app.update msg model
        let newUI = app.view newModel
        newUI.Event <- handle newModel newUI
        let diff = UI.diff ui newUI
        diff |> List.iter ( function | EventUI f -> f () | _ -> () )
        native.Update diff
    let ui = app.view app.model
    ui.Event <- handle app.model ui
    native.Update [InsertUI([], ui.UI)]



[<Demo>]
let elmUITest () =
    System.Windows.Forms.Application.Run()