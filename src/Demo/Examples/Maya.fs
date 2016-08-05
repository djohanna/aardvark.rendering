#if INTERACTIVE
#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#else
namespace Examples
#endif


open System
open Aardvark.Base
open Aardvark.Base.Rendering
    
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Interactive

open Default // makes viewTrafo and other tutorial specicific default creators visible


module FRP =
    open Aardvark.Base

    type Time = DateTime
   
    type Behavior<'a> = Beh of (Time -> ('a * ReactBeh<'a>))
    and ReactBeh<'a> = unit -> Behavior<'a>
    
    type Event<'a> = Ev of (Time -> (Option<'a> * ReactEvent<'a>))
    and  ReactEvent<'a> = unit -> Event<'a>

    let rec pureBehavior (value : 'a) = Beh(fun (t : Time) -> (value, fun () -> pureBehavior value))
    let rec timeBehavior = Beh(fun t -> (t, fun () -> timeBehavior))
    
    let rec pureEvent value = Ev(fun t -> (Some value, fun () -> pureEvent value))
    let rec noEvent () = Ev(fun t -> None, fun () -> noEvent ())

    let rec (<*>) ((Beh f) : Behavior<'a->'b>) (Beh(a) : Behavior<'a>) : Behavior<'b> = 
        let run t =
            let (v,cont) = a t
            let (map,mapCont) = f t
            map v, fun () -> mapCont() <*> cont()
        Beh run

    let rec (<**>) (Ev v : Event<Option<'a> -> Option<'b>>) ((Ev e): Event<'a>) : Event<'b> =
        let f t = 
            let (a,newS)  = e t
            let (cf,newC) = v t
            let cont () = newC() <**> newS()
            match cf with
                | None -> None, cont
                | Some cf -> 
                    let b = cf a 
                    b, cont
        Ev f

    let rec bind (Beh(e) : Behavior<'a>) (f : 'a -> Behavior<'b>) : Behavior<'b> = 
        let r t = 
            let (a,nextE) = e t
            let (Beh currentB) = f a
            let (b,nextB) = currentB t
            b, fun () -> bind (nextE ()) (fun a -> f a)
        Beh r

    let rec bindE (Ev(e) : Event<'a>) (f : 'a -> Event<'b>) : Event<'b> = 
        let r t = 
            let (a,nextE) = e t
            match a with
                | None   -> None, fun () -> bindE (nextE()) f
                | Some a -> 
                    let (Ev currentB) = f a
                    let (b,nextB) = currentB t
                    b, fun () -> bindE (nextE ()) (fun a -> f a)
        Ev r


    let rec toBeh (Ev e) : Behavior<Option<'b>> =
        Beh (fun t -> 
                let v, cont = e t
                v, fun () -> toBeh (cont())
        )

    type EventBuilder() =
        member x.Yield v = pureEvent v
        member x.Return (()) = noEvent ()
        member x.Zero () = noEvent ()
        member x.Bind(m,f) = bindE m f

    let evt = EventBuilder()

    type Engine() =
        let pendingTimes = System.Collections.Concurrent.ConcurrentQueue()

        member x.OfObservable (o : IObservable<'a>) : Event<'a> =
            let pending = System.Collections.Generic.Dictionary<DateTime,'a>()
            let self = ref Unchecked.defaultof<_>
            o.Subscribe(fun a -> 
                let t = DateTime.Now
                pending.[t] <- a 
                pendingTimes.Enqueue(t)
            ) |> ignore
            let f t =
                match pending.TryGetValue t with
                    | (true,v) -> 
                        pending.Remove t |> ignore
                        Some v, fun () -> !self
                    | _ -> None, fun () -> !self
            
            self := Ev f
            !self

        member x.Run(Beh bhf, time : (unit -> DateTime), samplingDt : TimeSpan, f : 'b -> 'c) =
            async {
                do! Async.SwitchToNewThread()
                let currentBh = ref Unchecked.defaultof<_>
                let rec doStep time last =
                    match pendingTimes.TryDequeue() with
                        | (true,v) -> 
                            let (last,bh) = bhf time
                            currentBh := bh
                            doStep time (Some last)
                        | _ -> last

                let mutable lastStep = time ()
                while true do
                    let current = time ()
                    match doStep current None with
                        | Some v -> 
                            lastStep <- current
                            f v |> ignore
                        | None -> 
                            if current - lastStep >= samplingDt then 
                                lastStep <- current
                                let (last,bh) = bhf current
                                currentBh := bh
                                f last |> ignore
                            else System.Threading.Thread.Sleep 10

            } |> Async.Start

    let test () =
        let f = new System.Windows.Forms.Form()
        let e = Engine()
        let keyDown = e.OfObservable(f.KeyDown)
        let test =
            evt {
                let! k = keyDown
                if k.KeyCode = System.Windows.Forms.Keys.A then
                    yield "a"
            } |> toBeh

        f.Show()
        e.Run(test, (fun () -> DateTime.Now), TimeSpan.FromSeconds 1.0, fun r -> printfn "%A" r)
        ()
//
//
//module Maya = 
//
//    FsiSetup.initFsi (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug";"Examples.exe"])
//
//    let quadSg =
//        let quad =
//            let index = [|0;1;2; 0;2;3|]
//            let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
//
//            IndexedGeometry(IndexedGeometryMode.TriangleList, index, SymDict.ofList [DefaultSemantic.Positions, positions :> Array], SymDict.empty)
//
//        // create a scenegraph given the indexedGeometry containing a quad
//        quad |> Sg.ofIndexedGeometry
//
//    let sg =
//        quadSg |> Sg.effect [
//                DefaultSurfaces.trafo |> toEffect                   // compose shaders by using FShade composition.
//                DefaultSurfaces.constantColor C4f.Red |> toEffect   // use standard trafo + map a constant color to each fragment
//                ]
//            // viewTrafo () creates camera controls and returns IMod<ICameraView> which we project to its view trafo component by using CameraView.viewTrafo
//            |> Sg.viewTrafo (viewTrafo   () |> Mod.map CameraView.viewTrafo ) 
//            // perspective () connects a proj trafo to the current main window (in order to take account for aspect ratio when creating the matrices.
//            // Again, perspective() returns IMod<Frustum> which we project to its matrix by mapping ofer Frustum.projTrafo.
//            |> Sg.projTrafo (perspective () |> Mod.map Frustum.projTrafo    )
//
//
//    let run () =
//        FsiSetup.initFsi (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug";"Examples.exe"])
//        setSg sg
//        win.Run()
//
//
//open Maya
//
//
//#if INTERACTIVE
//setSg sg
//printfn "Done. Modify sg and call setSg again in order to see the modified rendering result."
//#endif