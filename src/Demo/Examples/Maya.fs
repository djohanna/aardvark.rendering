#if INTERACTIVE
#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#load "FRPAgain.fsx"
open EventSystem
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


//module FRP =
//    open Aardvark.Base
//
//    type Time = DateTime
//   
//    type Behavior<'a> = Beh of (Time -> ('a * ReactBeh<'a>))
//    and ReactBeh<'a> = unit -> Behavior<'a>
//    
//    type Event<'a> = Ev of (Time -> (Option<'a> * ReactEvent<'a>))
//    and  ReactEvent<'a> = unit -> Event<'a>
//
//    let rec pureBehavior (value : 'a) = Beh(fun (t : Time) -> (value, fun () -> pureBehavior value))
//    let rec timeBehavior = Beh(fun t -> (t, fun () -> timeBehavior))
//    
//    let rec pureEvent value = Ev(fun t -> (Some value, fun () -> pureEvent value))
//    let rec noEvent () = Ev(fun t -> None, fun () -> noEvent ())
//
//    let rec (<*>) ((Beh f) : Behavior<'a->'b>) (Beh(a) : Behavior<'a>) : Behavior<'b> = 
//        let run t =
//            let (v,cont) = a t
//            let (map,mapCont) = f t
//            map v, fun () -> mapCont() <*> cont()
//        Beh run
//
//    let rec (<**>) (Ev v : Event<Option<'a> -> Option<'b>>) ((Ev e): Event<'a>) : Event<'b> =
//        let f t = 
//            let (a,newS)  = e t
//            let (cf,newC) = v t
//            let cont () = newC() <**> newS()
//            match cf with
//                | None -> None, cont
//                | Some cf -> 
//                    let b = cf a 
//                    b, cont
//        Ev f
//
//    let rec bind (Beh(e) : Behavior<'a>) (f : 'a -> Behavior<'b>) : Behavior<'b> = 
//        let r t = 
//            let (a,nextE) = e t
//            let (Beh currentB) = f a
//            let (b,nextB) = currentB t
//            b, fun () -> bind (nextE ()) (fun a -> f a)
//        Beh r
//
//    let rec bindE (Ev(e) : Event<'a>) (f : 'a -> Event<'b>) : Event<'b> = 
//        let r t = 
//            let (a,nextE) = e t
//            match a with
//                | None   -> None, fun () -> bindE (nextE()) f
//                | Some a -> 
//                    let (Ev currentB) = f a
//                    let (b,nextB) = currentB t
//                    b, fun () -> bindE (nextE ()) (fun a -> f a)
//        Ev r
//
//
//    let rec toBeh (Ev e) : Behavior<Option<'b>> =
//        Beh (fun t -> 
//                let v, cont = e t
//                v, fun () -> toBeh (cont())
//        )
//
//    type EventBuilder() =
//        member x.Yield v = pureEvent v
//        member x.Return (()) = noEvent ()
//        member x.Zero () = noEvent ()
//        member x.Bind(m,f) = bindE m f
//
//    let evt = EventBuilder()
//
//    type Engine() =
//        let pendingTimes = System.Collections.Concurrent.ConcurrentQueue()
//
//        member x.OfObservable (o : IObservable<'a>) : Event<'a> =
//            let pending = System.Collections.Generic.Dictionary<DateTime,'a>()
//            let self = ref Unchecked.defaultof<_>
//            o.Subscribe(fun a -> 
//                let t = DateTime.Now
//                pending.[t] <- a 
//                pendingTimes.Enqueue(t)
//            ) |> ignore
//            let f t =
//                match pending.TryGetValue t with
//                    | (true,v) -> 
//                        pending.Remove t |> ignore
//                        Some v, fun () -> !self
//                    | _ -> None, fun () -> !self
//            
//            self := Ev f
//            !self
//
//        member x.Run(Beh bhf, time : (unit -> DateTime), samplingDt : TimeSpan, f : 'b -> 'c) =
//            async {
//                do! Async.SwitchToNewThread()
//                let currentBh = ref Unchecked.defaultof<_>
//                let rec doStep time last =
//                    match pendingTimes.TryDequeue() with
//                        | (true,v) -> 
//                            let (last,bh) = bhf time
//                            currentBh := bh
//                            doStep time (Some last)
//                        | _ -> last
//
//                let mutable lastStep = time ()
//                while true do
//                    let current = time ()
//                    match doStep current None with
//                        | Some v -> 
//                            lastStep <- current
//                            f v |> ignore
//                        | None -> 
//                            if current - lastStep >= samplingDt then 
//                                lastStep <- current
//                                let (last,bh) = bhf current
//                                currentBh := bh
//                                f last |> ignore
//                            else System.Threading.Thread.Sleep 10
//
//            } |> Async.Start
//
//    let test () =
//        let f = new System.Windows.Forms.Form()
//        let e = Engine()
//        let keyDown = e.OfObservable(f.KeyDown)
//        let test =
//            evt {
//                let! k = keyDown
//                if k.KeyCode = System.Windows.Forms.Keys.A then
//                    yield "a"
//            } |> toBeh
//
//        f.Show()
//        e.Run(test, (fun () -> DateTime.Now), TimeSpan.FromSeconds 1.0, fun r -> printfn "%A" r)
//        ()


module Maya = 
    open  EventSystem.NewestShit
    FsiSetup.initFsi (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug";"Examples.exe"])

    type Input = MouseMove of V2d

    type Ball   = { position : V2d; speed : V2d; radius : double }
    type Rectangle = { position : V2d; size : V2d }

    type GameState = 
        {
            paddle    : Rectangle
            obstacles : list<Rectangle>
            ball      : Ball
        }
    let empty = 
        { 
            paddle = { position = V2d(0.0,-0.9); size = V2d(0.23, 0.05)} 
            obstacles = []
            ball = { position = V2d(0.0,0.9); speed = -V2d(1.0, 3.0).Normalized; radius = 0.05 } 
        }

    type Game = MainScreen | Running of GameState | GameOver
    
    let es = EventStream(Clock.absolute, empty)

    let moveTowardsMouse = 
        SF.whenever ( Pattern.lift (function MouseMove v -> Some v) ) (fun v -> 
            SF.continuous (fun last now (s : GameState) -> 
                let paddleToMouse = v - s.paddle.position
                let moveDirection = (now - last).TotalSeconds * 0.1 * V2d(paddleToMouse.X,0.0)
                let newPos = s.paddle.position + moveDirection
                { s with paddle = { s.paddle with  position = newPos } }
            )
        )

    let outOfBounds (v : V2d) : V2d =
        let x = if v.X < -1.0 then v.X + 1.0 elif v.X >= 1.0 then v.X - 1.0 else 0.0
        let y = if v.Y < -1.0 then v.Y + 1.0 elif v.Y >= 1.0 then v.Y - 1.0 else 0.0
        V2d(x,y)

    let reflectBall (o : Ball) (n : Ball) (b : Box2d) =
        let reflectBallOnce (op : V2d) (n : Ball) (b : Box2d) =
            let dir = n.speed |> Vec.normalize

            let mutable closest = V2d.Zero
            let mutable farthest = V2d.Zero
            b.GetMinMaxInDirection(dir, &closest, &farthest)

            let closest, farthest =
                if b.Contains op then farthest, closest
                else closest,farthest

            let ray = Ray3d(V3d(o.position, 0.0), V3d(dir, 0.0))
            // minimize t 
            // 0 <= t <= 1
            // 0 <= t1 <= 1
            // || (o + t * d) - (lo + t1 * ld) || < r

            // || (o - lo) + (t * d) - (t1 * ld) || < r

            let moved = Line2d(op, n.position)
            let lines = 
                [
                    Line2d(V2d(closest.X, closest.Y), V2d(closest.X, farthest.Y))
                    Line2d(V2d(closest.X, closest.Y), V2d(farthest.X, closest.Y))
                ]


            let mutable v = n.speed
            let mutable p = n.position

            let mutable changed = false
            let mutable hit = V2d.Zero

            for l in lines do
                let v3 = Func<V2d, V3d>(fun (v : V2d) -> V3d(v, 0.0))

                let mutable pt = V3d.Zero
                let distance = l.ToLine3d(v3).GetMinimalDistanceTo(moved.ToLine3d(v3), &pt)

                if distance < n.radius then
                    let n = V2d(-l.Direction.Y, l.Direction.X) |> Vec.normalize
                    let dd = Vec.dot (p - pt.XY)
                    p <- p - 2.0 * dd n * n
                    v <- v - 2.0 * Vec.dot v n * n
                    changed <- true

            if changed then Some (hit, { n with position = p; speed = v })
            else None

        let rec run (op : V2d) (n : Ball) =
            match reflectBallOnce op n b with
                | Some (hit, nn) -> run hit nn
                | None -> n

        run o.position n

    let ball =
        let world = Box2d.FromCenterAndSize(V2d.Zero, 2.0 * V2d.II)
        SF.continuous (fun last now s -> 
            let moveDirection = (now - last).TotalSeconds * 1.5 * s.ball.speed
            let newPos = s.ball.position + moveDirection
            let newBall = { s.ball with  position = newPos }

            { s with ball = reflectBall s.ball newBall world  }
        )

    win.Mouse.Move.Values.Subscribe(fun (oldP,newP) -> 
        let normalized = V2d(newP.Position) / V2d(win.Size)
        let r = V2d(2.0*normalized.X-1.0,1.0-2.0*normalized.Y)
        es.Push (MouseMove r)
    ) |> ignore

    

    es.Activate moveTowardsMouse
    es.Activate ball

    let s = win.Time |> Mod.map (fun _ -> 
        es.Evaluate()
    )

    let ballTrafo =
        s |> Mod.map (fun s -> Trafo3d.Translation(-0.5, -0.5, 0.0) * Trafo3d.Scale(s.ball.radius) * Trafo3d.Translation(V3d(s.ball.position,0.0)))

    let paddle =
        s |> Mod.map (fun s -> Trafo3d.Scale(V3d(s.paddle.size,1.0)) * Trafo3d.Translation(V3d(s.paddle.position,0.0)))


    let scene = 
        Sphere.solidUnitSphere C4b.Red 3 
        |> Sg.trafo ballTrafo 

    let sg =
        scene |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect                   // compose shaders by using FShade composition.
                DefaultSurfaces.constantColor C4f.Red |> toEffect   // use standard trafo + map a constant color to each fragment
                ]
            |> Sg.viewTrafo (Trafo3d.Identity |> Mod.constant)
            |> Sg.projTrafo (Trafo3d.Identity |> Mod.constant)


    let run () =
        FsiSetup.initFsi (Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; ".."; "bin";"Debug";"Examples.exe"])
        setSg sg
        win.Run()


open Maya


#if INTERACTIVE
setSg sg
printfn "Done. Modify sg and call setSg again in order to see the modified rendering result."
#endif