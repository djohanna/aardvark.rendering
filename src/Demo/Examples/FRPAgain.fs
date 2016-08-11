#if COMPILED
namespace EventSystem
#endif

open System
open System.Threading
open System.Collections.Generic
//open System.Reactive
//open System.Reactive.Linq
//open System.Reactive.Subjects
open Microsoft.FSharp.Control

type Event<'op> =
    | ApplicationStart
    | ApplicationEnd
    | NoEvent of DateTime
    | Event of DateTime * 'op

module Event =
    let time (e : Event<'a>) =
        match e with
            | ApplicationStart -> DateTime.MinValue
            | ApplicationEnd -> DateTime.MaxValue
            | NoEvent t -> t
            | Event(t,_) -> t

    let create op =
        Event(DateTime.Now, op)

module NewShit = 
    
    type Event<'op> =
        | Event         of DateTime * 'op
        | TimePassed    of start : DateTime * stop : DateTime
        | Init          of DateTime

    module Event =
        let time (e : Event<'a>) =
            match e with
                | Init t -> t
                | Event(t,_) -> t
                | TimePassed(_,t) -> t



    type Pattern<'op, 'a> =
        abstract member IsMatch : 'op -> PatternMatch<'op, 'a>

    and PatternMatch<'op, 'a> =
        | NoMatch
        | Match of 'a

    [<AutoOpen>]
    module PatternExt =
        type Pattern<'op, 'a> with
            member x.Match(e : Event<'op>) =
                match e with
                    | Event(_,e) -> x.IsMatch e
                    | _ -> NoMatch



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Pattern =
        let is<'op, 'a> =
            { new Pattern<'op, 'a> with
                member x.IsMatch op =
                    match op :> obj with
                        | (:? 'a as a) -> Match a
                        | _ -> NoMatch
            }

        let lift (f : 'op -> bool) =
            { new Pattern<'op, unit> with
                member x.IsMatch o =
                    if f o then Match ()
                    else NoMatch
            }

        let choose (f : 'op -> Option<'a>) =
            { new Pattern<'op, 'a> with
                member x.IsMatch o = 
                    match f o with
                        | Some a -> Match a
                        | None -> NoMatch
            }

        let rec ignore (p : Pattern<'op, 'a>) =
            { new Pattern<'op, unit> with
                member x.IsMatch o =
                    match p.IsMatch o with
                        | Match _ -> Match ()
                        | NoMatch -> NoMatch
            }

        let any =
            { new Pattern<'op, unit> with
                member x.IsMatch(o) =
                     Match ()
            }

    
    let (~~) (v : 'a) = Pattern.lift ((=) v)
    let (-->) (a : 'a) (b : 'b) = (a,b)

    type SF<'op, 'state> =
        | SF of (SF<'op, 'state> * Event<'op> * 'state -> 'state * list<SF<'op, 'state>>)

    type StartStop<'op, 'state, 'a, 'b> = { start : Pattern<'op, 'a>; stop : Pattern<'op, 'b>; body : SF<'op, 'state>}
    type DeltaTime = Δ of float
    type FromTo = Θ of DateTime * DateTime

    module SF =

        let init (state : 's) (t : DateTime) (SF inner as f) =
            inner(f, Init(t), state)

        let rec attempt (pattern : Pattern<'op, 'evt>) (f : 'evt -> SF<'op, 'state>) : SF<'op, 'state> =
            SF (fun (self,e,state) ->
                match pattern.Match e with
                    | NoMatch -> state, []
                    | Match evt -> evt |> f |> init state (Event.time e)
            )

        let once (f : 'state -> 'state) =
            SF (fun (self, e, state) ->
                let state = f state
                state, []
            )

        let rec whenever (pattern : Pattern<'op, 'evt>) (f : 'evt -> SF<'op, 'state>) : SF<'op, 'state> =
            SF (fun (self,e,state) ->
                match pattern.Match e with
                    | NoMatch -> state, [self]
                    | Match evt -> 
                        let state,cont = evt |> f |> init state (Event.time e)
                        state, (self::cont)
            )


//
//        let rec until (pattern : Pattern<'op, 'evt>) (f : DateTime -> DateTime -> 'state -> 'state) : SF<'op, 'state> =
//            SF (fun (self, e, state) ->
//                match e with
//                    | TimePassed(s,e) ->
//                        
//                        let state = f s e state
//                        state, [self]
//                    | _ ->
//                        match pattern.Match e with
//                            | Match _ -> state, []
//                            | _ -> state, [self]
//            )
//
//        let rec until' (endPattern : Pattern<'op, 'evt>) (runPattern : Pattern<'op, 'a>) (f : DateTime -> 'a -> 'state -> 'state) : SF<'op, 'state> =
//            SF (fun (self, e, state) ->
//                match endPattern.Match e with
//                    | Match _ -> state, []
//                    | NoMatch ->
//                        match runPattern.Match e with
//                            | Match m -> 
//                                let state = f (Event.time e) m state
//                                state, [self]
//                            | NoMatch ->
//                                state, [self]
//            )


        let continuous (f : DateTime -> DateTime -> 'state -> 'state) =
            SF (fun (self, e, state) ->
                match e with
                    | TimePassed(s, e) ->
                        let state = f s e state
                        state, [self]
                    | _ ->
                        state, [self]
            )

        let rec until (endPattern : Pattern<'op, 'evt>) (SF f as a) : SF<'op, 'state> =
            SF (fun (self, e, state) ->
                match endPattern.Match e with
                    | Match _ -> state, []
                    | NoMatch ->
                        let (state, c) = f(a, e, state)
                        state, (c |> List.map (until endPattern))
            )

        let startStop (start : Pattern<'op, _>) (stop : Pattern<'op, _>)  (sf : SF<'op, 'state>) =
            whenever start (fun e -> until stop sf)

        type StartStopBuilder() =

            member x.Yield(u : unit) : unit = ()
            member x.Yield(sf : SF<_,_>) = sf

            [<CustomOperation("continuously")>]
            member x.Cont(before : Pattern<'op,_> * Pattern<'op,_>, d : DateTime -> DateTime -> 'state -> 'state) : SF<_,_> = 
                let start, stop = before
                startStop start stop (continuous d)

            member x.For(pattern : Pattern<'op, 'evt>, f : 'evt -> SF<'op, 'state>) : SF<_,_> = whenever pattern f

            member x.For(ss : Pattern<'op, 'evt> * Pattern<'op, 'evt>, f : 'state * DeltaTime -> 'state) =
                let start, stop = ss
                startStop start stop (continuous(fun s e state -> f (state, Δ (e - s).TotalSeconds)))

            member x.For(ss : Pattern<'op, 'evt> * Pattern<'op, 'evt>, f : 'state * FromTo -> 'state) =
                let start, stop = ss
                startStop start stop (continuous(fun s e state -> f (state, Θ(s,e))))


            member x.Return (s : 'state) = s

            member x.Delay(f) = f()
            member x.While(f : unit -> Pattern<'op,_> * Pattern<'op,_>, body : unit) =
                let (start, stop) = f()
                (start, stop)

            member x.While(f : unit -> Pattern<'op,_> * Pattern<'op,_>, body : SF<_,_>) =
                let (start, stop) = f()
                startStop start stop body

        let run = StartStopBuilder()

   
    type EventStream<'state, 'op>(state : 'state) =
        let mutable active = List<SF<_,_>>()

        let mutable initial = true
        let mutable time = DateTime.Now
        let mutable state = state

        member x.Activate(f : SF<'op, 'state>) =
            active.Add f

        member x.Push(now : DateTime, op : 'op) =
            let e = Event(now, op)

            let newActive = List<_>()

            let initE = 
                if initial then Init now
                else TimePassed(time, now)

            for SF f as a in active do
                let (s, conts) = f(a, initE, state)
                state <- s
                for SF f as c in conts do
                    let (s,c) = f(c, e, s)
                    newActive.AddRange(c)
                    state <- s
            
            initial <- false
            active <- newActive
            time <- now

        member x.Evaluate(now : DateTime) : 'state =
            let newActive = List<_>()

            let initE = 
                if initial then Init now
                else TimePassed(time, now)

            for (SF f) as a in active do
                let (s, c) = f(a, initE, state)
                newActive.AddRange(c)
                state <- s

            initial <- false
            active <- newActive
            time <- now
            state
    

    type Op =
        | StartMove
        | StopMove
        | Down
        | Up
        | Move of float


    open SF
    let test2() = 
        let s = EventStream(0.0)


        let sf =
            SF.whenever 
                (Pattern.lift ((=) StartMove))
                (fun _ ->
                    printfn "start"
                    SF.until
                        (Pattern.lift ((=) StopMove))
                        (SF.continuous (fun ts te state ->
                            printfn "step %A" (te - ts).TotalSeconds
                            state + (te - ts).TotalSeconds
                        ))
                )

        let sf =
            SF.startStop 
                (Pattern.lift ((=) StartMove))
                (Pattern.lift ((=) StopMove))
                (
                    SF.continuous (fun ts te state ->
                        printfn "step %A" (te - ts).TotalSeconds
                        state + (te - ts).TotalSeconds
                    )
                )

        let sf =
            run {
                while ~~StartMove --> ~~StopMove do
                    continuously (fun ts te state ->
                        printfn "step %A" (te - ts).TotalSeconds
                        state + (te - ts).TotalSeconds
                    )
            }
 
        let sf =
            run {
                for state, Θ(start, stop) in ~~StartMove --> ~~StopMove do
                    return state + (stop - start).TotalSeconds
            }
        let sf =
            run {
                for state, Δ(dt) in ~~StartMove --> ~~StopMove do
                    return state + dt
            } 

//        let test (stream : IObservable<Op>) (up : IObservable<unit>) (move : IObservable<float>) =            
//            stream.Aggregate(
//                (false, Move 1.0),
//                (fun (down, _) e->
//                    match e with
//                        | Down -> (true,e)
//                        | Up -> (false, e)
//                        | _ -> (down, e)
//                )
//            ).Where(fun (down,e) -> down)
//             .Aggregate(0.0, fun state (_,m) ->
//                match m with
//                    | Move delta -> state + delta
//                    | _ -> state
//            )
//            
         
                
        s.Activate(sf)
        let t0 = DateTime.Now
        
        let t1 = t0 + TimeSpan.FromMilliseconds(100.0)
        let t2 = t1 + TimeSpan.FromMilliseconds(100.0)
        let t3 = t2 + TimeSpan.FromMilliseconds(100.0)
        let t4 = t3 + TimeSpan.FromMilliseconds(100.0)
        let t5 = t4 + TimeSpan.FromMilliseconds(100.0)

        s.Push(t0, StartMove)
        s.Push(t1, StopMove)
        s.Evaluate(t1) |> printfn "%A"
        s.Push(t3, StartMove)
        s.Evaluate(t4) |> printfn "%A"
        s.Evaluate(t5) |> printfn "%A"

    let test3() =
        let s = EventStream(0.0)




        let sf =
            SF.whenever 
                (Pattern.lift ((=) Down))
                (fun _ ->
                    printfn "down"
                    SF.until
                        (Pattern.lift ((=) Up)) 
                        (SF.whenever
                            (Pattern.choose (function Move v -> Some v | _ -> None)) 
                            (fun delta ->
                                printfn "move %A" delta
                                SF.once ((+) delta)
                            )
                        )
                )

        let sf =
            SF.startStop 
                (Pattern.lift ((=) Down))
                (Pattern.lift ((=) Up)) 
                (
                    SF.whenever
                        (Pattern.choose (function Move v -> Some v | _ -> None)) 
                        (fun delta ->
                            printfn "move %A" delta
                            SF.once ((+) delta)
                        )
                )

        let sf =
            let moveDelta = Pattern.choose (function Move v -> Some v | _ -> None)
            run {
                while ~~Down --> ~~Up do
                    for delta in moveDelta do
                        printfn "move %A" delta
                        yield SF.once ((+) delta)
            }               
        
        s.Activate(sf)   

        let events = [Down; Move 1.0; Move 0.5; Up; Move 1.0; Down; Move 0.1; Up; Move 1.0]

        let mutable t = DateTime.Now
        for e in events do
            s.Push(t, e)
            t <- t + TimeSpan.FromMilliseconds(1.0)

        s.Evaluate(t) |> printfn "res: %A"

open Aardvark.Base
open Aardvark.Base.Incremental

module NewestShit =

    type Event<'op> =
        | Event         of DateTime * 'op
        | TimePassed    of start : DateTime * stop : DateTime
        | Init          of DateTime

    [<AbstractClass>]
    type Pattern<'op, 'a>() =
        abstract member Match : 'op -> Match<'op, 'a>

    and Match<'op, 'a> =
        | NoMatch
        | Match of 'a
        | Continue of Pattern<'op, 'a>

    [<AutoOpen>]
    module PatternOperators = 
        let rec (|>>) (p : Pattern<'op, 'a>) (f : 'a -> 'b) =
            { new Pattern<'op, 'b>() with
                member x.Match o =
                    match p.Match o with
                        | NoMatch -> NoMatch
                        | Match v -> Match (f v)
                        | Continue c -> Continue (c |>> f)
            }

        let rec (.>.) (l : Pattern<'op, 'a>) (r : Pattern<'op, 'b>) =
            { new Pattern<'op, 'a * 'b>() with
                member x.Match o =
                    match l.Match o with
                        | NoMatch -> NoMatch
                        | Match a -> r |>> (fun b -> (a,b)) |> Continue
                        | Continue c -> c .>. r |> Continue
            }

        let rec (.>) (l : Pattern<'op, 'a>) (r : Pattern<'op, 'b>) =
            { new Pattern<'op, 'a>() with
                member x.Match o =
                    match l.Match o with
                        | NoMatch -> NoMatch
                        | Match a -> r |>> (fun b -> a) |> Continue
                        | Continue c -> c .> r |> Continue
            }

        let rec (>.) (l : Pattern<'op, 'a>) (r : Pattern<'op, 'b>) =
            { new Pattern<'op, 'b>() with
                member x.Match o =
                    match l.Match o with
                        | NoMatch -> NoMatch
                        | Match a -> r |>> (fun b -> b) |> Continue
                        | Continue c -> c >. r |> Continue
            }


        let rec (<|>) (l : Pattern<'op, 'a>) (r : Pattern<'op, 'a>) = 
            { new Pattern<'op, 'a>() with
                member x.Match o =
                    match l.Match o, r.Match o with
                        | Match v, _ | _, Match v -> Match v
                        | NoMatch, o | o, NoMatch -> o
                        | Continue l, Continue r -> l <|> r |> Continue
            }

    module Pattern =
        let never<'op, 'a> =
            { new Pattern<'op, 'a>() with
                member x.Match _ = NoMatch
            }

        let any<'op> =
            { new Pattern<'op, 'op>() with
                member x.Match o = Match o
            }

        let exactly v = 
            { new Pattern<'op, 'op>() with
                member x.Match o =
                    if o = v then Match o
                    else NoMatch
            }

        let lift (f : 'op -> Option<'a>) =
            { new Pattern<'op, 'a>() with
                member x.Match o = 
                    match f o with
                        | Some v -> Match v
                        | None -> NoMatch
            }

        let test (f : 'op -> bool) =
            { new Pattern<'op, 'op>() with
                member x.Match o = 
                    if f o then Match o
                    else NoMatch
            }

        let inline map (f : 'a -> 'b) (m : Pattern<'op, 'a>) = m |>> f

        let concat (l : list<Pattern<'op, 'a>>) =
            let rec concat (res : list<'a>) (l : list<Pattern<'op, 'a>>) =
                match l with
                    | [] ->
                        failwith "cannot create empty pattern"

                    | [single] -> 
                        single |> map (fun v -> v :: res |> List.rev)

                    | head :: rest ->
                        { new Pattern<'op, list<'a>>() with
                            member x.Match a =
                                match head.Match a with
                                    | NoMatch -> NoMatch
                                    | Match a -> concat (a :: res) (rest) |> Continue
                                    | Continue c -> concat res (c :: rest) |> Continue
                            
                        }

            concat [] l

        let rec choice (patterns : list<Pattern<'op, 'a>>) =
            { new Pattern<'op, 'a>() with
                member x.Match o =
                    let mutable result = None
                    let mutable next = System.Collections.Generic.List<_>()
                    for p in patterns do
                        match p.Match o with
                            | NoMatch -> ()
                            | Match v -> result <- Some v
                            | Continue c -> next.Add c
                    
                    match result with
                        | Some r -> Match r
                        | None -> 
                            let next = next |> Seq.toList 
                            match next with
                                | [] -> NoMatch
                                | _ -> next |> choice |> Continue

            }


        // broken!!!
        let sepBy (p : Pattern<'op, 'a>) (sep : Pattern<'op, 'b>) =
            let rec sepBy (res : list<'a>) (expectSep : bool) (cp : Pattern<'op, 'a>) (csep : Pattern<'op, 'b>) =
                { new Pattern<'op, list<'a>>() with
                    member x.Match o =
                        if expectSep then
                            match csep.Match o with
                                | NoMatch -> 
                                    Match (List.rev res)

                                | Match _ ->
                                    sepBy res false p sep |> Continue

                                | Continue csep ->
                                    sepBy res true p csep |> Continue
                                    
                        else 
                            match cp.Match o with
                                | NoMatch -> 
                                    Match (List.rev res)

                                | Match v -> 
                                    sepBy (v :: res) true p sep |> Continue

                                | Continue cp -> 
                                    sepBy res false cp sep |> Continue
                }

            sepBy [] false p sep

        
    // Click = Down [^Move]* Up (< 0.1s)

    [<AutoOpen>]
    module ExtendedPatternOperators = 
        let inline (~~) (v : 'a) = Pattern.exactly v


    type SF<'op, 'state> =
        abstract member Run : Event<'op> * 'state -> 'state * list<SF<'op, 'state>>

    module SF =
        let rec attempt (p : Pattern<'op, 'a>) (f : 'a -> SF<'op, 'state>) =
            { new SF<'op, 'state> with
                member x.Run(e,state) =
                    match e with
                        | Event(t,op) ->
                            match p.Match op with
                                | NoMatch -> state, []
                                | Match v -> state, [f v]
                                | Continue c -> state, [attempt c f]
                        | _ ->
                            state, [x]
            }

        let whenever (p : Pattern<'op, 'a>) (f : 'a -> SF<'op, 'state>) =
            { new SF<'op, 'state> with
                member x.Run(e,state) =
                    match e with
                        | Event(t,op) ->
                            match p.Match op with
                                | NoMatch -> state, [x]
                                | Match v -> state, [x; f v]
                                | Continue c -> state, [x; attempt c f]
                        | _ ->
                            state, [x]
            }

        let once (f : 'state -> 'state) =
            { new SF<'op, 'state> with
                member x.Run(_, state) =
                    f state, []
            }

        let rec until (endPattern : Pattern<'op, 'a>) (a : SF<'op, 'state>) : SF<'op, 'state> =
            { new SF<'op, 'state> with
                member x.Run(e,state) = 
                    match e with
                        | Event(_,op) ->
                            match endPattern.Match op with
                                | Match _ -> state, []
                                | Continue c -> state, [until (endPattern <|> c) a]
                                | NoMatch ->
                                    let (state, c) = a.Run(e, state)
                                    state, (c |> List.map (until endPattern))
                        | _ ->
                            let (state, c) = a.Run(e, state)
                            state, (c |> List.map (until endPattern))
            }
    
        let startStop (pstart : Pattern<'op, _>) (pend : Pattern<'op, _>)  (sf : SF<'op, 'state>) =
            let rec startStop (pattern : Choice<Pattern<'op, _>, Pattern<'op, _>>) (current : SF<'op, 'state>) =
                { new SF<'op, 'state> with
                    member x.Run(e,state) =
                        match pattern with
                            | Choice1Of2 cstop ->
                                match e with
                                    | Event(t,op) ->
                                        match cstop.Match op with
                                            | NoMatch -> 
                                                let state, conts = current.Run(e, state)
                                                state, conts |> List.map (startStop (Choice1Of2 cstop))

                                            | Match _ -> 
                                                state, [startStop (Choice2Of2 pstart) sf]

                                            | Continue c -> 
                                                state, [startStop (Choice1Of2 (c <|> cstop)) current]
                                    | _ ->
                                        let state, conts = current.Run(e, state)
                                        state, conts |> List.map (startStop (Choice1Of2 cstop))

                            | Choice2Of2 cstart ->
                                match e with
                                    | Event(t,op) ->
                                        match cstart.Match op with 
                                            | NoMatch -> state, [x]
                                            | Match _ -> state, [startStop (Choice1Of2 pend) sf]
                                            | Continue c -> state, [startStop (Choice2Of2 (pstart <|> c)) sf]
                                    | _ ->
                                        state, [x]
                }

            startStop (Choice2Of2 pstart) sf

            

        let continuous (f : DateTime -> DateTime -> 'state -> 'state) =
            { new SF<'op, 'state> with
                member x.Run(e, state) = 
                    match e with
                        | TimePassed(s, e) ->
                            let state = f s e state
                            state, [x]
                        | _ ->
                            state, [x]
            }

    type Clock =
        abstract member Now : DateTime

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Clock =
        let absolute =
            let start = DateTime.Now
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()
            { new Clock with
                member x.Now = start + sw.Elapsed
            }

        let fake =
            let start = DateTime.Now
            let mutable ms = 0L
            { new Clock with
                member x.Now =
                    let m = Interlocked.Increment(&ms)
                    start + TimeSpan.FromMilliseconds(float m)
                    
            }       

    type EventStream<'state, 'op>(clock : Clock, state : 'state) =
        let mutable active = List<SF<_,_>>()

        let mutable initial = true
        let mutable time = clock.Now
        let mutable state = state

        member x.Activate(f : SF<'op, 'state>) =
            lock x (fun () ->
                active.Add f
            )

        member x.Push(op : 'op) =
            lock x (fun () ->
                let now = clock.Now
                let e = Event(now, op)

                let newActive = List<_>()

                let initE = 
                    if initial then Init now
                    else TimePassed(time, now)

                for a in active do
                    let (s, conts) = a.Run(initE, state)
                    state <- s
                    for c in conts do
                        let (s,c) = c.Run(e, s)
                        newActive.AddRange(c)
                        state <- s
            
                initial <- false
                active <- newActive
                time <- now
            )

        member x.Evaluate() : 'state =
            lock x (fun () ->
                let newActive = List<_>()
                let now = clock.Now
                let initE = 
                    if initial then Init now
                    else TimePassed(time, now)

                for a in active do
                    let (s, c) = a.Run(initE, state)
                    newActive.AddRange(c)
                    state <- s

                initial <- false
                active <- newActive
                time <- now
                state
            )


    type Op =
        | StartMove
        | StopMove
        | Down
        | Up
        | Move of float

    let test2() = 
        let state = EventStream(Clock.fake, 0.0)


        let sf =
            SF.startStop 
                (~~StartMove)
                (~~StopMove)
                (
                    SF.continuous (fun ts te state ->
                        printfn "step %A" (te - ts).TotalSeconds
                        state + (te - ts).TotalSeconds
                    )
                )

        state.Activate(sf)

        state.Push StartMove
        state.Push StopMove                 // 0.001
        state.Evaluate() |> printfn "%A"    // 0.001
        state.Push StartMove                // 0.001
        state.Evaluate() |> printfn "%A"    // 0.002
        state.Evaluate() |> printfn "%A"    // 0.003

    let test3() =
        let s = EventStream(Clock.fake, 0.0)

        let sf =
            SF.startStop 
                (~~Down)
                (~~Up) 
                (
                    SF.whenever
                        (Pattern.lift (function Move v -> Some v | _ -> None)) 
                        (fun delta ->
                            printfn "move %A" delta
                            SF.once (fun state -> state + delta)
                        )
                )

        let basic = 
            SF.whenever ~~Down ( fun _ -> 
                SF.until ~~Up 
                    (
                        SF.whenever
                            ( Pattern.lift (function Move v -> Some v | _ -> None) ) 
                            ( fun delta ->
                                printfn "move %A" delta
                                SF.once ((+) delta)
                            )
                    )
        )

        s.Activate(sf)   

        let events = [Down; Move 1.0; Move 0.5; Up; Move 1.0; Down; Move 0.1; Up; Move 1.0]

        for e in events do
            s.Push(e)

        s.Evaluate() |> printfn "res: %A"

    let test4() =
        let s = EventStream(Clock.fake, 0.0)

        let sf =
            SF.startStop 
                (~~Down >. ~~Down)
                (~~Down >. ~~Down) 
                (
                    SF.whenever
                        (Pattern.lift (function Move v -> Some v | _ -> None)) 
                        (fun delta ->
                            printfn "move %A" delta
                            SF.once ((+) delta)
                        )
                )

        s.Activate(sf)   

        let events = [
            Down; Down; 
            Move 1.0; Move 0.5; 
            Down; Down; 
            Move 1.0; 
            Down; Down; 
            Move 0.1; 
            Down; Down; 
            Move 1.0
        ]

        for e in events do
            s.Push(e)

        s.Evaluate() |> printfn "res: %A"


    let testPattern() =
        let abc = (~~'a' <|> ~~'x') .> ~~'b' .> ~~'c'
        let abc = Pattern.concat [(~~'a' <|> ~~'x'); ~~'b'; ~~'c']

        let mutable current = [abc]
        let str = "ababcxbccbc"
        for c in str do 
            //printfn "%c: count: %A" c (List.length current)
            let derived = 
                current |> List.choose (fun p ->
                    match p.Match c with
                        | NoMatch -> 
                            None

                        | Match c -> 
                            printfn "matched %A" c
                            None

                        | Continue c -> Some c
                )
            current <- abc :: derived
            
    let run() = 
        //Parser.Parser.test()
        NewShit.test2()