﻿namespace Aardvark.Application.WinForms


open System
open System.Diagnostics
open System.Windows.Forms
open System.Threading
open System.Collections.Concurrent
open Aardvark.Base

type IControl =
    abstract member Paint : unit -> unit
    abstract member Invalidate : unit -> unit
    abstract member Invoke : (unit -> unit) -> unit
    abstract member IsInvalid : bool

type RunningMean(maxCount : int) =
    let values = Array.zeroCreate maxCount
    let mutable index = 0
    let mutable count = 0
    let mutable sum = 0.0

    member x.Add(v : float) =
        let newSum = 
            if count < maxCount then 
                count <- count + 1
                sum + v
            else 
                sum + v - values.[index]

        sum <- newSum
        values.[index] <- v
        index <- (index + 1) % maxCount
              
    member x.Average =
        if count = 0 then 0.0
        else sum / float count  

type Periodic(interval : int, f : float -> unit) =
    let times = RunningMean(100)
    let sw = Stopwatch()

    member x.RunIfNeeded() =
        if not sw.IsRunning then
            sw.Start()
        else
            let dt = sw.Elapsed.TotalMilliseconds
               
            if interval = 0 || dt >= float interval then
                times.Add dt
                sw.Restart()
                f(times.Average / 1000.0)


[<AllowNullLiteral>]
type MyTimer(f : unit -> unit, due : int64, interval : int64) =
    
    let sw = Stopwatch()
    let cancel = new CancellationTokenSource()

    let run() =
        let ct = cancel.Token
        try
            Thread.Sleep(int due)
            while true do
                ct.ThrowIfCancellationRequested()
                sw.Restart()
                f()
                sw.Stop()
                let t = sw.Elapsed.TotalMilliseconds |> int64
                let sleep = max 0L (interval - t) |> int
                Thread.Sleep(sleep)

        with :? OperationCanceledException ->
            Log.line "Timer cancelled"
            ()

    let thread = Thread(ThreadStart(run), Priority = ThreadPriority.Highest, IsBackground = true)
    do thread.Start()

    member x.Dispose() =
        cancel.Cancel()
        thread.Join()
        cancel.Dispose()

    


type MessageLoop() as this =



    static let rec interlockedChange (location : byref<'a>) (update : 'a -> 'a) =
        let mutable oldValue = location
        let newValue = update oldValue
        let mutable ex = Interlocked.CompareExchange(&location, newValue, oldValue)
        while not <| System.Object.ReferenceEquals(ex, oldValue) do
            oldValue <- ex
            let newValue = update oldValue
            ex <- Interlocked.CompareExchange(&location, newValue, oldValue)

    let mutable q : hset<IControl> = HSet.empty
    let mutable timer : MyTimer = null
    let periodic = ConcurrentHashSet<Periodic>()

    let rec processAll() =
        let mine = Interlocked.Exchange(&q, HSet.empty)
        let mine = mine |> HSet.toList
        for ctrl in mine do
            try 
                if not ctrl.IsInvalid then
                    ctrl.Invoke (fun () -> ctrl.Invalidate())
            with e ->
                printfn "%A" e

    member private x.Process() =
        //Application.DoEvents()
        //for p in periodic do p.RunIfNeeded()
        processAll()
        //Application.DoEvents()

    member x.Start() =
        if timer <> null then
            timer.Dispose()

    
        timer <- new MyTimer((fun _ -> this.Process()), 0L, 2L)

    member x.Draw(c : IControl) =
        interlockedChange &q (fun q -> HSet.add c q)

    member x.EnqueuePeriodic (f : float -> unit, intervalInMilliseconds : int) =
        let p = Periodic(intervalInMilliseconds, f)
        periodic.Add p |> ignore

        { new IDisposable with
            member x.Dispose() =
                periodic.Remove p |> ignore
        }
            
    member x.EnqueuePeriodic (f : float -> unit) =
        x.EnqueuePeriodic(f, 1)