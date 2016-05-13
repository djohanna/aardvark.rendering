﻿namespace Aardvark.Rendering.GL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Runtime
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL.Compiler


type IRenderProgram =
    inherit IAdaptiveProgram<unit>
    abstract member Run : unit -> FrameStatistics


[<AbstractClass>]
type AbstractRenderProgram<'input when 'input :> IAdaptiveObject>() =
    inherit DirtyTrackingAdaptiveObject<'input>()
    
    abstract member Dispose : unit -> unit
    abstract member Update : HashSet<'input> -> unit
    abstract member Run : unit -> FrameStatistics

    interface IRenderProgram with
        member x.Run() = x.Run()

    interface IAdaptiveProgram<unit> with
        member x.Update caller =
            x.EvaluateIfNeeded' caller AdaptiveProgramStatistics.Zero (fun dirty ->
                x.Update dirty
                AdaptiveProgramStatistics.Zero
            )

        member x.Run s = x.Run() |> ignore
        member x.Disassemble() = null
        member x.AutoDefragmentation 
            with get() = false
            and set _ = ()

        member x.StartDefragmentation() = System.Threading.Tasks.Task.FromResult(TimeSpan.Zero)
        member x.NativeCallCount = 0
        member x.FragmentCount = 1
        member x.ProgramSizeInBytes = 0L
        member x.TotalJumpDistanceInBytes = 0L
        member x.Dispose() = x.Dispose()
  
[<AbstractClass>]
type AbstractRenderProgram() =
    inherit AdaptiveObject()
    
    abstract member Dispose : unit -> unit
    abstract member Update : unit -> unit
    abstract member Run : unit -> FrameStatistics

    interface IRenderProgram with
        member x.Run() = x.Run()

    interface IAdaptiveProgram<unit> with
        member x.Update caller =
            x.EvaluateIfNeeded caller AdaptiveProgramStatistics.Zero (fun () ->
                x.Update ()
                AdaptiveProgramStatistics.Zero
            )

        member x.Run s = x.Run() |> ignore
        member x.Disassemble() = null
        member x.AutoDefragmentation 
            with get() = false
            and set _ = ()

        member x.StartDefragmentation() = System.Threading.Tasks.Task.FromResult(TimeSpan.Zero)
        member x.NativeCallCount = 0
        member x.FragmentCount = 1
        member x.ProgramSizeInBytes = 0L
        member x.TotalJumpDistanceInBytes = 0L
        member x.Dispose() = x.Dispose()
  

module RenderProgram =
    
    type private WrappedRenderProgram(inner : IAdaptiveProgram<unit>) =
        inherit AdaptiveObject()

        abstract member Run : unit -> FrameStatistics
        default x.Run() = inner.Run(); FrameStatistics.Zero

        member x.RunInner() = inner.Run()


        interface IRenderProgram with
            member x.Run() = x.Run()

        interface IAdaptiveProgram<unit> with
            member x.Update caller =
                x.EvaluateIfNeeded caller AdaptiveProgramStatistics.Zero (fun () ->
                    inner.Update(x)
                )

            member x.Run s = x.Run() |> ignore
            member x.Disassemble() = inner.Disassemble()
            member x.AutoDefragmentation 
                with get() = inner.AutoDefragmentation
                and set v = inner.AutoDefragmentation <- v

            member x.StartDefragmentation() = inner.StartDefragmentation()
            member x.NativeCallCount = inner.NativeCallCount
            member x.FragmentCount = inner.FragmentCount
            member x.ProgramSizeInBytes = inner.ProgramSizeInBytes
            member x.TotalJumpDistanceInBytes = inner.TotalJumpDistanceInBytes
            member x.Dispose() = inner.Dispose()
  

    let custom comparer handler input =
        let program = AdaptiveProgram.custom comparer handler input
        new WrappedRenderProgram(program) :> IRenderProgram

    [<AutoOpen>]
    module Compiler =
        let compileDelta (scope : RenderTaskScope) (l : Option<PreparedMultiRenderObject>) (r : PreparedMultiRenderObject) =
            let mutable last =
                match l with
                    | Some l -> Some l.Last
                    | None -> None

            let code = 
                [ for r in r.Children do
                    match last with
                        | Some last -> yield! Aardvark.Rendering.GL.Compiler.DeltaCompiler.compileDelta scope.currentContext last r
                        | None -> yield! Aardvark.Rendering.GL.Compiler.DeltaCompiler.compileFull scope.currentContext r
                    last <- Some r
                ]

            let myStats = ref FrameStatistics.Zero
            let stats = scope.stats
            let calls =
                code |> List.map (fun i ->
                    match i.IsConstant with
                        | true -> 
                            let i = i.GetValue()
                            let cnt = List.length i
                            let dStats = { FrameStatistics.Zero with InstructionCount = float cnt }
                            stats := !stats + dStats
                            myStats := !myStats + dStats

                            Mod.constant i

                        | false -> 
                            let mutable oldCount = 0
                            i |> Mod.map (fun i -> 
                                let newCount = List.length i
                                let dCount = newCount - oldCount
                                oldCount <- newCount
                                let dStats = { FrameStatistics.Zero with InstructionCount = float dCount }

                                stats := !stats + dStats
                                myStats := !myStats + dStats
                                i
                            )
                )


            { new IAdaptiveCode<Instruction> with
                member x.Content = calls
                member x.Dispose() =    
                    for o in code do
                        for i in o.Inputs do
                            i.RemoveOutput o

                    stats := !stats - !myStats
                    myStats := FrameStatistics.Zero

            }

        let compileFull scope r =
            compileDelta scope None r

    module Native =
        let private instructionToCall (i : Instruction) : NativeCall =
            let compiled = ExecutionContext.compile i
            compiled.functionPointer, compiled.args

        let optimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let inner = FragmentHandler.native 6

            let handler = FragmentHandler.warpDifferential instructionToCall ExecutionContext.callToInstruction (compileDelta scope) inner

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram

        let unoptimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let inner = FragmentHandler.native 6

            let handler = FragmentHandler.wrapSimple instructionToCall ExecutionContext.callToInstruction (compileFull scope) inner

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram

    module GLVM =
        let private glvmBase scope needsPrev mode comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            GLVM.vmInit()

            let vmStats = ref (VMStats())

            let prolog = GLVM.vmCreate()
            let epilog = GLVM.vmCreate()

            let getArgs (o : Instruction) =
                o.Arguments |> Array.map (fun arg ->
                    match arg with
                        | :? int as i -> nativeint i
                        | :? nativeint as i -> i
                        | :? float32 as f -> BitConverter.ToInt32(BitConverter.GetBytes(f), 0) |> nativeint
                        | _ -> failwith "invalid argument"
                )

            let appendToBlock (frag : FragmentPtr) (id : int) (instructions : seq<Instruction>) =
                for i in instructions do
                    match getArgs i with
                        | [| a |] -> GLVM.vmAppend1(frag, id, i.Operation, a)
                        | [| a; b |] -> GLVM.vmAppend2(frag, id, i.Operation, a, b)
                        | [| a; b; c |] -> GLVM.vmAppend3(frag, id, i.Operation, a, b, c)
                        | [| a; b; c; d |] -> GLVM.vmAppend4(frag, id, i.Operation, a, b, c, d)
                        | [| a; b; c; d; e |] -> GLVM.vmAppend5(frag, id, i.Operation, a, b, c, d, e)
                        | _ -> failwithf "invalid instruction: %A" i

            let handler() =
                {
                    compileNeedsPrev = needsPrev
                    nativeCallCount = ref 0
                    jumpDistance = ref 0
                    prolog = prolog
                    epilog = epilog
                    compileDelta = if needsPrev then compileDelta scope else fun _ r -> compileFull scope r
                    startDefragmentation = fun _ _ _ -> Task.FromResult TimeSpan.Zero
                    run = fun() -> 
                        GLVM.vmRun(prolog, mode, &vmStats.contents)
                    memorySize = fun () -> 0L
                    alloc = fun code -> 
                        let ptr = GLVM.vmCreate()
                        let id = GLVM.vmNewBlock ptr
                        appendToBlock ptr id code
                        ptr
                    free = GLVM.vmDelete
                    write = fun ptr code ->
                        GLVM.vmClear ptr
                        let id = GLVM.vmNewBlock ptr
                        appendToBlock ptr id code
                        false

                    writeNext = fun prev next -> GLVM.vmLink(prev, next); 0
                    isNext = fun prev frag -> GLVM.vmGetNext prev = frag
                    dispose = fun () -> GLVM.vmDelete prolog; GLVM.vmDelete epilog
                    disassemble = fun f -> []
                }

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()

                    let baseStats = !scope.stats
                    let vmStats = !vmStats

                    { baseStats with
                        ActiveInstructionCount = baseStats.ActiveInstructionCount - float vmStats.RemovedInstructions
                    }
            } :> IRenderProgram

        let optimized scope comparer input =
            glvmBase scope true VMMode.None comparer input

        let runtime scope comparer input =
            glvmBase scope false VMMode.RuntimeRedundancyChecks comparer input

        let unoptimized scope comparer input =
            glvmBase scope false VMMode.None comparer input

    [<AllowNullLiteral>]
    type private ManagedFragment =
        class
            val mutable public Next : ManagedFragment
            val mutable public Instructions : Instruction[]

            new(next, instructions) = { Next = next; Instructions = instructions }
            new(instructions) = { Next = null; Instructions = instructions }
        end


    module Managed =
        let private run (f : ManagedFragment) =
            let rec all (f : ManagedFragment) =
                if isNull f then 
                    Seq.empty
                else
                    seq {
                        yield f.Instructions
                        yield! all f.Next
                    }

            let all = all f
            for part in all do
                for i in part do
                    ExecutionContext.run i

        let optimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let handler () =
                let prolog = ManagedFragment [||]
                let epilog = ManagedFragment [||]
                {
                    compileNeedsPrev = true
                    nativeCallCount = ref 0
                    jumpDistance = ref 0
                    prolog = prolog
                    epilog = epilog
                    compileDelta = compileDelta scope
                    startDefragmentation = fun _ _ _ -> Task.FromResult TimeSpan.Zero
                    run = fun () -> run prolog
                    memorySize = fun () -> 0L
                    alloc = fun code -> ManagedFragment(code)
                    free = ignore
                    write = fun ptr code -> ptr.Instructions <- code; false
                    writeNext = fun prev next -> prev.Next <- next; 0
                    isNext = fun prev frag -> prev.Next = frag
                    dispose = fun () -> ()
                    disassemble = fun f -> f.Instructions |> Array.toList
                }  

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram

        let unoptimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let handler () =
                let prolog = ManagedFragment [||]
                let epilog = ManagedFragment [||]
                {
                    compileNeedsPrev = false
                    nativeCallCount = ref 0
                    jumpDistance = ref 0
                    prolog = prolog
                    epilog = epilog
                    compileDelta = fun _ r -> compileFull scope r
                    startDefragmentation = fun _ _ _ -> Task.FromResult TimeSpan.Zero
                    run = fun () -> run prolog
                    memorySize = fun () -> 0L
                    alloc = fun code -> ManagedFragment(code)
                    free = ignore
                    write = fun ptr code -> ptr.Instructions <- code; false
                    writeNext = fun prev next -> prev.Next <- next; 0
                    isNext = fun prev frag -> prev.Next = frag
                    dispose = fun () -> ()
                    disassemble = fun f -> f.Instructions |> Array.toList
                }  

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram

    module Debug =
        let private run (f : ManagedFragment) =
            let rec all (f : ManagedFragment) =
                if isNull f then 
                    Seq.empty
                else
                    seq {
                        yield f.Instructions
                        yield! all f.Next
                    }

            let all = all f |> Seq.collect id |> Seq.toArray
            for i in all do
                ExecutionContext.debug i

        let optimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let handler () =
                let prolog = ManagedFragment [||]
                let epilog = ManagedFragment [||]
                {
                    compileNeedsPrev = true
                    nativeCallCount = ref 0
                    jumpDistance = ref 0
                    prolog = prolog
                    epilog = epilog
                    compileDelta = compileDelta scope
                    startDefragmentation = fun _ _ _ -> Task.FromResult TimeSpan.Zero
                    run = fun () -> run prolog
                    memorySize = fun () -> 0L
                    alloc = fun code -> ManagedFragment(code)
                    free = ignore
                    write = fun ptr code -> ptr.Instructions <- code; false
                    writeNext = fun prev next -> prev.Next <- next; 0
                    isNext = fun prev frag -> prev.Next = frag
                    dispose = fun () -> ()
                    disassemble = fun f -> f.Instructions |> Array.toList
                }  

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram

        let unoptimized scope comparer input =
            let scope = { scope with stats = ref FrameStatistics.Zero }
            let handler () =
                let prolog = ManagedFragment [||]
                let epilog = ManagedFragment [||]
                {
                    compileNeedsPrev = false
                    nativeCallCount = ref 0
                    jumpDistance = ref 0
                    prolog = prolog
                    epilog = epilog
                    compileDelta = fun _ r -> compileFull scope r
                    startDefragmentation = fun _ _ _ -> Task.FromResult TimeSpan.Zero
                    run = fun () -> run prolog
                    memorySize = fun () -> 0L
                    alloc = fun code -> ManagedFragment(code)
                    free = ignore
                    write = fun ptr code -> ptr.Instructions <- code; false
                    writeNext = fun prev next -> prev.Next <- next; 0
                    isNext = fun prev frag -> prev.Next = frag
                    dispose = fun () -> ()
                    disassemble = fun f -> f.Instructions |> Array.toList
                }  

            { new WrappedRenderProgram(AdaptiveProgram.custom comparer handler input) with
                override x.Run() =
                    x.RunInner()
                    !scope.stats
            } :> IRenderProgram