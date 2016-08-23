#if INTERACTIVE
#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#r "FSharp.Quotations.Evaluator.dll"
#else
namespace Logics
#endif

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

[<ReflectedDefinition>]
module Nat =
    exception NoMatch

    [<ReflectedDefinition>]
    let (==>) (l : bool) (r : 'b)  = 
        if l then r else raise NoMatch

    let (!?) (v : obj) : bool = failwith "first stage"

    let solve (binder : 'a -> 'b) = ()
    
    type N = interface end
    type Z() = interface N
    type Succ<'s when 's :> N> = Succ of 's interface N

    type Sum<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Sum of 'a * 'b * 'c
    type Addition() =

        member x.Sum (Z : Z, M : N) = 
            true ==> Sum(Z, M, M)

        member x.Sum (N : N, M : N, K : N) = 
            !? ( Sum (N, M, K) )  ==>
                 Sum ( Succ N, N, Succ K ) 

    type Prod<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Prod of 'a * 'b * 'c
    type Product() =
        
        member x.Prod(Z : Z, M : N) =
            true ==> Prod (Z, M, Z)

        member x.Prod(N : N, M : N, P : N, K : N) =
            (!?(Prod(N,M,K)) && !?(Sum (K,M,P))) ==>
                Prod (Succ(N), M, P) 

  
open System
open Aardvark.Base
type Universe = list<obj>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Universe = 
    

    let empty : Universe = []
    let get (t : Type) (u : Universe)  =
        u |> List.filter (fun o -> 
            t.IsAssignableFrom(o.GetType())
        )

    let add (v : obj) (u : Universe) =
        v :: u

    let addMany xs (u:Universe) =
        List.fold (flip add) u xs |> System.Collections.Generic.HashSet |> Seq.toList

    let find (v : Var) (u : Universe)  =
        get v.Type u

    let rec instantiate (xs : list<Var>) (u : Universe) : list<list<Var*obj>> =   
        match xs with
            | [] -> []
            | [x] -> [find x u |> List.map (fun o -> x,o)]
            | x::xs -> 
                let rest = instantiate xs u
                [ for c in find x u do
                    for r in rest do
                        yield (x,c) :: r
                ]
    let instantiateMap (xs : list<Var>) (u : Universe) =   
        instantiate xs u |> List.map Map.ofList

    let ofList = List.fold (flip add) empty

[<AutoOpen>]
module Logics = 
    open Nat
    open FSharp.Quotations.Evaluator

    let rules = 
        [ for m in typeof<Product>.GetMethods() do
            yield FSharp.Quotations.Expr.TryGetReflectedDefinition m 
        ]

    let universe = [ Z :> obj ]
    
    let testE = 
        <@ fun (a : int, b : float) -> if float a > b then float a + b else float a - b @>


    let u =
        [
            1 :> obj
            1.0 :> obj
        ] |> Universe.ofList

    let rec replaceEval (reducer : Expr) (e : Expr) : Expr =
        let replace = replaceEval reducer
        match e with
            | DerivedPatterns.SpecificCall <@ (!?) @>(target,types,args) -> 
                Expr.Applications(reducer, [ args |> List.map replace ])
            | Quotations.ExprShape.ShapeCombination(o,es) ->
                Quotations.ExprShape.RebuildShapeCombination(o, es |> List.map replace)
            | Quotations.ExprShape.ShapeLambda(v,body) -> 
                Expr.Lambda(v, replace body)
            | Quotations.ExprShape.ShapeVar(v) -> Expr.Var v

    let tester = <@@ fun b -> true @@>

    let deriveKnowledge (u : Universe) (input : Expr) =

        let evalInKnowledge =
            <@@ fun (o : obj) ->
                    printfn "checking: %A" o
                    true
            @@>

        let secondStage = replaceEval evalInKnowledge input 

        let tryEval (e : Expr) =
            try e.EvaluateUntyped() |> Some
            with | :? NoMatch -> None
                 | e -> 
                    printfn "failed: %A" e.Message
                    raise e

        match secondStage with
            | DerivedPatterns.Lambdas([variables],body) ->
                let maps = Universe.instantiate variables u
                printfn "the substitutions are: %A" maps
                let s = maps |> List.map Map.ofList
                [ for instance in s do
                    let instance = 
                        body.Substitute(fun v ->    
                            match Map.tryFind v instance with
                                | Some s -> Some <| Expr.Coerce(Expr.Value(s,s.GetType()),v.Type)
                                | None -> None
                        )
                    printfn "the instance is: %A" instance
                    match tryEval instance with
                        | Some v -> yield v
                        | None -> ()
                ]
            | _ -> failwith "could not reduce rule."


    let applyRule (u : Universe) (ruleMethod : Expr) =
        match ruleMethod with
            | Patterns.Lambda(thisVar, body) ->
                let newKnowledge = deriveKnowledge u body
                if List.isEmpty newKnowledge then u
                else Universe.addMany newKnowledge u
            | _ -> failwith "could not match rule method"

    let stepRules (rules : list<Expr>) (universe : Universe) =
        List.fold applyRule universe rules

    open Nat
    let numerals =
        [
            Nat.Z() :> obj
        ] |> Universe.ofList

    let sum = 
        [
            <@@ fun this (Z : Z, M : N) ->
                 true ==> Sum(Z, M, M)
            @@>;
            <@@ fun this (N : N, M : N, K : N) -> 
                 (!? (Sum (N, M, K) ))  ==> Sum ( Succ N, N, Succ K ) 
            @@>
        ]

//        member x.Sum (N : N, M : N, K : N) = 
//            !? ( Sum (N, M, K) )  ==>
//                 Sum ( Succ N, N, Succ K ) 


    let test () =
        let once = stepRules sum numerals
        let twice = stepRules sum once
        let thrice = stepRules sum twice
        printfn "%A" twice
        Console.ReadLine()


//
//module SF =
//    
//    type Event<'a> = Req | Occ of 'a
//
//    type React<'e,'a> = 
//        | Done of 'a
//        | Await of list<Event<'e>> * (list<Event<'e>> -> React<'e,'a>)
//
//    type EvReq<'a>  = list<'a>
//    type EvOccs<'a> = list<'a>
//
//    let eval (f : EvReq<'e> -> EvOccs<'e>) (sf : React<'e,'a>) : 
        