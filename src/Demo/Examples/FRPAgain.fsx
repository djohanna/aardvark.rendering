#I @"../../../bin/Debug"
#I @"../../../bin/Release"
#load "LoadReferences.fsx"
#load "FRPAgain.fs"
open EventSystem

open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns

[<ReflectedDefinition>]
module Nat =
    type Head = 
        abstract member Check : bool
        abstract member Infer : (unit -> unit) -> unit
    type True() = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()
    type Rule<'a>([<ReflectedDefinition>]v : 'a) = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()
    type And<'a,'b>(r : Rule<'a>, l : Rule<'b>) = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()

    type Rule = Head * (unit -> unit) // check head and reduction


    [<ReflectedDefinition>]
    let (<==) (thing : 'a) (r : Head) = ()
    [<ReflectedDefinition>]
    let (==>) (r : Head) (thing : 'a)  = 
        ()

    let (&&&) l r = And(Rule l,Rule r)
    let True = True()
    let solve (binder : 'a -> 'b) = ()
    
    type N = interface end
    type Z() = interface N
    let Z = Z() 

    type Succ<'s when 's :> N> = Succ of 's interface N

    type Sum<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Sum of 'a * 'b * 'c
    type Addition() =
        member x.Sum (Z : Z, M : N) = 
            True ==> Sum(Z, M, M)

        member x.Sum (N : N, M : N, K : N) = 
            Rule ( Sum (N, M, K) ) ==>
                Sum ( Succ N, N, Succ K ) 

    type Prod<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Prod of 'a * 'b * 'c
    type Product() =
        
        member x.Prod(Z : Z, M : N) =
            True ==> Prod (Z, M, Z)

        member x.Prod(N : N, M : N, P : N, K : N) =
            Prod(N,M,K) &&& Sum (K,M,P) ==>
                Prod (Succ(N), M, P) 

    let test = solve ( fun (r : N) -> Sum(Z, Z, r) )

    let rules = 
         [ for m in typeof<Product>.GetMethods() do
            yield FSharp.Quotations.Expr.TryGetReflectedDefinition m ]

    let universe = [ Z :> obj ]
    
    let testE = 
        <@ fun (Z : Z, M : N) -> True ==> Sum(Z, M, M) @>
    let testE2 = 
        <@ fun (N : N, M : N, K : N)  -> Rule ( Sum (N, M, K) ) ==> Sum ( Succ N, N, Succ K )  @>
    open FSharp.Quotations.Patterns
    let k = 
        match testE with
            | Patterns.Lambda(var, 
                              Patterns.Let(e1, Patterns.TupleGet(x0,0), 
                                    Patterns.Let(c, 
                                        Patterns.TupleGet(x1,1), rest)))-> 
                                            printfn "values need to be invented for: %A" var.Type
                                            printfn "rest is: %A" rest
            | _ -> printfn "noope"
  
    open System
    open System.Collections.Generic
    type Universe = Dictionary<System.Type,HashSet<obj>>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>
    module Universe = 

        let empty () : Universe = Dictionary()
        let get (u : Universe) (t : Type) =
            match u.TryGetValue t with
                | (true,vs) -> [ for s in vs do yield Expr.Value(s,t) ]
                | _ -> []

        let add (u : Universe) (v : obj) (t : Type) =
            match u.TryGetValue t with
                | (true,hs) when not (hs.Contains v) -> hs.Add v |> ignore; u
                | _ -> 
                    let n = empty ()
                    for kvp in u do 
                        n.Add(kvp.Key,HashSet(kvp.Value))
                    n

    let extractTuples (u : Universe) (e : Expr) =
        match e with
            | DerivedPatterns.Lambdas(variables,body) ->
                printfn "%A -> %A" variables body
