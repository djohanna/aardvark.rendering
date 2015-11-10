namespace Aardvark.SceneGraph

open System
open System.Runtime.InteropServices
open System.Collections.Generic

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Ag
open Aardvark.Base.Rendering

module Sg =

    [<AbstractClass>]
    type AbstractApplicator(child : ISg) =
        interface IApplicator with
            member x.Child = child

        member x.Child = child

    type AdapterNode(node : obj) =
        interface ISg

        member x.Node = node

    type DynamicNode(child : IMod<ISg>) =
        interface ISg
        member x.Child = child

    type RenderNode(call : IMod<DrawCallInfo>, mode : IMod<IndexedGeometryMode>) =
        interface ISg

        member x.Mode = mode
        member x.DrawCallInfo = call

        new(call : IEvent<DrawCallInfo>, mode : IEvent<IndexedGeometryMode>) = RenderNode(Mod.fromEvent call, Mod.fromEvent mode)
        new(call : DrawCallInfo, mode : IndexedGeometryMode) = RenderNode(Mod.constant call, Mod.constant mode)
   
    type VertexAttributeApplicator(values : Map<Symbol, BufferView>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Values = values

        new(semantic : Symbol, value : BufferView, child : ISg)       = VertexAttributeApplicator(Map.ofList [semantic, value], child)
        new(values : SymbolDict<BufferView>, child : ISg)             = VertexAttributeApplicator(values |> Seq.map (fun (KeyValue(k,v)) -> k,v) |> Map.ofSeq, child)

    type VertexIndexApplicator(value : IMod<Array>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Value = value

        new(value : IEvent<Array>, child : ISg)       = VertexIndexApplicator(Mod.fromEvent value, child)

    type InstanceAttributeApplicator(values : Map<Symbol, BufferView>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Values = values

        new(semantic : Symbol, value : BufferView, child : ISg)       = InstanceAttributeApplicator(Map.ofList [semantic, value], child)
        new(values : SymbolDict<BufferView>, child : ISg)             = InstanceAttributeApplicator(values |> Seq.map (fun (KeyValue(k,v)) -> k,v) |> Map.ofSeq, child)
 

    type OnOffNode(on : IMod<bool>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.IsActive = on

        new(on : IEvent<bool>, child : ISg) = OnOffNode(Mod.fromEvent on, child)

    type PassApplicator(pass : IMod<uint64>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Pass = pass

    type UniformApplicator(uniformHolder : IUniformProvider, child : ISg) =
        inherit AbstractApplicator(child)

        member internal x.Uniforms = uniformHolder
        
        member x.TryFindUniform (scope : Scope) (name : Symbol) =
            uniformHolder.TryGetUniform (scope,name)

        new(name : string, value : IMod, child : ISg) = UniformApplicator( (new Providers.SimpleUniformHolder ([Symbol.Create name,value]) :> IUniformProvider), child)
        new(name : Symbol, value : IMod, child : ISg) = UniformApplicator( (new Providers.SimpleUniformHolder( [name,value]) :> IUniformProvider), child)
        new(map : Map<Symbol,IMod>, child : ISg) = UniformApplicator( (new Providers.SimpleUniformHolder( map) :> IUniformProvider), child)


    type SurfaceApplicator(surface : IMod<ISurface>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Surface = surface

        new(value : IEvent<ISurface>, child : ISg) = SurfaceApplicator(Mod.fromEvent value, child)
        new(value : ISurface, child : ISg) = SurfaceApplicator(Mod.constant value, child)

    type TextureApplicator(semantic : Symbol, texture : IMod<ITexture>, child : ISg) =
        inherit UniformApplicator(semantic, texture :> IMod, child)

        member x.Texture = texture

        new(semantic : Symbol, texture : IEvent<ITexture>, child : ISg) = TextureApplicator(semantic, Mod.fromEvent texture, child)
        new(texture : IMod<ITexture>, child : ISg) = TextureApplicator(DefaultSemantic.DiffuseColorTexture, texture, child)
        new(texture : IEvent<ITexture>, child : ISg) = TextureApplicator(DefaultSemantic.DiffuseColorTexture, texture, child)


    type TrafoApplicator(trafo : IMod<Trafo3d>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Trafo = trafo

        new(value : IEvent<Trafo3d>, child : ISg) = TrafoApplicator(Mod.fromEvent value, child)
        new(value : Trafo3d, child : ISg) = TrafoApplicator(Mod.constant value, child)
    
    type ViewTrafoApplicator(trafo : IMod<Trafo3d>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.ViewTrafo = trafo

        new(value : IEvent<Trafo3d>, child : ISg) = ViewTrafoApplicator(Mod.fromEvent value, child)

    type ProjectionTrafoApplicator(trafo : IMod<Trafo3d>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.ProjectionTrafo = trafo

        new(value : IEvent<Trafo3d>, child : ISg) = ProjectionTrafoApplicator(Mod.fromEvent value, child)


    type DepthTestModeApplicator(mode : IMod<DepthTestMode>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Mode = mode

        new(value : IEvent<DepthTestMode>, child : ISg) = DepthTestModeApplicator(Mod.fromEvent value, child)

    type CullModeApplicator(mode : IMod<CullMode>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Mode = mode

        new(value : IEvent<CullMode>, child : ISg) = CullModeApplicator(Mod.fromEvent value, child)

    type FillModeApplicator(mode : IMod<FillMode>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Mode = mode

        new(value : IEvent<FillMode>, child : ISg) = FillModeApplicator(Mod.fromEvent value, child)
        new(value : FillMode, child : ISg) = FillModeApplicator(Mod.constant value, child)

    type StencilModeApplicator(mode : IMod<StencilMode>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Mode = mode

        new(value : IEvent<StencilMode>, child : ISg) = StencilModeApplicator(Mod.fromEvent value, child)

    type BlendModeApplicator(mode : IMod<BlendMode>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Mode = mode

        new(value : IEvent<BlendMode>, child : ISg) = BlendModeApplicator(Mod.fromEvent value, child)

    type RasterizerStateApplicator(state : IMod<RasterizerState>, child : ISg) =
        inherit AbstractApplicator(child)

        let depth = state |> Mod.map (fun s -> s.DepthTest)
        let cull = state |> Mod.map (fun s -> s.CullMode)
        let fill = state |> Mod.map (fun s -> s.FillMode)
        let stencil = state |> Mod.map (fun s -> s.StencilMode)
        let blend = state |> Mod.map (fun s -> s.BlendMode)

        member x.RasterizerState = state
        member x.DepthTestMode = depth
        member x.CullMode = cull
        member x.FillMode = fill
        member x.StencilMode = stencil
        member x.BlendMode = blend

        new(value : IEvent<RasterizerState>, child : ISg) = RasterizerStateApplicator(Mod.fromEvent value, child)


    type Group(elements : seq<ISg>) =
        let aset = cset(elements)

        interface IGroup with
            member x.Children = x.ASet

        member x.ASet : aset<ISg> = aset :> aset<_>

        member x.Add v =
            transact (fun () ->
               aset.Add v
            )

        member x.Remove v =
            transact (fun () ->
                aset.Remove v
            )

        member x.Clear() =
            transact (fun () ->
                aset.Clear()
            )

        member x.UnionWith v =
            transact (fun () ->
                aset.UnionWith v
            )

        member x.ExceptWith v =
            transact (fun () ->
                aset.ExceptWith v
            )

        member x.SymmetricExceptWith v =
            transact (fun () ->
                aset.SymmetricExceptWith v
            )

        member x.IntersectWith v =
            transact (fun () ->
                aset.IntersectWith v
            )


        member x.Count = aset.Count

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = (aset :> System.Collections.IEnumerable).GetEnumerator()

        interface IEnumerable<ISg> with
            member x.GetEnumerator() = (aset :> seq<_>).GetEnumerator()

        interface ICollection<ISg> with
            member x.IsReadOnly = false
            member x.Add v = x.Add v |> ignore
            member x.Remove v = x.Remove v
            member x.Clear() = x.Clear()
            member x.Contains v = aset.Contains v
            member x.Count = x.Count
            member x.CopyTo(arr, index) =
                let mutable id = index
                for e in aset do
                    arr.[id] <- e
                    id <- id + 1

        interface ISet<ISg> with
            member x.Add v = x.Add v
            member x.UnionWith other = x.UnionWith other
            member x.IntersectWith other = x.IntersectWith other
            member x.ExceptWith other = x.ExceptWith other
            member x.SymmetricExceptWith other = x.SymmetricExceptWith other
            member x.IsSubsetOf other = (aset :> ISet<ISg>).IsSubsetOf other
            member x.IsSupersetOf other = (aset :> ISet<ISg>).IsSupersetOf other
            member x.IsProperSubsetOf other = (aset :> ISet<ISg>).IsProperSubsetOf other
            member x.IsProperSupersetOf other = (aset :> ISet<ISg>).IsProperSupersetOf other
            member x.Overlaps other = (aset :> ISet<ISg>).Overlaps other
            member x.SetEquals other = (aset :> ISet<ISg>).SetEquals other

        new() = Group(Seq.empty)
        
    type Set(content : aset<ISg>) =

        interface IGroup with
            member x.Children = content

        member x.ASet = content

    type AsyncLoadApplicator(fboSignature : IFramebufferSignature, child : ISg) =
        inherit AbstractApplicator(child)

        member x.FramebufferSignature = fboSignature

    type Environment (runtime : IRuntime, viewTrafo : IMod<Trafo3d>, projTrafo : IMod<Trafo3d>, viewSize : IMod<V2i>, child : ISg) =
        inherit AbstractApplicator(child)

        member x.Runtime = runtime
        member x.ViewTrafo = viewTrafo
        member x.ProjTrafo = projTrafo
        member x.ViewSize = viewSize

        member x.Scene = child


module SceneGraphCompletenessCheck =
    open System.Text.RegularExpressions

    let semantics =
        [
            "RenderObjects"
            "GlobalBoundingBox"
            "LocalBoundingBox"
        ]

    let genericNameRx = Regex @"(?<name>.*?)´[0-9]+"
    let cleanName (name : string) =
        let m = genericNameRx.Match name
        if m.Success then m.Groups.["name"].Value
        else name

    let intrisicNames =
        Dict.ofList [
            typeof<byte>, "byte"
            typeof<int8>, "int8"
            typeof<uint16>, "uint16"
            typeof<int16>, "int16"
            typeof<int>, "int"
            typeof<uint32>, "uint32"
            typeof<int64>, "int64"
            typeof<uint64>, "uint64"
            typeof<obj>, "obj"
        ]

    let rec prettyName (t : Type) =
        match intrisicNames.TryGetValue t with
            | (true, n) -> n
            | _ -> 
                if t.IsArray then 
                    sprintf "%s[]" (t.GetElementType() |> prettyName)
                elif t.IsGenericType then
                    let args = t.GetGenericArguments() |> Seq.map prettyName |> String.concat ","
                    sprintf "%s<%s>" (cleanName t.Name) args
                else
                    cleanName t.Name

    [<OnAardvarkInit>]
    let checkSemanticCompleteness() =
        let sgTypes = Introspection.GetAllClassesImplementingInterface(typeof<ISg>)

        let sgModule = typeof<Sg.Group>.DeclaringType

        for att in semantics do
            let semTypes = HashSet<Type>()
            for t in sgTypes do
                if t.DeclaringType = sgModule then
                    match t |> Ag.tryGetAttributeType att with
                        | Some attType ->
                            semTypes.Add attType |> ignore
                        | None ->
                            Log.warn "no semantic %A for type %s" att (prettyName t)

            if semTypes.Count > 1 then
                let allTypes = semTypes |> Seq.map prettyName |> String.concat "; "
                Log.warn "conflicting types for semantic functions %A [%s]" att allTypes


        ()

