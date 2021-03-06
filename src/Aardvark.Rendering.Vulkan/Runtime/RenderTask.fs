﻿namespace Aardvark.Rendering.Vulkan

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Rendering.Vulkan
open Microsoft.FSharp.NativeInterop
open Aardvark.Base.Incremental
open System.Diagnostics
open System.Collections.Generic
open Aardvark.Base.Runtime

#nowarn "9"
#nowarn "51"

type ICommandStreamResource =
    inherit IResourceLocation<VKVM.CommandStream>
    abstract member Stream : VKVM.CommandStream
    abstract member Resources : seq<IResourceLocation>

    abstract member GroupKey : list<obj>
    abstract member BoundingBox : IMod<Box3d>

module RenderCommands =
 
    [<RequireQualifiedAccess>]
    type Tree<'a> =
        | Empty
        | Leaf of 'a
        | Node of Option<'a> * list<Tree<'a>> 
     
    type ClearValues =
        {
            colors  : Map<Symbol, IMod<C4f>>
            depth   : Option<IMod<float>>
            stencil : Option<IMod<int>>
        }

    type PipelineState =
        {
            surface             : Aardvark.Base.Surface

            depthTest           : IMod<DepthTestMode>
            cullMode            : IMod<CullMode>
            blendMode           : IMod<BlendMode>
            fillMode            : IMod<FillMode>
            stencilMode         : IMod<StencilMode>
            multisample         : IMod<bool>
            writeBuffers        : Option<Set<Symbol>>
            globalUniforms      : IUniformProvider

            geometryMode        : IndexedGeometryMode
            vertexInputTypes    : Map<Symbol, Type>
            perGeometryUniforms : Map<string, Type>
        }
   

    type Geometry =
        {
            vertexAttributes    : Map<Symbol, IMod<IBuffer>>
            indices             : Option<Aardvark.Base.BufferView>
            uniforms            : Map<string, IMod>
            call                : IMod<list<DrawCallInfo>>
        }

    type TreeRenderObject(pipe : PipelineState, geometries : IMod<Tree<Geometry>>) =
        let id = newId()

        member x.Pipeline = pipe
        member x.Geometries = geometries

        interface IRenderObject with
            member x.AttributeScope = Ag.emptyScope
            member x.Id = id
            member x.RenderPass = RenderPass.main
            

    type RenderCommand =
        | Objects of aset<IRenderObject>
        | ViewDependent of pipeline : PipelineState * (Trafo3d -> Trafo3d -> list<Geometry>)

    type Command = 
        | Render of objects : aset<IRenderObject>
        | Clear of values : ClearValues
        | Blit of sourceAttachment : Symbol * target : IBackendTextureOutputView

    type PreparedPipelineState =
        {
            ppPipeline  : INativeResourceLocation<VkPipeline>
            ppLayout    : PipelineLayout
        }

    type PreparedGeometry =
        {
            pgOriginal      : Geometry
            pgDescriptors   : INativeResourceLocation<DescriptorSetBinding>
            pgAttributes    : INativeResourceLocation<VertexBufferBinding>
            pgIndex         : Option<INativeResourceLocation<IndexBufferBinding>>
            pgCall          : INativeResourceLocation<DrawCall>
            pgResources     : list<IResourceLocation>
        }

    type ResourceManager with
        member x.PreparePipelineState (renderPass : RenderPass, state : PipelineState) =
            let layout, program = x.CreateShaderProgram(renderPass, state.surface)

            let inputs = 
                layout.PipelineInfo.pInputs |> List.map (fun p ->
                    let name = Symbol.Create p.name
                    match Map.tryFind name state.vertexInputTypes with
                        | Some t -> (name, (false, t))
                        | None -> failf "could not get shader input %A" name
                )
                |> Map.ofList

            let inputState =
                x.CreateVertexInputState(layout.PipelineInfo, Mod.constant (VertexInputState.ofTypes inputs))

            let inputAssembly =
                x.CreateInputAssemblyState(Mod.constant state.geometryMode, program)

            let rasterizerState =
                x.CreateRasterizerState(state.depthTest, state.cullMode, state.fillMode)

            let colorBlendState =
                x.CreateColorBlendState(renderPass, state.writeBuffers, state.blendMode)

            let depthStencil =
                let depthWrite = 
                    match state.writeBuffers with
                        | None -> true
                        | Some s -> Set.contains DefaultSemantic.Depth s
                x.CreateDepthStencilState(depthWrite, state.depthTest, state.stencilMode)

            let pipeline = 
                x.CreatePipeline(
                    program,
                    renderPass,
                    inputState,
                    inputAssembly,
                    rasterizerState,
                    colorBlendState,
                    depthStencil,
                    state.writeBuffers
                )
            {
                ppPipeline  = pipeline
                ppLayout    = layout
            }

        member x.PrepareGeometry(state : PreparedPipelineState, g : Geometry) : PreparedGeometry =
            let resources = System.Collections.Generic.List<IResourceLocation>()

            let layout = state.ppLayout

            let descriptorSets, additionalResources = 
                x.CreateDescriptorSets(layout, UniformProvider.ofMap g.uniforms)

            resources.AddRange additionalResources

            let vertexBuffers = 
                layout.PipelineInfo.pInputs 
                    |> List.sortBy (fun i -> i.location) 
                    |> List.map (fun i ->
                        let sem = i.semantic 
                        match Map.tryFind sem g.vertexAttributes with
                            | Some b ->
                                x.CreateBuffer(b), 0L
                            | None ->
                                failf "geometry does not have buffer %A" sem
                    )

            let dsb = x.CreateDescriptorSetBinding(layout, Array.toList descriptorSets)
            let vbb = x.CreateVertexBufferBinding(vertexBuffers)

            let isIndexed, ibo =
                match g.indices with
                    | Some ib ->
                        let b = x.CreateIndexBuffer ib.Buffer
                        let ibb = x.CreateIndexBufferBinding(b, VkIndexType.ofType ib.ElementType)
                        resources.Add ibb
                        true, ibb |> Some
                    | None ->
                        false, None

            let call = x.CreateDrawCall(isIndexed, g.call)

            resources.Add dsb
            resources.Add vbb
            resources.Add call



            {
                pgOriginal      = g
                pgDescriptors   = dsb
                pgAttributes    = vbb
                pgIndex         = ibo
                pgCall          = call
                pgResources     = CSharpList.toList resources
            }

    type TreeCommandStreamResource(owner, key, pipe : PipelineState, things : IMod<Tree<Geometry>>, resources : ResourceLocationSet, manager : ResourceManager, renderPass : RenderPass, stats : nativeptr<V2i>) =
        inherit AbstractResourceLocation<VKVM.CommandStream>(owner, key)
         
        let id = newId()

        let mutable stream = Unchecked.defaultof<VKVM.CommandStream>
        let mutable entry = Unchecked.defaultof<VKVM.CommandStream>
        let preparedPipeline = manager.PreparePipelineState(renderPass, pipe)

        let bounds = lazy (Mod.constant Box3d.Invalid)
        let allResources = ReferenceCountingSet<IResourceLocation>()
        let mutable state = Tree.Empty

        let isActive =
            let isActive = NativePtr.alloc 1
            NativePtr.write isActive 1
            isActive

        let prepare (g : Geometry) =
            let res = manager.PrepareGeometry(preparedPipeline, g)
            for r in res.pgResources do 
                if allResources.Add r then 
                    resources.Add r
                    r.Update(AdaptiveToken.Top) |> ignore

            let stream = new VKVM.CommandStream()
            stream.IndirectBindDescriptorSets(res.pgDescriptors.Pointer) |> ignore
            stream.IndirectBindVertexBuffers(res.pgAttributes.Pointer) |> ignore
            match res.pgIndex with
                | Some ibb -> stream.IndirectBindIndexBuffer(ibb.Pointer) |> ignore
                | None -> ()
            stream.IndirectDraw(stats, isActive, res.pgCall.Pointer) |> ignore


            res, stream

        let release (pg : PreparedGeometry, stream : VKVM.CommandStream) =
            for r in pg.pgResources do
                if allResources.Remove r then 
                    resources.Remove r
            stream.Dispose()

        let isIdentical (pg : PreparedGeometry, stream : VKVM.CommandStream) (o : Geometry) =
            System.Object.ReferenceEquals(pg.pgOriginal, o)

        let rec destroy (t : Tree<PreparedGeometry * VKVM.CommandStream>) =
            match t with
                | Tree.Empty -> ()
                | Tree.Leaf v -> release v
                | Tree.Node(s,c) ->
                    s |> Option.iter release
                    c |> List.iter destroy

        let update (t : Tree<Geometry>) =
            
            let rec update (o : Tree<_>) (n : Tree<Geometry>) =
                match o, n with
                    
                    | _, Tree.Empty ->
                        destroy o
                        Tree.Empty

                    | Tree.Empty, Tree.Leaf g -> Tree.Leaf (prepare g)
                    | Tree.Empty, Tree.Node(self, children) ->
                        let self =
                            match self with
                                | Some self -> Some (prepare self)
                                | None -> None
                        let children = children |> List.map (update Tree.Empty)
                        Tree.Node(self, children)


                    | Tree.Leaf o, Tree.Leaf n -> 
                        if isIdentical o n then 
                            Tree.Leaf o
                        else
                            release o
                            Tree.Leaf(prepare n)

                    | Tree.Leaf o, Tree.Node(ns,nc) ->
                        let self = 
                            match ns with
                                | Some ns ->
                                    if isIdentical o ns then 
                                        Some o
                                    else
                                        release o
                                        Some (prepare ns)
                                | None ->
                                    None
                        let children = nc |> List.map (update Tree.Empty)
                        Tree.Node(self, children)

                    | Tree.Node(os, oc), Tree.Leaf(n) ->
                        oc |> List.iter destroy
                        let self = 
                            match os with
                                | None -> prepare n
                                | Some os ->
                                    if isIdentical os n then os
                                    else
                                        release os
                                        prepare n
                        Tree.Leaf self
                    | Tree.Node(os, oc), Tree.Node(ns, nc) ->
                        let self = 
                            match os, ns with
                                | None, Some ns -> Some (prepare ns)
                                | Some os, None -> release os; None
                                | None, None -> None
                                | Some os, Some ns ->
                                    if isIdentical os ns then 
                                        Some os
                                    else
                                        release os
                                        Some (prepare ns)
                        let children = List.map2 update oc nc
                        Tree.Node(self, children)
            
            state <- update state t

            let rec link (last : VKVM.CommandStream) (t : Tree<PreparedGeometry * VKVM.CommandStream>) =
                match t with
                    | Tree.Empty -> last
                    | Tree.Leaf(_,v) -> 
                        last.Next <- Some v
                        v
                    | Tree.Node(Some(_,s), children) ->
                        last.Next <- Some s
                        children |> List.fold link s
                    | Tree.Node(None, children) ->
                        children |> List.fold link last
                        
            let final = link entry state
            final.Next <- None

        member x.Stream = stream
        member x.GroupKey = [preparedPipeline.ppPipeline :> obj; id :> obj]
        member x.BoundingBox = bounds.Value

        interface ICommandStreamResource with
            member x.Stream = x.Stream
            member x.Resources = allResources :> seq<_>
            member x.GroupKey = x.GroupKey
            member x.BoundingBox = x.BoundingBox

        override x.Create() =
            stream <- new VKVM.CommandStream()
            entry <- new VKVM.CommandStream()
            stream.Call(entry) |> ignore

            if allResources.Add preparedPipeline.ppPipeline then resources.Add preparedPipeline.ppPipeline
            
        override x.Destroy() = 
            stream.Dispose()
            entry.Dispose()
            for r in allResources do resources.Remove r
            allResources.Clear()
            
        override x.GetHandle token =
            let tree = things.GetValue token
            update tree
            { handle = stream; version = 0 }   



module RenderTask =

    [<AbstractClass; Sealed; Extension>]
    type IRenderObjectExts private() =
        [<Extension>]
        static member ComputeBoundingBox (o : IRenderObject) : IMod<Box3d> =
            match o with
                | :? RenderObject as o ->
                    match Ag.tryGetAttributeValue o.AttributeScope "GlobalBoundingBox" with
                        | Success box -> box
                        | _ -> failwith "[Vulkan] could not get BoundingBox for RenderObject"
                    
                | :? MultiRenderObject as o ->
                    o.Children |> List.map IRenderObjectExts.ComputeBoundingBox |> Mod.mapN Box3d

                | :? PreparedMultiRenderObject as o ->
                    o.Children |> List.map IRenderObjectExts.ComputeBoundingBox |> Mod.mapN Box3d
                    
                | :? PreparedRenderObject as o ->
                    IRenderObjectExts.ComputeBoundingBox o.original

                | _ ->
                    failf "invalid renderobject %A" o

    module CommandStreams = 
        type CommandStreamResource(owner, key, o : IRenderObject, resources : ResourceLocationSet, manager : ResourceManager, renderPass : RenderPass, stats : nativeptr<V2i>) =
            inherit AbstractResourceLocation<VKVM.CommandStream>(owner, key)
         
            let mutable stream = Unchecked.defaultof<VKVM.CommandStream>
            let mutable prep : PreparedMultiRenderObject = Unchecked.defaultof<_>

            let compile (o : IRenderObject) =
                let o = manager.PrepareRenderObject(renderPass, o)
                for o in o.Children do
                    for r in o.resources do resources.Add r
                                
                    stream.IndirectBindPipeline(o.pipeline.Pointer) |> ignore
                    stream.IndirectBindDescriptorSets(o.descriptorSets.Pointer) |> ignore

                    match o.indexBuffer with
                        | Some ib ->
                            stream.IndirectBindIndexBuffer(ib.Pointer) |> ignore
                        | None ->
                            ()

                    stream.IndirectBindVertexBuffers(o.vertexBuffers.Pointer) |> ignore
                    stream.IndirectDraw(stats, o.isActive.Pointer, o.drawCalls.Pointer) |> ignore
                o



            let bounds = lazy (o.ComputeBoundingBox())


            member x.Stream = stream
            member x.Object = prep
            member x.GroupKey = [prep.Children.[0].pipeline :> obj; prep.Id :> obj]
            member x.BoundingBox = bounds.Value

            interface ICommandStreamResource with
                member x.Stream = x.Stream
                member x.Resources = prep.Children |> Seq.collect (fun c -> c.resources)
                member x.GroupKey = x.GroupKey
                member x.BoundingBox = x.BoundingBox

            override x.Create() =
                stream <- new VKVM.CommandStream()
                let p = compile o
                prep <- p

            override x.Destroy() = 
                stream.Dispose()
                for o in prep.Children do
                    for r in o.resources do resources.Remove r
                prep <- Unchecked.defaultof<_>

            override x.GetHandle _ = 
                { handle = stream; version = 0 }   

        type ClearCommandStreamResource(owner, key, pass : RenderPass, viewports : IMod<Box2i[]>, colors : Map<Symbol, IMod<C4f>>, depth : Option<IMod<float>>, stencil : Option<IMod<uint32>>) =
            inherit AbstractResourceLocation<VKVM.CommandStream>(owner, key)
         
            let mutable stream : VKVM.CommandStream = Unchecked.defaultof<_>

            let compile (token : AdaptiveToken) =
                stream.Clear()

                let hasDepth =
                    Option.isSome pass.DepthStencilAttachment

                let depthClear =
                    match depth, stencil with
                        | Some d, Some s ->
                            let d = d.GetValue token
                            let s = s.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.DepthBit ||| VkImageAspectFlags.StencilBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(float32 d, s))
                            ) |> Some

                        | Some d, None ->   
                            let d = d.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.DepthBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(float32 d, 0u))
                            ) |> Some
                             
                        | None, Some s ->
                            let s = s.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.StencilBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(1.0f, s))
                            ) |> Some

                        | None, None ->
                            None

                let colors = 
                    pass.ColorAttachments |> Map.toSeq |> Seq.choose (fun (i,(n,_)) ->
                        match Map.tryFind n colors with
                            | Some value -> 
                                let res = 
                                    VkClearAttachment(
                                        VkImageAspectFlags.ColorBit, 
                                        uint32 (if hasDepth then 1 + i else i),
                                        VkClearValue(color = VkClearColorValue(float32 = value.GetValue(token).ToV4f()))
                                    )
                                Some res
                            | None ->
                                None
                    ) |> Seq.toArray

                let clears =
                    match depthClear with
                        | Some c -> Array.append [|c|] colors
                        | None -> colors

                let rect =
                    let s = viewports.GetValue(token).[0]
                    VkClearRect(
                        VkRect2D(VkOffset2D(s.Min.X,s.Min.Y), VkExtent2D(uint32 (1 + s.Max.X - s.Min.X) , uint32 (1 + s.Max.Y - s.Min.Y))),
                        0u,
                        uint32 pass.LayerCount
                    )

                stream.ClearAttachments(
                    clears,
                    [| rect |]
                ) |> ignore

            let id = newId()

            interface ICommandStreamResource with
                member x.Stream = stream
                member x.Resources = Seq.empty
                member x.GroupKey = [id :> obj]
                member x.BoundingBox = Mod.constant Box3d.Invalid

            override x.Create() =
                stream <- new VKVM.CommandStream()
                
            override x.Destroy() = 
                stream.Dispose()
                stream <- Unchecked.defaultof<_>
            override x.GetHandle t = 
                compile t
                { handle = stream; version = 0 }   
                



    module Compiler = 
        open CommandStreams

        type RenderObjectCompiler(manager : ResourceManager, renderPass : RenderPass, user : IResourceUser) =
            inherit ResourceLocationSet(user)

            let stats : nativeptr<V2i> = NativePtr.alloc 1
            let cache = ResourceLocationCache<VKVM.CommandStream>(manager.ResourceUser)
            let clearCache = ResourceLocationCache<VKVM.CommandStream>(manager.ResourceUser)
            let mutable version = 0


            override x.InputChanged(t ,i) =
                base.InputChanged(t, i)
                match i with
                    | :? IResourceLocation<UniformBuffer> -> ()
                    | :? IResourceLocation -> version <- version + 1
                    | _ -> ()
 
            member x.Dispose() =
                cache.Clear()

            member x.Compile(o : IRenderObject) : ICommandStreamResource =
                let call = 
                    cache.GetOrCreate([o :> obj], fun owner key ->
//                        match o with
//                            | :? RenderCommands.TreeRenderObject as o ->
//                                new RenderCommands.TreeCommandStreamResource(owner, key, o.Pipeline, o.Geometries, x, manager, renderPass, stats) :> ICommandStreamResource
//                            | _ -> 
                                new CommandStreamResource(owner, key, o, x, manager, renderPass, stats) :> ICommandStreamResource
                    )
                call.Acquire()
                call |> unbox<ICommandStreamResource>
            
            member x.CompileClear(pass : RenderPass, viewports : IMod<Box2i[]>, colors : Map<Symbol, IMod<C4f>>, depth : Option<IMod<float>>, stencil : Option<IMod<uint32>>) =
                
                let call = 
                    clearCache.GetOrCreate([pass :> obj; viewports :> obj; colors :> obj; depth :> obj; stencil :> obj], fun owner key ->
                        new ClearCommandStreamResource(owner, key, pass, viewports, colors, depth, stencil) :> ICommandStreamResource
                    )
                call.Acquire()
                call |> unbox<ICommandStreamResource>

            member x.CurrentVersion = version

    module ChangeableCommandBuffers = 
        open Compiler

        [<AbstractClass>]
        type AbstractChangeableCommandBuffer(manager : ResourceManager, pool : CommandPool, renderPass : RenderPass, viewports : IMod<Box2i[]>, scissors : IMod<Box2i[]>) =
            inherit Mod.AbstractMod<CommandBuffer>()

            let locked = ReferenceCountingSet<ILockedResource>()

            let user =
                { new IResourceUser with
                    member x.AddLocked l = locked.Add l |> ignore
                    member x.RemoveLocked l = locked.Remove l |> ignore
                }

            let device = pool.Device
            let compiler = RenderObjectCompiler(manager, renderPass, user)
            //let mutable resourceVersion = 0
            let mutable cmdVersion = -1
            let mutable cmdViewports = [||]
            let mutable cmdScissors = [||]

            let cmdBuffer = pool.CreateCommandBuffer(CommandBufferLevel.Secondary)
            let dirty = HashSet<ICommandStreamResource>()

            abstract member Release : unit -> unit
            abstract member Prolog : VKVM.CommandStream
            abstract member Sort : AdaptiveToken -> bool
            default x.Sort _ = false

            override x.InputChanged(t : obj, o : IAdaptiveObject) =
                match o with
                    | :? ICommandStreamResource as r ->
                        lock dirty (fun () -> dirty.Add r |> ignore)
                    | _ ->
                        ()

            member x.Compile(o : IRenderObject) =
                let res = compiler.Compile(o)
                lock x (fun () ->
                    let o = x.OutOfDate
                    try x.EvaluateAlways AdaptiveToken.Top (fun t -> res.Update(t) |> ignore; res)
                    finally x.OutOfDate <- o
                )

        
            member x.Compile(o : RenderObjectCompiler -> ICommandStreamResource) =
                let res = o compiler
                lock x (fun () ->
                    let o = x.OutOfDate
                    try x.EvaluateAlways AdaptiveToken.Top (fun t -> res.Update(t) |> ignore; res)
                    finally x.OutOfDate <- o
                )

            member x.Changed() =
                cmdVersion <- -1
                x.MarkOutdated()
            

            member x.Destroy(r : ICommandStreamResource) =
                lock dirty (fun () -> dirty.Remove r |> ignore)
                r.Release()

            member x.Dispose() =
                compiler.Dispose()
                dirty.Clear()
                cmdBuffer.Dispose()

            override x.Compute (t : AdaptiveToken) =
                // update all dirty programs 
                let dirty =
                    lock dirty (fun () ->
                        let res = dirty |> HashSet.toArray
                        dirty.Clear()
                        res
                    )

                for d in dirty do
                    d.Update(t) |> ignore

                // update all resources
                let resourceChanged = compiler.Update t
                //resourceVersion <- compiler.CurrentVersion

                // refill the CommandBuffer (if necessary)
                let vps = viewports.GetValue t
                let scs = scissors.GetValue t
                let contentChanged      = cmdVersion < 0 || dirty.Length > 0
                let viewportChanged     = cmdViewports <> vps || cmdScissors <> scs
                let versionChanged      = cmdVersion >= 0 && resourceChanged
                let orderChanged        = x.Sort t

                if contentChanged || versionChanged || viewportChanged || orderChanged then
                    let first = x.Prolog
                    let cause =
                        String.concat "; " [
                            if contentChanged then yield "content"
                            if versionChanged then yield "resources"
                            if viewportChanged then yield "viewport"
                            if orderChanged then yield "order"
                        ]
                        |> sprintf "{ %s }"

                    Log.line "[Vulkan] recompile commands: %s" cause
                    cmdViewports <- vps
                    cmdScissors <- scs
                    cmdVersion <- 1
                    //cmdVersion <- resourceVersion

                    if viewportChanged then
                        first.SeekToBegin()
                        first.SetViewport(0u, vps |> Array.map (fun b -> VkViewport(float32 b.Min.X, float32 b.Min.Y, float32 b.SizeX + 1.0f, float32 b.SizeY + 1.0f, 0.0f, 1.0f))) |> ignore
                        first.SetScissor(0u, scs |> Array.map (fun b -> VkRect2D(VkOffset2D(b.Min.X, b.Min.Y), VkExtent2D(b.SizeX + 1, b.SizeY + 1)))) |> ignore

                    cmdBuffer.Reset()
                    cmdBuffer.Begin(renderPass, CommandBufferUsage.RenderPassContinue)
                    cmdBuffer.AppendCommand()
                    first.Run(cmdBuffer.Handle)
                    cmdBuffer.End()

                cmdBuffer
            

        [<AbstractClass>]
        type AbstractChangeableSetCommandBuffer(manager : ResourceManager, pool : CommandPool, renderPass : RenderPass, viewports : IMod<Box2i[]>, scissors : IMod<Box2i[]>) =
            inherit AbstractChangeableCommandBuffer(manager, pool, renderPass, viewports, scissors )

            abstract member Add : IRenderObject -> bool
            abstract member Remove : IRenderObject -> bool

        type ChangeableUnorderedCommandBuffer(manager : ResourceManager, pool : CommandPool, renderPass : RenderPass, viewports : IMod<Box2i[]>, scissors : IMod<Box2i[]>) =
            inherit AbstractChangeableSetCommandBuffer(manager, pool, renderPass, viewports, scissors)

            let first = new VKVM.CommandStream()
            let trie = Trie<VKVM.CommandStream>()
            do trie.Add([], first)

            let cache = Dict<IRenderObject, ICommandStreamResource>()
            override x.Prolog = first

            override x.Release() =
                cache.Clear()
                first.Dispose()
                trie.Clear()

            override x.Add(o : IRenderObject) =
                if not (cache.ContainsKey o) then
                    let resource = x.Compile o
                    let key = resource.GroupKey
                    trie.Add(key, resource.Stream)
                    cache.[o] <- resource
                    x.Changed()
                    true
                else
                    false

            override x.Remove(o : IRenderObject) =
                match cache.TryRemove o with
                    | (true, r) ->
                        let key = r.GroupKey
                        trie.Remove key |> ignore
                        x.Destroy r 
                        x.Changed()
                        true
                    | _ ->
                        false

        type ChangeableOrderedCommandBuffer(manager : ResourceManager, pool : CommandPool, renderPass : RenderPass, viewports : IMod<Box2i[]>, scissors : IMod<Box2i[]>, sorter : IMod<Trafo3d -> Box3d[] -> int[]>) =
            inherit AbstractChangeableSetCommandBuffer(manager, pool, renderPass, viewports, scissors)
        
            let first = new VKVM.CommandStream()

            let cache = Dict<IRenderObject, IMod<Box3d> * ICommandStreamResource>()

            let mutable camera = Mod.constant Trafo3d.Identity


            override x.Add(o : IRenderObject) =
                if not (cache.ContainsKey o) then
                    if cache.Count = 0 then
                        match Ag.tryGetAttributeValue o.AttributeScope "ViewTrafo" with
                            | Success trafo -> camera <- trafo
                            | _ -> failf "could not get camera view"

                    let res = x.Compile o
                    let bb = res.BoundingBox
                    cache.[o] <- (bb, res)
                    x.Changed()
                    true
                else
                    false

            override x.Remove(o : IRenderObject) =
                match cache.TryRemove o with
                    | (true, (_,res)) -> 
                        x.Destroy res
                        x.Changed()
                        true
                    | _ -> 
                        false

            override x.Prolog = first

            override x.Release() =
                first.Dispose()

            override x.Sort t =
                let sorter = sorter.GetValue t
                let all = cache.Values |> Seq.toArray

                let boxes = Array.zeroCreate all.Length
                let streams = Array.zeroCreate all.Length
                for i in 0 .. all.Length - 1 do
                    let (bb, s) = all.[i]
                    let bb = bb.GetValue t
                    boxes.[i] <- bb
                    streams.[i] <- s.Stream

                let viewTrafo = camera.GetValue t
                let perm = sorter viewTrafo boxes
                let mutable last = first
                for i in perm do
                    let s = streams.[i]
                    last.Next <- Some s
                last.Next <- None


                true

    [<AutoOpen>]
    module ``New Ordered Command Stuff`` =
        type PreparedPipelineState =
            {
                ppPipeline  : INativeResourceLocation<VkPipeline>
                ppLayout    : PipelineLayout
                ppUniforms  : IUniformProvider
            }

        type PreparedGeometry =
            {
                pgOriginal      : Geometry
                pgDescriptors   : INativeResourceLocation<DescriptorSetBinding>
                pgAttributes    : INativeResourceLocation<VertexBufferBinding>
                pgIndex         : Option<INativeResourceLocation<IndexBufferBinding>>
                pgCall          : INativeResourceLocation<DrawCall>
                pgResources     : list<IResourceLocation>
            }

        type ResourceManager with
            member x.PreparePipelineState (renderPass : RenderPass, surface : Aardvark.Base.Surface, state : PipelineState) =
                let layout, program = x.CreateShaderProgram(renderPass, surface)

                let inputs = 
                    layout.PipelineInfo.pInputs |> List.map (fun p ->
                        let name = Symbol.Create p.name
                        match Map.tryFind name state.vertexInputTypes with
                            | Some t -> (name, (false, t))
                            | None -> failf "could not get shader input %A" name
                    )
                    |> Map.ofList

                let inputState =
                    x.CreateVertexInputState(layout.PipelineInfo, Mod.constant (VertexInputState.ofTypes inputs))

                let inputAssembly =
                    x.CreateInputAssemblyState(Mod.constant state.geometryMode, program)

                let rasterizerState =
                    x.CreateRasterizerState(state.depthTest, state.cullMode, state.fillMode)

                let colorBlendState =
                    x.CreateColorBlendState(renderPass, state.writeBuffers, state.blendMode)

                let depthStencil =
                    let depthWrite = 
                        match state.writeBuffers with
                            | None -> true
                            | Some s -> Set.contains DefaultSemantic.Depth s
                    x.CreateDepthStencilState(depthWrite, state.depthTest, state.stencilMode)

                let pipeline = 
                    x.CreatePipeline(
                        program,
                        renderPass,
                        inputState,
                        inputAssembly,
                        rasterizerState,
                        colorBlendState,
                        depthStencil,
                        state.writeBuffers
                    )
                {
                    ppPipeline  = pipeline
                    ppLayout    = layout
                    ppUniforms  = state.globalUniforms
                }

            member x.PrepareGeometry(state : PreparedPipelineState, g : Geometry) : PreparedGeometry =
                let resources = System.Collections.Generic.List<IResourceLocation>()

                let layout = state.ppLayout

                let descriptorSets, additionalResources = 
                    x.CreateDescriptorSets(layout, UniformProvider.union (UniformProvider.ofMap g.uniforms) state.ppUniforms)

                resources.AddRange additionalResources

                let vertexBuffers = 
                    layout.PipelineInfo.pInputs 
                        |> List.sortBy (fun i -> i.location) 
                        |> List.map (fun i ->
                            let sem = i.semantic 
                            match Map.tryFind sem g.vertexAttributes with
                                | Some b ->
                                    x.CreateBuffer(b), 0L
                                | None ->
                                    failf "geometry does not have buffer %A" sem
                        )

                let dsb = x.CreateDescriptorSetBinding(layout, Array.toList descriptorSets)
                let vbb = x.CreateVertexBufferBinding(vertexBuffers)

                let isIndexed, ibo =
                    match g.indices with
                        | Some ib ->
                            let b = x.CreateIndexBuffer ib.Buffer
                            let ibb = x.CreateIndexBufferBinding(b, VkIndexType.ofType ib.ElementType)
                            resources.Add ibb
                            true, ibb |> Some
                        | None ->
                            false, None

                let call = x.CreateDrawCall(isIndexed, g.call)

                resources.Add dsb
                resources.Add vbb
                resources.Add call



                {
                    pgOriginal      = g
                    pgDescriptors   = dsb
                    pgAttributes    = vbb
                    pgIndex         = ibo
                    pgCall          = call
                    pgResources     = CSharpList.toList resources
                }



    module RuntimeCommands =
        

        [<AbstractClass>]
        type PreparedCommand() =
            inherit AdaptiveObject()

            let stream = new VKVM.CommandStream()

            let mutable prev : Option<PreparedCommand> = None
            let mutable next : Option<PreparedCommand> = None

            member x.Prev
                with get() = prev
                and set p = prev <- p

            member x.Next 
                with get() = next
                and set n =
                    next <- n
                    match n with
                        | Some n ->
                            stream.Next <- Some n.Stream
                        | None ->
                            stream.Next <- None

            interface ILinked<PreparedCommand> with
                member x.Prev 
                    with get() = x.Prev
                    and set p = x.Prev <- p

                member x.Next 
                    with get() = x.Next
                    and set n = x.Next <- n

            abstract member Compile : AdaptiveToken * VKVM.CommandStream -> unit
            abstract member Free : unit -> unit

            member x.Stream = stream

            member x.Update(t : AdaptiveToken) =
                x.EvaluateIfNeeded t () (fun t ->
                    x.Compile(t, stream)
                ) 

            member x.Dispose() =
                x.Free()
                stream.Dispose()

            interface IDisposable with
                member x.Dispose() = x.Dispose()

        and EmptyCommand() =
            inherit PreparedCommand()

            override x.Compile(_,_) = ()
            override x.Free() = ()

        and RenderObjectCommand(compiler : Compiler, o : IRenderObject) =
            inherit PreparedCommand()

            let mutable prepared : Option<PreparedMultiRenderObject> = None

            member x.GroupKey =
                match prepared with
                    | Some p -> [ p.First.pipeline :> obj; p.Id :> obj ]
                    | None -> failwith "inconsistent state"

            override x.Free() =
                match prepared with
                    | Some o -> 
                        for o in o.Children do
                            for r in o.resources do compiler.resources.Remove r   
                            
                        prepared <- None
                    | None ->
                        ()     

            override x.Compile(_, stream) =
                let o = compiler.manager.PrepareRenderObject(compiler.renderPass, o)
                for o in o.Children do
                    for r in o.resources do compiler.resources.Add r        

                    stream.IndirectBindPipeline(o.pipeline.Pointer) |> ignore
                    stream.IndirectBindDescriptorSets(o.descriptorSets.Pointer) |> ignore

                    match o.indexBuffer with
                        | Some ib ->
                            stream.IndirectBindIndexBuffer(ib.Pointer) |> ignore
                        | None ->
                            ()

                    stream.IndirectBindVertexBuffers(o.vertexBuffers.Pointer) |> ignore
                    stream.IndirectDraw(compiler.stats, o.isActive.Pointer, o.drawCalls.Pointer) |> ignore

                prepared <- Some o

        and UnorderedRenderObjectCommand(compiler : Compiler, objects : aset<IRenderObject>) =
            inherit PreparedCommand()

            let reader = objects.GetReader()

            let firstCommand =
                { new PreparedCommand() with
                    override x.Free() = ()
                    override x.Compile(_,_) = ()
                }

            let trie = Trie<PreparedCommand>()
            do trie.Add([], firstCommand)
            
            let cache = Dict<IRenderObject, RenderObjectCommand>()
            let dirty = HashSet<RenderObjectCommand>()

            let insert (cmd : RenderObjectCommand) =
                trie.Add(cmd.GroupKey, cmd)

            let remove (cmd : RenderObjectCommand) =
                trie.Remove(cmd.GroupKey) |> ignore

            override x.InputChanged(_,i) =
                match i with
                    | :? RenderObjectCommand as c ->
                        lock dirty (fun () -> dirty.Add c |> ignore)
                    | _ ->
                        ()

            override x.Free() =
                for cmd in cache.Values do cmd.Dispose()
                cache.Clear()
                dirty.Clear()
                firstCommand.Dispose()
                reader.Dispose()
                trie.Clear()

            override x.Compile(token, stream) =
                let deltas = reader.GetOperations token

                for d in deltas do
                    match d with
                        | Add(_,o) ->
                            let cmd = new RenderObjectCommand(compiler, o) 
                            // insert cmd into trie (link programs)
                            cmd.Update token
                            insert cmd
                            cache.[o] <- cmd


                        | Rem(_,o) ->
                            match cache.TryRemove(o) with
                                | (true, cmd) ->
                                    // unlink stuff (remove from trie)
                                    remove cmd
                                    lock dirty (fun () -> dirty.Remove cmd |> ignore)
                                    cmd.Dispose()
                                | _ ->
                                    ()


                let dirty = 
                    lock dirty (fun () ->
                        let res = HashSet.toArray dirty
                        dirty.Clear()
                        res
                    )

                for d in dirty do
                    d.Update(token)

                stream.Clear()
                stream.Call(firstCommand.Stream) |> ignore

        and ClearCommand(compiler : Compiler, colors : Map<Symbol, IMod<C4f>>, depth : Option<IMod<float>>, stencil : Option<IMod<uint32>>) =
            inherit PreparedCommand()

            override x.Compile(token, stream) =
                stream.Clear()

                let hasDepth =
                    Option.isSome compiler.renderPass.DepthStencilAttachment

                let depthClear =
                    match depth, stencil with
                        | Some d, Some s ->
                            let d = d.GetValue token
                            let s = s.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.DepthBit ||| VkImageAspectFlags.StencilBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(float32 d, s))
                            ) |> Some

                        | Some d, None ->   
                            let d = d.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.DepthBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(float32 d, 0u))
                            ) |> Some
                             
                        | None, Some s ->
                            let s = s.GetValue token

                            VkClearAttachment(
                                VkImageAspectFlags.StencilBit, 
                                0u,
                                VkClearValue(depthStencil = VkClearDepthStencilValue(1.0f, s))
                            ) |> Some

                        | None, None ->
                            None

                let colors = 
                    compiler.renderPass.ColorAttachments |> Map.toSeq |> Seq.choose (fun (i,(n,_)) ->
                        match Map.tryFind n colors with
                            | Some value -> 
                                let res = 
                                    VkClearAttachment(
                                        VkImageAspectFlags.ColorBit, 
                                        uint32 (if hasDepth then 1 + i else i),
                                        VkClearValue(color = VkClearColorValue(float32 = value.GetValue(token).ToV4f()))
                                    )
                                Some res
                            | None ->
                                None
                    ) |> Seq.toArray

                let clears =
                    match depthClear with
                        | Some c -> Array.append [|c|] colors
                        | None -> colors

                let rect =
                    let s = compiler.viewports.GetValue(token).[0]
                    VkClearRect(
                        VkRect2D(VkOffset2D(s.Min.X,s.Min.Y), VkExtent2D(uint32 (1 + s.Max.X - s.Min.X) , uint32 (1 + s.Max.Y - s.Min.Y))),
                        0u,
                        uint32 compiler.renderPass.LayerCount
                    )

                stream.ClearAttachments(
                    clears,
                    [| rect |]
                ) |> ignore

            override x.Free() =
                ()

        and OrderedCommand(compiler : Compiler, commands : alist<RuntimeCommand>) =
            inherit PreparedCommand()

            let reader = commands.GetReader()

            let first = new VKVM.CommandStream()
            let cache = SortedDictionaryExt<Index, PreparedCommand>(compare)
            let dirty = HashSet<PreparedCommand>()

            let destroy (cmd : PreparedCommand) =
                lock dirty (fun () -> dirty.Remove cmd |> ignore)

                match cmd.Prev with
                    | Some p -> p.Next <- cmd.Next
                    | None -> first.Next <- cmd.Next |> Option.map (fun c -> c.Stream)

                match cmd.Next with
                    | Some n -> n.Prev <- cmd.Prev
                    | None -> ()

                cmd.Dispose()

            let set (token : AdaptiveToken) (index : Index) (v : RuntimeCommand) =
                cache |> SortedDictionary.setWithNeighbours index (fun l s r ->
                    
                    // TODO: move causes destroy/prepare
                    match s with
                        | Some cmd -> destroy cmd
                        | None -> ()

                    let cmd = compiler.Compile v
                    cmd.Update(token)

                    match l with
                        | Some(_,l) -> l.Next <- Some cmd
                        | None -> first.Next <- Some cmd.Stream

                    match r with
                        | Some(_,r) -> 
                            cmd.Next <- Some r
                            r.Prev <- Some cmd
                        | None -> 
                            cmd.Next <- None

                    cmd
                ) |> ignore
                
            let remove (index : Index) =
                match cache.TryGetValue index with
                    | (true, cmd) ->
                        cache.Remove index |> ignore
                        destroy cmd
                    | _ ->
                        ()

            override x.InputChanged(_,i) =
                match i with
                    | :? PreparedCommand as c ->
                        lock dirty (fun () -> dirty.Add c |> ignore)
                    | _ ->
                        ()

            override x.Compile(token, stream) =
                let deltas = reader.GetOperations token

                for (index, op) in PDeltaList.toSeq deltas do
                    match op with
                        | Set cmd ->
                            set token index cmd
                        | Remove ->
                            remove index


                let dirty = 
                    lock dirty (fun () ->
                        let res = HashSet.toArray dirty
                        dirty.Clear()
                        res
                    )

                for d in dirty do
                    d.Update(token)

                stream.Clear()
                stream.Call(first) |> ignore

            override x.Free() =
                reader.Dispose()
                cache |> Seq.iter (fun (KeyValue(_,v)) -> v.Dispose())
                cache.Clear()
                dirty.Clear()
                first.Dispose()

        and IfThenElseCommand(compiler : Compiler, condition : IMod<bool>, ifTrue : RuntimeCommand, ifFalse : RuntimeCommand) =
            inherit PreparedCommand()

            let ifTrue = compiler.Compile ifTrue
            let ifFalse = compiler.Compile ifFalse

            override x.Free() =
                ifTrue.Dispose()
                ifFalse.Dispose()

            override x.Compile(token, stream) =
                let cond = condition.GetValue(token)

                ifTrue.Update(token)
                ifFalse.Update(token)

                stream.Clear()
                if cond then stream.Call(ifTrue.Stream) |> ignore
                else stream.Call(ifFalse.Stream) |> ignore

        and GeometryCommand(compiler : Compiler, pipeline : PreparedPipelineState, geometry : Geometry) =
            inherit PreparedCommand()

            static let alwaysActive =
                let ptr = NativePtr.alloc 1
                NativePtr.write ptr 1
                ptr

            let mutable prepared : Option<PreparedGeometry> = None

            override x.Free() =
                match prepared with
                    | Some pg ->
                        for r in pg.pgResources do
                            compiler.resources.Remove r
                        prepared <- None
                    | None ->
                        ()

            override x.Compile(_, stream) =
                let pg = compiler.manager.PrepareGeometry(pipeline, geometry)
                prepared <- Some pg

                for r in pg.pgResources do
                    compiler.resources.Add r
                
                stream.IndirectBindDescriptorSets(pg.pgDescriptors.Pointer) |> ignore

                match pg.pgIndex with
                    | Some index ->
                        stream.IndirectBindIndexBuffer(index.Pointer) |> ignore
                    | None ->
                        ()

                stream.IndirectBindVertexBuffers(pg.pgAttributes.Pointer) |> ignore
                stream.IndirectDraw(compiler.stats, alwaysActive, pg.pgCall.Pointer) |> ignore


        and GroupedCommand(compiler : Compiler, surface : Aardvark.Base.Surface, state : PipelineState, geometries : aset<Geometry>) =
            inherit PreparedCommand()
            
            let reader = geometries.GetReader()

            let first = new VKVM.CommandStream()
            let mutable last = first

            let cache = Dict<Geometry, GeometryCommand>()

            let mutable preparedPipeline = None

            override x.Free() =
                reader.Dispose()
                first.Dispose()
                match preparedPipeline with
                    | Some p ->
                        compiler.resources.Remove p.ppPipeline
                        preparedPipeline <- None
                    | None ->
                        ()

                for cmd in cache.Values do
                    cmd.Dispose()

                cache.Clear()
                last <- Unchecked.defaultof<_>
                ()

            override x.Compile(token, stream) =
                let pipeline = 
                    match preparedPipeline with
                        | None -> 
                            let pipeline = compiler.manager.PreparePipelineState(compiler.renderPass, surface, state)
                            compiler.resources.Add pipeline.ppPipeline
                            first.IndirectBindPipeline(pipeline.ppPipeline.Pointer) |> ignore
                            preparedPipeline <- Some pipeline

                            stream.Clear()
                            stream.Call(first) |> ignore

                            pipeline
                        | Some pipeline ->
                            pipeline


                let ops = reader.GetOperations token

                for d in ops do
                    match d with
                        | Add(_,g) ->
                            let cmd = new GeometryCommand(compiler, pipeline, g)
                            cmd.Update token

                            cache.[g] <- cmd
                            
                            let stream = cmd.Stream
                            last.Next <- Some stream
                            last <- stream

                        | Rem(_,g) ->
                            match cache.TryRemove g with
                                | (true, cmd) ->
                                    let stream = cmd.Stream

                                    match stream.Next with
                                        | Some n -> ()
                                        | None -> last <- stream.Prev.Value

                                    match stream.Prev with
                                        | Some s -> s.Next <- stream.Next
                                        | None -> failwith "impossible"


                                    cmd.Dispose()

                                | _ ->
                                    ()

        and LodTreeCommand(compiler : Compiler, surface : Aardvark.Base.Surface, state : PipelineState, loader : LodTreeLoader<Geometry>) as this =
            inherit PreparedCommand()

            let mutable preparedPipeline : Option<PreparedPipelineState> = None
            
            let first = new VKVM.CommandStream()
            let mutable last = first

            let mutable thread = { new IDisposable with member x.Dispose() = () }

            let toDelete = System.Collections.Generic.List<GeometryCommand>()


//            let innerCompiler =
//                let user = 
//                    { new IResourceUser with 
//                        member x.AddLocked(_) = () 
//                        member x.RemoveLocked(_) = ()
//                    }
//                { compiler with resources = ResourceLocationSet user }

            let prepare pipeline (g : Geometry) =
                use t = compiler.manager.Device.Token
                let cmd = new GeometryCommand(compiler, pipeline, g)
                cmd.Update(AdaptiveToken.Top)
                cmd

            let delete (cmd : GeometryCommand) =
                lock toDelete (fun () -> toDelete.Add cmd)

            let activate (cmd : GeometryCommand) =
                lock first (fun () ->
                    let s = cmd.Stream
                    s.Next <- None
                    last.Next <- Some s
                    last <- s
                )

            let deactivate (cmd : GeometryCommand) =
                lock first (fun () ->
                    let s = cmd.Stream
                    let prev = s.Prev.Value

                    match s.Next with
                        | Some n ->
                            prev.Next <- Some n
                        | None ->
                            last <- prev
                            prev.Next <- None

                )

            let flush() =
                Log.line "flush"
                transact (fun () -> this.MarkOutdated())

            let config pipeline =
                {
                    prepare     = prepare pipeline
                    delete      = delete
                    activate    = activate
                    deactivate  = deactivate
                    flush       = flush
                }

            override x.Free() =
                ()

            override x.Compile(token, stream) =
                Log.line "update"
                match preparedPipeline with
                    | None -> 
                        let pipeline = compiler.manager.PreparePipelineState(compiler.renderPass, surface, state)
                        compiler.resources.Add pipeline.ppPipeline
                        first.IndirectBindPipeline(pipeline.ppPipeline.Pointer) |> ignore
                        preparedPipeline <- Some pipeline

                        stream.Clear()
                        stream.Call(first) |> ignore
                        thread <- loader.Start (config pipeline)

                        ()
                    | Some _ ->
                        ()
                            
                let dead =
                    lock toDelete (fun () -> 
                        let arr = CSharpList.toArray toDelete
                        toDelete.Clear()
                        arr
                    )
                for d in dead do d.Dispose()

                //innerCompiler.resources.Update(token) |> ignore
                ()


        and Compiler =
            {
                resources       : ResourceLocationSet
                manager         : ResourceManager
                renderPass      : RenderPass
                stats           : nativeptr<V2i>
                viewports       : IMod<Box2i[]>
                scissors        : IMod<Box2i[]>
            }

            member x.Compile (cmd : RuntimeCommand) : PreparedCommand =
                match cmd with
                    | RuntimeCommand.EmptyCmd ->
                        new EmptyCommand()
                            :> PreparedCommand

                    | RuntimeCommand.RenderCmd objects ->
                        new UnorderedRenderObjectCommand(x, objects)
                            :> PreparedCommand

                    | RuntimeCommand.ClearCmd(colors, depth, stencil) ->
                        new ClearCommand(x, colors, depth, stencil)
                            :> PreparedCommand

                    | RuntimeCommand.OrderedCmd commands ->
                        new OrderedCommand(x, commands)
                            :> PreparedCommand

                    | RuntimeCommand.IfThenElseCmd(c,i,e) ->
                        new IfThenElseCommand(x, c, i, e)
                            :> PreparedCommand

                    | RuntimeCommand.GeometriesCmd(surface, state, geometries) ->
                        new GroupedCommand(x, surface, state, geometries)
                            :> PreparedCommand

                    | RuntimeCommand.LodTreeCmd(surface, state, geometries) ->
                        new LodTreeCommand(x, surface, state, geometries)
                            :> PreparedCommand
                        

                    | RuntimeCommand.DispatchCmd _  ->
                        failwith "[Vulkan] compute commands not implemented"

    type CommandTask(device : Device, renderPass : RenderPass, command : RuntimeCommand) =
        inherit AbstractRenderTask()

        let pool = device.GraphicsFamily.CreateCommandPool()
        let viewports = Mod.init [||]
        let scissors = Mod.init [||]

        let cmd = pool.CreateCommandBuffer(CommandBufferLevel.Primary)
        let inner = pool.CreateCommandBuffer(CommandBufferLevel.Secondary)

        let locks = ReferenceCountingSet<ILockedResource>()

        let user =
            { new IResourceUser with
                member x.AddLocked l = lock locks (fun () -> locks.Add l |> ignore)
                member x.RemoveLocked l = lock locks (fun () -> locks.Remove l |> ignore)
            }

        let stats = NativePtr.alloc 1
        let manager = new ResourceManager(user, device)
        let resources = ResourceLocationSet(user)

        let compiler =
            {
                RuntimeCommands.resources       = resources
                RuntimeCommands.manager         = manager
                RuntimeCommands.renderPass      = renderPass
                RuntimeCommands.stats           = stats
                RuntimeCommands.viewports       = viewports
                RuntimeCommands.scissors        = scissors
            }

        let compiled = compiler.Compile command

        override x.Release() =
            transact (fun () ->
                compiled.Dispose()
                inner.Dispose()
                cmd.Dispose()
                pool.Dispose()
                manager.Dispose()
            )

        override x.FramebufferSignature = Some (renderPass :> _)

        override x.Runtime = None

        override x.PerformUpdate(token : AdaptiveToken, rt : RenderToken) =
            ()

        override x.Use(f : unit -> 'r) =
            f()

        override x.Perform(token : AdaptiveToken, rt : RenderToken, desc : OutputDescription) =
            x.OutOfDate <- true

            let fbo =
                match desc.framebuffer with
                    | :? Framebuffer as fbo -> fbo
                    | fbo -> failwithf "unsupported framebuffer: %A" fbo

            
            let sc =
                if device.AllCount > 1u then
                    if renderPass.LayerCount > 1 then
                        [| desc.viewport |]
                    else
                        let range = 
                            { 
                                frMin = desc.viewport.Min; 
                                frMax = desc.viewport.Max;
                                frLayers = Range1i(0,renderPass.LayerCount-1)
                            }
                        range.Split(int device.AllCount) 
                            |> Array.map (fun { frMin = min; frMax = max } -> Box2i(min, max))
                        
                else
                    [| desc.viewport |]

            let vp = Array.create sc.Length desc.viewport

            let viewportChanged =
                if viewports.Value = vp && scissors.Value = sc then
                    false
                else
                    transact (fun () -> viewports.Value <- vp; scissors.Value <- sc)
                    true

            use tt = device.Token
            let commandChanged = 
                lock compiled (fun () ->
                    if compiled.OutOfDate then
                        compiled.Update(token)
                        true
                    else
                        false
                )

            let resourcesChanged = 
                resources.Update(token)

            if viewportChanged || commandChanged || resourcesChanged then
                let cause =
                    String.concat "; " [
                        if commandChanged then yield "content"
                        if resourcesChanged then yield "resources"
                        if viewportChanged then yield "viewport"
                    ]
                    |> sprintf "{ %s }"

                Log.line "[Vulkan] recompile commands: %s" cause

                inner.Reset()
                inner.Begin(renderPass, CommandBufferUsage.RenderPassContinue)

                inner.enqueue {
                    do! Command.SetViewports(vp)
                    do! Command.SetScissors(sc)
                }

                inner.AppendCommand()
                compiled.Stream.Run(inner.Handle)
                
                inner.End()


            tt.Sync()

            cmd.Reset()
            cmd.Begin(renderPass, CommandBufferUsage.OneTimeSubmit)
            cmd.enqueue {
                let oldLayouts = Array.zeroCreate fbo.ImageViews.Length
                for i in 0 .. fbo.ImageViews.Length - 1 do
                    let img = fbo.ImageViews.[i].Image
                    oldLayouts.[i] <- img.Layout
                    if VkFormat.hasDepth img.Format then
                        do! Command.TransformLayout(img, VkImageLayout.DepthStencilAttachmentOptimal)
                    else
                        do! Command.TransformLayout(img, VkImageLayout.ColorAttachmentOptimal)

                do! Command.BeginPass(renderPass, fbo, false)
                do! Command.Execute [inner]
                do! Command.EndPass

                for i in 0 .. fbo.ImageViews.Length - 1 do
                    let img = fbo.ImageViews.[i].Image
                    do! Command.TransformLayout(img, oldLayouts.[i])
            }   
            cmd.End()

            device.GraphicsFamily.RunSynchronously cmd

    open ChangeableCommandBuffers

    type RenderTask(device : Device, renderPass : RenderPass, shareTextures : bool, shareBuffers : bool) =
        inherit AbstractRenderTask()

        let pool = device.GraphicsFamily.CreateCommandPool()
        let passes = SortedDictionary<Aardvark.Base.Rendering.RenderPass, AbstractChangeableSetCommandBuffer>()
        let viewports = Mod.init [||]
        let scissors = Mod.init [||]
        
        let cmd = pool.CreateCommandBuffer(CommandBufferLevel.Primary)

        let locks = ReferenceCountingSet<ILockedResource>()

        let user =
            { new IResourceUser with
                member x.AddLocked l = lock locks (fun () -> locks.Add l |> ignore)
                member x.RemoveLocked l = lock locks (fun () -> locks.Remove l |> ignore)
            }

        let manager = new ResourceManager(user, device)

        static let sortByCamera (order : RenderPassOrder) (trafo : Trafo3d) (boxes : Box3d[]) =
            let sign = 
                match order with
                    | RenderPassOrder.BackToFront -> 1
                    | RenderPassOrder.FrontToBack -> -1
                    | _ -> failf "invalid order %A" order

            let compare (l : Box3d) (r : Box3d) =
                let l = trafo.Forward.TransformPos l.Center
                let r = trafo.Forward.TransformPos r.Center
                sign * compare l.Z r.Z


            boxes.CreatePermutationQuickSort(Func<_,_,_>(compare))

        member x.HookRenderObject(o : IRenderObject) =
            match o with
                | :? RenderObject as o -> 
                    x.HookRenderObject o:> IRenderObject

                | :? MultiRenderObject as o ->
                    MultiRenderObject(o.Children |> List.map x.HookRenderObject) :> IRenderObject

                | _ ->
                    o

        member x.Add(o : IRenderObject) =
            let o = x.HookRenderObject o
            let key = o.RenderPass
            let cmd =
                match passes.TryGetValue key with
                    | (true,c) -> c
                    | _ ->
                        let c = 
                            // TODO: sorting broken
//                            match key.Order with
//                                | RenderPassOrder.BackToFront | RenderPassOrder.FrontToBack -> 
//                                    ChangeableOrderedCommandBuffer(manager, pool, renderPass, viewports, Mod.constant (sortByCamera key.Order)) :> AbstractChangeableSetCommandBuffer
//                                | _ -> 
                                    ChangeableUnorderedCommandBuffer(manager, pool, renderPass, viewports, scissors) :> AbstractChangeableSetCommandBuffer
                        passes.[key] <- c
                        x.MarkOutdated()
                        c
            cmd.Add(o)

        member x.Remove(o : IRenderObject) =
            let key = o.RenderPass
            match passes.TryGetValue key with
                | (true,c) -> 
                    c.Remove o
                | _ ->
                    false

        member x.Clear() =
            for c in passes.Values do
                c.Dispose()
            passes.Clear()
            locks.Clear()
            cmd.Reset()
            x.MarkOutdated()

        override x.Release() =
            transact (fun () ->
                x.Clear()
                cmd.Dispose()
                pool.Dispose()
                manager.Dispose()
            )

        override x.FramebufferSignature = Some (renderPass :> _)

        override x.Runtime = None

        override x.PerformUpdate(token : AdaptiveToken, rt : RenderToken) =
            ()

        override x.Use(f : unit -> 'r) =
            f()

        override x.Perform(token : AdaptiveToken, rt : RenderToken, desc : OutputDescription) =
            x.OutOfDate <- true
            let range = 
                { 
                    frMin = desc.viewport.Min; 
                    frMax = desc.viewport.Max;
                    frLayers = Range1i(0,renderPass.LayerCount-1)
                }
            let ranges = range.Split(int device.AllCount)
            let sc =
                if device.AllCount > 1u then
                    if renderPass.LayerCount > 1 then
                        [| desc.viewport |]
                    else
                        ranges |> Array.map (fun { frMin = min; frMax = max } -> Box2i(min, max))
                        
                else
                    [| desc.viewport |]
            let vp = Array.create sc.Length desc.viewport

            if viewports.Value <> vp || scissors.Value <> sc then
                transact (fun () -> viewports.Value <- vp; scissors.Value <- sc)

            let fbo =
                match desc.framebuffer with
                    | :? Framebuffer as fbo -> fbo
                    | fbo -> failwithf "unsupported framebuffer: %A" fbo

            use tt = device.Token
            let passCmds = passes.Values |> Seq.map (fun p -> p.GetValue(token)) |> Seq.toList
            tt.Sync()


            cmd.Reset()
            cmd.Begin(renderPass, CommandBufferUsage.OneTimeSubmit)
            cmd.enqueue {
                let oldLayouts = Array.zeroCreate fbo.ImageViews.Length
                for i in 0 .. fbo.ImageViews.Length - 1 do
                    let img = fbo.ImageViews.[i].Image
                    oldLayouts.[i] <- img.Layout
                    if VkFormat.hasDepth img.Format then
                        do! Command.TransformLayout(img, VkImageLayout.DepthStencilAttachmentOptimal)
                    else
                        do! Command.TransformLayout(img, VkImageLayout.ColorAttachmentOptimal)

                do! Command.BeginPass(renderPass, fbo, false)
                do! Command.Execute passCmds
                do! Command.EndPass

                if ranges.Length > 1 then
                    let deviceCount = int device.AllCount
                    
                    for (sem,a) in Map.toSeq fbo.Attachments do
                        if sem <> DefaultSemantic.Depth then 
                            let img = a.Image
                            let layers = a.ArrayRange
                            let layerCount = 1 + layers.Max - layers.Min
                        
                            let aspect =
                                match VkFormat.toImageKind img.Format with
                                    | ImageKind.Depth -> ImageAspect.Depth
                                    | ImageKind.DepthStencil  -> ImageAspect.DepthStencil
                                    | _ -> ImageAspect.Color 

                            let subResource = img.[aspect, a.MipLevelRange.Min]
                            let ranges =
                                ranges |> Array.map (fun { frMin = min; frMax = max; frLayers = layers} ->
                                    layers, Box3i(V3i(min,0), V3i(max, 0))
                                )

                            do! Command.SyncPeers(subResource, ranges)

                    
                for i in 0 .. fbo.ImageViews.Length - 1 do
                    let img = fbo.ImageViews.[i].Image
                    do! Command.TransformLayout(img, oldLayouts.[i])
            }   
            cmd.End()

            device.GraphicsFamily.RunSynchronously cmd
            
    type DependentRenderTask(device : Device, renderPass : RenderPass, objects : aset<IRenderObject>, shareTextures : bool, shareBuffers : bool) =
        inherit RenderTask(device, renderPass, shareTextures, shareBuffers)

        let reader = objects.GetReader()

        override x.Perform(token : AdaptiveToken, rt : RenderToken, desc : OutputDescription) =
            x.OutOfDate <- true
            let deltas = reader.GetOperations token
            if not (HDeltaSet.isEmpty deltas) then
                transact (fun () -> 
                    for d in deltas do
                        match d with
                            | Add(_,o) -> x.Add o |> ignore
                            | Rem(_,o) -> x.Remove o |> ignore
                )

            base.Perform(token, rt, desc)

        override x.Runtime = Some device.Runtime

        override x.Release() =
            reader.Dispose()
            base.Release()

    type ClearTask(device : Device, renderPass : RenderPass, clearColors : Map<Symbol, IMod<C4f>>, clearDepth : IMod<Option<float>>, clearStencil : Option<IMod<uint32>>) =
        inherit AdaptiveObject()
        static let depthStencilFormats =
            HashSet.ofList [
                RenderbufferFormat.Depth24Stencil8
                RenderbufferFormat.Depth32fStencil8
                RenderbufferFormat.DepthStencil
            ]
        
        let pool = device.GraphicsFamily.CreateCommandPool()
        let cmd = pool.CreateCommandBuffer(CommandBufferLevel.Primary)

        let clearColors =
            renderPass.ColorAttachments |> Map.toSeq |> Seq.choose (fun (i, (s,_)) -> 
                match Map.tryFind s clearColors with
                    | Some c -> Some (i,c)
                    | None -> None
            )
            |> Seq.toArray

        let renderPassDepthAspect =
            match renderPass.DepthStencilAttachment with
                | Some signature ->
                    if depthStencilFormats.Contains signature.format then
                        ImageAspect.DepthStencil
                    else
                        ImageAspect.Depth
                | _ ->
                    ImageAspect.None

        member x.Run(caller : AdaptiveToken, t : RenderToken, outputs : OutputDescription) =
            x.EvaluateAlways caller (fun caller ->
                let fbo = unbox<Framebuffer> outputs.framebuffer
                use token = device.Token

                let colors = clearColors |> Array.map (fun (i,c) -> i, c.GetValue caller)
                let depth = clearDepth.GetValue caller
                let stencil = match clearStencil with | Some c -> c.GetValue(caller) |> Some | _ -> None


                token.enqueue {
                    let views = fbo.ImageViews
                    for (index, color) in colors do
                        let image = views.[index].Image
                        do! Command.ClearColor(image.[ImageAspect.Color], color)

                    if renderPassDepthAspect <> ImageAspect.None then
                        let image = views.[views.Length-1].Image
                        match depth, stencil with
                            | Some d, Some s    -> do! Command.ClearDepthStencil(image.[renderPassDepthAspect], d, s)
                            | Some d, None      -> do! Command.ClearDepthStencil(image.[ImageAspect.Depth], d, 0u)
                            | None, Some s      -> do! Command.ClearDepthStencil(image.[ImageAspect.Stencil], 0.0, s)
                            | None, None        -> ()
                }
                token.Sync()
            )

        interface IRenderTask with
            member x.Update(c, t) = ()
            member x.Run(c,t,o) = x.Run(c,t,o)
            member x.Dispose() = 
                cmd.Dispose()
                pool.Dispose()

            member x.FrameId = 0UL
            member x.FramebufferSignature = Some (renderPass :> _)
            member x.Runtime = Some device.Runtime
            member x.Use f = lock x f






