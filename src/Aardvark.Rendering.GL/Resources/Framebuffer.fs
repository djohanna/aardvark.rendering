namespace Aardvark.Rendering.GL

open System
open System.Threading
open System.Collections.Concurrent
open System.Runtime.InteropServices
open Aardvark.Base
open OpenTK
open OpenTK.Platform
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4
open Microsoft.FSharp.Quotations

[<AutoOpen>]
module private FramebufferMemoryUsage =
    let addPhysicalFbo (ctx:Context) =
        Interlocked.Increment(&ctx.MemoryUsage.PhysicalFramebufferCount) |> ignore
    let removePhysicalFbo (ctx:Context) =
        Interlocked.Decrement(&ctx.MemoryUsage.PhysicalFramebufferCount) |> ignore
    let addVirtualFbo (ctx:Context) =
        Interlocked.Increment(&ctx.MemoryUsage.VirtualFramebufferCount) |> ignore
    let removeVirtualFbo (ctx:Context) =
        Interlocked.Decrement(&ctx.MemoryUsage.VirtualFramebufferCount) |> ignore

type FramebufferSignature(runtime : IRuntime, colors : Map<int, Symbol * AttachmentSignature>, images : Map<int, Symbol>, depth : Option<AttachmentSignature>, stencil : Option<AttachmentSignature>) =
   
    static let empty = FramebufferSignature(Unchecked.defaultof<_>, Map.empty, Map.empty, None, None)

    let signatureAssignableFrom (mine : AttachmentSignature) (other : AttachmentSignature) =
        let myCol = RenderbufferFormat.toColFormat mine.format
        let otherCol = RenderbufferFormat.toColFormat other.format
        
        myCol = otherCol

    let colorsAssignableFrom (mine : Map<int, Symbol * AttachmentSignature>) (other : Map<int, Symbol * AttachmentSignature>) =
        mine |> Map.forall (fun id (sem, signature) ->
            match Map.tryFind id other with
                | Some (otherSem, otherSig) when sem = otherSem ->
                    signatureAssignableFrom signature otherSig
                | None -> true
                | _ -> false
        )

    let depthAssignableFrom (mine : Option<AttachmentSignature>) (other : Option<AttachmentSignature>) =
        match mine, other with
            | Some mine, Some other -> signatureAssignableFrom mine other
            | _ -> true

    static member Empty = empty

    member x.Runtime = runtime
    member x.ColorAttachments = colors
    member x.DepthAttachment = depth
    member x.StencilAttachment = depth
    member x.Images = images
    member x.IsAssignableFrom (other : IFramebufferSignature) =
        if x.Equals other then 
            true
        else
            match other with
                | :? FramebufferSignature as other ->
                    runtime = other.Runtime &&
                    colorsAssignableFrom colors other.ColorAttachments
                    // TODO: check depth and stencil (cumbersome for combined DepthStencil attachments)
                | _ ->
                    false

    override x.ToString() =
        sprintf "{ ColorAttachments = %A; DepthAttachment = %A; StencilAttachment = %A }" colors depth stencil

    interface IFramebufferSignature with
        member x.Runtime = runtime
        member x.ColorAttachments = colors
        member x.DepthAttachment = depth
        member x.StencilAttachment = stencil
        member x.IsAssignableFrom other = x.IsAssignableFrom other
        member x.Images = images

type TransformFeedbackSignature(runtime : IRuntime, program : Program, primitiveType : IndexedGeometryMode, wantedSemantics : list<Symbol>) =
    member x.WantedSemantics = wantedSemantics
    member x.PrimitiveType = primitiveType
    member x.Program = program

    interface IFramebufferSignature with
        member x.Runtime = runtime
        member x.ColorAttachments = Map.empty
        member x.DepthAttachment = None
        member x.StencilAttachment = None
        member x.IsAssignableFrom other = true
        member x.Images = Map.empty


type Framebuffer(ctx : Context, signature : IFramebufferSignature, create : Aardvark.Rendering.GL.ContextHandle -> int, destroy : int -> unit, 
                 bindings : list<int * Symbol * IFramebufferOutput>, depth : Option<IFramebufferOutput>) =
    inherit UnsharedObject(ctx, (fun h -> addPhysicalFbo ctx; create h), (fun h -> removePhysicalFbo ctx; destroy h))

    let mutable bindings = bindings
    let mutable depth = depth

    static let empty = 
        new Framebuffer(
            Unchecked.defaultof<_>,
            FramebufferSignature.Empty,
            (fun _ -> 0),
            (fun _ -> ()),
            [],
            None
        )

    let resolution() =
        match depth with
            | Some d -> d.Size
            | _ ->
                let res = 
                    bindings |> Seq.tryPick (fun (_,_,b) ->
                        Some b.Size
                    )
                match res with
                    | Some s -> s
                    | None -> V2i.II //failwith "could not determine framebuffer-size."

    let mutable size = resolution()

    let mutable outputBySem = 
        let bindings = (bindings |> List.map (fun (_,s,o) -> (s,o)))
        let depth = match depth with | Some d -> [DefaultSemantic.Depth, d] | _ -> []
        List.append bindings depth |> Map.ofList

    static member Empty = empty

    static member EmptyWithSignature s =
        new Framebuffer(
            Unchecked.defaultof<_>,
            s,
            (fun _ -> 0),
            (fun _ -> ()),
            [],
            None
        )


    member x.Size 
        with get() = size
        and set v = size <- v

    member x.Update(create : Aardvark.Rendering.GL.ContextHandle -> int, b : list<int * Symbol * IFramebufferOutput>, d : Option<IFramebufferOutput>) =
        base.Update(create)
        bindings <- b
        depth <- d
        let bindings = (bindings |> List.map (fun (_,s,o) -> (s,o)))
        let depth = match depth with | Some d -> [DefaultSemantic.Depth, d] | _ -> []
        outputBySem <- List.append bindings depth |> Map.ofList

    member x.Attachments = outputBySem
    member x.Signature = signature

    interface IFramebuffer with
        member x.Signature = signature
        member x.Size = x.Size
        member x.GetHandle caller = x.Handle :> obj
        member x.Attachments = outputBySem
        member x.Dispose() = base.DestroyHandles()

//    static member Default (ctx : Context, size : V2i, samples : int) =
//        let bindings = [0,DefaultSemantic.Colors,Texture(ctx, 0, TextureDimension.Texture2D, 1, samples, V3i(size.X, size.Y, 1), 1, ChannelType.RGBA8).Output 0 :> IFramebufferOutput]
//        let depth = Renderbuffer(ctx, 0, V2i.II, RenderbufferFormat.Depth24Stencil8, 1) :> IFramebufferOutput
//        new Framebuffer(ctx, (fun _ -> 0), (fun _ -> ()), bindings, Some depth)

[<AutoOpen>]
module FramebufferExtensions =

    let private depthStencilFormats =
        HashSet.ofList [
            RenderbufferFormat.Depth24Stencil8
            RenderbufferFormat.Depth32fStencil8
            RenderbufferFormat.DepthStencil
        ]

    let private init (bindings : list<int * Symbol * IFramebufferOutput>) (depth : Option<IFramebufferOutput>) (stencil : Option<IFramebufferOutput>) (c : Aardvark.Rendering.GL.ContextHandle) : int =
        let handle = GL.GenFramebuffer()
        GL.Check "could not create framebuffer"

        let mutable oldFbo = 0
        GL.GetInteger(GetPName.FramebufferBinding, &oldFbo)

        GL.BindFramebuffer(FramebufferTarget.Framebuffer, handle)
        GL.Check "could not bind framebuffer"

        let attach (o : IFramebufferOutput) (attachment) =
            match o with

                | :? Renderbuffer as o ->
                    GL.FramebufferRenderbuffer(FramebufferTarget.Framebuffer, attachment, RenderbufferTarget.Renderbuffer, o.Handle)
                    GL.Check "could not attach renderbuffer"


                | :? BackendTextureOutputView as r ->
                    let { texture = o; level = level; slice = slice } = r
                    let o = unbox<Texture> o

                    match o.Dimension with
                        | TextureDimension.TextureCube ->
                            let (_,target) = TextureExtensions.cubeSides.[slice]
                            if o.Count > 1 then
                                failwith "cubemaparray currently not implemented"
                            else
                                GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, attachment, target, o.Handle, level)
                            GL.Check "could not attach texture"
                        | _ ->
                            if o.Count > 1 then
                                GL.FramebufferTextureLayer(FramebufferTarget.Framebuffer, attachment, o.Handle, level, slice)
                            else
                                GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, attachment, TextureTarget.Texture2D, o.Handle, level)
                            GL.Check "could not attach texture"

        
                | _ ->
                    failwith "unsupported view"

        // attach all colors
        for (i,s,o) in bindings do
            let attachment = int FramebufferAttachment.ColorAttachment0 + i |> unbox<FramebufferAttachment>
            attach o attachment

        // attach depth
        match depth with
            | Some o ->
                if depthStencilFormats.Contains o.Format then
                    attach o FramebufferAttachment.DepthStencilAttachment
                else
                    attach o FramebufferAttachment.DepthAttachment
            | None ->
                ()

        // attach stencil
        match stencil with
            | Some o ->
                attach o FramebufferAttachment.StencilAttachment
            | None ->
                ()

        // check framebuffer
        let status = GL.CheckFramebufferStatus(FramebufferTarget.Framebuffer)
        GL.Check "could not get framebuffer status"

        if status <> FramebufferErrorCode.FramebufferComplete then
            raise <| OpenGLException(ErrorCode.InvalidFramebufferOperation, sprintf "framebuffer incomplete: %A" status)

        // unbind
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, oldFbo)
        GL.Check "could not unbind framebuffer"

        handle

    let private destroy (handle : int) =
        GL.DeleteFramebuffer handle
        GL.Check "could not delete framebuffer"

    type Context with

        member x.CreateFramebuffer (signature : IFramebufferSignature, bindings : list<int * Symbol * IFramebufferOutput>, depth : Option<IFramebufferOutput>, stencil : Option<IFramebufferOutput>) =
            let init = init bindings depth stencil
            addVirtualFbo x
            new Framebuffer(x, signature, init, destroy, bindings, depth)

        member x.Delete(f : Framebuffer) =
            removeVirtualFbo x
            f.DestroyHandles()

        member x.Update (f : Framebuffer, bindings : list<int * Symbol * IFramebufferOutput>, depth : Option<IFramebufferOutput>, stencil : Option<IFramebufferOutput>) =
            let init = init bindings depth stencil
            f.Update(init, bindings, depth)