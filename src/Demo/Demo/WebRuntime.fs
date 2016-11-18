namespace Aardvark.Rendering.Web

open System
open System.Threading
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering

open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils

open System
open System.Net

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

open Nessos.FsPickler

[<AllowNullLiteral>]
type WebResourceManager() =

    interface IResourceManager with
        member x.CreateSurface(signature : IFramebufferSignature, surface : IMod<ISurface>) : IResource<IBackendSurface> = failwith ""
        member x.CreateBuffer(buffer : IMod<IBuffer>) : IResource<IBackendBuffer> = failwith ""
        member x.CreateTexture(texture : IMod<ITexture>) : IResource<IBackendTexture> =
            failwith ""

and WebRuntime() =
    let manager = WebResourceManager()

    interface IRuntime with
        member x.ResourceManager = manager :> _
        member x.ContextLock =
            Monitor.Enter x
            { new IDisposable with member __.Dispose() = Monitor.Exit x }

        member x.CreateFramebufferSignature(attachments : SymbolDict<AttachmentSignature>, images : Set<Symbol>) : IFramebufferSignature = failwith ""
        member x.DeleteFramebufferSignature(s : IFramebufferSignature) = failwith ""


        member x.PrepareBuffer      (b : IBuffer) : IBackendBuffer = failwith ""
        member x.PrepareTexture     (t : ITexture) : IBackendTexture = failwith ""
        member x.PrepareSurface     (signature : IFramebufferSignature, surface : ISurface) : IBackendSurface = failwith ""
        member x.PrepareRenderObject(signature : IFramebufferSignature, ro : IRenderObject) : IPreparedRenderObject = failwith ""


        member x.DeleteBuffer (b:  IBackendBuffer ): unit = failwith ""
        member x.DeleteTexture(b:  IBackendTexture) : unit = failwith ""
        member x.DeleteSurface(b:  IBackendSurface) : unit = failwith ""



        member x.CreateStreamingTexture(mipMaps : bool) : IStreamingTexture = failwith ""
        member x.CreateTexture      (size : V2i, format : TextureFormat, levels : int, samples : int, count : int) : IBackendTexture = failwith ""
        member x.CreateTextureCube  (size : V2i, format : TextureFormat, levels : int, samples : int) : IBackendTexture = failwith ""
        member x.CreateRenderbuffer (size : V2i, format : RenderbufferFormat, samples : int) : IRenderbuffer = failwith ""
        member x.CreateFramebuffer  (signature : IFramebufferSignature, attachments : Map<Symbol, IFramebufferOutput>): IFramebuffer = failwith ""
        member x.CreateMappedBuffer () : IMappedBuffer = failwith ""
        member x.CreateMappedIndirectBuffer(indexed : bool) : IMappedIndirectBuffer = failwith ""

        member x.DeleteStreamingTexture( t : IStreamingTexture) : unit = failwith ""
        member x.DeleteRenderbuffer     (a : IRenderbuffer ) : unit = failwith ""
        member x.DeleteFramebuffer      (a : IFramebuffer  ) : unit = failwith ""

        member x.CompileClear  (fboSignature : IFramebufferSignature, clearColors : IMod<Map<Symbol, C4f>>, clearDepth : IMod<Option<double>>) : IRenderTask = failwith ""
        member x.CompileRender (fboSignature : IFramebufferSignature, config : BackendConfiguration, objs : aset<IRenderObject>) : IRenderTask = failwith ""
    
        member x.GenerateMipMaps     (t : IBackendTexture) : unit = failwith ""
        member x.ResolveMultisamples (output : IFramebufferOutput, tex : IBackendTexture, trafo : ImageTrafo ) : unit = failwith "" 
        member x.Upload              (texture : IBackendTexture, level : int, slice : int, source : PixImage ) : unit = failwith ""
        member x.Download            (texture : IBackendTexture, level : int, slice : int, target : PixImage ) : unit = failwith ""
        member x.DownloadStencil     (texture : IBackendTexture, level : int, slice : int, target : Matrix<int>) : unit = failwith ""
        member x.DownloadDepth       (texture : IBackendTexture, level : int, slice : int, target : Matrix<float32>) : unit = failwith ""


module UTF8 =
    open System.Text

    let private utf8 = Encoding.UTF8
    let toString (bs : byte []) =
        utf8.GetString bs

    let ofString (s : String) = utf8.GetBytes(s)




module Test =
    
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics
    open Aardvark.Application
    open Aardvark.Application.WinForms
    open Aardvark.Rendering.GL
    open Chiron

    type TypeName = string
    type WebDrawCall =
        {
            firstIndex : int
            baseVertex : int
            faceVertexCount : int 
        } with
            static member ToJson(a : WebDrawCall) = json {
                    do! Json.write "firstIndex" a.firstIndex
                    do! Json.write "baseVertex" a.baseVertex
                    do! Json.write "faceVertexCount" a.faceVertexCount
                }


    type WebBuffer = 
        {
            typeName : TypeName
            offset : int
            stride : int
            value  : byte[]
        } with
            static member ToJson(a : WebBuffer) = json {
                    do! Json.write "typeName" a.typeName
                    do! Json.write "offset" a.offset
                    do! Json.write "stride" a.stride
                    do! Json.write "value" Json.ar
                }
    type Uniform =
        {
            typeName : TypeName
            value    : string
        } with 
            static member ToJson(a : Uniform) = json {
                    do! Json.write "typeName" a.typeName
                    do! Json.write "value" a.value
                }

    type WebPreparedObj =
        {
            uniforms     : array<string * Uniform>
            vertexInputs : array<string * WebBuffer>
            drawCall     : WebDrawCall
            shader       : string
        } with 
            static member ToJson(a : WebPreparedObj) = json {
                    do! Json.write "typeName" a.uniforms
                    do! Json.write "vertexInputs" a.vertexInputs
                    do! Json.write "drawCall" a.drawCall
                    do! Json.write "shader" a.shader
                }



    let run () = 

        use app = new OpenGlApplication()
        let runtime = app.Runtime |> unbox<Aardvark.Rendering.GL.Runtime>
        let win = app.CreateSimpleRenderWindow(1)

        let objs = 
            Sg.box' C4b.Red Box3d.Unit
            |> Sg.effect [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Red  |> toEffect ]

        let ros = objs.RenderObjects()

        let webGL : FShade.GLSL.CompilerConfiguration = 
            {
                languageVersion = Version(1,2)
                enabledExtensions = Set.empty
                createUniformBuffers = false
                createGlobalUniforms = false
                createBindings = false
                createDescriptorSets = false
                createInputLocations = false
                expectRowMajorMatrices = true
                createPerStageUniforms = true
                flipHandedness = false
                depthRange = Range1d(-1.0,1.0)
                treatUniformsAsInputs = false
            }

        let compileSurface (s : IMod<ISurface>) =
            let hereSurface = win.Runtime.ResourceManager.CreateSurface(win.FramebufferSignature, s)
            let s = s.GetValue()
            match s with
                | :? FShadeSurface as gen -> 
                    let esShader = FShade.GLES.compileEffect gen.Effect
                    //printfn "%A" shader
                    let generated = gen.GenerateWithConfig(webGL, win.FramebufferSignature)
                    match generated with
                        | Success(bs) -> 
                            let p = hereSurface.Handle.GetValue()
                            p.Inputs, p.Uniforms, bs.Code.Replace("#version 410","")
                        | Error e ->    
                            failwithf "could not compile shader: %s" e
                | _ -> failwith "wrong surface"

        let mkFreshSet () =
            [|
                for o in ros |> ASet.toList do
                    match o with
                        | :? RenderObject as ro -> 
                            let inputs,uniforms,code = compileSurface ro.Surface
                            let vertexInputs =
                                [|
                                    for (name,t) in inputs do
                                        match ro.VertexAttributes.TryGetAttribute(name |> Sym.ofString) with
                                            | Some v -> 
                                                match v.Buffer.GetValue() with
                                                    | :? ArrayBuffer as a -> 
                                                        let b = {
                                                            typeName = a.ElementType.Name
                                                            offset = v.Offset
                                                            stride = v.Stride
                                                            value = a.Data
                                                         }
                                                        yield name, b
                                                    | :? NullBuffer as n -> 
                                                        printfn "warning there is null buffer: %A" (name,t)
                                                    | _ -> failwith ""
                                            | _ -> failwith "could not get input varying"
                                |]
                            let uniforms =
                                [|
                                    for (name,t) in uniforms do
                                        match ro.Uniforms.TryGetUniform(ro.AttributeScope, name |> Sym.ofString) with
                                            | Some v -> 
                                                yield name, { typeName = t.Name; value = sprintf "%A" (v.GetValue()) }
                                            | None -> printfn "could not get uniform: %A" (name,t)
                                |]
                            let drawCall = 
                                match ro.DrawCallInfos |> Mod.force with
                                    | [s] -> 
                                        { baseVertex = s.BaseVertex; firstIndex = s.FirstIndex; faceVertexCount = s.FaceVertexCount }
                                    | _ -> failwith "not suppored"
                            yield 
                                { uniforms = uniforms; vertexInputs = vertexInputs; drawCall = drawCall; shader = code }
                        | _ -> failwith "only works for base render objects"
            |]

        printfn "%A" ros

        let json = Nessos.FsPickler.Json.FsPickler.CreateJsonSerializer(true,true)

        let echo (webSocket : WebSocket) =
            fun cx -> socket {
                let loop = ref true
                while !loop do
                    //Thread.Sleep 1000
                    let! msg = webSocket.read()
                    printfn "boss wants more frame"

                    match msg with
                        | (Text, data, true) ->
                            let str = UTF8.toString data
                            let renderObjects = mkFreshSet ()
                            let json = json.PickleToString(renderObjects) |> UTF8.ofString
                            printfn "submitting frame"
                            do! webSocket.send Text json true
                            printfn "done it"
                        | (Ping, _, _) ->
                            do! webSocket.send Pong [||] true
                        | (Close, _, _) ->
                            do! webSocket.send Close [||] true
                            loop := false
                        | _ -> ()
                }

        let app : WebPart =
            choose [
                path "/websocket" >=> handShake echo
                //GET >=> choose [ path "/" >=> file "index.html"; browseHome ];
                NOT_FOUND "Found no handlers."
            ]


        let webRenderTask =
            async {
                startWebServer { defaultConfig with logger = Loggers.ConsoleWindowLogger LogLevel.Verbose } app
            } |> Async.Start
        

        win.RenderTask <- 
            RenderTask.ofList [
                runtime.CompileClear(win.FramebufferSignature, Mod.constant C4f.Black)
                runtime.CompileRender(win.FramebufferSignature,ros)
            ]      
        win.Run()