namespace Examples

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Microsoft.FSharp.NativeInterop

open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.Rendering
open Aardvark.Base.ShaderReflection
open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Text
open Aardvark.Application.OpenVR
open Aardvark.Rendering.Vulkan.KHXDeviceGroup
open KHRBindMemory2
open KHXDeviceGroup
open KHXDeviceGroup.KHRBindMemory2

#nowarn "9"
#nowarn "51"

[<RequireQualifiedAccess>]
type Backend =
    | GL 
    | Vulkan 
    | Both
    
[<RequireQualifiedAccess>]
type Display =
    | Mono
    | Stereo
    | OpenVR

type RenderConfig =
    {
        backend     : Backend
        game        : bool
        debug       : bool
        samples     : int
        display     : Display
        scene       : ISg
    }


[<AutoOpen>]
module ``FShade Extensions`` =
    open FShade

    type LightDirAttribute() = inherit FShade.SemanticAttribute("LightDirection")
    type CamDirAttribute() = inherit FShade.SemanticAttribute("CameraDirection")
    type SpecularColorAttribute() = inherit FShade.SemanticAttribute("SpecularColor")

    type UniformScope with
        member x.AmbientColor : V4d = x?Material?AmbientColor
        member x.DiffuseColor : V4d = x?Material?DiffuseColor
        member x.EmissiveColor : V4d = x?Material?EmissiveColor
        member x.ReflectiveColor : V4d = x?Material?ReflectiveColor
        member x.SpecularColor : V4d = x?Material?SpecularColor
        member x.Shininess : float = x?Material?Shininess
        member x.BumpScale : float = x?Material?BumpScale



module Utilities =
    open System.Windows.Forms
    open System.Runtime.InteropServices


//    type VkStructureType with   
//        static member VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHX  = unbox<VkStructureType> 1000060002
//
//    [<StructLayout(LayoutKind.Sequential)>]
//    type VkBindImageMemoryInfoKHX = 
//        struct
//            val mutable public sType : VkStructureType
//            val mutable public pNext : nativeint
//            val mutable public image : VkImage
//            val mutable public memory : VkDeviceMemory
//            val mutable public memoryOffset : VkDeviceSize
//            val mutable public deviceIndexCount : uint32
//            val mutable public pDeviceIndices : nativeptr<uint32>
//            val mutable public _SFRRectCount : uint32
//            val mutable public pSFRRects : nativeptr<VkRect2D>
//        
//            new(sType : VkStructureType, pNext : nativeint, image : VkImage, memory : VkDeviceMemory, memoryOffset : VkDeviceSize, deviceIndexCount : uint32, pDeviceIndices : nativeptr<uint32>, _SFRRectCount : uint32, pSFRRects : nativeptr<VkRect2D>) = { sType = sType; pNext = pNext; image = image; memory = memory; memoryOffset = memoryOffset; deviceIndexCount = deviceIndexCount; pDeviceIndices = pDeviceIndices; _SFRRectCount = _SFRRectCount; pSFRRects = pSFRRects }
//        end
//        
//    type VkBindImageMemory2KHXDel = delegate of VkDevice * uint32 * nativeptr<VkBindImageMemoryInfoKHX> -> VkResult


//    typedef struct VkBindImageMemoryInfoKHX {
//        VkStructureType    sType;
//        const void*        pNext;
//        VkImage            image;
//        VkDeviceMemory     memory;
//        VkDeviceSize       memoryOffset;
//        uint32_t           deviceIndexCount;
//        const uint32_t*    pDeviceIndices;
//        uint32_t           SFRRectCount;
//        const VkRect2D*    pSFRRects;
//    } VkBindImageMemoryInfoKHX;
//    typedef VkResult (VKAPI_PTR *PFN_vkBindImageMemory2KHX)(VkDevice device, uint32_t bindInfoCount, const VkBindImageMemoryInfoKHX* pBindInfos);


    [<AbstractClass>]
    type OutputMod<'a, 'b>(inputs : list<IOutputMod>) =
        inherit AbstractOutputMod<'b>()

        let mutable handle : Option<'a> = None

        abstract member View : 'a -> 'b
        default x.View a = unbox a
        
        abstract member TryUpdate : AdaptiveToken * 'a -> bool
        default x.TryUpdate(_,_) = false

        abstract member Create : AdaptiveToken -> 'a
        abstract member Destroy : 'a -> unit

        override x.Create() =
            for i in inputs do i.Acquire()

        override x.Destroy() =
            for i in inputs do i.Release()
            match handle with
                | Some h -> 
                    x.Destroy h
                    handle <- None
                | _ ->
                    ()

        override x.Compute(t, rt) =
            let handle = 
                match handle with
                    | Some h ->
                        if not (x.TryUpdate(t, h)) then
                            x.Destroy(h)
                            let h = x.Create(t)
                            handle <- Some h
                            h
                        else
                            h
                    | None ->
                        let h = x.Create t
                        handle <- Some h
                        h
            x.View handle
                    
    module OutputMod =
        let custom (dependent : list<IOutputMod>) (create : AdaptiveToken -> 'a) (tryUpdate : AdaptiveToken -> 'a -> bool) (destroy : 'a -> unit) (view : 'a -> 'b) =
            { new OutputMod<'a, 'b>(dependent) with
                override x.Create t = create t
                override x.TryUpdate(t,h) = tryUpdate t h
                override x.Destroy h = destroy h
                override x.View h = view h
            } :> IOutputMod<_> 
            
        let simple (create : AdaptiveToken -> 'a) (destroy : 'a -> unit) =
            { new OutputMod<'a, 'a>([]) with
                override x.Create t = create t
                override x.Destroy h = destroy h
            } :> IOutputMod<_>

    module private Shader =
        open FShade

        let stereoTexture =
            sampler2dArray {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let renderStereo (v : Effects.Vertex) =
            fragment {
                let index =
                    if v.tc.X > 0.5 then 1
                    else 0

                let tc =
                    if v.tc.X > 0.5 then V2d((v.tc.X - 0.5) * 2.0, v.tc.Y)
                    else V2d(v.tc.X * 2.0, v.tc.Y)

                return stereoTexture.SampleLevel(tc, index, uniform?Dependent)
            }


    type ISimpleRenderWindow =
        inherit IDisposable

        abstract member Runtime : IRuntime
        abstract member Sizes : IMod<V2i>
        abstract member Samples : int
        abstract member Time : IMod<DateTime>
        abstract member Keyboard : IKeyboard
        abstract member Mouse : IMouse

        abstract member View : IMod<Trafo3d[]>
        abstract member Proj : IMod<Trafo3d[]>

        abstract member Scene : ISg with get, set
        abstract member Run : unit -> unit

    [<AbstractClass>]
    type private SimpleRenderWindow(win : IRenderWindow, view : IMod<Trafo3d[]>, proj : IMod<Trafo3d[]>) =
        let mutable scene = Sg.empty


        abstract member Compile : IRenderWindow * ISg -> IRenderTask
        abstract member Release : unit -> unit
        default x.Release() = ()

        member x.Dispose() =
            x.Release()
            win.TryDispose() |> ignore

        member x.Runtime = win.Runtime
        member x.Sizes = win.Sizes
        member x.Samples = win.Samples
        member x.Time = win.Time
        member x.Keyboard = win.Keyboard
        member x.Mouse = win.Mouse

        member x.Run() = 
            win.Run()
            x.Dispose()

        member x.Scene
            with get() = scene
            and set sg = 
                scene <- sg
                let task = x.Compile(win, sg)
                win.RenderTask <- task
        
        interface ISimpleRenderWindow with
            member x.Runtime = x.Runtime
            member x.Sizes = x.Sizes
            member x.Samples = x.Samples
            member x.Time = x.Time
            member x.Keyboard = x.Keyboard
            member x.Mouse = x.Mouse
            member x.Run() = x.Run()
            
            member x.View = view
            member x.Proj = proj

            member x.Scene
                with get() = x.Scene
                and set sg = x.Scene <- sg

        interface IDisposable with
            member x.Dispose() = x.Dispose()

    let private hookSg (win : IRenderControl) (sg : ISg) =
        let fillMode = Mod.init FillMode.Fill
        let cullMode = Mod.init CullMode.None

        let status = Mod.init ""

        let alt = win.Keyboard.IsDown(Aardvark.Application.Keys.LeftAlt)
        win.Keyboard.KeyDown(Aardvark.Application.Keys.X).Values.Add (fun () ->
            if Mod.force alt then
                transact (fun () ->
                    let newFillMode = 
                        match fillMode.Value with
                            | FillMode.Fill -> FillMode.Line
                            | _ -> FillMode.Fill

                    status.Value <- sprintf "fill: %A" newFillMode
                    fillMode.Value <- newFillMode
                )
            ()
        )

        win.Keyboard.KeyDown(Aardvark.Application.Keys.Y).Values.Add (fun () ->
            if Mod.force alt then
                transact (fun () ->
                    let newCull = 
                        match cullMode.Value with
                            | CullMode.None -> CullMode.Clockwise
                            | CullMode.Clockwise -> CullMode.CounterClockwise
                            | _ -> CullMode.None
                    status.Value <- sprintf "cull: %A" newCull
                    cullMode.Value <- newCull
                )
            ()
        )




        sg |> Sg.fillMode fillMode |> Sg.cullMode cullMode

    let private createMonoScreen (cfg : RenderConfig) =
        let app =
            match cfg.backend with
                | Backend.GL -> new OpenGlApplication(cfg.debug) :> IApplication
                | Backend.Vulkan -> new VulkanApplication(cfg.debug) :> IApplication
                | Backend.Both -> new MultiApplication([|new OpenGlApplication(cfg.debug) :> IApplication; new VulkanApplication(cfg.debug) :> IApplication|]) :> IApplication

        let win = 
            if cfg.game && cfg.backend = Backend.GL then (unbox<OpenGlApplication> app).CreateGameWindow(cfg.samples) :> IRenderWindow
            else app.CreateSimpleRenderWindow(cfg.samples) :> IRenderWindow

        let view =
            CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
                |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
                |> Mod.map CameraView.viewTrafo

        let proj =
            win.Sizes 
                |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 1000.0 (float s.X / float s.Y))
                |> Mod.map Frustum.projTrafo

        { new SimpleRenderWindow(win, view |> Mod.map Array.singleton, proj |> Mod.map Array.singleton) with
            override x.Compile(win, sg) =
                sg
                |> hookSg win
                |> Sg.viewTrafo view
                |> Sg.projTrafo proj
                |> Sg.compile win.Runtime win.FramebufferSignature

            override x.Release() =
                app.Dispose()

        } :> ISimpleRenderWindow

//    type VkImageCreateFlags with
//        static member VK_IMAGE_CREATE_BIND_SFR_BIT_KHX = unbox<VkImageCreateFlags> 64
//        static member VK_IMAGE_CREATE_ALIAS_BIT_KHR = unbox<VkImageCreateFlags> 0x00000400
//
//    type VkHate<'a>() =
////        static let vkBindImageMemory2KHXDel = 
////            let ptr = VkRaw.vkGetInstanceProcAddr(VkRaw.activeInstance, "vkBindImageMemory2KHX")
////            Marshal.GetDelegateForFunctionPointer(ptr, typeof<VkBindImageMemory2KHXDel>) |> unbox<VkBindImageMemory2KHXDel>
//        static member vkBindImageMemory2KHX(a,b,c) = 
//            let ptr = VkRaw.vkGetDeviceProcAddr(a, "vkBindImageMemory2KHX")
//            let del = Marshal.GetDelegateForFunctionPointer(ptr, typeof<VkBindImageMemory2KHXDel>) |> unbox<VkBindImageMemory2KHXDel>
//            
//            del.Invoke(a,b,c)
//            

    let createMultiDeviceTexture2 (size : V2i) (format : TextureFormat) (device : Device) =
        


//        let mem = device.DeviceMemory
//        let mutable features = VkPeerMemoryFeatureFlagsKHX()
//        VkRaw.vkGetDeviceGroupPeerMemoryFeaturesKHX(device.Handle, uint32 mem.HeapIndex, 0u, 1u, &&features)
//
//        printfn "%A" features

        let mutable imageInfo =
            VkImageCreateInfo(
                VkStructureType.ImageCreateInfo, 0n,
                VkImageCreateFlags.AliasBitKhr,
                VkImageType.D2d,
                VkFormat.ofTextureFormat format,
                VkExtent3D(size.X, size.Y, 1),
                1u, 1u, VkSampleCountFlags.D1Bit,
                VkImageTiling.Optimal,
                VkImageUsageFlags.TransferSrcBit ||| VkImageUsageFlags.TransferDstBit ||| VkImageUsageFlags.SampledBit ,
                device.AllSharingMode,
                device.AllQueueFamiliesCnt, 
                device.AllQueueFamiliesPtr,
                VkImageLayout.Undefined
            )


        let mutable localImg = VkImage.Null
        VkRaw.vkCreateImage(device.Handle, &&imageInfo, NativePtr.zero, &&localImg)
            |> ignore

        let mutable reqs = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(device.Handle, localImg, &&reqs)

        //let mem = device.DeviceMemory.AllocRaw(reqs.size)

        let mutable allocInfo =
            VkMemoryAllocateFlagsInfoKHX(
                VkStructureType.MemoryAllocateFlagsInfoKhx, 0n,
                unbox (int VkMemoryAllocateFlagBitsKHX.VkMemoryAllocateDeviceMaskBitKhx),
                3u
            )

        let mutable info = 
            VkMemoryAllocateInfo(
                VkStructureType.MemoryAllocateInfo, NativePtr.toNativeInt &&allocInfo,
                reqs.size,
                uint32 device.DeviceMemory.Index
            )

        let mutable memhandle = VkDeviceMemory.Null
        VkRaw.vkAllocateMemory(device.Handle, &&info, NativePtr.zero, &&memhandle) |> ignore

        let mem = new DeviceMemory(device.DeviceMemory, memhandle, int64 reqs.size, 0n)

        
        do 
            let deviceIndices = [| 0u; 1u |]
            deviceIndices |> NativePtr.withA (fun pDeviceIndices ->
//                let mutable bindBla =
//                    VkBindImageMemoryDeviceGroupInfoKHX(
//                        VkStructureType.BindImageMemoryDeviceGroupInfoKhx, 0n,
//                        2u, pDeviceIndices,
//                        0u, NativePtr.zero
//                    )
//
//                let mutable bindInfo =
//                    VkBindImageMemoryInfoKHR(
//                        VkStructureType.BindImageMemoryInfoKhr, NativePtr.toNativeInt &&bindBla,
//                        localImg, 
//                        mem.Handle,
//                        0UL
//                    )
//                VkRaw.vkBindImageMemory2KHR(device.Handle, 1u, &&bindInfo) |> printfn "BIND: %A"

                let mutable bindInfo =
                    VkBindImageMemoryInfoKHX(
                        VkStructureType.BindImageMemoryInfoKhx, 0n,
                        localImg,
                        mem.Handle, 0UL,
                        2u, pDeviceIndices,
                        0u, NativePtr.zero
                    )

                VkRaw.vkBindImageMemory2KHX(device.Handle, 1u, &&bindInfo) |> printfn "BIND: %A"

            )


        let mutable peerImg = VkImage.Null
        VkRaw.vkCreateImage(device.Handle, &&imageInfo, NativePtr.zero, &&peerImg)
            |> ignore

        let mutable reqs = VkMemoryRequirements()
        VkRaw.vkGetImageMemoryRequirements(device.Handle, peerImg, &&reqs)

        do
            let deviceIndices = [| 1u; 0u |]
            deviceIndices |> NativePtr.withA (fun pDeviceIndices ->
//                let mutable bindBla =
//                    VkBindImageMemoryDeviceGroupInfoKHX(
//                        VkStructureType.BindImageMemoryDeviceGroupInfoKhx, 0n,
//                        2u, pDeviceIndices,
//                        0u, NativePtr.zero
//                    )
//
//                let mutable bindInfo =
//                    VkBindImageMemoryInfoKHR(
//                        VkStructureType.BindImageMemoryInfoKhr, NativePtr.toNativeInt &&bindBla,
//                        peerImg, 
//                        mem.Handle,
//                        0UL
//                    )
//
//                VkRaw.vkBindImageMemory2KHR(device.Handle, 1u, &&bindInfo) |> printfn "BIND: %A"
                let mutable bindInfo =
                    VkBindImageMemoryInfoKHX(
                        VkStructureType.BindImageMemoryInfoKhx, 0n,
                        peerImg,
                        mem.Handle, 0UL,
                        2u, pDeviceIndices,
                        0u, NativePtr.zero
                    )

                VkRaw.vkBindImageMemory2KHX(device.Handle, 1u, &&bindInfo) |> printfn "BIND: %A"
            )

        let lImg = new Aardvark.Rendering.Vulkan.Image(device, localImg, V3i(size, 1), 1, 1, 1, TextureDimension.Texture2D, VkFormat.ofTextureFormat format, mem, VkImageLayout.Undefined)
        let pImg = new Aardvark.Rendering.Vulkan.Image(device, peerImg, V3i(size, 1), 1, 1, 1, TextureDimension.Texture2D, VkFormat.ofTextureFormat format, mem, VkImageLayout.Undefined)

//        device.perform {
//            do! Command.TransformLayout(lImg, VkImageLayout.TransferDstOptimal)
//            do! Command.TransformLayout(pImg, VkImageLayout.TransferDstOptimal)
//        }

        lImg, pImg





    let private createStereoScreen (cfg : RenderConfig) =
        let app =
            match cfg.backend with
                | Backend.GL -> new OpenGlApplication(cfg.debug) :> IApplication
                | Backend.Vulkan -> new VulkanApplication(cfg.debug,true) :> IApplication
                | Backend.Both -> failwith "not implemented"

        let win = app.CreateSimpleRenderWindow(1)
        let runtime = app.Runtime

        let samples = 1 //cfg.samples
        let signature =
            runtime.CreateFramebufferSignature(
                SymDict.ofList [
                    DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = samples }
                    DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = samples }
                ],
                Set.empty,
                2, 
                Set.ofList [
                    "ProjTrafo"; 
                    "ViewTrafo"; 
                    "ModelViewTrafo"; 
                    "ViewProjTrafo"; 
                    "ModelViewProjTrafo"
                    
                    "ProjTrafoInv"; 
                    "ViewTrafoInv"; 
                    "ModelViewTrafoInv"; 
                    "ViewProjTrafoInv"; 
                    "ModelViewProjTrafoInv"
                ]
            )  

        let s = Mod.constant (V2i(1024, 1024)) //win.Sizes |> Mod.map (fun s -> s / V2i(2,1))

//        let device : Device = failwith ""
//        let img : VkImage = failwith ""
//        let mem : VkDeviceMemory = failwith ""
//        let offset = 0UL
//
//        let r = VkRect2D()
//
//
//        let mutable allocInfo =
//            VkMemoryAllocateFlagsInfoKHX(
//                VkStructureType.MemoryAllocateFlagsInfoKhx, 0n,
//                unbox VkMemoryAllocateFlagBitsKHX.VkMemoryAllocateDeviceMaskBitKhx,
//                3u
//            )
//
//        let mutable info = 
//            VkMemoryAllocateInfo(
//                VkStructureType.MemoryAllocateInfo, NativePtr.toNativeInt &&allocInfo,
//                1UL,
//                0u
//            )
//
//        let groupInfo =
//            VkBindImageMemoryDeviceGroupInfoKHX(
//                VkStructureType.BindImageMemoryDeviceGroupInfoKhx, 0n,
//                2u, NativePtr.zero,
//                4u, NativePtr.zero
//            )
//
//        let mutable info =
//            VkBindImageMemoryInfoKHR(
//                VkStructureType.BindImageMemoryInfoKhr, 0n,
//                img,
//                mem,
//                offset
//            )
//
//        VkRaw.vkBindImageMemory2KHR(device.Handle, 1u, &&info)
//            |> ignore


        
        let device = 
            let runtime = unbox<Runtime> runtime
            runtime.Device

        let colors =
            OutputMod.custom 
                []
                (fun t -> runtime.CreateTextureArray(s.GetValue t, TextureFormat.Rgba8, 1, samples, 2))
                (fun t h -> h.Size.XY = s.GetValue t)
                (fun h -> runtime.DeleteTexture h)
                id
                
        let depth =
            OutputMod.custom 
                []
                (fun t -> runtime.CreateTextureArray(s.GetValue t, TextureFormat.Depth24Stencil8, 1, samples, 2))
                (fun t h -> h.Size.XY = s.GetValue t)
                (fun h -> runtime.DeleteTexture h)
                id

        let resolved =
            OutputMod.custom 
                []
                //(fun t -> runtime.CreateTexture(s.GetValue t * V2i(2,1), TextureFormat.Rgba8, 1, 1))
                (fun t -> createMultiDeviceTexture2 (s.GetValue t * V2i(2,1)) TextureFormat.Rgba8 device)
                (fun t (h,_) -> h.Size.XY = s.GetValue t * V2i(2,1))
                (fun (l,p) -> runtime.DeleteTexture l; runtime.DeleteTexture p)
                id
                
        let framebuffer =
            OutputMod.custom
                [colors; depth]
                (fun t -> runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, colors.GetValue(t).[TextureAspect.Color, 0] :> IFramebufferOutput; DefaultSemantic.Depth, depth.GetValue(t).[TextureAspect.Depth, 0] :> IFramebufferOutput]))
                (fun t h -> false)
                (fun h -> runtime.DeleteFramebuffer h)
                id


        let view = 
            CameraView.lookAt (V3d(6.0, 6.0, 6.0)) V3d.Zero V3d.OOI
                |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
                |> Mod.map CameraView.viewTrafo

        let near = 0.1
        let far = 1000.0

        let views =
            view |> Mod.map (fun view ->
                [| 
                    view * Trafo3d.Translation(0.05, 0.0, 0.0)
                    view * Trafo3d.Translation(-0.05, 0.0, 0.0)
                |]
            )

        let projs =
            // taken from oculus rift
            let outer = 1.0537801252809621805875367233154
            let inner = 0.77567951104961310377955052031392

            win.Sizes |> Mod.map (fun size ->
                let aspect = float size.X / float size.Y 
                let y = tan (120.0 * Constant.RadiansPerDegree / 2.0) / aspect //(outer + inner) / (2.0 * aspect)

                [|
                    { left = -outer * near; right = inner * near; top = y * near; bottom = -y * near; near = near; far = far } |> Frustum.projTrafo 
                    { left = -inner * near; right = outer * near; top = y * near; bottom = -y * near; near = near; far = far } |> Frustum.projTrafo 
                |]
            )

        let compile (sg : ISg) =

            framebuffer.Acquire()
            resolved.Acquire()

            let stereoTask =
                sg
                |> hookSg win
                |> Sg.uniform "ViewTrafo" views
                |> Sg.uniform "ProjTrafo" projs
                |> Sg.viewTrafo view
                |> Sg.uniform "CameraLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.uniform "LightLocation" (view |> Mod.map (fun t -> t.Backward.C3.XYZ))
                |> Sg.compile runtime signature

            let clearTask =
                runtime.CompileClear(signature, ~~C4f.Black, ~~1.0)



            let copy (src : Image) (dst : Image) =
                { new Command() with
                    member x.Compatible = QueueFlags.All
                    member x.Enqueue(cmd) =
                        cmd.AppendCommand()

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 1u)
                        let mutable resolve =
                            VkImageCopy(
                                src.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                dst.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                VkExtent3D(src.Size.X, src.Size.Y, src.Size.Z)
                            )

                        VkRaw.vkCmdCopyImage(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, VkImageLayout.TransferDstOptimal, 1u, &&resolve)

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 2u)
                        let mutable resolve =
                            VkImageCopy(

                                src.[ImageAspect.Color, 0, 1].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                dst.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(src.Size.X, 0, 0),
                                VkExtent3D(src.Size.X, src.Size.Y, src.Size.Z)
                            )

                        VkRaw.vkCmdCopyImage(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, VkImageLayout.TransferDstOptimal, 1u, &&resolve)
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)

                        VkRaw.vkCmdPipelineBarrier(
                            cmd.Handle,
                            VkPipelineStageFlags.TransferBit,
                            VkPipelineStageFlags.TransferBit,
                            unbox 0x00000004,
                            0u,NativePtr.zero,
                            0u,NativePtr.zero,
                            0u,NativePtr.zero
                        )

                        Disposable.Empty
                }


            let syncPeer (src : Image) (dst : Image) =
                { new Command() with
                    member x.Compatible = QueueFlags.All
                    member x.Enqueue(cmd) =
                        cmd.AppendCommand()

                        let srclayout = src.Layout
                        let dstlayout = dst.Layout

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 1u)

                        cmd.enqueue {
                            do! Command.TransformLayout(src, VkImageLayout.TransferSrcOptimal)
                            do! Command.TransformLayout(dst, VkImageLayout.TransferDstOptimal)
                        }

                        let mutable resolve =
                            VkImageCopy(
                                src.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                dst.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                VkExtent3D(src.Size.X / 2, src.Size.Y, src.Size.Z)
                            )

                        VkRaw.vkCmdCopyImage(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, VkImageLayout.TransferDstOptimal, 1u, &&resolve)

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 2u)

                        cmd.enqueue {
                            do! Command.TransformLayout(src, VkImageLayout.TransferSrcOptimal)
                            do! Command.TransformLayout(dst, VkImageLayout.TransferDstOptimal)
                        }

                        let mutable resolve =
                            VkImageCopy(
                                src.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(src.Size.X / 2, 0, 0),
                                dst.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(src.Size.X / 2, 0, 0),
                                VkExtent3D(src.Size.X / 2, src.Size.Y, src.Size.Z)
                            )

                        VkRaw.vkCmdCopyImage(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, VkImageLayout.TransferDstOptimal, 1u, &&resolve)
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)

                        VkRaw.vkCmdPipelineBarrier(
                            cmd.Handle,
                            VkPipelineStageFlags.TransferBit,
                            VkPipelineStageFlags.TransferBit,
                            unbox 0x00000004,
                            0u,NativePtr.zero,
                            0u,NativePtr.zero,
                            0u,NativePtr.zero
                        )

                        
                        cmd.enqueue {
                            do! Command.TransformLayout(src, srclayout)
                            do! Command.TransformLayout(dst, dstlayout)
                        }


                        Disposable.Empty
                }
            let clear (img : Image) =
                { new Command() with
                    member x.Compatible = QueueFlags.All
                    member x.Enqueue(cmd) =
                        cmd.AppendCommand()

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 1u)
                        let mutable color = VkClearColorValue(float32 = V4f(1.0f, 0.0f, 0.0f, 1.0f))
                        let mutable range = img.[ImageAspect.Color].VkImageSubresourceRange
                        VkRaw.vkCmdClearColorImage(cmd.Handle, img.Handle, VkImageLayout.TransferDstOptimal, &&color, 1u, &&range)

                        
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 2u)
                        let mutable color = VkClearColorValue(float32 = V4f(0.0f, 1.0f, 0.0f, 1.0f))
                        let mutable range = img.[ImageAspect.Color].VkImageSubresourceRange
                        VkRaw.vkCmdClearColorImage(cmd.Handle, img.Handle, VkImageLayout.TransferDstOptimal, &&color, 1u, &&range)
                        
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)
                        
                        Disposable.Empty
                }   

            
            let clearTarget (img : Image) =
                { new Command() with
                    member x.Compatible = QueueFlags.All
                    member x.Enqueue(cmd) =
                        cmd.AppendCommand()

                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 1u)
                        let mutable color = VkClearColorValue(float32 = V4f(0.0f, 1.0f, 1.0f, 1.0f))
                        let mutable range = img.[ImageAspect.Color].VkImageSubresourceRange
                        VkRaw.vkCmdClearColorImage(cmd.Handle, img.Handle, VkImageLayout.TransferDstOptimal, &&color, 1u, &&range)

                        
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 2u)
                        let mutable color = VkClearColorValue(float32 = V4f(1.0f, 1.0f, 0.0f, 1.0f))
                        let mutable range = img.[ImageAspect.Color].VkImageSubresourceRange
                        VkRaw.vkCmdClearColorImage(cmd.Handle, img.Handle, VkImageLayout.TransferDstOptimal, &&color, 1u, &&range)
                        
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)
                        
                        Disposable.Empty
                }   

            let copyToBuffer (src : Image) (deviceIndex : int) (dst : Buffer) =
                { new Command() with
                    member x.Compatible = QueueFlags.All
                    member x.Enqueue(cmd) =
                        cmd.AppendCommand()

//                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)
//                        let mutable resolve =
//                            VkBufferImageCopy(
//                                0UL,
//                                0u, 0u, 
//                                src.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
//                                VkOffset3D(0, 0, 0),
//                                VkExtent3D(src.Size.X, src.Size.Y, src.Size.Z)
//                            )
//
//                        VkRaw.vkCmdCopyImageToBuffer(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, 1u, &&resolve)
//
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 1u <<< deviceIndex)
                        let mutable resolve =
                            VkBufferImageCopy(
                                0UL,
                                0u, 0u, 
                                src.[ImageAspect.Color, 0, 0].VkImageSubresourceLayers,
                                VkOffset3D(0, 0, 0),
                                VkExtent3D(src.Size.X, src.Size.Y, src.Size.Z)
                            )

                        VkRaw.vkCmdCopyImageToBuffer(cmd.Handle, src.Handle, VkImageLayout.TransferSrcOptimal, dst.Handle, 1u, &&resolve)
                        VkRaw.vkCmdSetDeviceMaskKHX(cmd.Handle, 3u)
                        Disposable.Empty
                }



            let dependent =
                Mod.custom (fun t ->
                    let (local, peer) = resolved.GetValue(t)
                    let src = colors.GetValue t
                    let fbo = framebuffer.GetValue t
                    let out = OutputDescription.ofFramebuffer fbo




                    clearTask.Run(t, RenderToken.Empty, out)
                    stereoTask.Run(t, RenderToken.Empty, out)

                    let size = local.Size.XY / V2i(2,1)

//                    let b0 = device.HostMemory |> Buffer.create VkBufferUsageFlags.TransferDstBit (4L * 2L * int64 size.X * int64 size.Y)
//                    let b1 = device.HostMemory |> Buffer.create VkBufferUsageFlags.TransferDstBit (4L * 2L * int64 size.X * int64 size.Y)

                    let beginCmd(cmd : CommandBuffer) =
                        let mutable groupInfo =
                            VkDeviceGroupCommandBufferBeginInfoKHX(
                                VkStructureType.DeviceGroupCommandBufferBeginInfoKhx, 0n,
                                3u
                            )

                        let mutable info =
                            VkCommandBufferBeginInfo(
                                VkStructureType.CommandBufferBeginInfo, NativePtr.toNativeInt &&groupInfo,
                                VkCommandBufferUsageFlags.OneTimeSubmitBit,
                                NativePtr.zero
                            )

                        VkRaw.vkBeginCommandBuffer(
                            cmd.Handle,
                            &&info
                        ) |> ignore

                        cmd.Recording <- true

                    use cmd = device.GraphicsFamily.DefaultCommandPool.CreateCommandBuffer(CommandBufferLevel.Primary)
                    
                    //beginCmd cmd
                    cmd.Begin(CommandBufferUsage.OneTimeSubmit)
                    cmd.enqueue {
                        local.Layout <- VkImageLayout.Undefined
                        let rl = local.Layout
                        
                        do! Command.TransformLayout(unbox src, VkImageLayout.TransferSrcOptimal)
                        do! Command.TransformLayout(local, VkImageLayout.TransferDstOptimal)
                        do! copy (unbox src) local
                        //do! clear local

//
//                        do! Command.TransformLayout(local, VkImageLayout.ShaderReadOnlyOptimal)
//                        do! Command.TransformLayout(peer, VkImageLayout.ShaderReadOnlyOptimal)
                        do! syncPeer local peer
                        
//                        do! Command.TransformLayout(peer, VkImageLayout.TransferSrcOptimal)
//                        do! Command.TransformLayout(local, VkImageLayout.TransferSrcOptimal)
//                        do! copyToBuffer local 0 b0
//                        do! copyToBuffer local 1 b1

                        do! Command.TransformLayout(local, VkImageLayout.ShaderReadOnlyOptimal)
                    }
                    cmd.End()

                    device.GraphicsFamily.RunSynchronously(cmd)
//
//                    let r = local
//                    let img = PixImage<byte>(Col.Format.RGBA, r.Size.XY)
//                    b0.Memory.MappedTensor4 (V4i(r.Size.X, r.Size.Y, 1, 4), fun (src : NativeTensor4<byte>) ->
//                        NativeVolume.using img.Volume (fun pDst ->
//                            NativeVolume.copy src.[*,*,0,*] pDst
//                        )
//                    )
//                    img.SaveAsImage @"C:\Users\steinlechner\Desktop\0.jpg"
//
//                    let img = PixImage<byte>(Col.Format.RGBA, r.Size.XY)
//                    b1.Memory.MappedTensor4 (V4i(r.Size.X, r.Size.Y, 1, 4), fun (src : NativeTensor4<byte>) ->
//                        NativeVolume.using img.Volume (fun pDst ->
//                            NativeVolume.copy src.[*,*,0,*] pDst
//                        )
//                    )
//                    img.SaveAsImage @"C:\Users\steinlechner\Desktop\1.jpg"
//
//                    device.Delete b0
//                    device.Delete b1

                    //System.Console.ReadLine() |> ignore

                    local :> ITexture
                )

            let task =
                Sg.fullScreenQuad
                    |> Sg.uniform "Dependent" (Mod.constant 0.0)
                    |> Sg.diffuseTexture dependent
                    |> Sg.shader {
                        do! DefaultSurfaces.diffuseTexture
                    }
                    |> Sg.compile runtime win.FramebufferSignature

            let dummy =
                { new AbstractRenderTask() with
                    member x.FramebufferSignature = Some win.FramebufferSignature
                    member x.Runtime = Some win.Runtime
                    member x.PerformUpdate (_,_) = ()
                    member x.Perform (_,_,_) = ()
                    member x.Release() =
                        stereoTask.Dispose()
                        clearTask.Dispose()
                        //framebuffer.Release()
                        resolved.Release()
                    member x.Use f = f()
                }

            RenderTask.ofList [dummy; task]


        let res =
            { new SimpleRenderWindow(win, views, projs) with
                override x.Compile(win, sg) =
                    compile sg

                override x.Release() =
                    win.Dispose()
                    runtime.DeleteFramebufferSignature signature
        
                    app.Dispose()
        

            } :> ISimpleRenderWindow

        res

    let private createOpenVR (cfg : RenderConfig) =
        match cfg.backend with
            | Backend.Vulkan ->
                let app = VulkanVRApplicationLayered(cfg.samples, cfg.debug)

                let hmdLocation = app.Hmd.MotionState.Pose |> Mod.map (fun t -> t.Forward.C3.XYZ)


                { new SimpleRenderWindow(app, app.Info.viewTrafos, app.Info.projTrafos) with
                    override x.Compile(win, sg) =
                        sg
                        |> Sg.uniform "ViewTrafo" app.Info.viewTrafos
                        |> Sg.uniform "ProjTrafo" app.Info.projTrafos
                        |> Sg.uniform "CameraLocation" hmdLocation
                        |> Sg.uniform "LightLocation" hmdLocation
                        |> Sg.compile app.Runtime app.FramebufferSignature
                } :> ISimpleRenderWindow

            | Backend.GL -> 
                failwith "no OpenGL OpenVR backend atm."

            | Backend.Both -> 
                failwith "no OpenGL OpenVR backend atm."


    let createWindow (cfg : RenderConfig) =
        match cfg.display with
            | Display.Mono -> createMonoScreen cfg
            | Display.Stereo -> createStereoScreen cfg
            | Display.OpenVR -> createOpenVR cfg

    let runConfig (cfg : RenderConfig) =
        let win = createWindow cfg
        win.Scene <- cfg.scene
        win.Run()

    let run (display : Display) (backend : Backend) (scene : ISg) =
        runConfig {
            scene = scene
            game = false
            display = display
            backend = backend
            debug = true
            samples = 8
        }

[<AutoOpen>]
module ``Render Utilities`` =
    type ShowBuilder() =
        member x.Yield(()) =
            {
                backend = Backend.Vulkan
                debug = true
                game = false
                samples = 8
                display = Display.Mono
                scene = Sg.empty
            }

        [<CustomOperation("backend")>]
        member x.Backend(s : RenderConfig, b : Backend) =
            { s with backend = b }

        [<CustomOperation("debug")>]
        member x.Debug(s : RenderConfig, d : bool) =
            { s with debug = d }

        [<CustomOperation("samples")>]
        member x.Samples(state : RenderConfig, s : int) =
            { state with samples = s }

        [<CustomOperation("display")>]
        member x.Display(s : RenderConfig, d : Display) =
            { s with display = d }

        [<CustomOperation("scene")>]
        member x.Scene(state : RenderConfig, s : ISg) =
            { state with scene = s }


        [<CustomOperation("game")>]
        member x.Game(state : RenderConfig, game : bool) =
            { state with game = game }

        member x.Run(cfg : RenderConfig) =
            Utilities.runConfig cfg

    type WindowBuilder() =
        member x.Yield(()) =
            {
                backend = Backend.Vulkan
                debug = true
                game = false
                samples = 8
                display = Display.Mono
                scene = Sg.empty
            }

        [<CustomOperation("backend")>]
        member x.Backend(s : RenderConfig, b : Backend) =
            { s with backend = b }

        [<CustomOperation("debug")>]
        member x.Debug(s : RenderConfig, d : bool) =
            { s with debug = d }

        [<CustomOperation("samples")>]
        member x.Samples(state : RenderConfig, s : int) =
            { state with samples = s }

        [<CustomOperation("display")>]
        member x.Display(s : RenderConfig, d : Display) =
            { s with display = d }

        [<CustomOperation("game")>]
        member x.Game(state : RenderConfig, game : bool) =
            { state with game = game }

        member x.Run(cfg : RenderConfig) =
            Utilities.createWindow cfg
            
    let window = WindowBuilder()
    let show = ShowBuilder()