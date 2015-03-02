﻿namespace Aardvark.Rendering.GL

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

type RenderbufferFormat =
    | DepthComponent = 6402
    | R3G3B2 = 10768
    | Rgb4 = 32847
    | Rgb5 = 32848
    | Rgb8 = 32849
    | Rgb10 = 32850
    | Rgb12 = 32851
    | Rgb16 = 32852
    | Rgba2 = 32853
    | Rgba4 = 32854
    | Rgba8 = 32856
    | Rgb10A2 = 32857
    | Rgba12 = 32858
    | Rgba16 = 32859
    | DepthComponent16 = 33189
    | DepthComponent24 = 33190
    | DepthComponent32 = 33191
    | R8 = 33321
    | R16 = 33322
    | Rg8 = 33323
    | Rg16 = 33324
    | R16f = 33325
    | R32f = 33326
    | Rg16f = 33327
    | Rg32f = 33328
    | R8i = 33329
    | R8ui = 33330
    | R16i = 33331
    | R16ui = 33332
    | R32i = 33333
    | R32ui = 33334
    | Rg8i = 33335
    | Rg8ui = 33336
    | Rg16i = 33337
    | Rg16ui = 33338
    | Rg32i = 33339
    | Rg32ui = 33340
    | DepthStencil = 34041
    | Rgba32f = 34836
    | Rgb32f = 34837
    | Rgba16f = 34842
    | Rgb16f = 34843
    | Depth24Stencil8 = 35056
    | R11fG11fB10f = 35898
    | Rgb9E5 = 35901
    | Srgb8 = 35905
    | Srgb8Alpha8 = 35907
    | DepthComponent32f = 36012
    | Depth32fStencil8 = 36013
    | StencilIndex1Ext = 36166
    | StencilIndex1 = 36166
    | StencilIndex4Ext = 36167
    | StencilIndex4 = 36167
    | StencilIndex8 = 36168
    | StencilIndex8Ext = 36168
    | StencilIndex16Ext = 36169
    | StencilIndex16 = 36169
    | Rgba32ui = 36208
    | Rgb32ui = 36209
    | Rgba16ui = 36214
    | Rgb16ui = 36215
    | Rgba8ui = 36220
    | Rgb8ui = 36221
    | Rgba32i = 36226
    | Rgb32i = 36227
    | Rgba16i = 36232
    | Rgb16i = 36233
    | Rgba8i = 36238
    | Rgb8i = 36239
    | Rgb10A2ui = 36975

type Renderbuffer =
    class
        val mutable public Context : Context
        val mutable public Handle : int
        val mutable public Size : V2i
        val mutable public Format : RenderbufferFormat
        val mutable public Samples : int

        interface IResource with
            member x.Context = x.Context
            member x.Handle = x.Handle

        new (ctx : Context, handle : int, size : V2i, format : RenderbufferFormat, samples : int) =
            { Context = ctx; Handle = handle; Size = size; Format = format; Samples = samples }
    end


[<AutoOpen>]
module RenderbufferExtensions =
    open ChannelType

    let private lookup (name : string) (l : list<'a * 'b>) =
        let d = Dict.ofList l
        fun a ->
            match d.TryGetValue a with
                | (true, b) -> b
                | _ -> failwithf "unknown %s: %A" name a  

    let private storageFormat =
        lookup "RenderBufferFormat" [
            RenderbufferFormat.DepthComponent, RenderbufferStorage.DepthComponent
            RenderbufferFormat.R3G3B2, RenderbufferStorage.R3G3B2
            RenderbufferFormat.Rgb4, RenderbufferStorage.Rgb4
            RenderbufferFormat.Rgb5, RenderbufferStorage.Rgb5
            RenderbufferFormat.Rgb8, RenderbufferStorage.Rgb8
            RenderbufferFormat.Rgb10, RenderbufferStorage.Rgb10
            RenderbufferFormat.Rgb12, RenderbufferStorage.Rgb12
            RenderbufferFormat.Rgb16, RenderbufferStorage.Rgb16
            RenderbufferFormat.Rgba2, RenderbufferStorage.Rgba2
            RenderbufferFormat.Rgba4, RenderbufferStorage.Rgba4
            RenderbufferFormat.Rgba8, RenderbufferStorage.Rgba8
            RenderbufferFormat.Rgb10A2, RenderbufferStorage.Rgb10A2
            RenderbufferFormat.Rgba12, RenderbufferStorage.Rgba12
            RenderbufferFormat.Rgba16, RenderbufferStorage.Rgba16
            RenderbufferFormat.DepthComponent16, RenderbufferStorage.DepthComponent16
            RenderbufferFormat.DepthComponent24, RenderbufferStorage.DepthComponent24
            RenderbufferFormat.DepthComponent32, RenderbufferStorage.DepthComponent32
            RenderbufferFormat.R8, RenderbufferStorage.R8
            RenderbufferFormat.R16, RenderbufferStorage.R16
            RenderbufferFormat.Rg8, RenderbufferStorage.Rg8
            RenderbufferFormat.Rg16, RenderbufferStorage.Rg16
            RenderbufferFormat.R16f, RenderbufferStorage.R16f
            RenderbufferFormat.R32f, RenderbufferStorage.R32f
            RenderbufferFormat.Rg16f, RenderbufferStorage.Rg16f
            RenderbufferFormat.Rg32f, RenderbufferStorage.Rg32f
            RenderbufferFormat.R8i, RenderbufferStorage.R8i
            RenderbufferFormat.R8ui, RenderbufferStorage.R8ui
            RenderbufferFormat.R16i, RenderbufferStorage.R16i
            RenderbufferFormat.R16ui, RenderbufferStorage.R16ui
            RenderbufferFormat.R32i, RenderbufferStorage.R32i
            RenderbufferFormat.R32ui, RenderbufferStorage.R32ui
            RenderbufferFormat.Rg8i, RenderbufferStorage.Rg8i
            RenderbufferFormat.Rg8ui, RenderbufferStorage.Rg8ui
            RenderbufferFormat.Rg16i, RenderbufferStorage.Rg16i
            RenderbufferFormat.Rg16ui, RenderbufferStorage.Rg16ui
            RenderbufferFormat.Rg32i, RenderbufferStorage.Rg32i
            RenderbufferFormat.Rg32ui, RenderbufferStorage.Rg32ui
            RenderbufferFormat.DepthStencil, RenderbufferStorage.DepthStencil
            RenderbufferFormat.Rgba32f, RenderbufferStorage.Rgba32f
            RenderbufferFormat.Rgb32f, RenderbufferStorage.Rgb32f
            RenderbufferFormat.Rgba16f, RenderbufferStorage.Rgba16f
            RenderbufferFormat.Rgb16f, RenderbufferStorage.Rgb16f
            RenderbufferFormat.Depth24Stencil8, RenderbufferStorage.Depth24Stencil8
            RenderbufferFormat.R11fG11fB10f, RenderbufferStorage.R11fG11fB10f
            RenderbufferFormat.Rgb9E5, RenderbufferStorage.Rgb9E5
            RenderbufferFormat.Srgb8, RenderbufferStorage.Srgb8
            RenderbufferFormat.Srgb8Alpha8, RenderbufferStorage.Srgb8Alpha8
            RenderbufferFormat.DepthComponent32f, RenderbufferStorage.DepthComponent32f
            RenderbufferFormat.Depth32fStencil8, RenderbufferStorage.Depth32fStencil8
            RenderbufferFormat.StencilIndex1Ext, RenderbufferStorage.StencilIndex1Ext
            RenderbufferFormat.StencilIndex1, RenderbufferStorage.StencilIndex1
            RenderbufferFormat.StencilIndex4Ext, RenderbufferStorage.StencilIndex4Ext
            RenderbufferFormat.StencilIndex4, RenderbufferStorage.StencilIndex4
            RenderbufferFormat.StencilIndex8, RenderbufferStorage.StencilIndex8
            RenderbufferFormat.StencilIndex8Ext, RenderbufferStorage.StencilIndex8Ext
            RenderbufferFormat.StencilIndex16Ext, RenderbufferStorage.StencilIndex16Ext
            RenderbufferFormat.StencilIndex16, RenderbufferStorage.StencilIndex16
            RenderbufferFormat.Rgba32ui, RenderbufferStorage.Rgba32ui
            RenderbufferFormat.Rgb32ui, RenderbufferStorage.Rgb32ui
            RenderbufferFormat.Rgba16ui, RenderbufferStorage.Rgba16ui
            RenderbufferFormat.Rgb16ui, RenderbufferStorage.Rgb16ui
            RenderbufferFormat.Rgba8ui, RenderbufferStorage.Rgba8ui
            RenderbufferFormat.Rgb8ui, RenderbufferStorage.Rgb8ui
            RenderbufferFormat.Rgba32i, RenderbufferStorage.Rgba32i
            RenderbufferFormat.Rgb32i, RenderbufferStorage.Rgb32i
            RenderbufferFormat.Rgba16i, RenderbufferStorage.Rgba16i
            RenderbufferFormat.Rgb16i, RenderbufferStorage.Rgb16i
            RenderbufferFormat.Rgba8i, RenderbufferStorage.Rgba8i
            RenderbufferFormat.Rgb8i, RenderbufferStorage.Rgb8i
            RenderbufferFormat.Rgb10A2ui, RenderbufferStorage.Rgb10A2ui
        ]

    let toRenderbufferFormat (t : ChannelType) : RenderbufferFormat =
        match t with
        | R8 -> RenderbufferFormat.R8
        | R16 -> RenderbufferFormat.R16
        | R16_SNORM -> 0x8F98 |> unbox<RenderbufferFormat>
        | RG8 -> RenderbufferFormat.Rg8
        | RG16 -> RenderbufferFormat.Rg16
        | RG16_SNORM -> 0x8F99 |> unbox<RenderbufferFormat>
        | R3_G3_B2 -> RenderbufferFormat.R3G3B2
        | RGB4 -> RenderbufferFormat.Rgb4
        | RGB5 -> RenderbufferFormat.Rgb5
        | RGB8 -> RenderbufferFormat.Rgb8
        | RGB10 -> RenderbufferFormat.Rgb10
        | RGB12 -> RenderbufferFormat.Rgb12
        | RGB16_SNORM -> 0x8F9A |> unbox<RenderbufferFormat> //RenderbufferFormat.Rgb16Snorm
        | RGBA2 -> RenderbufferFormat.Rgba2
        | RGBA4 -> RenderbufferFormat.Rgba4
        | RGBA8 -> RenderbufferFormat.Rgba8
        | RGB10_A2 -> RenderbufferFormat.Rgb10A2
        | RGB10_A2UI -> RenderbufferFormat.Rgb10A2ui
        | RGBA12 -> RenderbufferFormat.Rgba12
        | RGBA16 -> RenderbufferFormat.Rgba16
        | SRGB8 -> RenderbufferFormat.Srgb8
        | SRGB8_ALPHA8 -> RenderbufferFormat.Srgb8Alpha8
        | R16F -> RenderbufferFormat.R16f
        | RG16F -> RenderbufferFormat.Rg16f
        | RGB16F -> RenderbufferFormat.Rgb16f
        | RGBA16F -> RenderbufferFormat.Rgba16f
        | R32F -> RenderbufferFormat.R32f
        | RG32F -> RenderbufferFormat.Rg32f
        | RGB32F -> RenderbufferFormat.Rgb32f
        | RGBA32F -> RenderbufferFormat.Rgba32f
        | R11F_G11F_B10F -> RenderbufferFormat.R11fG11fB10f
        | RGB9_E5 -> RenderbufferFormat.Rgb9E5
        | R8I -> RenderbufferFormat.R8i
        | R8UI -> RenderbufferFormat.R8ui
        | R16I -> RenderbufferFormat.R16i
        | R16UI -> RenderbufferFormat.R16ui
        | R32I -> RenderbufferFormat.R32i
        | R32UI -> RenderbufferFormat.R32ui
        | RG8I -> RenderbufferFormat.Rg8i
        | RG8UI -> RenderbufferFormat.Rg8ui
        | RG16I -> RenderbufferFormat.Rg16i
        | RG16UI -> RenderbufferFormat.Rg16ui
        | RG32I -> RenderbufferFormat.Rg32i
        | RG32UI -> RenderbufferFormat.Rg32ui
        | RGB8I -> RenderbufferFormat.Rgb8i
        | RGB8UI -> RenderbufferFormat.Rgb8ui
        | RGB16I -> RenderbufferFormat.Rgb16i
        | RGB16UI -> RenderbufferFormat.Rgb16ui
        | RGB32I -> RenderbufferFormat.Rgb32i
        | RGB32UI -> RenderbufferFormat.Rgb32ui
        | RGBA8I -> RenderbufferFormat.Rgba8i
        | RGBA8UI -> RenderbufferFormat.Rgba8ui
        | RGBA16I -> RenderbufferFormat.Rgba16i
        | RGBA16UI -> RenderbufferFormat.Rgba16ui
        | RGBA32I -> RenderbufferFormat.Rgba32i
        | RGBA32UI -> RenderbufferFormat.Rgba32ui
        | _ -> failwith "unknown internal format"



    let private updateRenderbuffer (handle : int) (size : V2i) (format : RenderbufferStorage) (samples : int) =
        GL.BindRenderbuffer(RenderbufferTarget.Renderbuffer, handle)
        GL.Check "could not bind renderbuffer"

        match samples with
            | 1 ->
                GL.RenderbufferStorage(RenderbufferTarget.Renderbuffer, format, size.X, size.Y)
            | sam ->
                GL.RenderbufferStorageMultisample(RenderbufferTarget.Renderbuffer, sam, format, size.X, size.Y)
        GL.Check "could not set renderbuffer storage"

        GL.BindRenderbuffer(RenderbufferTarget.Renderbuffer, 0)
        GL.Check "could not unbind renderbuffer"  

    type Context with
        member x.CreateRenderbuffer (size : V2i, format : RenderbufferFormat, ?samples : int) =
            let samples = defaultArg samples 1

            using x.ResourceLock (fun _ ->

                let handle = GL.GenRenderbuffer()
                GL.Check "could not create renderbuffer"

                updateRenderbuffer handle size (storageFormat format) samples
                
                Renderbuffer(x, handle, size, format, samples)
            )

        member x.CreateRenderbuffer (size : V2i, format : ChannelType, ?samples : int) =
            match samples with
                | Some s -> x.CreateRenderbuffer(size, toRenderbufferFormat format, s)
                | None -> x.CreateRenderbuffer(size, toRenderbufferFormat format)

        member x.Update(r : Renderbuffer, size : V2i, format : RenderbufferFormat, ?samples : int) =
            let samples = defaultArg samples 1

            if r.Size <> size || r.Format <> format || r.Samples <> samples then
                using x.ResourceLock (fun _ ->
                    GL.BindRenderbuffer(RenderbufferTarget.Renderbuffer, r.Handle)
                    GL.Check "could not bind renderbuffer"

                    match samples with
                        | 1 ->
                            GL.RenderbufferStorage(RenderbufferTarget.Renderbuffer, storageFormat format, size.X, size.Y)
                        | sam ->
                            GL.RenderbufferStorageMultisample(RenderbufferTarget.Renderbuffer, sam, storageFormat format, size.X, size.Y)
                    GL.Check "could not set renderbuffer storage"

                    GL.BindRenderbuffer(RenderbufferTarget.Renderbuffer, 0)
                    GL.Check "could not unbind renderbuffer"


                    r.Size <- size
                    r.Format <- format
                    r.Samples <- samples
                )

        member x.Delete(r : Renderbuffer) =
            using x.ResourceLock (fun _ ->
                GL.DeleteRenderbuffer(r.Handle)
                GL.Check "could not delete renderbuffer"
            )