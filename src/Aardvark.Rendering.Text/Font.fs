﻿namespace Aardvark.Rendering.Text

#nowarn "9"
#nowarn "51"

open System
open System.Linq
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.CompilerServices
open Aardvark.Base
open Aardvark.Base.Rendering
open System.Drawing
open System.Drawing.Drawing2D


[<Flags>]
type FontStyle =
    | Regular = 0
    | Bold = 1
    | Italic = 2

module private GDI32 =

    
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop
    open System.Collections.Generic


    [<AutoOpen>]
    module private Wrappers = 
        [<StructLayout(LayoutKind.Sequential)>]
        type KerningPair =
            struct
                val mutable public first : uint16
                val mutable public second : uint16
                val mutable public amount : int
            end

        [<StructLayout(LayoutKind.Sequential)>]
        type ABC =
            struct
                val mutable public A : float32
                val mutable public B : float32
                val mutable public C : float32
            end

        [<DllImport("gdi32.dll")>]
        extern int GetKerningPairs(nativeint hdc, int count, KerningPair* pairs)

        [<DllImport("gdi32.dll")>]
        extern nativeint SelectObject(nativeint hdc,nativeint hFont)

        [<DllImport("gdi32.dll")>]
        extern bool GetCharABCWidthsFloat(nativeint hdc, uint32 first, uint32 last, ABC* pxBuffer)

        [<DllImport("gdi32.dll")>]
        extern bool GetCharWidthFloat(nativeint hdc, uint32 first, uint32 last, float32* pxBuffer)


    let getKerningPairs (g : Graphics) (f : Font) =
        let hdc = g.GetHdc()
        let hFont = f.ToHfont()
        let old = SelectObject(hdc, hFont)
        try
            let cnt = GetKerningPairs(hdc, 0, NativePtr.zero)
            let ptr = NativePtr.stackalloc cnt
            GetKerningPairs(hdc, cnt, ptr) |> ignore

            let res = Dictionary<char * char, float>()
            for i in 0..cnt-1 do
                let pair = NativePtr.get ptr i
                let c0 = pair.first |> char
                let c1 = pair.second |> char
                res.[(c0,c1)] <- 0.25 * float pair.amount / float f.Size

            res
        finally
            SelectObject(hdc, old) |> ignore
            g.ReleaseHdc(hdc)

    let getCharWidths (g : Graphics) (f : Font) (c : char) =
        let hdc = g.GetHdc()

        let old = SelectObject(hdc, f.ToHfont())
        try
            let mutable size = ABC()
            if GetCharABCWidthsFloat(hdc, uint32 c, uint32 c, &&size) then
                0.75 * V3d(size.A, size.B, size.C) / float f.Size
            else
                let mutable size = 0.0f
                if GetCharWidthFloat(hdc, uint32 c, uint32 c, &&size) then
                    0.75 * V3d(0.0, float size, 0.0) / float f.Size
                else
                    Log.warn "no width for: '%c'" c
                    V3d.Zero
        finally
            SelectObject(hdc, old) |> ignore
            g.ReleaseHdc(hdc)

module FontInfo =
    let getKerningPairs (g : Graphics) (f : Font) =
        match Environment.OSVersion with
            | Windows -> GDI32.getKerningPairs g f
            | Linux -> failwithf "[Font] implement kerning for Linux"
            | Mac -> failwithf "[Font] implement kerning for Mac OS"

    let getCharWidths (g : Graphics) (f : Font) (c : char) =
        match Environment.OSVersion with
            | Windows -> GDI32.getCharWidths g f c
            | Linux -> failwithf "[Font] implement character sizes for Linux"
            | Mac -> failwithf "[Font] implement character sizes for Mac OS"


type Shape(path : Path) =

    static let quad = 
        Path.ofList [
            PathSegment.line V2d.OO V2d.OI
            PathSegment.line V2d.OI V2d.II
            PathSegment.line V2d.II V2d.IO
            PathSegment.line V2d.IO V2d.OO
        ] |> Shape

    let mutable geometry = None
    
    static member Quad = quad

    member x.Path = path

    member x.Geometry =
        match geometry with
            | Some g -> g
            | None ->
                let g = Path.toGeometry path
                geometry <- Some g
                g

type Glyph internal(path : Path, graphics : Graphics, font : System.Drawing.Font, c : char) =
    inherit Shape(path)
    let widths = lazy (FontInfo.getCharWidths graphics font c)

    member x.Character = c
    
    member x.Path = path
    member x.Advance = 
        let sizes = widths.Value
        sizes.X + sizes.Y + sizes.Z

    member x.Before = -0.1666


type Font private(f : System.Drawing.Font) =
    static let mutable fontCount = 0

    do Interlocked.Increment &fontCount |> ignore

    let largeScaleFont = new System.Drawing.Font(f.FontFamily, 2000.0f, f.Style, f.Unit, f.GdiCharSet, f.GdiVerticalFont)
    let graphics = Graphics.FromHwnd(0n, PageUnit = f.Unit)

    let kerningTable = FontInfo.getKerningPairs graphics largeScaleFont

    let lineHeight = 1.0

    let getPath (c : char) =
        use path = new GraphicsPath()
        let size = 1.0f
        use fmt = new System.Drawing.StringFormat()
        fmt.Trimming <- StringTrimming.None
        fmt.FormatFlags <- StringFormatFlags.FitBlackBox ||| StringFormatFlags.NoClip ||| StringFormatFlags.NoWrap 
        path.AddString(String(c, 1), f.FontFamily, int f.Style, size, PointF(0.0f, 0.0f), fmt)


        if path.PointCount = 0 then
            { outline = [||] }
        else
            // build the interior polygon and boundary triangles using the 
            // given GraphicsPath
            let types = path.PathTypes
            let points = path.PathPoints |> Array.map (fun p -> V2d(p.X, 1.0f - p.Y))

            let mutable start = V2d.NaN
            let currentPoints = List<V2d>()
            let segments = List<PathSegment>()

            for (p, t) in Array.zip points types do
                let t = t |> int |> unbox<PathPointType>

                let close = t &&& PathPointType.CloseSubpath <> PathPointType.Start


                match t &&& PathPointType.PathTypeMask with
                    | PathPointType.Line ->
                        if currentPoints.Count > 0 then
                            let last = currentPoints.[currentPoints.Count - 1]
                            segments.Add(Line(last, p))
                            currentPoints.Clear()
                        currentPoints.Add p
                        


                    | PathPointType.Bezier ->
                        currentPoints.Add p
                        if currentPoints.Count >= 4 then
                            let p0 = currentPoints.[0]
                            let p1 = currentPoints.[1]
                            let p2 = currentPoints.[2]
                            let p3 = currentPoints.[3]
                            segments.Add(Bezier3(p0, p1, p2, p3))
                            currentPoints.Clear()
                            currentPoints.Add p3

                    | PathPointType.Start | _ ->
                        currentPoints.Add p
                        start <- p
                        ()

                if close then
                    if not start.IsNaN && p <> start then
                        segments.Add(Line(p, start))
                    currentPoints.Clear()
                    start <- V2d.NaN

            let bounds = segments |> Seq.map PathSegment.bounds |> Box2d
            Log.warn "bounds(%c) = %A / %A" c bounds.Min bounds.Size




            { outline = CSharpList.toArray segments }

    let glyphCache = ConcurrentDictionary<char, Glyph>()

    let get (c : char) =
        glyphCache.GetOrAdd(c, fun c ->
            let path = getPath c
            Glyph(path, graphics, largeScaleFont, c)
        )



    member x.Dispose() =
        f.Dispose()
        kerningTable.Clear()
        largeScaleFont.Dispose()
        graphics.Dispose()

    member x.Family = f.FontFamily.Name
    member x.LineHeight = lineHeight
    member x.Style = unbox<FontStyle> (int f.Style)
    member x.Spacing = 1.0

    member x.GetGlyph(c : char) =
        get c

    member x.GetKerning(l : char, r : char) =
        match kerningTable.TryGetValue ((l,r)) with
            | (true, v) -> v
            | _ -> 0.0

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    new(family : string, style : FontStyle) = 
        let f = new System.Drawing.Font(family, 1.0f, unbox (int style), GraphicsUnit.Point)
        new Font(f)

    new(family : string) = 
        new Font(family, FontStyle.Regular)

type ShapeCache(r : IRuntime) =
    static let cache = ConcurrentDictionary<IRuntime, ShapeCache>()


    let pool = Aardvark.Base.Rendering.GeometryPool.createAsync r
    let ranges = ConcurrentDictionary<Shape, Range1i>()

    let surface = 
        r.PrepareEffect [
            Path.Shader.pathVertex |> toEffect
            DefaultSurfaces.trafo |> toEffect
            Path.Shader.pathFragment |> toEffect
        ]

    let types =
        Map.ofList [
            DefaultSemantic.Positions, typeof<V3f>
            Path.Attributes.KLMKind, typeof<V4f>
        ]

    let vertexBuffers =
        { new IAttributeProvider with
            member x.TryGetAttribute(sem) =
                match Map.tryFind sem types with    
                    | Some t -> BufferView(pool.GetBuffer sem, t) |> Some
                    | _ -> None

            member x.All = Seq.empty
            member x.Dispose() = ()
        }

    static member GetOrCreateCache(r : IRuntime) =
        cache.GetOrAdd(r, fun r ->
            new ShapeCache(r)
        )

    member x.Surface = surface :> ISurface
    member x.VertexBuffers = vertexBuffers

    member x.GetBufferRange(glyph : Shape) =
        ranges.GetOrAdd(glyph, fun glyph ->
            let range = pool.Add(glyph.Geometry)
            range
        )

    member x.Dispose() =
        //pool.Dispose()
        surface.Dispose()
        ranges.Clear()



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Font =
    let inline glyph (c : char) (f : Font) = f.GetGlyph c
    let inline kerning (l : char) (r : char) (f : Font) = f.GetKerning(l,r)
    let inline lineHeight (f : Font) = f.LineHeight

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Glyph =
    let inline advance (g : Glyph) = g.Advance
    let inline path (g : Glyph) = g.Path
    let inline geometry (g : Glyph) = g.Geometry
    let inline char (g : Glyph) = g.Character




