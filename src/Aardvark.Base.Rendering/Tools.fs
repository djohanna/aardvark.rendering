namespace Aardvark.Base.Rendering


open System
open System.Threading
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Collections.Concurrent
open Aardvark.Base
open Aardvark.Base.Incremental
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferView =
    let ofArray (data : 'a[]) = 
        let b = ArrayBuffer(data) :> IBuffer
        BufferView(Mod.constant b, typeof<'a>)

    let map (f : 'a -> 'b) (view : BufferView) =
        let newBuffer = 
            view.Buffer |> Mod.map (fun b ->
                match b with
                    | :? INativeBuffer as nb ->
                        nb.Use (fun ptr ->
                            let cnt = (nb.SizeInBytes - view.Offset) / sizeof<'a>
                            let target = Array.zeroCreate cnt

                            let copyValue (source : nativeint) (i : int) =
                                let value = 
                                    source
                                        |> NativePtr.ofNativeInt<'a>
                                        |> NativePtr.read
                                        |> f
                                target.[i] <- value

                            let stride =
                                if view.Stride = 0 then nativeint sizeof<'a>
                                else nativeint view.Stride

                            let mutable ptr = ptr + nativeint view.Offset

                            for i in 0..cnt-1 do
                                copyValue ptr i
                                ptr <- ptr + stride


                            ArrayBuffer(target) :> IBuffer
                        )
                    | _ ->
                        failwithf "[Buffer] cannot map %A" b
            )

        BufferView(newBuffer, typeof<'b>)


type IUnmanaged =
    inherit IAdaptiveObject
    abstract member ElementType : Type
    abstract member ElementSize : int
    abstract member Use : IAdaptiveObject * (nativeint -> int64 -> 'a) -> 'a

type IUnmanaged<'a when 'a : unmanaged> =
    inherit IUnmanaged

module Unmanaged =
    open System.Reflection

    [<AbstractClass>]
    type private AbstractUnmanaged<'a when 'a : unmanaged>() =
        inherit AdaptiveObject()

        static let aType = typeof<'a>
        static let aSize = sizeof<'a>

        abstract member Use : (nativeint -> int64 -> 'r) -> 'r

        member x.ElementType = aType
        member x.ElementSize = aSize
        member x.Use(caller : IAdaptiveObject, cont : nativeint -> int64 -> 'r) =
            x.EvaluateAlways caller (fun () ->
                x.Use(fun ptr cnt -> cont ptr (cnt * int64 aSize))
            )

        interface IUnmanaged<'a> with
            member x.ElementType = aType
            member x.ElementSize = aSize
            member x.Use(caller : IAdaptiveObject, cont : nativeint -> int64 -> 'r) = x.Use(caller, cont)

    type private UnmanagedValue<'a when 'a : unmanaged>(data : IMod<'a>) =
        inherit AbstractUnmanaged<'a>()

        override x.Use(f : nativeint -> int64 -> 'r) =
            let mutable v = data.GetValue x
            f (NativePtr.toNativeInt &&v) 1L

    type private UnmanagedArray<'a when 'a : unmanaged>(data : IMod<'a[]>) =
        inherit AbstractUnmanaged<'a>()

        override x.Use(f : nativeint -> int64 -> 'r) =
            let v = data.GetValue x
            let gc = GCHandle.Alloc(v, GCHandleType.Pinned)
            try f (gc.AddrOfPinnedObject()) v.LongLength
            finally gc.Free()

    type private UnmanagedBuffer<'a when 'a : unmanaged>(view : BufferView) =
        inherit AbstractUnmanaged<'a>()

        override x.Use(f : nativeint -> int64 -> 'r) =
            match view.Buffer.GetValue x with
                | :? INativeBuffer as nb ->
                    let size = nb.SizeInBytes - view.Offset |> int64
                    nb.Use(fun ptr -> f (ptr + nativeint view.Offset) size)
                | b ->
                    failwithf "[UnmanagedBuffer] cannot read: %A" b


    type private UnmanagedConvertedValue<'a, 'b when 'b : unmanaged>(input : IMod<'a>, convert : 'a -> 'b) =
        inherit UnmanagedValue<'b>(input |> Mod.map convert)

    type private UnmanagedConvertedArray<'a, 'b when 'b : unmanaged>(input : IMod<'a[]>, convert : 'a -> 'b) =
        inherit UnmanagedArray<'b>(input |> Mod.map (Array.map convert))

    type private UnmanagedConvertedBuffer<'a, 'b when 'a : unmanaged and 'b : unmanaged>(input : BufferView, convert : 'a -> 'b) =
        inherit UnmanagedBuffer<'b>(input |> BufferView.map convert)


    type private Converter<'a, 'b when 'b : unmanaged> private() =
        static let a = typeof<'a>
        static let b = typeof<'b>

        static let convert = 
            if a = b then id |> unbox<'a -> 'b>
            else PrimitiveValueConverter.converter<'a, 'b>

        static let create =
            if a = b then 
                if a.IsArray then 
                    fun (i : IMod) -> UnmanagedArray<'b>(unbox i) :> IUnmanaged<'b>
                else
                    fun (i : IMod) -> UnmanagedValue<'b>(unbox i) :> IUnmanaged<'b>
            else
                if a.IsArray then
                    fun (i : IMod) -> UnmanagedConvertedArray<'a, 'b>(unbox i, convert) :> IUnmanaged<'b>
                else
                    fun (i : IMod) -> UnmanagedConvertedValue<'a, 'b>(unbox i, convert) :> IUnmanaged<'b>

        static member Create(input : IMod) = create input

    type private BufferConverter<'a, 'b when 'a : unmanaged and 'b : unmanaged> private() =
        static let a = typeof<'a>
        static let b = typeof<'b>
        static let ma = typeof<IMod<'a>>

        static let convert = 
            if a = b then id |> unbox<'a -> 'b>
            else PrimitiveValueConverter.converter<'a, 'b>


        static let createBuffer =
            if a = b then 
                fun (i : BufferView) -> UnmanagedBuffer<'b>(i) :> IUnmanaged
            else
                fun (i : BufferView) -> UnmanagedConvertedBuffer<'a, 'b>(i, convert) :> IUnmanaged

        static member CreateBuffer(input : BufferView) =
            createBuffer input


    let private cType = typedefof<Converter<int, int>>
    let private bcType = typedefof<BufferConverter<int, int>>

    let ofModUntyped<'a when 'a : unmanaged> (input : IMod) : IUnmanaged<'a> =
        let targetType = typeof<'a>
        match input.GetType() with
            | ModOf t -> 
                let t = cType.MakeGenericType [|t; targetType|]
                let create = t.GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                create.Invoke(null, [|input|]) |> unbox<IUnmanaged<'a> >

            | _ -> 
                failwithf "[Unmanaged] cannot convert %A to %A" input targetType

    let ofMod<'a, 'b when 'b : unmanaged> (input : IMod<'a>) : IUnmanaged<'b> =
        Converter<'a, 'b>.Create(input)

    let ofBufferView<'a when 'a : unmanaged> (input : BufferView) : IUnmanaged<'a> =
        let t = bcType.MakeGenericType [| input.ElementType; typeof<'a> |]
        let create = t.GetMethod("CreateBuffer", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
        create.Invoke(null, [|input|]) |> unbox<IUnmanaged<'a>>


    let ofMod' (targetType : Type) (input : IMod) : IUnmanaged =
        match input.GetType() with
            | ModOf t -> 
                let t = cType.MakeGenericType [|t; targetType|]
                let create = t.GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
                create.Invoke(null, [|input|]) |> unbox<IUnmanaged>

            | _ -> 
                failwithf "[Unmanaged] cannot convert %A to %A" input targetType

    let ofBufferView' (targetType : Type) (input : BufferView) : IUnmanaged =
        let t = bcType.MakeGenericType [| input.ElementType; targetType |]
        let create = t.GetMethod("CreateBuffer", BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static)
        create.Invoke(null, [|input|]) |> unbox<IUnmanaged>

type IWriter =
    inherit IAdaptiveObject
    abstract member Write : IAdaptiveObject -> unit

[<AutoOpen>]
module private BufferWriter =
    type MultiBufferWriter(target : IMappedBuffer, elementType : Type, data : IUnmanaged) =
        inherit AdaptiveObject()

        let elementSize = Marshal.SizeOf elementType

        let mutable regions = RangeSet.empty

        member x.AddRange(offset : int, size : int) =
            let range = Range1i.FromMinAndSize(offset, size - 1)
            regions <- RangeSet.insert range regions

        member x.RemoveRange(offset : int, size : int) =
            let range = Range1i.FromMinAndSize(offset, size - 1)
            regions <- RangeSet.remove range regions

        member x.Write(caller : IAdaptiveObject) =
            x.EvaluateIfNeeded caller () (fun () ->
                data.Use(x, fun ptr pSize ->
                    for r in regions do
                        let offset = r.Min * elementSize
                        let size = (r.Size + 1) * elementSize
                        target.Write(ptr, offset, min (int pSize) size)
                )
            )

        interface IWriter with
            member x.Write caller = x.Write caller

type AdaptiveBuffer(runtime : IRuntime, elementType : Type) =
    inherit DirtyTrackingAdaptiveObject<IWriter>()

    let elementSize = Marshal.SizeOf elementType

    let l = obj()
    let store = runtime.CreateMappedBuffer()
    let mutable currentSize = 0
    let writers = Dict<obj, MultiBufferWriter>()
    let mutable used = RangeSet.empty

    let fixupSize() =
        let max = RangeSet.max used
        if max = Int32.MinValue then
            if currentSize <> 0 then
                currentSize <- 0
                store.Resize 0
        else
            let needed = (max + 1) * elementSize |> Fun.NextPowerOfTwo
            if needed <> currentSize then
                currentSize <- needed
                store.Resize(currentSize)
                
    let addUsed (off : int) (count : int) =
        lock l (fun () ->
            let r = Range1i.FromMinAndSize(off, count-1)
            used <- RangeSet.insert r used
            fixupSize()
        )

    let remUsed (off : int) (count : int) =
        lock l (fun () ->
            let r = Range1i.FromMinAndSize(off, count-1)
            used <- RangeSet.remove r used
            fixupSize()
        )

    let getOrCreate (x : AdaptiveBuffer) (data : 'a) (f : 'a -> IUnmanaged) =
        let mutable isNew = false
        let mutable res = Unchecked.defaultof<_>
        Monitor.Enter writers
        try
            res <- 
                writers.GetOrCreate(data, fun _ ->
                    let u = f data
                    let res = MultiBufferWriter(store, elementType, u)
                    isNew <- true
                    res
                )
            res
        finally
            Monitor.Exit writers
            if isNew then
                lock x (fun () -> x.Dirty.Add res |> ignore)
                transact (fun () -> x.MarkOutdated())


    member x.Set (i : int, v : IMod) =
        let writer = 
            getOrCreate x v (Unmanaged.ofMod' elementType)

        addUsed i 1
        writer.AddRange(i, 1)

        { new IDisposable with 
            member x.Dispose() =
                writer.RemoveRange(i, 1)
                remUsed i 1
        }

    member x.Set (startIndex : int, count : int, data : BufferView) =
        let writer = 
            getOrCreate x data (Unmanaged.ofBufferView' elementType)

        addUsed startIndex count
        writer.AddRange(startIndex, count)

        { new IDisposable with 
            member x.Dispose() =
                writer.RemoveRange(startIndex, count)
                remUsed startIndex count
        }



    member x.GetValue(caller : IAdaptiveObject) =
        x.EvaluateAlways' caller (fun dirty ->
            if x.OutOfDate then
                for d in dirty do
                    d.Write(x)

            store.GetValue x
        )

    member x.Dispose() =
        if Interlocked.Exchange(&currentSize, 0) <> 0 then
            store.Dispose()
            lock writers (fun () -> writers.Clear())
            lock x (fun () -> x.Dirty.Clear())
            used <- RangeSet.empty

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IMod with
        member x.IsConstant = false
        member x.GetValue c = x.GetValue c :> obj

    interface IMod<IBuffer> with
        member x.GetValue c = x.GetValue c

type AdaptiveBuffer<'a when 'a : unmanaged>(runtime : IRuntime) =
    inherit AdaptiveBuffer(runtime, typeof<'a>)


[<AbstractClass; Sealed; Extension>]
type RuntimeAdaptiveBufferExtensions private() =

    [<Extension>]
    static member CreateAdaptiveBuffer(this : IRuntime, elementType : Type) =
        new AdaptiveBuffer(this, elementType)
        
    [<Extension>]
    static member CreateAdaptiveBuffer<'a when 'a : unmanaged>(this : IRuntime) =
        new AdaptiveBuffer<'a>(this)