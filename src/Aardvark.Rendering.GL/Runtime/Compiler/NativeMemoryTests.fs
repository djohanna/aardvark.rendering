namespace Aardvark.Rendering.GL.Tests
#nowarn "44"

open System
open FsUnit
open NUnit.Framework
open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Aardvark.Base.Threading
open Aardvark.Rendering.GL


module MemoryManagerTests =
    
    let create() = new MemoryManager(16) //16, Marshal.AllocHGlobal, fun ptr _ -> Marshal.FreeHGlobal ptr)

//    module ReaderWriterLock = Aardvark.Base.ReaderWriterLock
//    let isNull (a : 'a) = match a with | null -> true | _ -> false
//    type managedptr internal(block : Block) =
//        let mutable block = block
//
//        let check() =
//            if isNull block then raise <| ObjectDisposedException("MemoryBlock")
//
//        member x.Parent = check(); block.Memory
//        member x.Size = if isNull block then 0 else block.Size
//        member x.Offset = if isNull block then 0n else 0n //dunno block.Offset
//        member x.Free = if isNull block then true else block.IsFree
//
//        override x.ToString() = 
//            if isNull block then "(null)"
//            else string block
//
//        member internal x.Block
//            with get() = block
//            and set b = block <- b
//
//        member x.Write(offset : int, source : nativeint, size : int) =
//            check()
//            if offset + size > block.Size then failwith "[Memory] write exceeding size"
//
//            ReaderWriterLock.read block.Memory.PointerLock (fun () ->
//                let target = block.Parent.Pointer + block.Offset + nativeint offset
//                Marshal.Copy(source, target, size)
//            )
//
//        member x.Read(offset : int, target : nativeint, size : int) =
//            check()
//            if offset + size > block.Size then failwith "[Memory] read exceeding size"
//
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                let source = block.Parent.Pointer + block.Offset + nativeint offset
//                Marshal.Copy(source, target, size)
//            )
//
//        member x.Write(offset : int, data : 'a) =
//            check()
//            if offset + sizeof<'a> > block.Size then failwith "[Memory] write exceeding size"
//
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                let ptr = block.Parent.Pointer + block.Offset + nativeint offset |> NativePtr.ofNativeInt
//                NativePtr.write ptr data
//            )
//
//        member x.Read(offset : int) : 'a =
//            check()
//            if offset + sizeof<'a> > block.Size then failwith "[Memory] read exceeding size"
//
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                let ptr = block.Parent.Pointer + block.Offset + nativeint offset |> NativePtr.ofNativeInt
//                NativePtr.read ptr
//            )
//
//        member x.Write(offset : int, data : 'a[]) =
//            check()
//            if offset + sizeof<'a> * data.Length > block.Size then failwith "[Memory] write exceeding size"
//
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                let mutable ptr = block.Parent.Pointer + block.Offset + nativeint offset |> NativePtr.ofNativeInt
//                for i in 0..data.Length-1 do
//                    NativePtr.set ptr i data.[i]
//            )
//
//        member x.Read(offset : int, data : 'a[]) =
//            check()
//            if offset + sizeof<'a> * data.Length > block.Size then failwith "[Memory] read exceeding size"
//
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                let mutable ptr = block.Parent.Pointer + block.Offset + nativeint offset |> NativePtr.ofNativeInt
//                for i in 0..data.Length-1 do
//                    data.[i] <- NativePtr.get ptr i
//            )
//
//        member x.Read(offset : int, count : int) : 'a[] =
//            check()
//            if offset + sizeof<'a> * count > block.Size then failwith "[Memory] read exceeding size"
//
//            let arr = Array.zeroCreate count
//            x.Read(offset, arr)
//            arr
//
//        member x.Move(sourceOffset : int, targetOffset : int, length : int) =
//            check()
//            ReaderWriterLock.read block.Parent.PointerLock (fun () ->
//                Marshal.Move(
//                    x.Parent.Pointer + x.Offset + nativeint sourceOffset,
//                    x.Parent.Pointer + x.Offset + nativeint targetOffset,
//                    length
//                )
//            )
//
//        member x.Int8Array      : int8[]    = x.Read(0, block.Size)
//        member x.Int16Array     : int16[]   = x.Read(0, block.Size / 2)
//        member x.Int32Array     : int[]     = x.Read(0, block.Size / 4)
//        member x.Int64Array     : int64[]   = x.Read(0, block.Size / 8)
//
//        member x.UInt8Array     : uint8[]   = x.Read(0, block.Size)
//        member x.UInt16Array    : uint16[]  = x.Read(0, block.Size / 2)
//        member x.UInt32Array    : uint32[]  = x.Read(0, block.Size / 4)
//        member x.UInt64Array    : uint64[]  = x.Read(0, block.Size / 8)

    type Interlocked with
        static member Change(location : byref<int>, f : int -> int) =
            let mutable v = location
            let mutable res = f v
            let mutable r = Interlocked.CompareExchange(&location, res, v)

            while v <> r do
                v <- r
                res <- f v
                r <- Interlocked.CompareExchange(&location, res, v)

            res



    [<Test>]
    let ``[Memory] simple alloc test``() =
        let m = create()

        let b0 = m.Alloc(10)
        let b1 = m.Alloc(6)

        m.Validate()

    [<Test>]
    let ``[Memory] simple free test``() =
        let m = create()

        let b0 = m.Alloc(10)
        m.Free b0


        let b1 = m.Alloc(16)
        
        m.Validate()

    [<Test>]
    let ``[Memory] free collapse left``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(2)
        let b2 = m.Alloc(2)
        m.Free b0
        m.Free b1

        
        m.Validate()

    [<Test>]
    let ``[Memory] free collapse right``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(2)
        let b2 = m.Alloc(2)
        let r = m.Alloc(10)
        m.Free b2
        m.Free b1

        
        m.Validate()

    [<Test>]
    let ``[Memory] free collapse both``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(2)
        let b2 = m.Alloc(2)
        let r = m.Alloc(10)
        m.Free b2
        m.Free b0
        m.Free b1

        
        m.Validate()


    [<Test>]
    let ``[Memory] realloc move``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(2)
        
        m.Realloc(b0, 4) |> should be True
        m.Validate()

        b0.Size |> should equal 4
        // dunno b0.Offset |> should equal 4n


    [<Test>]
    let ``[Memory] realloc exact space left``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(4)
        let b2 = m.Alloc(2)
        
        m.Free(b1)

        m.Realloc(b0, 6) |> should be False
        m.Validate()


    [<Test>]
    let ``[Memory] realloc more space left``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(5)
        let b2 = m.Alloc(2)
        
        m.Free(b1)

        m.Realloc(b0, 6) |> should be False
        m.Validate()


    [<Test>]
    let ``[Memory] realloc shrink``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(4)

        m.Realloc(b0, 1) |> should be False
        m.Validate()


    [<Test>]
    let ``[Memory] realloc 0``() =
        let m = create()

        let b0 = m.Alloc(2)
        let b1 = m.Alloc(4)

        m.Realloc(b0, 0) |> should be True // dunno False -> True
        m.Validate()

        b0.Size |> should equal 0


    [<Test>]
    let ``[Memory] resize``() =
        let m = create()

        let b0 = m.Alloc(10)
        let b1 = m.Alloc(100)

        m.Validate()


    [<Test>]
    let ``[Memory Performance] allocations``() =
        let m = create()
        let r = Random()


        // warm-up
        for i in 0..100 do
            m.Free(m.Alloc(r.Next(1 <<< 5) + 1))


        let sw = Stopwatch()
        let mutable iterations = 0

        sw.Start()
        while sw.Elapsed.TotalMilliseconds < 1000.0 do
            m.Alloc(r.Next(1 <<< 5) + 1) |> ignore
            iterations <- iterations + 1

        sw.Stop()
        let microseconds = sw.Elapsed.TotalMilliseconds * 1000.0

        Console.WriteLine("{0} µs/allocation", microseconds / float iterations)

    [<Test>]
    let ``[Memory Performance] free``() =
        let m = create()
        let r = Random()

        // warm-up
        for i in 0..100 do
            m.Free(m.Alloc(r.Next(1 <<< 5) + 1))

        let sw = Stopwatch()

        let blocks = Array.init (1 <<< 17) (fun _ -> m.Alloc(r.Next(1 <<< 5) + 1))
        let blocks = blocks.RandomOrder() |> Seq.toArray

        sw.Start()
        for i in 0..blocks.Length-1 do
            m.Free(blocks.[i])
        sw.Stop()
        let microseconds = sw.Elapsed.TotalMilliseconds * 1000.0

        Console.WriteLine("{0} µs/free", microseconds / float blocks.Length)


    [<Test>]
    let ``[Memory Performance] realloc no space``() =
        let m = create()
        let r = Random()

        // warm-up
        for i in 0..100 do
            let b = m.Alloc(r.Next(1 <<< 5) + 1)
            m.Realloc(b, b.Size + 1) |> ignore
            m.Free(b)

        let sw = Stopwatch()

        let blocks = Array.init (1 <<< 17) (fun _ -> m.Alloc(r.Next(1 <<< 5) + 1))
        let blocks = blocks.RandomOrder() |> Seq.toArray

        sw.Start()
        for i in 0..blocks.Length-1 do
            m.Realloc(blocks.[i], blocks.[i].Size + 1) |> ignore
        sw.Stop()
        let microseconds = sw.Elapsed.TotalMilliseconds * 1000.0

        Console.WriteLine("{0} µs/realloc (no space left)", microseconds / float blocks.Length)


    [<Test>]
    let ``[Memory Performance] realloc next free``() =
        let m = create()
        let r = Random()

        // warm-up
        for i in 0..100 do
            let b = m.Alloc(r.Next(1 <<< 5) + 1)
            m.Realloc(b, b.Size + 1) |> ignore
            m.Free(b)


        let sw = Stopwatch()

        let blocks = Array.init (1 <<< 18) (fun _ -> m.Alloc(r.Next(1 <<< 5) + 1))
        for i in 0..2..blocks.Length-1 do
            m.Free(blocks.[i])

        let blocks = blocks |> Array.mapi (fun i a -> if i % 2 <> 0 then Some a else None) 
                            |> Array.choose id

        let blocks = blocks.RandomOrder() |> Seq.toArray

        sw.Start()
        for i in 0..blocks.Length-1 do
            m.Realloc(blocks.[i], blocks.[i].Size + 1) |> ignore
        sw.Stop()
        let microseconds = sw.Elapsed.TotalMilliseconds * 1000.0

        Console.WriteLine("{0} µs/realloc (next free)", microseconds / float blocks.Length)


    let startTask (f : unit -> unit) =
        Task.Factory.StartNew(f, TaskCreationOptions.LongRunning) |> ignore

    let run (f : unit -> unit) =
        f()


    [<Test>]
    let ``[Memory] concurrent allocations``() =
        let cnt = 200uy
        let mem = new MemoryManager(16)

        let r = Random()
        let start = new ManualResetEventSlim(false)
        let finished = new SemaphoreSlim(0)
        let allblocks = ref Map.empty<byte,Block>
        let currentWrites = ref 0
        let maxParallelWrites = ref 0

        for i in 0uy..cnt - 1uy do
            startTask (fun () ->
                let size = r.Next 100 + 1
                start.Wait()

                let b = mem.Alloc size

                let current = Interlocked.Increment(&currentWrites.contents)
                Interlocked.Change(&maxParallelWrites.contents, max current) |> ignore
                b.Write(0, Array.create size i)
                Interlocked.Decrement(&currentWrites.contents) |> ignore

                

                Interlocked.Change(&allblocks.contents, Map.add i b) |> ignore

                finished.Release() |> ignore
            )

        start.Set()

        for i in 1uy..cnt do
            finished.Wait()

        mem.Validate()

        for (i,b) in Map.toSeq !allblocks do
            let data : byte[] = b.Read(0, b.Size) //b |> ManagedPtr.readArray 0 
            data |> should equal (Array.create b.Size i)

        Console.WriteLine("parallel writes: {0}", !maxParallelWrites)
        allblocks.Value.Count |> should equal (int cnt)
        !maxParallelWrites |> should greaterThan 1


    [<Test>]
    let ``[Memory] concurrent allocations / frees``() =
        let cnt = 200uy
        let mem = create()

        let r = Random()
        let start = new ManualResetEventSlim(false)
        let finished = new SemaphoreSlim(0)
        let allblocks = ref Map.empty



        for i in 0uy..cnt - 1uy do
            startTask (fun () ->
                let size = r.Next 100 + 1
                start.Wait()

                let b = mem.Alloc size
                b.Write(0, Array.create size i)

                Interlocked.Change(&allblocks.contents, Map.add i b) |> ignore

                finished.Release() |> ignore
            )

        start.Set()

        for i in 1uy..cnt do
            finished.Wait()

        mem.Validate()



        
        start.Reset()

        for i in 0uy..cnt - 1uy do
            startTask (fun () ->
                let free = r.Next(1) = 0
                let b = Map.find i !allblocks
                start.Wait()

                if free then
                    b.Dispose() //free b
                else
                    mem.Realloc(b,b.Size+2) |> ignore
                    b.Write(b.Size-2,[|i;i|])
                    //b |> ManagedPtr.realloc (b.Size + 2) |> ignore
                    //b |> ManagedPtr.writeArray (b.Size-2) [|i;i|]

                finished.Release() |> ignore
            )

        start.Set()

        for i in 1uy..cnt do
            finished.Wait()

        mem.Validate()

        for (i,b) in Map.toSeq !allblocks do
            if not b.IsFree then
                let data : byte[] = b.Read(0,b.Size)// |> ManagedPtr.readArray 0 
                data |> should equal (Array.create b.Size i)
            else
                b.Size |> should equal 0

        allblocks.Value.Count |> should equal (int cnt)


    [<Test>]
    let ``[Memory] concurrent random operations``() =
        let mem = create()
        let r = Random()

        let blocks = ref []

        let removeAny (s : list<Block>) =
            match s with
                | h::t -> t, Some h
                | _ -> [], None

        let add (ptr : Block) (l : list<Block>) =
            ptr::l

        let cnt = 2000
        let exns = ref []
        let sem = new SemaphoreSlim(0)

        for i in 1..cnt do
            startTask (fun () ->
            
                let op = r.Next(4)

                try
                    try
                        match op with
                            | 0 | 1 | 2 -> 
                                let b = mem.Alloc (r.Next(100) + 1)
                                Interlocked.Change(&blocks.contents, add b) |> ignore

                            | 3 -> 
                                let b = Interlocked.Change(&blocks.contents, removeAny)
                                match b with
                                    | Some b -> b.Dispose()// |> ManagedPtr.free
                                    | None -> ()

                            | 4 -> 
                                let b = Interlocked.Change(&blocks.contents, removeAny)
                                match b with
                                    | Some b -> 
                                        b.Memory.Realloc(b, (r.Next(100) + 1)) |> ignore
                                        //b |> ManagedPtr.realloc (r.Next(100) + 1) |> ignore
                                        if b.Size > 0 then
                                            Interlocked.Change(&blocks.contents, add b) |> ignore
                                    | _ -> ()
                            | _ -> failwith ""

                    with e ->
                        Interlocked.Change(&exns.contents, fun l -> e::l) |> ignore
                finally 
                    sem.Release() |> ignore
            )

        for i in 1..cnt do
            sem.Wait()

        for e in !exns do
            Console.WriteLine("{0}", e)

        !exns |> should equal []

    [<Test>]
    let ``[Memory] random operations``() =
        let mem = create ()
        let r = Random()

        let blocks = ref []

        let removeAny (s : list<Block>) =
            match s with
                | h::t -> t, Some h
                | _ -> [], None

        let add (ptr : Block) (l : list<Block>) =
            ptr::l

        let cnt = 2000
        let exns = ref []
        let sem = new SemaphoreSlim(0)

        for i in 1..cnt do
            run (fun () ->
            
                let op = r.Next(4)

                try
                    try
                        match op with
                            | 0 | 1 | 2 -> 
                                let b = mem.Alloc (r.Next(100) + 1)
                                Interlocked.Change(&blocks.contents, add b) |> ignore

                            | 3 -> 
                                let b = Interlocked.Change(&blocks.contents, removeAny)
                                match b with
                                    | Some b -> b.Dispose() //|> ManagedPtr.free
                                    | None -> ()

                            | 4 -> 
                                let b = Interlocked.Change(&blocks.contents, removeAny)
                                match b with
                                    | Some b -> 
                                        b.Memory.Realloc(b,(r.Next(100) + 1)) |> ignore// b |> ManagedPtr.realloc (r.Next(100) + 1) |> ignore
                                        if b.Size > 0 then
                                            Interlocked.Change(&blocks.contents, add b) |> ignore
                                    | _ -> ()
                            | _ -> failwith ""


                    with e ->
                        Interlocked.Change(&exns.contents, fun l -> e::l) |> ignore
                finally 
                    sem.Release() |> ignore
            )

        for i in 1..cnt do
            sem.Wait()

        for e in !exns do
            Console.WriteLine("{0}", e)

        !exns |> should equal []

