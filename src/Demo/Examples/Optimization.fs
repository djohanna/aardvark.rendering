module Optimization

open System
open System.Threading
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph
open Aardvark.Base.Incremental.Operators
open Aardvark.SceneGraph.Semantics
open Aardvark.Rendering.NanoVg
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

[<AutoOpen>]
module ``Move to Base`` =

    [<AutoOpen>]
    module private Utils =
        let inline div2Floor (v : int64) =
            if v % 2L = 0L then v / 2L
            else (v - 1L) / 2L

        let inline div2Ceil (v : int64) =
            if v % 2L = 0L then v / 2L
            else (v + 1L) / 2L


        let inline floor3d (v : V3d) =
            V3l(v.X |> floor |> int64, v.Y |> floor |> int64, v.Z |> floor |> int64)

    [<CustomEquality; NoComparison>]
    type GridCell =
        struct
            val mutable public X : int64
            val mutable public Y : int64
            val mutable public Z : int64
            val mutable public Exp : int

            static member Containing (box : Box3d) =
                let s = box.Size
                let e = s.NormMax |> Fun.Log2 |> ceil |> int

                let size = pown 2.0 e

                let shift = 
                    let ehalf = floor (float (e + 1) * 0.5) |> int
                    -((pown 4.0 ehalf) - 1.0) / 3.0

                let i = (box.Min - shift) / size |> floor3d
                let mutable cell = GridCell(i.X, i.Y, i.Z, e)

                while not (cell.Contains box) do
                    cell <- cell.Parent

                cell

            member x.BoundingBox =
                let size = pown 2.0 x.Exp

                let shift = 
                    let ehalf = floor (float (x.Exp + 1) * 0.5) |> int
                    let shift = -((pown 4.0 ehalf) - 1.0) / 3.0
                    shift / size

                Box3d(
                    (shift + float x.X) * size          |> float,
                    (shift + float x.Y) * size          |> float,
                    (shift + float x.Z) * size          |> float,
                    (shift + float x.X + 1.0) * size    |> float,
                    (shift + float x.Y + 1.0) * size    |> float,
                    (shift + float x.Z + 1.0) * size    |> float
                )

            member x.Center =
                let size = pown 2.0 x.Exp

                let shift = 
                    let ehalf = floor (float (x.Exp + 1) * 0.5) |> int
                    let shift = -((pown 4.0 ehalf) - 1.0) / 3.0
                    shift / size

                V3d(
                    (shift + float x.X + 0.5) * size    |> float,
                    (shift + float x.Y + 0.5) * size    |> float,
                    (shift + float x.Z + 0.5) * size    |> float
                )          

            member x.Contains (p : V3d) =
                x.BoundingBox.Contains p

            member x.Contains (p : Box3d) =
                x.BoundingBox.Contains p

            member x.Parent =
                if x.Exp % 2 = 0 then GridCell(div2Ceil x.X, div2Ceil x.Y, div2Ceil x.Z, x.Exp + 1)
                else GridCell(div2Floor x.X, div2Floor x.Y, div2Floor x.Z, x.Exp + 1)

            member x.IndexInParent =
                if x.Exp % 2 = 0 then 
                    (((int x.X + 1) % 2) <<< 2) |||
                    (((int x.Y + 1) % 2) <<< 1) |||
                    (((int x.Z + 1) % 2) <<< 0)
                else
                    (((int x.X) % 2) <<< 2) |||
                    (((int x.Y) % 2) <<< 1) |||
                    (((int x.Z) % 2) <<< 0)
                    


            member x.Children =
                let e = x.Exp - 1
                let l = if x.Exp % 2 = 0 then 0L else -1L
                let h = if x.Exp % 2 = 0 then 1L else 0L

                let z = x.Z * 2L
                let y = x.Y * 2L
                let x = x.X * 2L
                [|
                    GridCell(x+l,    y+l,    z+l,    e)
                    GridCell(x+l,    y+l,    z+h,    e)
                    GridCell(x+l,    y+h,    z+l,    e)
                    GridCell(x+l,    y+h,    z+h,    e)
                    GridCell(x+h,    y+l,    z+l,    e)
                    GridCell(x+h,    y+l,    z+h,    e)
                    GridCell(x+h,    y+h,    z+l,    e)
                    GridCell(x+h,    y+h,    z+h,    e)
                |]

            member x.GetChild (index : int) =
                let xc = (index >>> 2) &&& 0x1 |> int64
                let yc = (index >>> 1) &&& 0x1 |> int64
                let zc = (index >>> 0) &&& 0x1 |> int64
                let l = if x.Exp % 2 = 0 then 0L else -1L

                GridCell(2L * x.X + xc + l, 2L * x.Y + yc + l, 2L * x.Z + zc + l, x.Exp - 1)

            member x.Index = V3l(x.X, x.Y, x.Z)

            override x.ToString() =
                sprintf "{ Index = (%d, %d, %d); Exp = %d }" x.X x.Y x.Z x.Exp

            override x.GetHashCode() =
                HashCode.Combine(x.X.GetHashCode(), x.Y.GetHashCode(), x.Z.GetHashCode(), x.Exp)

            override x.Equals o =
                match o with
                    | :? GridCell as o -> x.X = o.X && x.Y = o.Y && x.Z = o.Z && x.Exp = o.Exp
                    | _ -> false

            new(x,y,z,e) = { X = x; Y = y; Z = z; Exp = e }
            new(x : int,y : int,z : int,e : int) = { X = int64 x; Y = int64 y; Z = int64 z; Exp = e }
        end

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GridCell =
        let inline ofBox (b : Box3d) = GridCell.Containing b
        let inline parent (c : GridCell) = c.Parent
        let inline children (c : GridCell) = c.Children
        let inline child (i : int) (c : GridCell) = c.GetChild i
        let inline bounds (c : GridCell) = c.BoundingBox

[<AutoOpen>]
module ``Database Stuff`` =
    open System.IO
    open System.IO.Compression
    open System.Runtime.CompilerServices
    open Nessos.FsPickler
    open Nessos.FsPickler.Combinators
    open Nessos.FsPickler.Json

    type IBlobFile =
        abstract member Name : string
        abstract member HasContent : bool
        abstract member Size : int64
        abstract member Read : unit -> byte[]
        abstract member Write : byte[] -> unit
        abstract member Delete : unit -> unit

    type IBlobStore =
        inherit IDisposable
        abstract member Create : string -> IBlobFile
        abstract member GetOrCreate : string -> IBlobFile

    module BlobStore =
        open System.Collections.Concurrent

        type private ZipFile(archive : ZipArchive, name : string, entry : ZipArchiveEntry) =
            let mutable entry = entry
            

            member x.Name = name
            member x.HasContent = not (isNull entry)
            member x.Size = if isNull entry then 0L else entry.Length
            member x.Delete() =
                if not (isNull entry) then
                    entry.Delete()
                    entry <- null

            member x.Write(data : byte[]) =
                if not (isNull entry) then entry.Delete()
                entry <- archive.CreateEntry(name, CompressionLevel.NoCompression)

                use stream = entry.Open()
                stream.Write(data, 0, data.Length)

            member x.Read() : byte[] =
                if isNull entry then
                    [||]
                else
                    use stream = entry.Open()

                    let size = stream.Length |> int
                    let arr = Array.zeroCreate size
                    let mutable read = 0
                    while read < size do
                        read <- read + stream.Read(arr, read, size - read)

                    arr

            interface IBlobFile with
                member x.Name = x.Name
                member x.HasContent = x.HasContent
                member x.Size = x.Size
                member x.Write(data) = x.Write(data)
                member x.Read() = x.Read()
                member x.Delete() = x.Delete()

        type private ZipStore (file : string) =
            let stream = File.Open(file, FileMode.OpenOrCreate, FileAccess.ReadWrite)
            let archive = new ZipArchive(stream, ZipArchiveMode.Update)

            member x.GetOrCreate(name : string) =
                let e = archive.GetEntry(name)
                ZipFile(archive, name, e) :> IBlobFile

            member x.Create(name : string) =
                ZipFile(archive, name, null) :> IBlobFile

            member x.Dispose() =
                stream.Flush()
                archive.Dispose()
                stream.Dispose()

            interface IBlobStore with
                member x.Dispose() = x.Dispose()
                member x.Create(name) = x.Create(name)
                member x.GetOrCreate(name) = x.GetOrCreate(name)


        type private File(path : string, name : string) =
            let f = new System.IO.FileInfo(path)


            member x.Name = name
            member x.HasContent = f.Exists
            member x.Size = f.Length
            member x.Delete() = if f.Exists then f.Delete()
            member x.Write(data : byte[]) =
                use stream = f.OpenWrite()
                stream.Write(data, 0, data.Length)

            member x.Read() : byte[] =
                if not f.Exists then
                    [||]
                else
                    use stream = f.OpenRead()
                    let size = stream.Length |> int
                    let arr = Array.zeroCreate size
                    let mutable read = 0
                    while read < size do
                        read <- read + stream.Read(arr, read, size - read)

                    arr

            interface IBlobFile with
                member x.Name = x.Name
                member x.HasContent = x.HasContent
                member x.Size = x.Size
                member x.Write(data) = x.Write(data)
                member x.Read() = x.Read()
                member x.Delete() = x.Delete()

        type private FolderStore (folder : string) =
            let folder = Path.GetFullPath folder
            do if not (Directory.Exists folder) then Directory.CreateDirectory(folder) |> ignore


            member x.GetOrCreate(name : string) =
                let path = Path.Combine(folder, name)
                File(path, name) :> IBlobFile

            member x.Create(name : string) =
                let path = Path.Combine(folder, name)
                File(path, name) :> IBlobFile

            interface IBlobStore with
                member x.Dispose() = ()
                member x.Create(name) = x.Create(name)
                member x.GetOrCreate(name) = x.GetOrCreate(name)

        
        type private BufferedFile(mark : BufferedFile -> unit, file : IBlobFile, name : string) =
            let mutable content =
                if file.HasContent then Some (file.Read())
                else None

            let mutable dirty = false

            member x.Name = name
            member x.HasContent = Option.isSome content
            member x.Size =
                match content with
                    | Some c -> c.LongLength
                    | None -> 0L

            member x.Read() =
                match content with
                    | Some content -> content
                    | None -> [||]


            member x.Delete() =
                match content with
                    | Some _ -> 
                        content <- None
                        dirty <- true
                        mark x
                    | None -> ()

            member x.Write(data : byte[]) =
                content <- Some data
                dirty <- true
                mark x

            member x.Persist() =
                if dirty then
                    match content with
                        | Some c -> file.Write c
                        | None -> if file.HasContent then file.Delete()
                    dirty <- false

            override x.GetHashCode() = name.GetHashCode()
            override x.Equals o =
                match o with
                    | :? BufferedFile as o -> name = o.Name
                    | _ -> false

            interface IBlobFile with
                member x.Name = x.Name
                member x.HasContent = x.HasContent
                member x.Size = x.Size
                member x.Write(data) = x.Write(data)
                member x.Read() = x.Read()
                member x.Delete() = x.Delete()

        type BufferedStore(store : IBlobStore) =
            
            let toPersist = ConcurrentHashQueue<BufferedFile>()
            let mutable count = 0
            //let persistCount = new SemaphoreSlim(0)

            let mark (f : BufferedFile) =
                if toPersist.Enqueue(f) then
                    Interlocked.Increment(&count) |> ignore
                    //persistCount.Release() |> ignore

            let persistor =
                async {
                    let! ct = Async.CancellationToken
                    do! Async.SwitchToNewThread()
                    while true do
                        do! Async.Sleep 500

                        do while toPersist.Count > 65536 do
                                match toPersist.TryDequeue() with
                                    | (true, f) -> 
                                        f.Persist()
                                        Interlocked.Decrement(&count) |> ignore
                                    | _ -> ()

                        
                }

            let cts = new CancellationTokenSource()
            let persist = Async.StartAsTask(persistor, cancellationToken = cts.Token)


            member x.GetOrCreate(name : string) =
                let file = store.GetOrCreate(name)
                new BufferedFile(mark, file, name) :> IBlobFile


            member x.Create(name : string) =
                let file = store.Create(name)
                new BufferedFile(mark, file, name) :> IBlobFile


            member x.Dispose() =
                cts.Cancel()
                persist.Wait()

                while toPersist.Count > 0 do
                    match toPersist.TryDequeue() with
                        | (true, f) -> f.Persist()
                        | _ -> ()

                cts.Dispose()
                //persistCount.Dispose()
                store.Dispose()

            interface IBlobStore with
                member x.Dispose() = x.Dispose()
                member x.Create(name) = x.Create(name)
                member x.GetOrCreate(name) = x.GetOrCreate(name)


        let buffered (store : IBlobStore) =
            match store with
                | :? BufferedStore -> store
                | _ -> new BufferedStore(store) :> IBlobStore

        let zip (file : string) =
            new ZipStore(file) :> IBlobStore

        let folder (folder : string) =
            new FolderStore(folder) :> IBlobStore

        let temp () =
            new FolderStore(Path.Combine(Path.GetTempPath(), Guid.NewGuid() |> string)) :> IBlobStore


    type TypeInfo<'a> private() =
        static let isBlittable = 
            let t = typeof<'a>
            if t.IsValueType then
                try
                    let v = Unchecked.defaultof<'a>
                    let gc = GCHandle.Alloc(v, GCHandleType.Pinned)
                    gc.Free()
                    true
                with _ ->
                    false
            else
                false

        static let isBlittableArray = 
            let t = typeof<'a>
            if t.IsArray then
                let bb = typedefof<TypeInfo<_>>.MakeGenericType [|t.GetElementType()|]
                bb.GetProperty("IsBlittable", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Static).GetValue(null) |> unbox<bool>
            else
                false
                     

        static member IsBlittable = isBlittable
        static member IsBlittableArray = isBlittableArray

    type ArrayBlitter<'a> private() =
        static let elementType = typeof<'a>.GetElementType()
        static let elementSize = Marshal.SizeOf elementType

        static let sizeOf (v : 'a) =
            (v |> unbox<Array>).Length * elementSize


        static member Read(f : IBlobFile) =
            let arr = f.Read()
            let res = Array.CreateInstance(elementType, arr.Length / elementSize)
            let gc = GCHandle.Alloc(res, GCHandleType.Pinned)
            try Marshal.Copy(arr, 0, gc.AddrOfPinnedObject(), arr.Length)
            finally gc.Free()
            res |> unbox<'a>

        static member Write(f : IBlobFile, value : 'a) =
            let arr : byte[] = Array.zeroCreate (sizeOf value)
            let gc = GCHandle.Alloc(value, GCHandleType.Pinned)
            try Marshal.Copy(gc.AddrOfPinnedObject(), arr, 0, arr.Length)
            finally gc.Free()
            f.Write(arr)

    [<AbstractClass; Sealed; Extension>]
    type BlobStoreExtensions private() =
        static let pickler = FsPickler.CreateBinarySerializer()
       

        [<Extension>]
        static member Load<'a>(this : IBlobFile, size : byref<int64>) : 'a =
            if TypeInfo<'a>.IsBlittableArray then
                ArrayBlitter<'a>.Read(this)
            else
                let arr = this.Read() 
                size <- arr.LongLength
                arr |> pickler.UnPickle

        [<Extension>]
        static member Store<'a>(this : IBlobFile, value : 'a, size : byref<int64>) =
            if TypeInfo<'a>.IsBlittableArray then
                ArrayBlitter<'a>.Write(this, value)
            else
                let data = value |> pickler.Pickle
                size <- data.LongLength
                this.Write(data)

        [<Extension>]
        static member Load<'a>(this : IBlobFile) : 'a =
            if TypeInfo<'a>.IsBlittableArray then
                ArrayBlitter<'a>.Read(this)
            else
                let arr = this.Read() 
                arr |> pickler.UnPickle

        [<Extension>]
        static member Store<'a>(this : IBlobFile, value : 'a) =
            if TypeInfo<'a>.IsBlittableArray then
                ArrayBlitter<'a>.Write(this, value)
            else
                let data = value |> pickler.Pickle
                this.Write(data)

    [<AutoOpen>]
    module WeakOpt = 
        [<AllowNullLiteral>]
        type WeakOption<'a when 'a : not struct>(value : 'a) =
            let value = WeakReference<'a>(value)

            member x.OptionValue = 
                match value.TryGetTarget() with
                    | (true, v) -> Some v
                    | _ -> None

            member x.Value = 
                match value.TryGetTarget() with
                    | (true, v) -> v
                    | _ -> failwith "weak got collected"
      
            static member WeakNone : WeakOption<'a> = null
            static member WeakSome (v : 'a) = WeakOption(v)

        [<AutoOpen>]
        module ``WeakOption Extensions`` =

            let WeakSome(v) = WeakOption(v)
            let WeakNone<'a when 'a : not struct> : WeakOption<'a> = null

            let (|WeakSome|WeakNone|) (w : WeakOption<'a>) =
                match w with
                    | null -> WeakNone
                    | v ->
                        match v.OptionValue with
                            | Some v -> WeakSome v
                            | None -> WeakNone

    //[<AutoOpen>]
    module StrongOpt = 
        type WeakOption<'a> = Option<'a>

        [<AutoOpen>]
        module ``WeakOption Extensions`` =

            let WeakSome(v) = Some(v)
            let WeakNone<'a when 'a : not struct> : WeakOption<'a> = None

            let (|WeakSome|WeakNone|) (w : WeakOption<'a>) =
                match w with
                    | None -> WeakNone
                    | Some v -> WeakSome v

    [<CustomPickler>]
    type Database (store : IBlobStore) =
        static let current = new ThreadLocal<ref<Option<Database>>>(fun () -> ref None)

        static let withDb (db : Database) (f : unit -> 'a) =
            let r = current.Value
            let old = !r
            r := Some db
            try f()
            finally r:= old
        
        let size (tup : obj * int64) = snd tup

        static member Current = current.Value.Value.Value

        static member CreatePickler (resolver : IPicklerResolver) : Pickler<Database> =
            Pickler.int |> Pickler.wrap 
                (fun _ -> Database.Current)
                (fun db -> 0)
    

        member x.Use (f : unit -> 'a) =
            withDb x f

        member x.Ref(name : string) =
            let file = store.GetOrCreate name
            dref<'a>(x, file)

        member x.Ref(name : string, value : 'a) =
            let file = store.GetOrCreate name
            file.Store(value)
            dref<'a>(x, file, value)

        member x.NewRef(value : 'a) =
            let name = Guid.NewGuid() |> string
            let file = store.Create name
            file.Store(value)
            dref<'a>(x, file, value)


        member x.Dispose() =
            store.Dispose()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

    and [<CustomPickler>] dref<'a when 'a : not struct> =
        class
            val mutable public Database : Database
            val mutable public File : IBlobFile
            val mutable Cache : WeakOption<'a>

            static member CreatePickler (resolver : IPicklerResolver) : Pickler<dref<'a>> =
                Pickler.string |> Pickler.wrap 
                    (fun str -> Database.Current.Ref(str))
                    (fun ref -> ref.Name)
    

            member x.Name = x.File.Name

            member x.Size = x.File.Size

            member x.Value
                with get() : 'a =
                    match x.Cache with
                        | WeakSome v -> v
                        | _ -> 
                            let v = x.Database.Use (fun () -> x.File.Load())
                            x.Cache <- WeakSome v
                            v

                and set (v : 'a) =
                    match x.Cache with
                        | WeakSome o when Object.Equals(o, v) -> ()
                        | _ ->
                            x.Database.Use (fun () -> x.File.Store(v))
                            x.Cache <- WeakSome v
                  
            member x.HasValue =
                x.File.HasContent

            member x.ClearCache() =
                x.Cache <- WeakNone

            member x.Delete() =
                x.File.Delete()
                x.Cache <- WeakNone

            override x.ToString() =
                match x.Cache with
                    | WeakSome v -> sprintf "{ %s = %A }" x.Name v
                    | WeakNone -> sprintf "{ %s = ? }" x.Name

            internal new(db : Database, file : IBlobFile) = { Database = db; File = file; Cache = WeakNone }
            internal new(db : Database, file : IBlobFile, value : 'a) = { Database = db; File = file; Cache = WeakSome value }
        end




[<AutoOpen>]
module ``Octree impl`` =

    [<AutoSerializable(false)>]
    type Point =
        struct
            val mutable public Position : V3f
            val mutable public Color : C4b


            static member (-) (p : Point, v : V3f) = Point(p.Position - v, p.Color)
            static member (+) (p : Point, v : V3f) = Point(p.Position + v, p.Color)
            static member (-) (v : V3f, p : Point) = Point(p.Position - v, p.Color)
            static member (+) (v : V3f, p : Point) = Point(p.Position + v, p.Color)



            new(p,c) = { Position = p; Color = c }
        end

    [<AllowNullLiteral>]
    type Node =
        class
            val mutable public Count : int
            val mutable public Points : dref<Point[]>
            val mutable public Children : dref<Node>[]



            static member Empty : Node = null
            static member Leaf(cnt, points) = Node(cnt, points, null)
            static member Node(cnt, points, children) = Node(cnt, points, children)


            private new(cnt, p,c) = { Count = cnt; Points = p; Children = c }
        end

    [<AutoOpen>]
    module ``Node Extensions`` =
        let (|Node|Leaf|Empty|) (n : Node) =
            if isNull n then Empty
            elif isNull n.Children then Leaf(n.Count, n.Points)
            else Node(n.Count, n.Points, n.Children)

        let Empty = Node.Empty
        let Leaf(cnt, points) = Node.Leaf(cnt, points)
        let Node(cnt, points, children) = Node.Node(cnt, points, children)


    type Octree = { db : Database; splitThreshold : int; cell : GridCell; count : int; offset : V3d; bounds : Box3d; root : dref<Node> }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module private OctreeNodeImp =

        let count (n : Node) =
            match n with
                | Empty -> 0
                | n -> n.Count

        let private cluster (cell : GridCell) (points : Point[]) =
            let splitPoint = cell.Center

            let clustered = Array.init 8 (fun i -> System.Collections.Generic.List<Point>(points.Length / 4))

            for i in 0..points.Length-1 do
                let p = points.[i]
                let index = 
                    let p = p.Position
                    (if float p.X >= splitPoint.X then 4 else 0) +
                    (if float p.Y >= splitPoint.Y then 2 else 0) +
                    (if float p.Z >= splitPoint.Z then 1 else 0)

                clustered.[index].Add(p)

            clustered |> Array.mapi (fun i c -> i, CSharpList.toArray c)

        let rec build (tree : Octree) (depth : int) (cell : GridCell) (points : Point[]) : Node =
            if points.Length = 0 then
                Empty

            elif points.Length < tree.splitThreshold then
                Leaf(points.Length, tree.db.NewRef(points))

            else
                if depth > 50 then
                    Log.warn "leaf with %d points" points.Length
                    Leaf(points.Length, tree.db.NewRef(points))
                else
                    let clustered = cluster cell points
                    let children = 
                        clustered |> Array.map (fun (i, childPoints) ->
                            let c = cell.GetChild i
                            tree.db.NewRef(build tree (depth + 1) c childPoints)
                        )
         
                    Node(points.Length, tree.db.NewRef [||], children)

        let rec addContained (tree : Octree) (depth : int) (cell : GridCell) (points : Point[]) (node : dref<Node>) =
            match node.Value with
                | Empty -> 
                    node.Value <- build tree depth cell points

                | Leaf(cnt, pts) ->
                    let newCnt = cnt + points.Length
                    let newPoints = Array.append pts.Value points
                    if newCnt < tree.splitThreshold then
                        pts.Value <- newPoints
                        node.Value <- Leaf(newCnt, pts)
                    else
                        pts.Delete()
                        node.Value <- build tree depth cell newPoints

                | Node(cnt, pts, children) ->
                    let newCnt = cnt + points.Length
                    
                    for (i, contained) in cluster cell points do
                        let child = children.[i]
                        let childCell = cell |> GridCell.child i
                        addContained tree (depth + 1) childCell contained child

                    node.Value <- Node(newCnt, pts, children)

        let rec mergeWith (tree : Octree) (depth : int) (cell : GridCell) (newTree : dref<Node>) (node : dref<Node>) =
            match node.Value with
                | Empty ->
                    node.Value <- newTree.Value
                    newTree.Delete()

                | Leaf(cnt, points) ->
                    node.Value <- newTree.Value
                    newTree.Delete()
                    addContained tree depth cell points.Value node

                | Node(oldCount, pts, oldChildren) ->
                    match newTree.Value with
                        | Empty ->
                            newTree.Delete()

                        | Leaf(c, pts) ->
                            newTree.Delete()
                            let points = pts.Value
                            pts.Delete()
                            addContained tree depth cell pts.Value node

                        | Node(newCount, newPts, newChildren) ->
                            newPts.Delete()
                            newTree.Delete()

                            for i in 0..7 do
                                let oldChild = oldChildren.[i]
                                let newChild = newChildren.[i]
                                let cell = cell.GetChild i
                                mergeWith tree (depth + 1) cell newChild oldChild 

                            node.Value <- Node(oldCount + newCount, pts, oldChildren)


        let rec postProcess (tree : Octree) (node : dref<Node>) : int * Point[] =
            match node.Value with
                | Empty -> 
                    0, [||]

                | Leaf(cnt,pts) ->
                    0, pts.Value

                | Node(cnt,pts,children) ->
                    let mutable maxDepth = 0
                    let childPoints = System.Collections.Generic.List()
                    for i in 0..children.Length-1 do
                        let d, pts = postProcess tree children.[i]
                        childPoints.AddRange(pts)
                        maxDepth <- max d maxDepth

                    if maxDepth = 0 && childPoints.Count <> cnt then
                        Log.warn "unexpected node-count: { was = %A; expected = %A }" childPoints.Count cnt

                    let d = maxDepth + 1

                    if childPoints.Count <> 0 then
                        if childPoints.Count > tree.splitThreshold then
                            let percent = float tree.splitThreshold / float childPoints.Count
                            let lodPoints = childPoints.TakeRandomly(percent) |> Seq.toArray
                            pts.Value <- lodPoints
                            d, lodPoints
                        else
                            let lodPoints = childPoints |> CSharpList.toArray
                            pts.Value <- lodPoints
                            d, lodPoints
                    else
                        Log.warn "empty inner node"
                        d, [||]


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module private OctreeImp =
        let empty (db : Database) (splitThreshold : int) = { db = db; splitThreshold = splitThreshold; cell = GridCell(); count = 0; offset = V3d.Zero; bounds = Box3d.Invalid; root = db.NewRef(Empty) }

        let isEmpty (t : Octree) =
            match t.root.Value with
                | Empty -> true
                | _ -> false

        let wrap (t : Octree) = 
            let parentCell = t.cell.Parent
            let orgIndex = t.cell.IndexInParent

            let children = 
                Array.init 8 (fun i -> 
                    if i = orgIndex then t.root
                    else t.db.NewRef(Empty)
                )

            let newRoot = Node(t.count, t.db.NewRef [||], children)
            { t with cell = parentCell; root = t.db.NewRef(newRoot) }

        let add (points : Point[]) (t : Octree) : Octree =
            if points.Length = 0 then 
                t

            elif isEmpty t then
                let bounds = Box3d(points |> Seq.map (fun p -> V3d p.Position))
                let cell = GridCell.ofBox bounds
                let root = OctreeNodeImp.build t 0 cell points
                t.root.Value <- root
                { t with bounds = bounds; count = points.Length; cell = cell }

            else
                
                let rootBounds = t.cell |> GridCell.bounds
                let inside, outside = points |> Array.partition (fun p -> rootBounds.Contains (V3d p.Position))

                let t = 
                    if inside.Length > 0 then
                        OctreeNodeImp.addContained t 0 t.cell inside t.root
                        { t with count = t.count + inside.Length}
                    else 
                        t
                let t = 
                    if outside.Length <> 0 then
                        let outsideBounds = Box3d (outside |> Seq.map (fun p -> V3d p.Position))
                        let newBounds = t.bounds.Union(outsideBounds)

                        let allInside(t : Octree) = 
                            let bb = t.cell |> GridCell.bounds
                            bb.Contains(outsideBounds)

                        let mutable res = wrap t
                        while not (allInside res) do
                            res <- wrap res

                        OctreeNodeImp.addContained res 0 res.cell outside res.root
                        { res with bounds = newBounds; count = res.count + outside.Length }
                    else
                        t

                if t.count <> t.root.Value.Count then
                    Log.warn "out of sync"

                t

        let mergeWith (other : Octree) (self : Octree) : Octree =
            let mutable smaller, greater = 
                if other.cell.Exp < self.cell.Exp then other, self
                else self, other

            while smaller.cell.Exp < greater.cell.Exp do
                smaller <- wrap smaller

            while smaller.cell <> greater.cell do
                smaller <- wrap smaller
                greater <- wrap greater

            OctreeNodeImp.mergeWith self 0 greater.cell smaller.root greater.root
            { greater with bounds = Box3d.Union(smaller.bounds, greater.bounds); count = smaller.count + greater.count }

        let postProcess (offset : V3d) (t : Octree) =
            OctreeNodeImp.postProcess t t.root |> ignore

            { t with offset = offset }


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Octree =
        open System.IO
        open System.Collections.Concurrent
        open System.Diagnostics

        let build (db : Database) (splitThreshold : int) (offset : V3d) (pointCount : int) (points : seq<Point>) =

            let maxInMemoryChunks = 16
            let chunkSize = 1 <<< 20
            
            let chunks = ConcurrentBag<int * Point[]>()
            let trees = ConcurrentBag<Octree>()
            use freeChunks = new SemaphoreSlim(maxInMemoryChunks)
            use producedChunks = new SemaphoreSlim(0)
            use producedTrees = new SemaphoreSlim(0)


            let totalChunks = ceil (float pointCount / float chunkSize) |> int
            let totalMerges = totalChunks - 1

            let mutable parsedPoints = 0
            let mutable parsedChunks = 0
            let mutable processedChunks = 0
            let mutable merged = 0

            let reporter =
                async {
                    do! Async.SwitchToNewThread()
                    while true do
                        Log.line "parser:   %.2f%%" (100.0 * float parsedPoints / float pointCount)
                        Log.line "building: %.2f%%" (100.0 * float processedChunks / float totalChunks)
                        Log.line "merge:    %.2f%%" (100.0 * float merged / float totalMerges)
                        Thread.Sleep(500)
                }


            let parser =
                async {
                    let sw = Stopwatch()

                    do! Async.SwitchToNewThread()
                    sw.Start()

                    for chunk in Seq.chunkBySize chunkSize points do
                        parsedPoints <- parsedPoints + chunk.Length
                        sw.Stop()
                        freeChunks.Wait() 
                        sw.Start()
                        chunks.Add(parsedChunks, chunk)
                        producedChunks.Release() |> ignore
                        parsedChunks <- parsedChunks + 1

                    sw.Stop()

                    return sw.Elapsed
                }

            let smallTreeBuilder =
                async {
                    let! ct = Async.CancellationToken
                    do! Async.SwitchToNewThread()
                    let sw = Stopwatch()
                    sw.Start()
                    while processedChunks < totalChunks do
                        sw.Stop()
                        producedChunks.Wait(ct)
                        sw.Start()
                        match chunks.TryTake() with
                            | (true, (i,chunk)) ->
                                let mutable tree = OctreeImp.empty db splitThreshold
                                tree <- OctreeImp.add chunk tree
                                trees.Add(tree)
                                producedTrees.Release() |> ignore

                                freeChunks.Release() |> ignore
                                Interlocked.Increment(&processedChunks) |> ignore
                            | _ ->
                                producedChunks.Release() |> ignore
                    sw.Stop()
                    return sw.Elapsed
                }
            
            let merger =
                async {
                    do! Async.SwitchToNewThread()
                    let sw = Stopwatch()
                    sw.Start()
                    while merged < totalMerges do
                        producedTrees.Wait(); producedTrees.Wait()
                        match trees.TryTake(), trees.TryTake() with
                            | (true, l), (true, r) ->
                                Log.startTimed "merge running"
                                let res = OctreeImp.mergeWith l r
                                Log.stop()
                                trees.Add res
                                producedTrees.Release() |> ignore
                                Interlocked.Increment(&merged) |> ignore

                            | (true, t), _ | _, (true, t) ->
                                trees.Add t
                                producedTrees.Release() |> ignore
                                producedTrees.Release() |> ignore

                            | _ ->
                                producedTrees.Release() |> ignore
                                producedTrees.Release() |> ignore
                    sw.Stop()
                    return sw.Elapsed
                }


            use cts = new CancellationTokenSource()
            Async.Start(reporter, cancellationToken = cts.Token)

            let sw = Stopwatch()
            sw.Start()
            
            let threads = Environment.ProcessorCount / 2 |> max 2
            let parser = Async.StartAsTask parser
            let smallTreeBuilders = Array.init threads (fun _ -> Async.StartAsTask smallTreeBuilder)
            let mergers = Array.init threads (fun _ -> Async.StartAsTask merger)
            

            let parseTime = parser.Result.TotalSeconds
            let buildTime = smallTreeBuilders |> Array.sumBy (fun t -> t.Result.TotalSeconds)
            let mergeTime = mergers |> Array.sumBy (fun t -> t.Result.TotalSeconds)
            sw.Stop()

            Log.line "wallclock:     %.3fs" sw.Elapsed.TotalSeconds
            Log.line "parsing took:  %.3fs" parseTime
            Log.line "building took: %.3fs" buildTime
            Log.line "merging took:  %.3fs" mergeTime
            
            Log.line "remaining: %A" trees.Count
            match trees.TryTake() with
                | (true, v) -> Log.line "tree: %A" v
                | _ -> Log.warn "no tree"

            cts.Cancel()
            Environment.Exit 0

            let mutable tree = OctreeImp.empty db splitThreshold

            let chunkSize = 1048576
            let mutable buffer = Array.zeroCreate chunkSize

            let mutable pointsProcessed = 0

            Log.startTimed "build"
            let e = points.GetEnumerator()
            while e.MoveNext() do
                let mutable cnt = 0
                buffer.[cnt] <- e.Current
                cnt <- cnt + 1
                while cnt < chunkSize && e.MoveNext() do
                    buffer.[cnt] <- e.Current
                    cnt <- cnt + 1

                if cnt <> chunkSize then
                    Array.Resize(&buffer, cnt)

                pointsProcessed <- pointsProcessed + cnt
                tree <- OctreeImp.add buffer tree
                Log.line "progress: %.3f%%" (100.0 * float pointsProcessed / float pointCount)

            e.Dispose()
            Log.stop()

            OctreeImp.postProcess offset tree



    module Pts = 
        open System.IO

        let exps =
            Array.init 64 (fun i ->
                pown 10.0 (-i)
            )

        let rec private trimWs (str : string, start : byref<int>) =
            if start < str.Length then 
                let c = str.[start]
                if c = ' ' || c = '\t' then
                    start <- start + 1
                    trimWs(str, &start)

        let rec private trimToFirst (str : string, search : char, start : byref<int>) =
            if start < str.Length then 
                let c = str.[start]
                if c = ' ' || c = '\t' then
                    start <- start + 1
                    trimToFirst(str, search, &start)
                elif c = search then
                    start <- start + 1
                    true
                else
                    false
            else
                false

        let rec private readIntAcc (str : string, current : int, pos : bool, start : byref<int>) =
            if start < str.Length then
                let c = str.[start]
                start <- start + 1

                if c = '+' then 
                    readIntAcc (str, current, pos, &start)
                elif c = '-' then
                    readIntAcc (str, current, false, &start)
                else
                    let i = int c - 48
                    if i >= 0 && i < 10 then
                        readIntAcc (str, 10 * current + i, pos, &start)
                    else
                        start <- start - 1
                        if pos then current
                        else -current
            else
                if pos then current
                else -current

        let private readInt (str : string, start : byref<int>) =
            trimWs(str, &start)
            readIntAcc(str, 0, true, &start)

        let private readDouble (str : string, start : byref<int>) =

            let real = readInt(str, &start)

            let s = start
            let worked = trimToFirst(str, '.', &start)
            if worked then 
                trimWs(str, &start)
                let s = start
                let frac = readInt(str, &start)
            
                let exp = start - s

                if frac = 0 then float real
                elif real > 0 then float real + float frac * exps.[exp]
                elif real < 0 then float real - float frac * exps.[exp]
                else float frac * exps.[exp]
            else 
                start <- s
                float real

        let inline private tryReadPoint (offset : V3d, str : string) =
            let mutable start = 0
            let x = readDouble(str, &start)
            let y = readDouble(str, &start)
            let z = readDouble(str, &start)
            readInt(str, &start) |> ignore
            let r = readInt(str, &start) |> byte
            let g = readInt(str, &start) |> byte
            let b = readInt(str, &start) |> byte
            let point = Point(V3f(x - offset.X,y - offset.Y,z - offset.Z), C4b(r,g,b,255uy))
            Some point

        let inline private readOffset (str : string) =
            let mutable start = 0
            let x = readDouble(str, &start)
            let y = readDouble(str, &start)
            let z = readDouble(str, &start)
            V3d(x,y,z)


        let read (file : string) =
            let stream = File.OpenRead(file)
            let reader = new StreamReader(stream)
            let cnt = reader.ReadLine().Trim() |> Int32.Parse
            if cnt > 0 then
                let line = reader.ReadLine()
                let off = readOffset line
                let points = 
                    seq {
                        try
                            match tryReadPoint(off, line) with
                                | Some p -> yield p
                                | None -> ()

                            for i in 2..cnt do
                                let line = reader.ReadLine()
                                match tryReadPoint(off, line) with
                                    | Some p -> yield p
                                    | None -> ()
                        finally
                            reader.Dispose()
                            stream.Dispose()
                    }
                off, cnt, points
            else
                V3d.Zero, 0, Seq.empty

        let storeRandom (size : int) (file : string) =
            let rand = Random()

            use stream = File.OpenWrite(file)
            use writer = new StreamWriter(stream)

            let cnt = size * size * size
            writer.WriteLine(string cnt)
            let builder = System.Text.StringBuilder()


            let sem = new SemaphoreSlim(0)
            let mutable remaining = cnt
            
            let queued = new SemaphoreSlim(65536)

            let points = System.Collections.Concurrent.ConcurrentBag<int * string>()

            let finished = new SemaphoreSlim(0)
            let producer =
                async {
                    do! Async.SwitchToNewThread()
                    let mutable run = true
                    while run do
                        queued.Wait()
                        let mutable created = 0
                        let builder = System.Text.StringBuilder()
                        while run && created < 65536 do
                            let id = Interlocked.Decrement(&remaining)
                            if id >= 0 then
                                let x = id / (size * size)
                                let id = id % (size * size)
                                let y = id / size
                                let id = id % size
                                let z = id

                                let r = rand.Next(256) |> byte
                                let g = rand.Next(256) |> byte
                                let b = rand.Next(256) |> byte
                    
                                builder.AppendFormat("{0:0.00000000} {1:0.00000000} {2:0.00000000} 0 {3} {4} {5}\r\n", x, y, z, r, g, b) |> ignore                            
                                created <- created + 1
                            else
                                run <- false

                        points.Add (created, builder.ToString())
                        sem.Release() |> ignore

                    finished.Release() |> ignore
                }

            let threads = 12
            for i in 1..threads do
                Async.Start producer
            

            let mutable i = 0
            while i < cnt do
                sem.Wait()
                match points.TryTake() with
                    | (true, (c, str)) -> 
                        queued.Release() |> ignore
                        writer.Write(str)
                        Log.line "wrote %.2f%%" (100.0 * float i / float cnt)
                        i <- i + c
                    | _ -> ()

        let createGrid (size : int) =
            let rand = Random()

            let cnt = size * size * size
            let sem = new SemaphoreSlim(0)
            let mutable remaining = cnt
            
            let queued = new SemaphoreSlim(65536)

            let points = System.Collections.Concurrent.ConcurrentBag<System.Collections.Generic.List<Point>>()

            let finished = new SemaphoreSlim(0)
            let producer =
                async {
                    do! Async.SwitchToNewThread()
                    let mutable run = true
                    while run do
                        queued.Wait()
                        let mutable created = 0
                        let builder = System.Collections.Generic.List<Point>()
                        while run && created < 65536 do
                            let id = Interlocked.Decrement(&remaining)
                            if id >= 0 then
                                let x = id / (size * size)
                                let id = id % (size * size)
                                let y = id / size
                                let id = id % size
                                let z = id

                                let r = rand.Next(256) |> byte
                                let g = rand.Next(256) |> byte
                                let b = rand.Next(256) |> byte
                    
                                builder.Add(Point(V3f(x,y,z), C4b(r,g,b,255uy)))
                                //builder.AppendFormat("{0:0.00000000} {1:0.00000000} {2:0.00000000} 0 {3} {4} {5}\r\n", x, y, z, r, g, b) |> ignore                            
                                created <- created + 1
                            else
                                run <- false

                        points.Add (builder)
                        sem.Release() |> ignore

                    finished.Release() |> ignore
                }

            let threads = 12
            for i in 1..threads do
                Async.Start producer
            
            let points = 
                seq {
                    let mutable i = 0
                    while i < cnt do
                        sem.Wait()
                        match points.TryTake() with
                            | (true, (pts)) -> 
                                yield! pts
                                i <- i + pts.Count
                            | _ -> ()
                }

            V3d.Zero, cnt, points


    // @"D:\Sonstiges\Laserscan-P20_Beiglboeck-2015.pts"






let run() =
    Aardvark.Init()
//    Pts.storeRandom 1024 @"E:\random.pts"
//    Environment.Exit 0

    let db = new Database(BlobStore.zip @"E:\store.zip" |> BlobStore.buffered)

    let top = db.Ref("tree")

    if not top.HasValue then
        let offset, cnt, pts = Pts.read @"D:\Sonstiges\Laserscan-P20_Beiglboeck-2015.pts"
        let tree = Octree.build db 5000 offset cnt pts
        top.Value <- tree

    let tree = top.Value
    printfn "tree: %A" tree
    printfn "root: %A" tree.root.Value.Count
    db.Dispose()

    //System.IO.File.Delete @"C:\Users\Schorsch\Desktop\data.zip"
    Environment.Exit 0

//    let db = new Database(BlobStore.zip @"C:\Users\schorsch\Desktop\test.zip")
//
//
//
//    let ref = db.Ref("a")
//
//    if not ref.HasValue then
//        printfn "create"
//        let tree = Node(db.NewRef(Leaf 1), 2, db.NewRef(Leaf 3))
//        ref.Value <- tree
//
//    printfn "ref = %A" ref.Value
//
//
//    db.Dispose()
//    Environment.Exit 0

    let app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()


    let quadSg =
        let index = [|0;1;2; 0;2;3|]
        let flatten (a : 'a[]) =
            index |> Array.map (fun i -> a.[i]) :> Array

        let quad =
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions,                  flatten [| V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]
                        DefaultSemantic.Normals,                    flatten [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |]
                        DefaultSemantic.DiffuseColorCoordinates,    flatten [| V2f.OO; V2f.IO; V2f.II; V2f.OI |]
                    ]
            )
                
        quad |> Sg.ofIndexedGeometry


    let sg = 
        Sg.group' [
            for x in -3 .. 3 do
                for y in -3 .. 3 do
                    let t = Trafo3d.Translation(3.0 * float x, 3.0 * float y, 0.0)
                    yield quadSg |> Sg.trafo (Mod.constant t)
        ]

    let view = CameraView.LookAt(V3d(-2.0,-2.0,2.0), V3d.Zero, V3d.OOI)
    let proj = 
        win.Sizes 
            |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 500.0 (float s.X / float s.Y))


    let view = DefaultCameraController.control win.Mouse win.Keyboard win.Time view


    let sg =
        sg |> Sg.effect [
            DefaultSurfaces.trafo |> toEffect
            DefaultSurfaces.constantColor C4f.Red |> toEffect
           ]
           |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
           |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)


    let renderJobs = sg.RenderObjects()

    let opt = renderJobs |> Optimizations.findIndirectDraws win.FramebufferSignature app.Runtime

    win.RenderTask <- app.Runtime.CompileRender(win.FramebufferSignature, opt) |> DefaultOverlays.withStatistics
    win.Run()







