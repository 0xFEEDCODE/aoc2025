module aoc2024.util

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open Microsoft.Extensions.Configuration
open System.Net.Http

open System.Threading

let smapi x = x |> Seq.mapi (fun i x -> (i, x))

let lw (a, b) = (lazy a, lazy b)
let (?) is_true (a, b) = if is_true then a else b
let ($) isTrue (a: Lazy<'a>, b: Lazy<'a>) = if isTrue then a.Force() else b.Force()
let inline (+=) (x: byref<_>) y = x <- x + y
let inline (-=) (x: byref<_>) y = x <- x - y

let inline ONE<'a when 'a: (static member One: 'a)> : 'a =
    LanguagePrimitives.GenericOne

let inline ZERO<'a when 'a: (static member Zero: 'a)> : 'a =
    LanguagePrimitives.GenericZero

module Seq =
    let str = String "st"

    let removeFirst predicate seq =
        let idx = seq |> Seq.findIndex predicate
        seq |> Seq.removeAt idx

    let removeLast predicate seq =
        let idx = seq |> Seq.findIndexBack predicate
        seq |> Seq.removeAt idx

    let removeAllItemsThat predicate seq = seq |> Seq.where (not << predicate)

    let cartesianProduct sequences =
        let (|SeqEmpty|SeqCons|) xs =
            if Seq.isEmpty xs then
                SeqEmpty
            else
                SeqCons(xs |> Seq.head, xs |> Seq.skip 1)

        let rec loop acc =
            function
            | SeqEmpty -> acc
            | SeqCons(h, t) ->
                loop
                    (h
                     |> Seq.collect (fun x ->
                         Seq.map
                             (fun y ->
                                 seq {
                                     yield x
                                     yield! y
                                 })
                             acc))
                    t

        loop (seq { Seq.empty }) sequences

    let cartesianProductRepeat nRepeat sequences =
        cartesianProduct (sequences |> Seq.replicate nRepeat)



module List =
    let rec cartesianWithRepeat lists =
        //seq { for x in xs do for y in ys -> x, y  }
        let rec cp acc =
            function
            | [] -> acc
            | h :: t -> cp (h |> List.collect (fun x -> List.map (fun y -> x :: y) acc)) t

        cp lists


    let cartesianProduct lists =
        let rec cp acc =
            function
            | [] -> acc
            | h :: t -> cp (h |> List.collect (fun x -> List.map (fun y -> x :: y) acc)) t

        cp [ [] ] lists

    let cartesianProductRepeat nRepeat sequences =
        cartesianProduct (sequences |> List.replicate nRepeat)

module String =
    let extractNum str =
        Regex.Match(str, @"^-?[0-9]\d*(\.\d+)?$").Value |> int

    let extractAllNums str =
        Regex.Matches(str, @"-?[0-9]\d*(\.\d+)?")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value |> int)
        |> Seq.toList

    let extractAllNumsUint64 str =
        Regex.Matches(str, @"-?[0-9]\d*(\.\d+)?")
        |> Seq.cast<Match>
        |> Seq.map _.Value
        |> Seq.map (UInt64.Parse)
        |> Seq.toList

    let extractAllNumsBig str =
        Regex.Matches(str, @"-?[0-9]\d*(\.\d+)?")
        |> Seq.cast<Match>
        |> Seq.map _.Value
        |> Seq.map bigint.Parse
        |> Seq.toList

    let permute (data: string) =
        let swap (i: int) (j: int) (data: StringBuilder) =
            let temp = data[i]
            data[i] <- data[j]
            data[j] <- temp

        let mutable acc = List.empty

        let rec perm k (sb: StringBuilder) =
            if k = 1 then
                let str = sb.ToString()

                if not (acc |> List.contains str) then
                    acc <- str :: acc
            else
                let sb_copy = StringBuilder().Append(sb)
                perm (k - 1) sb_copy

                for i = 0 to (k - 1) do
                    swap (k % 2 = 0)?(i, 0) (k - 1) sb

                    perm (k - 1) sb

        perm data.Length (StringBuilder(data))
        acc


let perm (data: 'a seq) =
    let swap (i: int) (j: int) (data: 'a array) =
        let temp = data[i]
        data[i] <- data[j]
        data[j] <- temp

    let mutable acc = List.empty

    let rec perm k data =
        if k = 1 then
            acc <- data :: acc
        else
            perm (k - 1) (data |> Seq.toArray)

            for i = 0 to (k - 1) do
                swap (k % 2 = 0)?(i, 0) (k - 1) data

                perm (k - 1) data

    perm (data |> Seq.length) (data |> Seq.toArray)
    acc |> List.toSeq

type Point2D =
    struct
        val x: int
        val y: int

        new(x: int, y: int) = { x = x; y = y }

        member this.GetManhattanDistance(other: Point2D) =
            abs (this.x - other.x) + abs (this.y - other.y)

        override this.ToString() = $"[{this.x};{this.y}]"
        static member (+)(a: Point2D, b: Point2D) = Point2D(a.x + b.x, a.y + b.y)
        static member (-)(a: Point2D, b: Point2D) = Point2D(a.x - b.x, a.y - b.y)
    end

type Point3D =
    struct
        val x: int
        val y: int
        val z: int
        new(x: int, y: int, z: int) = { x = x; y = y; z = z }
        override this.ToString() = $"[{this.x};{this.y};{this.z}]"

        static member (+)(a: Point3D, b: Point3D) =
            Point3D(a.x + b.x, a.y + b.y, a.z + b.z)

        static member (-)(a: Point3D, b: Point3D) =
            Point3D(a.x - b.x, a.y - b.y, a.z - b.z)
    end

type Line =
    struct
        val s: Point2D
        val e: Point2D

        new(s: Point2D, e: Point2D) = { s = s; e = e }
        override this.ToString() = $"Start: {this.s}; End: {this.e}"

        member private this.IntersectsAtAxis startA endA startB endB =
            let start1 = (min startA startB)
            let start2 = (max startA startB)
            let end1 = (min endA endB)
            let end2 = (max endA endB)

            start2 <= end1 && start1 <= start2 && end1 <= end2

        member this.Intersects(other: Line) =
            let ax1, ay1, ax2, ay2 = (this.s.x, this.s.y, this.e.x, this.e.y)
            let bx1, by1, bx2, by2 = (other.s.x, other.s.y, other.e.x, other.e.y)

            if (ax1 = ax2 && bx1 = bx2 && ax1 = bx1) then
                this.IntersectsAtAxis ay1 ay2 by1 by2
            elif (ay1 = ay2 && by1 = by2 && ay1 = by1) then
                this.IntersectsAtAxis ax1 ax2 bx1 bx2
            else
                false

        member this.GetIntersectingLine(other: Line) =
            let ax1, ay1, ax2, ay2 = (this.s.x, this.s.y, this.e.x, this.e.y)
            let bx1, by1, bx2, by2 = (other.s.x, other.s.y, other.e.x, other.e.y)

            let getIntersect startA endA startB endB =
                let start2 = (max startA startB)
                let end1 = (min endA endB)
                (start2, end1)

            if (ax1 = ax2 && bx1 = bx2 && ax1 = bx1 && this.IntersectsAtAxis ay1 ay2 by1 by2) then
                let isectStart, isectEnd = getIntersect ay1 ay2 by1 by2
                Line(Point2D(ax1, isectStart), Point2D(ax2, isectEnd)) |> Some
            elif (ay1 = ay2 && by1 = by2 && ay1 = by1 && this.IntersectsAtAxis ax1 ax2 bx1 bx2) then
                let isectStart, isectEnd = getIntersect ax1 ax2 bx1 bx2
                Line(Point2D(isectStart, ay1), Point2D(isectEnd, ay2)) |> Some
            else
                None

        member this.GetIntersectingLength(other: Line) =
            let ax1, ay1, ax2, ay2 = (this.s.x, this.s.y, this.e.x, this.e.y)
            let bx1, by1, bx2, by2 = (other.s.x, other.s.y, other.e.x, other.e.y)

            let getIntersect startA endA startB endB =
                let start2 = (max startA startB)
                let end1 = (min endA endB)
                (start2, end1)

            if (ax1 = ax2 && bx1 = bx2 && ax1 = bx1 && this.IntersectsAtAxis ay1 ay2 by1 by2) then
                let isectStart, isectEnd = getIntersect ay1 ay2 by1 by2
                1 + (isectEnd - isectStart) |> Some
            elif (ay1 = ay2 && by1 = by2 && ay1 = by1 && this.IntersectsAtAxis ax1 ax2 bx1 bx2) then
                let isectStart, isectEnd = getIntersect ax1 ax2 bx1 bx2
                1 + (isectEnd - isectStart) |> Some
            else
                None
    end

type aocIO(year) =
    let year = year
    let httpClient = new HttpClient()

    let sessionToken =
        let secrets =
            ConfigurationBuilder().AddUserSecrets(Assembly.GetExecutingAssembly()).Build()

        (secrets.AsEnumerable() |> Seq.find (fun s -> s.Key = "tokenAOC")).Value

    let getCallerModuleName () =
        let st = StackTrace()

        let frameIdx =
            ((+) 1 |> Seq.initInfinite)
            |> Seq.skipWhile (fun i -> st.GetFrame(i).GetMethod().ReflectedType.Name = "aocIO")
            |> Seq.head

        st.GetFrame(frameIdx).GetMethod().ReflectedType.Name

    let getPathToInputFile moduleName =
        Path.Combine(Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName, $"{moduleName}.txt")

    member this.readInput() =
        let moduleName = getCallerModuleName ()
        File.ReadLines(getPathToInputFile moduleName)

    member this.getInput(?overrideIfExists: bool) =
        let overrideIfExists = defaultArg overrideIfExists false

        let moduleName = getCallerModuleName ()
        let pathToInputFile = getPathToInputFile moduleName

        let mutable shouldDownloadInput = true

        if (not overrideIfExists) then
            if (File.Exists pathToInputFile) then
                shouldDownloadInput <- false

        if shouldDownloadInput then
            printfn "Downloading"

            let urlDayPart =
                if moduleName.StartsWith "day0" then
                    moduleName[4..]
                else
                    moduleName[3..]

            let url = $"https://adventofcode.com/{year}/day/{urlDayPart}/input"
            let request = new HttpRequestMessage(HttpMethod.Get, url)
            request.Headers.Add("Cookie", $"session={sessionToken}")
            let cts = new CancellationTokenSource()

            printfn $"Getting input from: %A{url}"
            let response = httpClient.Send request

            if (not response.IsSuccessStatusCode) then
                failwith $"Could not get input, http error - {response.StatusCode}"

            let content = response.Content.ReadAsStringAsync(cts.Token)
            content |> Async.AwaitTask |> ignore
            File.WriteAllText(pathToInputFile, content.Result)

        this.readInput ()

    member this.submitAnswer part answer =
        let moduleName = getCallerModuleName ()

        let urlDayPart =
            if moduleName.StartsWith "day0" then
                moduleName[4..]
            else
                moduleName[3..]

        let url = $"https://adventofcode.com/{year}/day/{urlDayPart}/answer"
        let request = new HttpRequestMessage(HttpMethod.Post, url)
        request.Headers.Add("Cookie", $"session={sessionToken}")

        let payload = List<KeyValuePair<string, string>>()
        payload.Add(KeyValuePair<string, string>("level", part.ToString()))
        payload.Add(KeyValuePair<string, string>("answer", answer.ToString()))
        request.Content <- new FormUrlEncodedContent(payload)

        let cts = new CancellationTokenSource()
        let response = httpClient.Send request

        if (not response.IsSuccessStatusCode) then
            failwith $"Could submit answer, http error - {response.StatusCode}"

        let content = response.Content.ReadAsStringAsync(cts.Token)
        content |> Async.AwaitTask |> ignore

        let html = content.Result
        let pattern = "(?i)(?s)<article><p>(?<content>.*?)</p></article>"
        let m = Regex.Match(html, pattern)

        if m.Success then
            let content = m.Groups.["content"].Value
            printfn $"%A{content}"
            content
        else
            failwith "wtf"


module Grid =
    let getNRowsAndNCols grid =
        (grid |> Seq.length, grid |> Seq.head |> Seq.length)

    let iter action (source: array<'a> array) =
        let nRows, nCols = getNRowsAndNCols source

        for y in 0 .. nRows - 1 do
            for x in 0 .. nCols - 1 do
                action x y

    let countBy action (source: array<'a> array) =
        source |> Seq.collect id |> Seq.countBy action

    let initializeFromStringSeq (source: string seq) =
        let grid =
            Array.zeroCreate<Array> (source |> Seq.length)
            |> Seq.map (fun _ -> Array.zeroCreate<'a> (source |> Seq.head |> Seq.length))
            |> Seq.toArray

        let mutable y = 0

        for line in source do
            let mutable x = 0

            for ch in line do
                grid[y][x] <- ch
                &x += 1

            &y += 1

        grid

    let createGrid<'a> nRows nCols =
        Array.zeroCreate<Array> nRows
        |> Seq.map (fun _ -> Array.zeroCreate<'a> nCols)
        |> Seq.toArray

    let createGridDV<'a> nRows nCols (initValue: 'a) =
        Array.zeroCreate<Array> nRows
        |> Seq.map (fun _ -> Array.create<'a> nCols initValue)
        |> Seq.toArray

    let getAdjacentNeighbours (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

    let getDiagonalNeighbours (point: Point2D) =
        [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

    let getAllNeighbours (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1); (1, 1); (-1, 1); (1, -1); (-1, -1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray


module Math =
    let inline gcd a b =
        let rec gcd a b =
            if a <> LanguagePrimitives.GenericZero then
                gcd (b % a) a
            else
                b

        gcd a b

    let inline lcm a b = (a * b) / (gcd a b)


module Point2D =
    let getAdjacentNeighbours (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

    let getDiagonalNeighbours (point: Point2D) =
        [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

    let getAllNeighbours (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1); (1, 1); (-1, 1); (1, -1); (-1, -1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

type Pair<'a, 'b> =
    struct
        val Item1: 'a
        val Item2: 'b

        new(item1, item2) = { Item1 = item1; Item2 = item2 }
    end

type Triple<'a, 'b, 'c> =
    struct
        val Item1: 'a
        val Item2: 'b
        val Item3: 'c

        new(item1, item2, item3) =
            { Item1 = item1
              Item2 = item2
              Item3 = item3 }
    end

type Quad<'a, 'b, 'c, 'd> =
    struct
        val Item1: 'a
        val Item2: 'b
        val Item3: 'c
        val Item4: 'd

        new(item1, item2, item3, item4) =
            { Item1 = item1
              Item2 = item2
              Item3 = item3
              Item4 = item4 }
    end

let inline spair a b = Pair(a, b)
let inline striple a b c = Triple(a, b, c)
let inline squad a b c d = Quad(a, b, c, d)

let inline spair_fst (pair: Pair<_, _>) = pair.Item1
let inline spair_snd (pair: Pair<_, _>) = pair.Item2

let inline spair_ref_fst (pair: Pair<_, _> byref) = pair.Item1
let inline spair_ref_snd (pair: Pair<_, _> byref) = pair.Item2

let inline striple_fst (triple: Triple<_, _, _>) = triple.Item1
let inline striple_snd (triple: Triple<_, _, _>) = triple.Item2
let inline striple_trd (triple: Triple<_, _, _>) = triple.Item3

let inline striple_ref_fst (triple: Triple<_, _, _> byref) = triple.Item1
let inline striple_ref_snd (triple: Triple<_, _, _> byref) = triple.Item2
let inline striple_ref_trd (triple: Triple<_, _, _> byref) = triple.Item3

let inline squad_fst (quad: Quad<_, _, _, _>) = quad.Item1
let inline squad_snd (quad: Quad<_, _, _, _>) = quad.Item2
let inline squad_trd (quad: Quad<_, _, _, _>) = quad.Item3
let inline squad_fth (quad: Quad<_, _, _, _>) = quad.Item4

let inline squad_ref_fst (quad: Quad<_, _, _, _> byref) = quad.Item1
let inline squad_ref_snd (quad: Quad<_, _, _, _> byref) = quad.Item2
let inline squad_ref_trd (quad: Quad<_, _, _, _> byref) = quad.Item3
let inline squad_ref_fth (quad: Quad<_, _, _, _> byref) = quad.Item4

let aocIO = aocIO 2024
