module aoc2025.day07

open System.Collections.Generic
open aoc2025.util

let useSampleInput = 0

let fall (s: int * int) (gr: char array array) =
    let bound = gr |> Grid.getBoundary

    let iib boundary point =
        snd point >= boundary.minX
        && snd point <= boundary.maxX
        && fst point >= boundary.minY
        && fst point <= boundary.maxY

    let iib = iib bound

    let mutable splitPos = Dictionary<int * int, bool>()

    for y in 0 .. ((gr |> Seq.length) - 1) do
        for x in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
            if gr[y][x] = '^' then
                splitPos[(y, x)] <- true

    let mutable memo = Dictionary<(int * int) * (int * int), double>()
    let mutable splitsFound = HashSet<(int * int)>()

    let rec loop pos =
        let cy, cx = pos

        if not (iib pos) then
            double 0
        else
            let splits = splitPos.Keys |> Seq.where (fun (sy, sx) -> sx = cx && sy >= cy)

            if splits |> Seq.isEmpty then
                double 1
            else
                let split = splits |> Seq.minBy fst
                let sy, sx = split

                splitsFound.Add(split) |> ignore

                let l = (sy, sx - 1)
                let r = (sy, sx + 1)

                let getResultForPos newPos =
                    let value =
                        if memo.ContainsKey(pos, newPos) then
                            memo[(pos, newPos)]
                        else
                            loop newPos

                    if not (memo.ContainsKey(pos, newPos)) then
                        memo[(pos, newPos)] <- value

                    value

                let lr = getResultForPos l
                let rr = getResultForPos r

                double (lr + rr)

    let nLoops = (loop s)
    let nSplits = splitsFound.Count
    (nSplits, nLoops)

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let gr = inp |> Grid.initializeFromStringSeq
    let mutable s = (0, 0)

    for y in 0 .. ((gr |> Seq.length) - 1) do
        for x in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
            if gr[y][x] = 'S' then
                s <- (y, x)

    let (ans1, ans2) = fall s gr
    printfn $"%A{ans1 |> uint64}"
    printfn $"%A{ans2 |> uint64}"
    0
