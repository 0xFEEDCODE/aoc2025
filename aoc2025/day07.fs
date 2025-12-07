module aoc2025.day07

open System.Collections.Generic
open aoc2025.util

let useSampleInput = 0

(*let fall1 (s: P2D) (gr: char array array) =
    let bound = gr |> Grid.getBoundary
    let iib = Grid.isInBounds bound

    let mutable gr = gr

    let mutable nSplits = Dictionary<P2D, bool>()

    let rec loop pos =
        if pos.y >= bound.maxY then
            1
        else
            let newPos = ((pos.y + 1, pos.x).AsP2DYX())

            let cons = (nSplits.ContainsKey(newPos))
            let cons = false

            let isSplit = (iib newPos) && gr[newPos.y][newPos.x] = '^' && not cons

            if isSplit then
                let l = (newPos.y, pos.x - 1).AsP2DYX()
                let r = (newPos.y, pos.x + 1).AsP2DYX()

                nSplits[newPos] <- true

                match (iib l && gr[l.y][l.x] <> '^', iib r && gr[r.y][r.x] <> '^') with
                | (true, true) -> loop l + loop r
                | (true, false) -> loop l
                | (false, true) -> loop r
            else if iib newPos && not cons then
                loop newPos
            else
                0

    loop s
//nSplits.Count*)

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

    let mutable res = Dictionary<(int * int) * (int * int), double>()

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

                let l = (sy, sx - 1)
                let r = (sy, sx + 1)

                let lr = if res.ContainsKey(pos, l) then res[(pos, l)] else loop l
                let rr = if res.ContainsKey(pos, r) then res[(pos, r)] else loop r


                if not (res.ContainsKey(pos, r)) then
                    res[(pos, r)] <- lr

                if not (res.ContainsKey(pos, l)) then
                    res[(pos, l)] <- rr

                double (lr + rr)

    (loop s)

(*
    let rec loop pos =
        let cy, cx = pos

        if cy >= bound.maxY then
            1
        else
            let mutable y = cy

            while (gr[y][cx] <> '^' && (y < bound.maxY)) do
                y <- y + 1

            let (newPos) = (y, cx)
            let (ny, nx) = newPos

            let isSplit = (iib newPos) && gr[ny][nx] = '^'

            if isSplit then
                let l = (ny, cx - 1)
                let ly, lx = l
                let r = (ny, cx + 1)
                let ry, rx = r

                match (iib l && gr[ly][lx] <> '^', iib r && gr[ry][rx] <> '^') with
                | (true, true) -> loop l + loop r
                | (true, false) -> loop l
                | (false, true) -> loop r
            else if iib newPos then
                loop newPos
            else
                0

    loop s
    *)


(*
    
    let q = Queue()
    q.Enqueue s
    
    while (q.Count > 0) do
        let cy, cx = q.Dequeue()
        
        let mutable y = cy
        
        if cy >= bound.maxY then
            count <- count + 1

        while (gr[y][cx] <> '^' && (y < bound.maxY)) do
            y <- y + 1

        let (newPos) = (y, cx)
        let (ny, nx) = newPos
        
        let isSplit = (iib newPos) && gr[ny][nx] = '^'

        if isSplit then
            let l = (ny, cx - 1)
            let ly, lx = l
            let r = (ny, cx + 1)
            let ry, rx = r

            if iib l && gr[ly][lx] <> '^' then
              q.Enqueue(l)
            if iib r && gr[ry][rx] <> '^' then
              q.Enqueue(r)

    count*)
//nSplits.Count


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

    let ans1 = fall s gr
    printfn $"%A{ans1 |> uint64}"
    0
