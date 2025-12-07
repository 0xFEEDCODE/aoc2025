module aoc2025.day04

open aoc2025.util
open aoc2025.util.Grid

let useSampleInput = 0

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let gr = inp |> Grid.initializeFromStringSeq
    let bound = gr |> Grid.getBoundary

    let iib = isInBounds bound

    let mutable ans1 = 0


    for y in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
        for x in 0 .. ((gr |> Seq.length) - 1) do
            let p = (x, y).AsP2DXY()

            if gr[y][x] = '@' then
                let neigh = p |> P2D.getAllNeighbours

                let c =
                    (0, neigh)
                    ||> Seq.fold (fun acc x ->
                        if (iib x) then
                            let (nx, ny) = (x.x, x.y)
                            acc + (if (gr[ny][nx] = '@') then 1 else 0)
                        else
                            acc)

                if c < 4 then
                    &ans1 += 1

    printfn $"%A{ans1}"


    let mutable c = 0
    let mutable ans2 = 0

    while c <> -1 do
        c <- 0

        for y in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
            for x in 0 .. ((gr |> Seq.length) - 1) do
                let p = (x, y).AsP2DXY()

                if gr[y][x] = '@' then
                    let neigh = p |> P2D.getAllNeighbours

                    let tc =
                        (0, neigh)
                        ||> Seq.fold (fun acc x ->
                            if (iib x) then
                                let (nx, ny) = (x.x, x.y)
                                acc + (if (gr[ny][nx] = '@') then 1 else 0)
                            else
                                acc)

                    if tc < 4 then
                        &c += 1
                        gr[y][x] <- '.'

        &ans2 += c
        c <- if c = 0 then -1 else c

    printfn $"%A{ans2}"
    0
