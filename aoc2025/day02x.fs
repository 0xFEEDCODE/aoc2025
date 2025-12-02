module aoc2025.day03

open System.Collections.Generic
open aoc2025.util

let bff sp goal obst bounds =
    let explore = Queue()
    let paths = Dictionary<P2D, Option<P2D>>()

    explore.Enqueue sp
    paths[sp] <- None

    while explore.Count > 0 do
        let c = explore.Dequeue()

        if not (obst |> Seq.contains (c)) && c <> goal then
            for next in c |> P2D.getAdjacentNeighbours do
                if (next |> Grid.isInBounds bounds) && not (paths.ContainsKey(next)) then
                    explore.Enqueue next
                    paths[next] <- Some(c)

    seq {
        let mutable x = paths[goal]

        while x.IsSome do
            yield x.Value
            x <- paths[x.Value]
    }


let solve () =
    let io = aocIO
    let gr = Grid.createGrid<char> 12 12
    let sp = (0, 1).AsP2D()
    let ep = (12, 12).AsP2D()
    let obst = [ (1, 1); (2, 2); (3, 3); (4, 4) ] |> Seq.map (_.AsP2D())
    let obst = []

    let bounds = gr |> Grid.getBoundary

    let p = bff sp ep obst bounds

    for y in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
        for x in 0 .. ((gr |> Seq.length) - 1) do
            let x = (x, y).AsP2D()

            if x = sp then
                printfn $"S"
            elif x = ep then
                printfn $"E"
            elif obst |> Seq.contains (x) then
                printfn $"#"
            elif p |> Seq.contains (x) then
                printfn $"X"

        printfn ""



    0
