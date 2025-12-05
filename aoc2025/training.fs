module aoc2025.training

open System.Collections.Generic
open aoc2025.util

let dj sp goal obst bounds =
    let explore = PriorityQueue<P2D, int>()
    let cost = Dictionary<P2D, int>()
    let cameFrom = Dictionary<P2D, Option<P2D>>()

    explore.Enqueue(sp, 0)
    cameFrom[sp] <- None

    while explore.Count > 0 do
        let c = explore.Dequeue()
        let currentCost = cost[c]

        if not (obst |> Seq.contains (c)) && c <> goal then
            for next in (c |> P2D.getAdjacentNeighbours |> Seq.where (Grid.isInBounds bounds)) do
                let newCost = currentCost + 1

                if not (cost.ContainsKey(next)) || (newCost < cost[next]) then
                    //explore.Enqueue next
                    cameFrom[next] <- Some(c)

    seq {
        let mutable c = None
        cameFrom.TryGetValue(goal, &c) |> ignore

        while c.IsSome do
            let origin = c.Value
            yield origin
            c <- cameFrom[origin]
    }

let bff sp goal obst bounds =
    let explore = Queue()
    let cameFrom = Dictionary<P2D, Option<P2D>>()

    explore.Enqueue sp
    cameFrom[sp] <- None

    while explore.Count > 0 do
        let c = explore.Dequeue()

        if not (obst |> Seq.contains (c)) && c <> goal then
            for next in c |> P2D.getAdjacentNeighbours do
                if (next |> Grid.isInBounds bounds) && not (cameFrom.ContainsKey(next)) then
                    explore.Enqueue next
                    cameFrom[next] <- Some(c)

    seq {
        let mutable c = None
        cameFrom.TryGetValue(goal, &c) |> ignore

        while c.IsSome do
            let origin = c.Value
            yield origin
            c <- cameFrom[origin]
    }


let solve () =
    let gr = Grid.createGrid<char> 12 12
    let sp = (0, 1).AsP2D()
    let ep = (11, 11).AsP2D()
    let obst = [ (1, 1); (0, 2); (2, 2); (3, 3); (4, 4) ] |> Seq.map (_.AsP2D())

    let bounds = gr |> Grid.getBoundary

    let p = bff sp ep obst bounds
    printfn $"%A{p |> Seq.length}"

    for y in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
        for x in 0 .. ((gr |> Seq.length) - 1) do
            let x = (x, y).AsP2D()

            if x = sp then printf $"S"
            elif x = ep then printf $"E"
            elif obst |> Seq.contains (x) then printf $"#"
            elif p |> Seq.contains (x) then printf $"X"
            else printf $"."

        printfn ""



    0
