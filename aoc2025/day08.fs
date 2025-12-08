module aoc2025.day08

open System.Collections.Generic
open aoc2025.util

let useSampleInput = 1

let dist (p1: P3D) (p2: P3D) =
    let p1x, p2x = double p1.x, double p2.x
    let p1y, p2y = double p1.y, double p2.y
    let p1z, p2z = double p1.z, double p2.z
    sqrt
    (pown (p1x - p2x) 2 + pown (p1y - p2y) 2 + pown (p1z - p2z) 2)

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 0 then
            io.readSampleInput ()
        else
            io.getInput ()

    let junctions =
        inp
        |> Seq.map (fun l ->
            let spl = l.Split ',' |> Array.map int
            P3D(spl[0], spl[1], spl[2]))
        |> Seq.toArray

    let mutable circuits =
        new System.Collections.Generic.List<System.Collections.Generic.List<P3D>>()

    let proximity = Dictionary<double, (P3D * P3D)>()

    let nConnectionsToMake = 1000

    for j1 in junctions do
        for j2 in junctions do
            if (j1 <> j2) then
                let dist = dist j1 j2
                proximity[dist] <- (j1, j2)

    let sorted =
        proximity |> Seq.map _.Key |> Seq.sort |> Seq.take (nConnectionsToMake * 10)

    let mutable orderedByClosest = []

    for s in sorted do
        let j1, j2 = proximity[s]
        orderedByClosest <- orderedByClosest @ [ j1; j2 ]

    for j in junctions do
        let nl = new List<P3D>()
        nl.Insert(0, j)
        circuits.Insert(0, nl)

    let mutable ans1 = double 0
    let mutable ans2 = double 0

    let mutable i = 0
    let mutable nConnections = 0

    while (circuits.Count <> 1) do
        let j = orderedByClosest[i]
        let closest = orderedByClosest[(i + 1)]

        let c1, c2 = (j, closest)

        let idx1, idx2 =
            circuits.FindIndex(_.Contains(c1)), circuits.FindIndex(_.Contains(c2))

        if (idx1 <> idx2) then
            let rc1 = circuits[idx1]
            let rc2 = circuits[idx2]

            let biggerIdx = max idx1 idx2
            let smallerIdx = min idx1 idx2
            circuits.RemoveAt(biggerIdx)
            circuits.RemoveAt(smallerIdx)

            let nl = new System.Collections.Generic.List<P3D>()
            nl.AddRange(rc1)
            nl.AddRange(rc2)

            circuits.Insert(0, nl)

        if nConnections = nConnectionsToMake then
            ans1 <-
                circuits
                |> Seq.map _.Count
                |> Seq.map double
                |> Seq.sortDescending
                |> Seq.take 3
                |> Seq.reduce (fun x y -> double x * double y)

        if circuits.Count = 1 then
            ans2 <- (double c1.x) * (double c2.x)

        &nConnections += 1
        &i += 2


    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
