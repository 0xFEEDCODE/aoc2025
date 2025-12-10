module aoc2025.day10

open System.Collections.Generic
open System
open aoc2025.util

let useSampleInput = 0

let alg (goalLC: int list) (bc: int list list) =

    let mutable bestFound = Int32.MaxValue

    let pressButton (lc: int list) b =
        let newLC = List(lc)

        for lightPos in b do
            newLC[lightPos] <- if lc[lightPos] = 0 then 1 else 0

        newLC |> Seq.toList

    let q = PriorityQueue()

    let nl = List(goalLC)

    for i in 0 .. (nl |> Seq.length) - 1 do
        nl[i] <- 0

    let nl = nl |> Seq.toList

    for b in bc do
        let nlc, np = (pressButton nl b), 1
        q.Enqueue((nlc, np), 0)

    while (q.Count > 0) do
        let lc, nPresses = q.Dequeue()

        if lc = goalLC then
            if nPresses < bestFound then
                bestFound <- nPresses
        else if nPresses < bestFound then
            for b in bc do
                let nlc, np = (pressButton lc b), (nPresses + 1)
                q.Enqueue((nlc, np), np)
    bestFound



let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let inp =
        inp
        |> Seq.map (fun l ->
            let spl = l.Split ' '
            let lc = (spl[0])

            let lc =
                lc[1 .. (lc.Length) - 2]
                |> Seq.map (fun x -> if x = '.' then 0 else 1)
                |> Seq.toList

            let bc =
                spl[1 .. (spl.Length - 2)]
                |> Seq.map (fun x ->
                    let spl = x[1 .. (x.Length - 2)].Split ','
                    spl |> Seq.map int |> Seq.toList)
                |> Seq.toList

            (lc, bc))

    let mutable ans1 = 0

    for (lc, bc) in inp do
        printfn $"%A{lc}"
        &ans1 += alg lc bc

    printfn $"%A{ans1}"
    0
