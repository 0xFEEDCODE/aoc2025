module aoc2025.day10

open System.Collections.Generic
open System
open System.Linq
open aoc2025.util

let useSampleInput = 1

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



let alg2 (goalJC: int list) (bc: int list list) =
    let gl = goalJC.Length

    let m1 = Dictionary<(List<int>), bool>()
    let m2 = Dictionary<(List<int>), bool>()
    let m3 = Dictionary<(List<int>), int>()

    let ncd = Dictionary<int, int>()
    let rd = Dictionary<int, int>()

    let gs = goalJC |> Seq.sum

    let popRD (l: List<int>) =
        for i in 0 .. ((goalJC |> Seq.length) - 1) do
            rd[i] <- 0

        for i in 0 .. (l.Count - 1) do
            let nPresses = l[i]

            for jpos in bc[i] do
                rd[jpos] <- rd[jpos] + nPresses

    let isGoal (l: List<int>) =
        if m1.ContainsKey l then
            m1[l]
        else
            popRD l
            let mutable i = 0
            let mutable flag = true

            while i < gl && flag do
                flag <- goalJC[i] = rd[i]
                &i += 1

            m1[l] <- flag
            flag

    let canReachGoal () =
        let mutable flag = true
        let mutable i = 0

        while i < gl && flag do
            flag <- rd[i] < goalJC[i]
            &i += 1

        flag

    let getDist () =
        let mutable dist = 0
        let mutable i = 0

        while i < gl do
            dist <- dist + goalJC[i] - rd[i]
            &i += 1

        dist

    let mutable bestFound = UInt64.MaxValue

    let q = PriorityQueue<List<int>, int>()

    let temp =
        List(
            [ for _ in 0 .. (bc |> Seq.length) - 1 do
                  yield 0 ]
        )

    q.Enqueue(temp, 0)
    let mutable cons = Dictionary<List<int>, unit>()

    (*
    printfn $"%A{(isGoal (List([1;3;0;3;1;1])))}"
    printfn $"%A{(isGoal (List([1;3;0;3;1;2])))}"
    printfn $"%A{(isGoal (List([1;3;0;3;1;3])))}"
    *)

    while (q.Count > 0) do
        let c = q.Dequeue()
        let nPresses = c.Sum() |> uint64

        let consider = not (cons.ContainsKey c) && nPresses < bestFound

        cons[c] <- ()

        if isGoal c then
            if nPresses < bestFound then
                bestFound <- nPresses
                printfn $"%A{bestFound}"
        else if consider then

            for i in 0 .. (c.Count) - 1 do

                let mutable flag = true

                let mutable nc = c.ToList()
                let mutable incr = 100

                while flag && incr > 0 do
                    &incr -= 1
                    nc[i] <- c[i] + incr
                    popRD nc

                    let prio = getDist ()

                    if prio > 0 && canReachGoal () then
                        q.Enqueue(nc, prio)

    if bestFound = UInt64.MaxValue then
        failwith "wtf"

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
            let l = spl |> Seq.length
            let lc = (spl[0])

            let lc = lc[1 .. l - 2] |> Seq.map (fun x -> if x = '.' then 0 else 1) |> Seq.toList

            let bc =
                spl[1 .. l - 2]
                |> Seq.map (fun x ->
                    let spl = x[1 .. (x.Length - 2)].Split ','
                    spl |> Seq.map int |> Seq.toList)
                |> Seq.toList

            let jc = spl[l - 1]
            let jc = (jc[1 .. jc.Length - 2].Split ',') |> Seq.map int |> Seq.toList

            (lc, bc, jc))

    let mutable ans1 = double 0

    (*
    for (lc, bc, jc) in inp do
        printfn $"%A{lc}"
        &ans1 += alg lc bc
        *)

    for (lc, bc, jc) in inp do
        printfn $"%A{(lc, bc, jc)}"
        &ans1 += double (alg2 jc bc)
        printfn $"%A{ans1 |> uint64}"

    printfn $"%d{ans1 |> uint64}"
    0
