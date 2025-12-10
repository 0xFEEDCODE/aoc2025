module aoc2025.day10

open System.Collections.Generic
open System
open aoc2025.util

let useSampleInput = 0

let alg2 (startJC: int list) (bc: int list list) =
    let bc = bc |> Seq.sortBy (fun b -> b |> Seq.length) |> Seq.toList
    let memo = Dictionary<(int list * int list) * int, int list>()

    let mutable bestFound = Int32.MaxValue

    let nTimesCanPress (jc: int list) (b: int list) =
        b |> Seq.map (fun jpos -> jc[jpos]) |> Seq.min
        
    let pressButtonNTimes (jc: int list) (b: int list) incr =
        if (memo.ContainsKey(((jc, b), incr))) then
            memo[((jc, b), incr)]
        else
            let mutable newJC = List(jc)

            for jpos in b do
                newJC[jpos] <- jc[jpos] - incr

            let res = newJC |> Seq.toList
            memo[((jc, b), incr)] <- res
            res

    let canReach (jc: int list) = jc |> List.forall (fun x -> x >= 0)
    let dist (jc: int list) = jc |> List.sum

    let q = PriorityQueue()

    for b in bc do
        let njc, np = (pressButtonNTimes startJC b 1), 1
        q.Enqueue((njc, np), 0)

    let cons = Dictionary<int list, int>()

    while (q.Count > 0) do
        let jc, nPresses = q.Dequeue()

        if not (cons.ContainsKey(jc)) || cons[jc] > nPresses then
            cons[jc] <- nPresses

            let reached = jc |> Seq.forall (fun x -> x = 0)

            if reached then
                if nPresses < bestFound then
                    bestFound <- nPresses
                    printfn $"new best found - %A{bestFound}"
            else if nPresses < bestFound && canReach jc then
                for b in bc do
                    let nTimesCanPress = nTimesCanPress jc b

                    if nTimesCanPress > 0 then
                        for n in 1..nTimesCanPress do
                            let njc, np = (pressButtonNTimes jc b n), (nPresses + n)
                            q.Enqueue((njc, np), dist njc)

                (*let sortedPossibilities =
                    bc
                    |> Seq.map (fun b -> (b, nTimesCanPress jc b))
                    |> Seq.sortByDescending snd

                let lowestPrio =
                    [ for (b, nCanPress) in sortedPossibilities do
                          let njc, np = (pressButtonNTimes jc b 1), (nPresses + 1)

                          let prio = (dist njc)
                          yield (prio, (njc, np))

                          let njc, np = (pressButtonNTimes njc b nCanPress), (nPresses + 1 + nCanPress)
                          let prio = (dist njc)
                          yield (prio, (njc, np)) ]

                for (prio, (njc, np)) in lowestPrio do
                    q.Enqueue((njc, np), prio)*)
    (*
                let takeN = (min 10 (lowestPrio |> Seq.length))

                for (prio, (njc, np)) in lowestPrio |> Seq.take (takeN) do
                    q.Enqueue((njc, np), prio)
                    *)

    (*
                for (b, possible) in sortedPossibilities do
                    let njc, np = (pressButton jc b), (nPresses + 1)

                    if canReach njc then
                        for b2, nCanPress in possible do
                            let njc, np = (pressButtonNTimes njc b2 nCanPress), (nPresses + 1 + nCanPress)
                            let prio = (dist njc)
                            q.Enqueue((njc, np), prio)

                        let prio = (dist njc)
                        //printfn $"%A{(njc, prio)}"
                        q.Enqueue((njc, np), prio)
                        *)

    (*
            if possibilities |> Seq.isEmpty |> not then
                let bc = possibilities |> Seq.head
                let njc, np = (pressButton jc bc), (nPresses + 1)

                if canReach njc then
                    let prio = dist njc
                    q.Enqueue((njc, np), prio + nPresses)
                    *)

    (*
            for b in possibilities do
                for i in (biggestElem) .. (-1) .. 1 do
                    let njc, np = (pressButton2 jc b i), (nPresses + i)

                    if canReach njc then
                        let prio = dist njc
                        q.Enqueue((njc, np), prio + nPresses)
                        *)

    if bestFound = Int32.MaxValue then
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
