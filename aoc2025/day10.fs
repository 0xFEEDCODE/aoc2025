module aoc2025.day10

open System.Collections.Generic
open System.Linq
open aoc2025.util

let useSampleInput = 0

let alg (goalJC: int list) (bc: int list list) =
    let mutable counters = Dictionary<int, Dictionary<int, int list>>()

    let jcs = goalJC |> Seq.sum

    for i in 0 .. (bc |> Seq.length) - 1 do
        let b = bc[i]
        let mutable ct = goalJC |> Seq.map (fun _ -> 0) |> Seq.toArray

        counters[i] <- Dictionary()
        counters[i].Add(0, ct |> Seq.toList)

        let mutable maxedOut = false
        let mutable oneMore = false
        let mutable nPresses = 0

        while (not maxedOut && not oneMore) do
            for jpos in b do
                if not maxedOut then
                    ct[jpos] <- ct[jpos] + 1

                    if goalJC[jpos] - ct[jpos] < 0 then
                        maxedOut <- true

            nPresses <- nPresses + 1

            if not maxedOut then
                counters[i].Add(nPresses, ct |> Seq.toList)


    let crOld (jc: int list) =
        let mutable canReach = true

        let mutable i = 0

        while canReach && i <= (jc.Length - 1) do
            if jc[i] > goalJC[i] then
                canReach <- false

            &i += 1

        canReach


    let ccToJc (cc: int array) =
        let mutable jc =
            [| for i in 0 .. (goalJC.Length) - 1 do
                   yield 0 |]

        for i in 0 .. (bc |> Seq.length) - 1 do
            let v = counters[i][cc[i]]

            for j in 0 .. (v |> Seq.length) - 1 do
                jc[j] <- jc[j] + v[j]

        jc |> Seq.toList

    let mutable tempJC = goalJC |> Seq.toArray

    let canReachAndIsGoalAndDist (cc: int list) =
        let mutable canReach = true
        let mutable i = 0
        let mutable distance = 0

        for i in 0 .. (tempJC.Length - 1) do
            tempJC[i] <- 0

        while canReach && i < (bc |> Seq.length) do
            let v = counters[i][cc[i]]

            let mutable j = 0

            while canReach && j < (v |> Seq.length) do
                tempJC[j] <- tempJC[j] + v[j]

                if tempJC[j] > goalJC[j] then
                    canReach <- false

                &j += 1

            &i += 1

        let mutable isGoal = canReach

        if canReach then
            for i in 0 .. (goalJC.Length - 1) do
                if tempJC[i] <> goalJC[i] then
                    isGoal <- false

                distance <- distance + (goalJC[i] - tempJC[i])

        (canReach, isGoal, distance)

    let getDist (jc: int list) = jcs - (jc |> Seq.sum)

    let cons = Dictionary<int list, unit>()

    let q = PriorityQueue()
    let c = bc |> List.map (fun _ -> 0)
    (*
    q.Enqueue((c), 0)
    *)
    (*
    let test = [|1;3;0;3;1;2|]
    let test = [|1;2;0;3;1;2|]
    let xy = canReachAndIsGoal test
    *)
    q.Enqueue((c, 0))

    let mutable found = []

    let mutable fewestFound = None

    while q.Count > 0 do
        let cc = q.Dequeue()

        let nStepsCC = cc |> Seq.sum

        let (consider, isGoal, dist) = canReachAndIsGoalAndDist cc

        if isGoal then
            found <- found @ [ cc ]
            let nPresses = cc |> Seq.sum

            if fewestFound.IsNone || nPresses < fewestFound.Value then
                fewestFound <- Some(nPresses)

            printfn $"Found! %A{(fewestFound.Value, nPresses, cc)}"
        else if consider && (fewestFound.IsNone || nStepsCC < fewestFound.Value) then
            for i in 0 .. (bc |> Seq.length) - 1 do
                let cts = counters[i]

                let ncc = cc |> Seq.toArray

                let maxNPRess = cts.Keys.Max()

                for nPresses in 0..maxNPRess do
                    let newNPRessessTotal = nStepsCC + nPresses

                    if ((fewestFound.IsNone || fewestFound.Value > newNPRessessTotal)) then
                        ncc[i] <- nPresses

                        let nccCopy = ncc |> Seq.toList

                        if not (cons.ContainsKey(nccCopy)) then
                            //let nPlacesZero = ncc |> Seq.where (fun x -> x = 0) |> Seq.length

                            cons.Add(nccCopy, ())
                            q.Enqueue(nccCopy, dist)

    found |> Seq.map (fun x -> x |> Seq.sum) |> Seq.min


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
        &ans1 += double (alg jc bc)
        printfn $"%A{ans1 |> uint64}"

    printfn $"%d{ans1 |> uint64}"
    0
