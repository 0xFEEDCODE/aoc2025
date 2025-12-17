module aoc2025.day10

open System
open System.Collections.Generic
open System.Linq
open System.Text
open aoc2025.util

let useSampleInput = 0

type ButtonIndex = int
type Nsteps = int

type SingleButtonConfiguration(buttonIndex: int, nsteps: int) =
    let bIdx = buttonIndex
    let mutable nSteps = nsteps

    member this.ButtonIndex = bIdx

    member this.NSteps
        with get () = nsteps
        and set (value) = nSteps <- value

    member this.AsTuple = (bIdx, nSteps)

    member this.Clone() = SingleButtonConfiguration(bIdx, nSteps)

    override x.GetHashCode() = hash (bIdx) * hash (nSteps)

    override x.Equals(b) =
        match b with
        | :? SingleButtonConfiguration as otherBC -> (bIdx, nSteps) = (otherBC.ButtonIndex, otherBC.NSteps)
        | _ -> false

    override this.ToString() = (buttonIndex, nSteps).ToString()

type ButtonConfiguration(bc: SingleButtonConfiguration list) =
    let mutable bc = List(bc)
    let mutable totalNSteps = bc |> Seq.sumBy _.NSteps

    member this.ButtonConfigurations = bc
    member this.TotalNSteps = totalNSteps

    member this.Clone() = ButtonConfiguration(bc |> Seq.toList)

    new() = ButtonConfiguration([])
    new(singleBC: SingleButtonConfiguration) = ButtonConfiguration([ singleBC ])

    member this.AddBC(newSingleBC: SingleButtonConfiguration) =
        bc.Add(newSingleBC)
        totalNSteps <- bc |> Seq.sumBy _.NSteps

    member this.ContainsButton(singleBC: SingleButtonConfiguration) =
        bc |> Seq.exists (fun b -> b.ButtonIndex = singleBC.ButtonIndex)

    member this.TryGetButtonConfiguration(buttonIndex: int) =
        bc |> Seq.tryFind (fun b -> b.ButtonIndex = buttonIndex)

    member this.GetSingleButtonConfiguration(buttonIndex: int) =
        (bc |> Seq.find (fun b -> b.ButtonIndex = buttonIndex))


    member this.MergeNonOverlapping(otherBC: ButtonConfiguration) =
        let mergedBC = this.Clone()

        for obc in otherBC.ButtonConfigurations do
            if not (mergedBC.ContainsButton(obc)) then
                mergedBC.AddBC(obc.Clone())

        mergedBC

    member this.TrimEmpty() =
        bc.RemoveAll(fun x -> x.NSteps <= 0) |> ignore

    override this.GetHashCode() = hash (bc)

    override this.Equals(b) =
        match b with
        | :? ButtonConfiguration as otherBC ->
            (bc
             |> Seq.forall (fun x ->
                 otherBC.ContainsButton(x)
                 && otherBC.GetSingleButtonConfiguration(x.ButtonIndex).NSteps = x.NSteps))
        | _ -> false

    (*
    member this.GetKey() =
        let mutable sb = StringBuilder()

        for i in 0..20 do
            let b = (this.TryGetButtonConfiguration(i))

            if b.IsSome then
                sb <- sb.Append("x")
                sb <- sb.Append(b.Value.NSteps)
            else
                sb <- sb.Append("0")

            sb <- sb.Append(" ")

        sb.ToString()
        *)


    override this.ToString() =
        let mutable sb = StringBuilder()

        for b in bc |> Seq.sortBy (_.ButtonIndex) do
            sb <- sb.Append(b.ToString())
            sb <- sb.Append " "

        sb.ToString()


let alg (goalJC: int list) (bc: int list list) =

    let emptyJC = goalJC |> List.map (fun _ -> 0)
    let emptyCC = bc |> List.map (fun _ -> 0)

    let jcAsString (jc: int seq) =
        let mutable sb = StringBuilder()
        let jc = jc |> Seq.toList

        for i in 0 .. (jc |> Seq.length) - 1 do
            sb <- sb.Append(jc[i])
            sb <- sb.Append " "

        sb.ToString()

    let bcToJC (buttonConf: ButtonConfiguration) =
        let mutable jc = emptyJC |> Seq.toArray

        for (bIdx, ns) in buttonConf.ButtonConfigurations |> Seq.map _.AsTuple do
            for jpos in bc[bIdx] do
                jc[jpos] <- jc[jpos] + ns

        jc |> Seq.toList

    let distToGoal (jc: int list) =
        let mutable dist = 0

        for i in 0 .. (goalJC |> Seq.length) - 1 do
            dist <- dist + (goalJC[i] - jc[i])

        dist

    let counters = Dictionary<int list, int list * (int * int * int list) list>()

    let popCTS () =
        for i in 0 .. (bc |> Seq.length) - 1 do
            let b = bc[i]
            let mutable ct = goalJC |> Seq.map (fun _ -> 0) |> Seq.toArray

            let mutable maxedOut = false
            let mutable nPresses = 0

            let mutable npl = []
            let mutable cts = List<int * int * int list>()

            cts.Add((i, 0, ct |> Seq.toList))

            while (not maxedOut) do
                for jpos in b do
                    if not maxedOut then
                        ct[jpos] <- ct[jpos] + 1

                        if goalJC[jpos] - ct[jpos] < 0 then
                            maxedOut <- true

                if nPresses > 0 && not maxedOut then
                    npl <- npl @ [ nPresses ]

                nPresses <- nPresses + 1
                cts.Add((i, nPresses, ct |> Seq.toList))

            let indexes =
                [ for i in 0 .. (ct.Length) - 1 do
                      if ct[i] <> 0 then yield i else 0 ]

            counters.Add(b, (indexes, cts |> Seq.toList))

    popCTS ()

    let indexes = goalJC |> List.indexed |> Seq.map fst

    let isGoal (bc: ButtonConfiguration) =
        let jc = bcToJC bc

        let mutable isGoal = true

        for jpos in 0 .. (goalJC.Length - 1) do
            if jc[jpos] <> goalJC[jpos] then
                isGoal <- false

        isGoal

    let isValid (bc: ButtonConfiguration) =
        let jc = bcToJC bc
        let mutable isValid = true

        for jpos in 0 .. (goalJC.Length - 1) do
            if jc[jpos] > goalJC[jpos] then
                isValid <- false

        isValid


    let getCombinations idx (pairs: (int * int) list) (restrictions: ButtonConfiguration list) =
        let keys = (pairs |> Seq.map fst) |> Seq.distinct
        let n = keys.Count()
        let target = goalJC[idx]
        let pairs = pairs |> List.sort

        let found = List()

        let pairsIndexes = (pairs |> Seq.map fst |> Seq.distinct |> Seq.toList)

        let restrictionRes =
            pairsIndexes
            |> Seq.map (fun pIdx ->
                (pIdx,
                 (restrictions
                  |> Seq.map (fun x -> (x, x.TryGetButtonConfiguration(pIdx)))
                  |> Seq.where (fun (x, y) -> y.IsSome)
                  |> Seq.map fst
                  |> Seq.toList)))
            |> dict

        let mutable overlappingRes =
            Dictionary(
                pairs
                |> List.map (fun (bIdx, nSteps) ->

                    let overlapping = restrictionRes[bIdx]

                    ((bIdx, nSteps),
                     (overlapping |> Seq.isEmpty)
                     || (overlapping
                         |> Seq.tryFind (fun bc -> bc.GetSingleButtonConfiguration(bIdx).NSteps = nSteps))
                         .IsSome))
                |> dict
            )


        let rec loop (curr: ButtonConfiguration) (pairsIdx: int list) =
            let nStepsTotal = curr.TotalNSteps
            let currPairs = curr.ButtonConfigurations

            if currPairs.Count() = n || nStepsTotal = target then
                if nStepsTotal = target then
                    found.Add(curr)
            else
                let nextPairIdx = pairsIdx[0]
                let newPairsIdx = pairsIdx[1..]

                let newRemaining =
                    let isLastOption = currPairs.Count() = n - 1

                    pairs
                    |> List.where (fun (bIdx, ns) ->
                        let canHitTarget = (ns + nStepsTotal) <= target


                        if bIdx = nextPairIdx && canHitTarget then
                            let overlapRes = overlappingRes[(bIdx, ns)]

                            if isLastOption then
                                (overlapRes && (ns + nStepsTotal = target))
                            else
                                overlapRes
                        else
                            false)

                [ for r in newRemaining do
                      let newCurr = curr.Clone()
                      newCurr.AddBC(SingleButtonConfiguration(r))
                      yield loop newCurr newPairsIdx ]
                |> ignore

        loop (ButtonConfiguration()) ((pairs |> Seq.map fst |> Seq.distinct |> Seq.toList))
        found |> Seq.toList |> List.where isValid

    let getCombinationsRestricted idx (pairs: (int * int) list) (restrictions: ButtonConfiguration list) =
        let keys = (pairs |> Seq.map fst) |> Seq.distinct
        let n = keys.Count()
        let target = goalJC[idx]
        let pairs = pairs |> List.sort

        let found = List()

        let pairsIndexes = (pairs |> Seq.map fst |> Seq.distinct |> Seq.toList)

        let restrictionRes =
            pairsIndexes
            |> Seq.map (fun pIdx ->
                (pIdx,
                 (restrictions
                  |> Seq.map (fun x -> (x, x.TryGetButtonConfiguration(pIdx)))
                  |> Seq.where (fun (x, y) -> y.IsSome)
                  |> Seq.map fst
                  |> Seq.toList)))
            |> dict

        let mutable overlappingRepo =
            Dictionary(
                pairs
                |> List.map (fun (bIdx, nSteps) ->

                    let overlapping = restrictionRes[bIdx]

                    ((bIdx, nSteps),
                     (overlapping
                      |> Seq.where (fun bc -> bc.GetSingleButtonConfiguration(bIdx).NSteps = nSteps))))
                |> dict
            )

        let mutable starting =
            pairs
            |> List.map (fun (bIdx, nSteps) ->
                let allOverlaps = overlappingRepo[(bIdx, nSteps)]

                allOverlaps
                |> Seq.map (fun ol ->
                    let overlapping =
                        pairsIndexes
                        |> List.map ol.TryGetButtonConfiguration
                        |> List.where _.IsSome
                        |> List.map _.Value

                    if
                        overlapping
                        |> Seq.exists (fun olp -> ol.GetSingleButtonConfiguration(olp.ButtonIndex).NSteps <> olp.NSteps)
                    then
                        failwith "wtf"

                    let nTargetCount = overlapping |> List.length
                    let nTargetSteps = overlapping |> Seq.sumBy _.NSteps

                    let res =
                        (ol, pairsIndexes |> List.where (fun i -> ol.TryGetButtonConfiguration(i).IsNone), nTargetCount, nTargetSteps)

                    res)
                |> Seq.groupBy (fun (a, b, c, d) -> bcToJC a)
                |> Seq.map (fun (jc, x) -> x |> Seq.minBy (fun (a, b, c, d) -> a.TotalNSteps))
                |> Seq.toList)
            |> List.collect id
            |> List.distinct

        let pairs =
            pairs
            |> List.where (fun (bIdx, ns) -> ((overlappingRepo[(bIdx, ns)] |> Seq.isEmpty)))
            |> List.sortByDescending snd

        let mutable fewestFound = Int32.MaxValue
        let half = n / 2

        let rec loop (curr: ButtonConfiguration) (pairsIdx: int list) nTargetCount nTargetSteps =
            if nTargetCount = n || nTargetSteps = target then
                if nTargetSteps = target then
                    let jc = bcToJC curr

                    if curr.TotalNSteps < fewestFound then
                        fewestFound <- curr.TotalNSteps

                    if jc[idx] = target then
                        found.Add(curr)
            else if curr.TotalNSteps <= fewestFound then
                if not (nTargetCount > half && nTargetSteps = 0) then
                    let nextPairIdx = pairsIdx[0]
                    let newPairsIdx = pairsIdx[1..]

                    let isLast = (n - nTargetCount) = 1

                    let candidates =
                        pairs
                        |> List.where (fun p ->
                            let (bIdx, ns) = p
                            let canHitTarget = (ns + nTargetSteps) <= target

                            let canHitTarget =
                                if isLast then
                                    canHitTarget && (ns + nTargetSteps) = target
                                else
                                    canHitTarget

                            bIdx = nextPairIdx && canHitTarget)

                    candidates
                    |> List.iter (fun (bIdx, ns) ->
                        let newBC = curr.Clone()
                        newBC.AddBC(SingleButtonConfiguration((bIdx, ns)))

                        loop newBC newPairsIdx (nTargetCount + 1) (nTargetSteps + ns))

        if idx = 2 then
            ()

        printfn $"Processing..."

        for (bc, pIndexes, nTargetCount, nTargetSteps) in starting do
            loop bc pIndexes nTargetCount nTargetSteps

        printfn $"processed"

        let nsteps = found |> Seq.map _.TotalNSteps
        let max = nsteps |> Seq.max
        let min = nsteps |> Seq.min
        let doubleMin = (min * (2))
        printfn $"%A{(min, max)}"

        printfn $"Found %A{found.Count}"

        let result =
            found
            |> Seq.where (fun x -> x.TotalNSteps <= doubleMin && isValid x)
            |> Seq.map (fun x -> (x, distToGoal (bcToJC x)))
            |> Seq.sortBy snd
            |> Seq.map fst
            |> Seq.toList
        (**)
        (*
        let result =
            found
            |> Seq.where (fun x -> x.TotalNSteps <= doubleMin && isValid x)
            |> Seq.toList
            *)

        printfn $"Filtered %A{result.Length}"


        (*
        let result = result |> List.map fst |> List.truncate 20000
        *)

        (*
        let goalJC = goalJC
        *)
        (*
        let debug = result |> List.map (fun x -> (x, jcAsString (bcToJC x)))
        let debug = result |> List.map (fun x -> (x, distToGoal (bcToJC x)))
        *)
        (*
        let debug = result |> List.map (fun x -> (x, jcAsString (bcToJC x)))
        *)
        result

    let mutable pairCombinations = Dictionary<int, ButtonConfiguration list>()

    let candidateGroups =
        [ for i in indexes do
              yield (i, bc |> Seq.where (fun x -> x.Contains i) |> Seq.toList) ]
        |> List.sortBy (fun (_, l) -> l |> Seq.sumBy _.Length)
        |> dict

    // Find pairs for the smallest candidate groups first
    let (leastPairsIdx, leastPairsCg) =
        let kv = candidateGroups |> Seq.head
        (kv.Key, kv.Value)

    let cts =
        leastPairsCg
        |> Seq.map (fun x -> snd counters[x])
        |> Seq.collect id
        |> Seq.map (fun (a, b, c) -> (a, b))
        |> Seq.distinct
        |> Seq.toList

    pairCombinations[leastPairsIdx] <- getCombinations leastPairsIdx cts []

    let mutable restrictions = pairCombinations[leastPairsIdx] |> List.distinct

    let mutable processedIndexes = [ leastPairsIdx ]

    let mutable indexesLeftToProcess =
        [ for i in indexes do
              if i <> leastPairsIdx then
                  yield i ]

    let mutable previousIdx = leastPairsIdx

    let mutable otherIndexesInCg =
        (leastPairsCg |> Seq.collect id)
        |> Seq.distinct
        |> Seq.where (fun i -> i <> previousIdx)
        |> Seq.sortBy (fun x -> (candidateGroups[x] |> Seq.sumBy _.Length))

    while not (otherIndexesInCg |> Seq.isEmpty) do
        for idx in otherIndexesInCg do
            let (cg) = candidateGroups[idx]
            printfn $"%A{idx}"

            let cts =
                cg
                |> Seq.map (fun x -> snd counters[x])
                |> Seq.collect id
                |> Seq.map (fun (a, b, c) -> (a, b))
                |> Seq.distinct
                |> Seq.toList


            printfn $"restrictions"
            let res = getCombinationsRestricted idx cts restrictions
            printfn $"processed"
            restrictions <- res
            (*
            for k in res do
                let jc = bcToJC k
                printfn $"%A{jc}"
            res |> List.iter _.TrimEmpty()
            *)

            (*
            let fewestNSTeps =
                (restrictions |> Seq.map _.GetTotalNSteps())
                |> Seq.sort
                |> Seq.truncate 100000
                |> Seq.toList

            restrictions <-
                restrictions
                |> List.where (fun bc -> fewestNSTeps.Contains(bc.GetTotalNSteps()))
                *)

            pairCombinations[idx] <- res
            indexesLeftToProcess <- indexesLeftToProcess |> List.where (fun x -> x <> idx)
            processedIndexes <- processedIndexes @ [ idx ]
            previousIdx <- idx

            otherIndexesInCg <-
                (cg |> Seq.collect id)
                |> Seq.distinct
                |> Seq.where (fun i -> i <> idx && indexesLeftToProcess.Contains idx)
                |> Seq.sortBy (fun x -> (candidateGroups[x] |> Seq.sumBy _.Length))

        // clear res
        printfn $"LTP %A{indexesLeftToProcess.Length}"

    for idx in indexesLeftToProcess do
        let (cg) = candidateGroups[idx]

        let cts =
            cg
            |> Seq.map (fun x -> snd counters[x])
            |> Seq.collect id
            |> Seq.map (fun (a, b, c) -> (a, b))
            |> Seq.distinct
            |> Seq.toList

        let res = getCombinationsRestricted idx cts restrictions
        restrictions <- res

        pairCombinations[idx] <- res
        indexesLeftToProcess <- indexesLeftToProcess |> List.where (fun x -> x <> idx)
        previousIdx <- idx

    (pairCombinations.Values |> Seq.collect id)
    |> Seq.where isGoal
    |> Seq.toList
    |> List.map _.TotalNSteps
    |> List.min


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

    let mutable nFound = 0

    for (lc, bc, jc) in inp |> Seq.skip 6 do
        printfn $"%A{(lc, bc, jc)}"
        let r = double (alg jc bc)
        &ans1 += r

        if r <> Int32.MaxValue then
            nFound <- nFound + 1
            printfn $"%A{nFound}"

        printfn $"%A{ans1 |> uint64}"

    printfn $"Found %A{(nFound)} out of %A{(inp |> Seq.length)}"
    printfn $"%d{ans1 |> uint64}"
    0


(*
    let getCombinationsRestrictedN idx (pairs: (int * int) list) (restrictions: ButtonConfiguration list) =
        let keys = (pairs |> Seq.map fst) |> Seq.distinct
        let n = keys.Count()
        let target = goalJC[idx]
        let pairs = pairs |> List.sort

        let found = List()
        let mutable fewestStepsFound = Int32.MaxValue

        let pairsIndexes = (pairs |> Seq.map fst |> Seq.distinct |> Seq.toList)

        let restrictionRes =
            pairsIndexes
            |> Seq.map (fun pIdx -> (pIdx, restrictions |> Seq.map _.TryGetButtonConfiguration(pIdx) |> Seq.where _.IsSome))
            |> dict

        let overlapsRes =
            pairs
            |> List.map (fun (bIdx, nSteps) ->
                let overlapping = restrictionRes[bIdx]

                ((bIdx, nSteps),
                 ((overlapping |> Seq.isEmpty)
                  || (overlapping |> Seq.exists (fun bc -> bc.Value.NSteps = nSteps)))))
            |> dict

        let candidates =
            pairs
            |> Seq.map (fun (bIdx, ns) ->
                let res =
                    restrictions
                    |> Seq.map (fun r -> (r, r.TryGetButtonConfiguration(bIdx)))
                    |> Seq.where (fun (r, x) -> x.IsSome && x.Value.NSteps = ns)
                    |> Seq.map fst

                let pairsIndexes = pairsIndexes |> List.where (fun x -> x <> bIdx)

                (*
                if res |> Seq.isEmpty then
                    seq { (ButtonConfiguration(SingleButtonConfiguration(bIdx, ns)), ns, pairsIndexes) }
                else
                *)
                res |> Seq.map (fun r -> (r, ns, pairsIndexes)))
            |> Seq.collect id
            |> Seq.distinctBy (fun (bc, _, __) -> bc)
            |> Seq.toList

        let rec loop (curr: ButtonConfiguration) (pairsIdx: int list) =
            let nStepsTotal = curr.GetTotalNSteps()

            if pairsIdx.IsEmpty then
                let jc = bcToJC curr

                if jc[idx] = target then
                    if nStepsTotal < fewestStepsFound then
                        fewestStepsFound <- nStepsTotal

                    found.Add(curr)

            else if nStepsTotal <= fewestStepsFound then
                let nextPairIdx = pairsIdx[0]
                let newPairsIdx = pairsIdx[1..]

                let next =
                    pairs
                    |> List.where (fun p ->
                        let (bIdx, ns) = p

                        if curr.ButtonConfigurations.IsEmpty then true
                        else if bIdx = nextPairIdx then overlapsRes[(bIdx, ns)]
                        else false)

                next
                |> List.iter (fun p ->
                    let newBC = curr.Clone()
                    newBC.AddBC(SingleButtonConfiguration(p))
                    loop newBC newPairsIdx)


        candidates |> Seq.iter (fun (bc, _, pi) -> loop bc pi)

        let result =
            found
            |> Seq.toList
            |> List.where isValid
            |> List.where (fun r -> (bcToJC r)[idx] = target)

        result
*)
