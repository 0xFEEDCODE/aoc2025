module aoc2025.day10

open System
open System.Collections.Generic
open aoc2025.util

let useSampleInput = 0
let nSkip = 0

type Counter = int
type JotageCounter = int list
type AffectedIndexes = int list

type BinaryState =
    | ON
    | OFF

type Pattern = BinaryState list
type ButtonConfiguration = AffectedIndexes

let pushButton cp bc =
    let newCP = cp |> Seq.toArray

    for i in bc do
        newCP[i] <- if newCP[i] = OFF then ON else OFF

    newCP |> Seq.toList

let divBy2 jc =
    let newJC = jc |> Seq.toArray

    for i in 0 .. (jc |> Seq.length) - 1 do
        newJC[i] <- newJC[i] / 2

    newJC |> Seq.toList

let decrJC (jc: JotageCounter) (bc: ButtonConfiguration) n zeroIndex =
    let mutable newJC = jc |> Seq.toArray
    let mutable hitsZero = false
    let mutable hitsLTZero = false

    for i in bc do
        let newValue = jc[i] - n
        newJC[i] <- newValue

        if i = zeroIndex && newValue = 0 then
            hitsZero <- true

        if newValue < 0 then
            hitsLTZero <- true

    (newJC |> Seq.toList, hitsZero, hitsLTZero)

let pushButtonJC (jc: JotageCounter) (bc: ButtonConfiguration) =
    let newJC, _, _ = decrJC jc bc 1 0
    newJC


let tryFindShortestPath (goalJC: int list) (bc: int list list) limit =
    let counters = Dictionary<int, IReadOnlyList<AffectedIndexes>>()

    for i in 0 .. (bc |> Seq.length) - 1 do
        let b = bc[i]

        let l = List(b)

        for jpos in b do
            if not (counters.ContainsKey(jpos)) then
                counters[jpos] <- List()

            let l2 = List(counters[jpos])
            l2.Add(l |> Seq.toList)
            counters[jpos] <- l2


    let startJC =
        [ for i in 0 .. goalJC.Length - 1 do
              yield goalJC[i] ]


    let q = PriorityQueue()

    let mutable jcIndexes =
        counters
        |> Seq.sortBy (fun x -> x.Value |> Seq.distinct |> Seq.length)
        |> Seq.map _.Key
        |> Seq.toList

    let next = Dictionary<int list, int>()

    next[startJC] <- 0

    let mutable fewestSteps = Int32.MaxValue

    let allEQ0 l = l |> Seq.forall (fun x -> x = 0)

    let mutable nProcessed = 0UL

    // Decrement until 0 at target IDX
    for i in jcIndexes do
        let options = counters[i]

        for kv in next do
            let (jc, np) = (kv.Key, kv.Value)
            q.Enqueue((jc, jc[i], np, counters[i].Count - 1), 0)

        next.Clear()

        while q.Count > 0 && nProcessed < limit do
            nProcessed <- nProcessed + 1UL

            let (jc, jcVal, nPresses, optIdx) = q.Dequeue()

            let handleZero jc np =
                if allEQ0 jc && np < fewestSteps then
                    fewestSteps <- np

                if not (next.ContainsKey(jc) && next[jc] < np) then
                    next[jc] <- np

            if jcVal = 0 then
                handleZero jc nPresses
            else if jcVal > 0 && optIdx >= 0 && nPresses <= fewestSteps then
                q.Enqueue((jc, jcVal, nPresses, optIdx - 1), jcVal)

                let affectedIndexes = options[optIdx]

                let mutable forceStop = false
                let mutable n = if optIdx = 0 then (jcVal - 1) else 0

                while not forceStop do
                    n <- n + 1
                    let (newJC, hitsZero, hitsLTZero) = decrJC jc affectedIndexes n i

                    if hitsLTZero then
                        forceStop <- true
                    else
                        let newNPresses = nPresses + n

                        if hitsZero then
                            handleZero newJC newNPresses
                            forceStop <- true
                        else
                            q.Enqueue((newJC, jcVal - n, newNPresses, optIdx - 1), jcVal - n)

    if fewestSteps = Int32.MaxValue then
        None
    else
        Some(fewestSteps)

let getPossiblePaths (sp: Pattern) (gp: Pattern) (bcs: ButtonConfiguration list) =
    let rec loop acc currentPattern currentNPresses (bcs: ButtonConfiguration list) buttonsPressed =
        if currentPattern = gp then
            acc @ [ (currentNPresses, buttonsPressed) ]
        else
            let res =
                match bcs with
                | button :: remainingButtons ->
                    let pressChoice =
                        (loop acc (pushButton currentPattern button) (currentNPresses + 1) remainingButtons (buttonsPressed @ [ button ]))

                    let noPressChoice =
                        (loop acc currentPattern currentNPresses remainingButtons buttonsPressed)

                    let choices = [ pressChoice; noPressChoice ] |> List.collect id
                    choices
                | _ -> []

            res

    let pathsFound = loop [] sp 0 bcs []
    pathsFound


let alg1 (sp: Pattern) (gp: Pattern) (bcs: ButtonConfiguration list) =
    let mutable fewestFound = Int32.MaxValue

    let rec loop cp np (bcs: ButtonConfiguration list) history =
        if cp = gp then
            if np < fewestFound then
                fewestFound <- np

            (np, history)
        else
            match bcs with
            | button :: remainingButtons when np < fewestFound ->
                min (loop (pushButton cp button) (np + 1) remainingButtons (history @ [ button ])) (loop cp np remainingButtons history)
            | _ -> (Int32.MaxValue, history)

    let np, history = loop sp 0 bcs []
    (np, history)

let alg2 (jc: JotageCounter) (bcs: ButtonConfiguration list) =
    let evenValue, oddValue = ON, OFF
    let allEvenPattern = jc |> List.map (fun _ -> evenValue)

    let allEQ0 l = l |> Seq.forall (fun x -> x = 0)
    let anyLT0 l = l |> Seq.exists (fun x -> x < 0)

    let memo = Dictionary()
    let memoSP = Dictionary()

    let getOrCreateEntryInDict (key: 'a) (factoryFn: unit -> 'b) (dict: Dictionary<'a, 'b>) =
        if not (dict.ContainsKey(key)) then
            dict[key] <- factoryFn ()

        dict[key]

    let getOrCreateEntryInMemo key factoryFn =
        getOrCreateEntryInDict key factoryFn memo

    let getOrCreateEntryInMemoSP key factoryFn =
        getOrCreateEntryInDict key factoryFn memoSP

    let getPathsToEven jc =
        let parityPattern =
            jc |> List.map (fun x -> if x % 2 = 0 then evenValue else oddValue)

        getPossiblePaths parityPattern allEvenPattern bcs

    let tryFindShortestPathWithSearchLimit jc =
        let searchLimit = (1000000UL)
        tryFindShortestPath jc bcs searchLimit

    let rec findFewestStepsLoop jc =
        if allEQ0 jc then
            Some(0)
        else if not (anyLT0 jc) then
            let sp =
                getOrCreateEntryInMemoSP jc (fun _ -> tryFindShortestPathWithSearchLimit jc)

            if sp.IsNone then
                let pathstoEven = getPathsToEven jc

                let res =
                    [ for np, path in pathstoEven do
                          let nextJC = (jc, path) ||> Seq.fold pushButtonJC

                          let isValid = not (nextJC |> List.exists (fun x -> x < 0))

                          if isValid then
                              let canDiv = nextJC |> List.forall (fun x -> x > 0)

                              match (canDiv) with
                              | true ->
                                  let nextJC = nextJC |> divBy2

                                  let res = getOrCreateEntryInMemo nextJC (fun _ -> (findFewestStepsLoop nextJC))

                                  if res.IsSome then
                                      yield 2 * res.Value + np
                              | false ->
                                  let sp =
                                      getOrCreateEntryInMemoSP nextJC (fun _ -> (tryFindShortestPathWithSearchLimit nextJC))

                                  if sp.IsSome then
                                      yield sp.Value + np ]

                if not (res.IsEmpty) then Some(res |> List.min) else None
            else
                sp
        else
            None

    match (findFewestStepsLoop jc) with
    | Some(res) -> res
    | None -> (tryFindShortestPathWithSearchLimit jc).Value

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

            let lc: Pattern =
                lc[1 .. (lc.Length) - 2]
                |> Seq.map (fun x -> if x = '.' then OFF else ON)
                |> Seq.toList

            let bc: ButtonConfiguration list =
                spl[1 .. l - 2]
                |> Seq.map (fun x ->
                    let spl = x[1 .. (x.Length - 2)].Split ','
                    spl |> Seq.map int |> Seq.toList)
                |> Seq.toList

            let jc = spl[l - 1]

            let jc: JotageCounter =
                (jc[1 .. jc.Length - 2].Split ',') |> Seq.map int |> Seq.toList

            (lc, bc, jc))
        |> Seq.toList

    let mutable ans1 = double 0
    let mutable ans2 = double 0

    for (lp, bc, jc) in inp |> Seq.skip nSkip do
        printfn $"%A{(lp, bc, jc)}"

        let startPattern = lp |> List.map (fun _ -> OFF)

        let r1 = double ((alg1 startPattern lp bc) |> fst)
        &ans1 += r1

        let r2 = double (alg2 jc bc)
        &ans2 += r2
        printfn $"%A{(r2, ans2)}"

    printfn $"%d{ans1 |> uint64}"
    printfn $"%d{ans2 |> uint64}"
    0
