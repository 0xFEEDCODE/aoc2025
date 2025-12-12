module aoc2025.day11

open System.Collections.Generic
open System.Linq
open aoc2025.util

let useSampleInput = 0

let mutable leadsTo = Dictionary<string, HashSet<string>>()
let mutable lt2 = Dictionary<string, HashSet<string>>()

(*
let popLeadsTo (connections: Dictionary<string, string list>)  =
    if leadsTo |> Seq.isEmpty then
        leadsTo["out"] <- HashSet()
        for x in connections.Keys do
            leadsTo[x] <- HashSet()
            
    let start = "out"
    
    let q = Queue()
    q.Enqueue(start)
    
    while q.Count > 0 do
        let current = q.Dequeue()
        
        let connected = connections.Where(fun x -> x.Value.Contains current) 
        for kv in connected do
            leadsTo[kv.Key].Add(current) |> ignore
            q.Enqueue(kv.Key)
    ()
    *)




let canReach (connections: Dictionary<string, string list>) start goal =
    let q = PriorityQueue()
    q.Enqueue((start, List<string>()), 0)

    let mutable paths = []

    if leadsTo |> Seq.isEmpty then
        for x in connections.Keys do
            leadsTo[x] <- HashSet()

    while (q.Count > 0) do
        let (device, history) = q.Dequeue()
        history.Add(device)


        let hasShortcut = (leadsTo[device].Contains goal)

        if device = goal || hasShortcut then
            paths <- paths @ [ history ]

            for i in 0 .. (history.Count - 1) do
                let path =
                    [ for j in i + 1 .. (history.Count - 1) do
                          yield history[j] ]

                for x in path do
                    leadsTo[history[i]].Add(x) |> ignore

        else if device = "out" then
            ()
        else
            let outputs = connections[device]

            let mutable viableOutputs =
                outputs |> List.where (fun o -> not (history.Contains(o)))

            viableOutputs <- outputs |> List.where (fun o -> o = goal)

            for out in viableOutputs do
                let newHistory = history.ToList()
                q.Enqueue((out, newHistory), 0)

    paths

let fp (connections: Dictionary<string, string list>) start goal =
    let q = PriorityQueue()
    let h = List<string>()
    h.Add(start)
    q.Enqueue((start, h), 0)

    let mutable paths = List<List<string>>()

    if lt2 |> Seq.isEmpty then
        for x in connections.Keys do
            lt2[x] <- HashSet()

        lt2["out"] <- HashSet()

    while (q.Count > 0) do
        let (device, history) = q.Dequeue()

        if device = goal then
            for h in history do
                lt2[h].Add(goal) |> ignore

            paths.Add(history)
        else
            let outputs = connections[device]

            let viableOutputs =
                outputs
                |> List.where (fun o ->
                    not (history.Contains(o))
                    && (o = goal || leadsTo[o].Contains(goal) || lt2[o].Contains(goal)))

            if viableOutputs |> Seq.isEmpty then
                printfn $"Nowhere to go"

            for out in viableOutputs do
                let newHistory = history.ToList()
                newHistory.Add(out)

                let prio = if out = "dac" || out = "fft" then 0 else history.Count + 10

                q.Enqueue((out, newHistory), prio)

    paths.Distinct()

let findPaths2 (connections: Dictionary<string, string list>) =
    if leadsTo |> Seq.isEmpty then
        for x in connections.Keys do
            leadsTo[x] <- HashSet()

        leadsTo["out"] <- HashSet()

    let pop () =
        let explore = Queue()
        explore.Enqueue("out")

        let mutable explored = HashSet<string>()
        explored.Add("out") |> ignore

        while explore.Count > 0 do
            let current = explore.Dequeue()

            let connected = connections.Where(fun x -> x.Value.Contains current)

            for kv in connected do
                leadsTo[kv.Key].Add(current) |> ignore

                for conn in kv.Value do
                    leadsTo[kv.Key].UnionWith(leadsTo[conn])

                if not (explored.Contains kv.Key) then
                    explored.Add(kv.Key) |> ignore
                    explore.Enqueue(kv.Key)


    pop ()
    pop ()

    for p1 in connections.Keys do
        for p2 in connections.Keys do
            if p1 <> p2 then
                (canReach connections p1 p2) |> ignore

    let d = leadsTo |> Seq.where (fun x -> x.Value.Contains "dac") |> Seq.toList
    let f = leadsTo |> Seq.where (fun x -> x.Value.Contains "fft") |> Seq.toList
    printfn $"%A{(d |> Seq.length)}"
    printfn $"%A{(f |> Seq.length)}"
    printfn $"Processed"

    let svr_fft = fp connections "svr" "fft"
    let fft_dac = fp connections "fft" "dac"
    let dac_out = fp connections "dac" "out"

    let res =
        (double (svr_fft.Count()))
        * (double (fft_dac.Count()))
        * (double (dac_out.Count()))
        |> uint64

    res

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    printfn $"%A{inp}"

    let connections = Dictionary<string, string list>()

    for l in inp do
        let spl = l.Split ':'
        let dev = spl[0]
        let outputs = ((spl[1..][0]).Split ' ' |> Seq.toList)[1..]
        connections[dev] <- outputs


    (*
    let nPaths = findPaths connections
    printfn $"%A{nPaths}"
    *)

    let nPaths2 = findPaths2 connections
    printfn $"%A{nPaths2}"
    0
