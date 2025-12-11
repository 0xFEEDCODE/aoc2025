module aoc2025.day11

open System.Collections.Generic
open System.Linq
open aoc2025.util

let useSampleInput = 0

let findPaths (connections : Dictionary<string, string list>) =
    let start = "you"
    let goal = "out"
    
    let q = PriorityQueue()
    q.Enqueue((start, List<string>()), 0)
    
    let mutable paths = []
    
    while (q.Count > 0) do
        let (device, history) = q.Dequeue()
        
        if device = goal then
            paths <- paths @ [history]

        else
            let outputs = connections[device]
            let viableOutputs = outputs |> List.where(fun o -> not (history.Contains(o)))
        
            for out in viableOutputs do
                
                let newHistory = history.ToList()
                newHistory.Add(out)
                
                q.Enqueue((out, newHistory), 0)
        
    paths |> Seq.length
    
    

let solve () =
    let io = aocIO
    let inp = if useSampleInput=1 then io.readSampleInput () else io.getInput ()
    printfn $"%A{inp}"
    
    let connections =  Dictionary<string, string list>()
    
    for l in inp do
        let spl = l.Split ':'
        let dev = spl[0]
        let outputs = ((spl[1..][0]).Split ' ' |> Seq.toList)[1..]
        connections[dev] <- outputs
        
    
    let nPaths = findPaths connections
    printfn $"%A{nPaths}"
    0