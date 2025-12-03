module aoc2025.day03

open System
open Microsoft.FSharp.Core
open aoc2025.util

let useSampleInput = 0


let asDoubleDigit (x : int * int) = ((fst x).ToString() + (snd x).ToString()) |> int

let asNum (x : string) =
    if x.Length = 0 then
        float 0
    else
        UInt64.Parse(x) |> float

let alg (n: 'a list) =
    
    let l = n |> Seq.length
    
    let mutable bestFound = (0,0)
    
    for i in 0..l-1 do
        for j in (i+1)..l-1 do
            let temp = (n[i], n[j])
            
            if (asDoubleDigit temp) >  (asDoubleDigit bestFound) then
                bestFound <- temp
        
            
    asDoubleDigit bestFound
    

let alg2 (n: int list) =
    let l = n |> Seq.length
    
    let mutable nLeft = 12
    
    let indexed = n |> smapi |> Seq.toList
    
    let mutable res = []
    let mutable i = 0
    
    while nLeft > 0 do
        
        let c = n[i]
        
        let betterDigits = indexed[(i+1)..] |> Seq.where(fun x -> (snd x) > c)
        let nBetterDigits = betterDigits |> Seq.length
        let nextBetterDigit = betterDigits |> Seq.tryHead
        
        let cond =
            if nextBetterDigit.IsNone then
                false
            else
                let (j,v) = nextBetterDigit.Value
                not (j+nLeft <= l)
            
        if nBetterDigits = 0 || cond then
            res <- res @ [c]
            nLeft <- nLeft-1
        
        &i += 1
        
    res |> Seq.fold(fun acc x -> acc + x.ToString()) "" |> uint64
        
    
let solve () =
    let io = aocIO
    let inp = (useSampleInput = 1)?(io.readSampleInput(), io.getInput ())
    
    let banks = (inp |> Seq.map(fun x -> x |> Seq.map(fun y -> Int32.Parse(y.ToString())) |> Seq.toList) |> Seq.toList)
    
    let ans1 = banks |> Seq.map alg |> Seq.sum
    let ans2 = banks |> Seq.map (fun x -> uint64 (alg2 (x))) |> Seq.sum 
    
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
    
    0
