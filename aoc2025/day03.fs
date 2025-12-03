module aoc2025.day03

open System
open Microsoft.FSharp.Core
open aoc2025.util

let useSampleInput = 1

let alg nDigitsTarget (n: int list) =
    let nElements = n |> Seq.length
    
    let indexed = n |> smapi |> Seq.toList
    
    let rec loop i acc nLeft =
        if nLeft = 0 then
            acc
        else
            let valueAtCurrentIndex = n[i]
            
            let betterDigits = indexed[(i+1)..] |> Seq.where(fun (_, digit) -> digit > valueAtCurrentIndex)
            let nextBetterDigit = betterDigits |> Seq.tryHead
            
            match nextBetterDigit with
            | Some(betterDigitIndex, _) when (betterDigitIndex+nLeft-1 < nElements) ->
                // Value at current index is not best candidate, move to the best candidate index instead
                loop betterDigitIndex acc nLeft
            | _ ->
                // Value at current index is best candidate, add to acc and increment idx
                loop (i+1) (acc @ [valueAtCurrentIndex]) (nLeft-1)
            
    
    (loop 0 [] nDigitsTarget) |> Seq.fold(fun acc x -> acc + x.ToString()) "" |> uint64
    
let solve () =
    let io = aocIO
    let inp = (useSampleInput = 1)?(io.readSampleInput(), io.getInput ())
    
    let banks = (inp |> Seq.map(fun x -> x |> Seq.map(fun y -> Int32.Parse(y.ToString())) |> Seq.toList) |> Seq.toList)
    
    let ans1 = banks |> Seq.map (alg 2) |> Seq.sum
    let ans2 = banks |> Seq.map (alg 12) |> Seq.sum 
    
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
    
    0
