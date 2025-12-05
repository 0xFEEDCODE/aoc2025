module aoc2025.day03

open System
open Microsoft.FSharp.Core
open aoc2025.util

let useSampleInput = 1

let alg nDigitsTarget digits =
    let nElements = digits |> List.length
    let digitsIndexed = digits |> smapi |> Seq.toList

    let rec loop i acc nLeft =
        if nLeft = 0 then
            acc
        else
            let valueAtCurrentIndex = digits[i]

            let betterDigits =
                digitsIndexed[(i + 1) ..]
                |> List.where (fun (_, digit) -> digit > valueAtCurrentIndex)

            let nextBetterDigit = betterDigits |> List.tryHead

            match nextBetterDigit with
            | Some(betterDigitIndex, _) when (betterDigitIndex + nLeft - 1 < nElements) ->
                // Value at current index is not best candidate, move to the best candidate index instead
                loop betterDigitIndex acc nLeft
            | _ ->
                // Value at current index is best candidate, add to acc and increment idx
                loop (i + 1) (acc @ [ valueAtCurrentIndex ]) (nLeft - 1)

    (loop 0 [] nDigitsTarget)
    |> List.fold (fun acc x -> acc + x.ToString()) ""
    |> uint64

let solve () =
    let io = aocIO
    let inp = (useSampleInput = 1)?(io.readSampleInput (), io.getInput ())

    let banks =
        (inp
         |> Seq.map (fun x -> x |> Seq.map (fun y -> Int32.Parse(y.ToString())) |> Seq.toList)
         |> Seq.toList)

    let ans1 = banks |> Seq.map (alg 2) |> Seq.sum
    let ans2 = banks |> Seq.map (alg 12) |> Seq.sum

    printfn $"%A{ans1}"
    printfn $"%A{ans2}"

    0
