module aoc2025.day06

open System
open System.Collections.Generic
open System.Text
open aoc2025.util

let useSampleInput = 1

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let inp =
        inp
        |> Seq.map (fun x -> (x.Split ' ') |> Seq.where (fun x -> x.Trim() <> "") |> Seq.toList)
        |> Seq.where (fun x -> x |> Seq.length > 0)
        |> Seq.toList

    let nCols = inp |> Seq.head |> Seq.length
    let nRows = inp |> Seq.length


    let mutable s = double 0

    for c in 0 .. nCols - 1 do
        let mutable nums = []

        for r in 0 .. nRows - 2 do
            nums <- nums @ [ Double.Parse(inp[r][c]) ]

        let calc =
            match inp[nRows - 1][c] with
            | "*" -> (double -1, nums) ||> Seq.fold (fun acc x -> if acc = -1 then x else x * acc)
            | "+" -> (double 0, nums) ||> Seq.fold (fun acc x -> x + acc)

        s <- s + calc

    let useSampleInput = 0

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    (*
                                            123 328  51 64 
                                             45 64  387 23 
                                              6 98  215 314
                                            *   +   *   + 
                                                *)


    let inp = inp |> Seq.map (fun x -> x |> Seq.toList) |> Seq.toList

    let mutable splitIndexes = []
    let lastRow = (inp |> Seq.last)

    for i in 0 .. (lastRow |> Seq.length) - 1 do
        let x = lastRow[i]

        if x = '*' || x = '+' then
            splitIndexes <- splitIndexes @ [ i ]

    splitIndexes <- splitIndexes @ [ (inp |> Seq.maxBy (_.Length) |> Seq.length) + 1 ]

    let nRows = inp |> Seq.length

    let nums = Dictionary<int, (char * string list)>()
    let opRow = inp[(nRows - 1)]

    for r in 0 .. nRows - 2 do

        for j in 0 .. (splitIndexes |> Seq.length) - 2 do
            let s = splitIndexes[j]
            let e = splitIndexes[j + 1] - 2

            let op = opRow[s]

            let n = ("", inp[r][s..e]) ||> Seq.fold (fun acc x -> acc + x.ToString())

            if not (nums.ContainsKey j) then
                nums[j] <- (op, [])

            nums[j] <-
                let op, arr = nums[j]
                (op, arr @ [ n ])


    printfn $"%A{nums}"


    (*
                                            123 328  51 64 
                                             45 64  387 23 
                                              6 98  215 314
                                            *   +   *   + 
                                                *)

    let mutable s = double 0
    for (op, nums) in nums.Values do
        let order =
            if (nums[0] |> Seq.length) < (nums[(nums |> Seq.length) - 1] |> Seq.length) then
                1
            else
                0

        let ml = nums |> Seq.maxBy _.Length |> Seq.length

        let digits = Dictionary<int, char list>()

        for n in nums do

            let l = n |> Seq.length

            for i in 0 .. (n |> Seq.length) - 1 do
                if n[i] |> Char.IsDigit then
                    let splIdx = i

                    let idx = if order = 0 then ml + i else (ml - l) + i

                    if not (digits.ContainsKey(idx)) then
                        digits[idx] <- []

                    digits[idx] <- digits[idx] @ [ n[splIdx] ]


        let mutable nums = []

        for (kv) in digits do
            let x = ("", kv.Value) ||> Seq.fold (fun acc x -> acc + x.ToString())
            nums <- nums @ [ Double.Parse x ]

        let calc =
            match op with
            | '*' -> (double -1, nums) ||> Seq.fold (fun acc x -> if acc = -1 then x else x * acc)
            | '+' -> (double 0, nums) ||> Seq.fold (fun acc x -> x + acc)

        s <- s + calc
        printfn $"%A{(nums, calc)}"



    printfn $"%A{s |> uint64}"
    0
