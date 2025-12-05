module aoc2025.day05

open aoc2025.util

let useSampleInput = 0

type Range =
    { s: double
      e: double }

    member this.overlaps(other: Range) =
        this.s <= other.s && this.e <= other.e && this.e >= other.s
        || this.s <= other.e && this.s >= other.s && this.e <= other.e && this.e >= other.s

    member this.length =
        let r = (this.e - this.s)
        r + (double 1)


let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let splitIdx = inp |> Seq.findIndex (fun x -> x = "")
    printfn $"%A{splitIdx}"

    let freshRanges =
        inp
        |> Seq.take (splitIdx)
        |> Seq.map (fun x ->
            let spl = x.Split '-'
            { s = double spl[0]; e = double spl[1] })
        |> Seq.toList

    let ingredients = inp |> Seq.skip (splitIdx + 1) |> Seq.map double

    let ans1 =
        ingredients
        |> Seq.where (fun x -> freshRanges |> Seq.tryFind (fun (r) -> x >= r.s && x <= r.e) |> _.IsSome)
        |> Seq.length

    let mutable stabilized = false
    let mutable ranges = freshRanges

    while not stabilized do
        let l = ranges |> Seq.length

        stabilized <- true

        let mutable newRanges = []

        for i in 0 .. (l - 1) do
            let r1 = ranges[i]

            let merges =
                ranges |> Seq.where (fun r2 -> r1 <> r2 && (r1.overlaps r2 || r2.overlaps r1))

            if (not (merges |> Seq.isEmpty)) then
                let merge = merges |> Seq.maxBy _.e
                let s = min (r1.s) (merge.s)
                let e = max (r1.e) (merge.e)
                let nr = { s = s; e = e }

                if s <= r1.s && e >= r1.e then
                    if not (newRanges |> Seq.contains nr) then
                        newRanges <- newRanges @ [ nr ]
                        stabilized <- false
            else
                newRanges <- newRanges @ [ r1 ]


        ranges <- newRanges

    let ans2 = ranges |> Seq.distinct |> Seq.map (_.length) |> Seq.sum
    printfn $"%d{ans2 |> uint64}"






    (*
    printfn $"%A{ans1}"
    *)

    0
