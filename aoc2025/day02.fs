module aoc2025.day02

open aoc2025.util

let isValid n =
    let s = n.ToString().TrimStart '0'
    let l = s.Length

    let even = (l % 2) = 0

    not even
    || (let f, s = (s[.. (l / 2) - 1], s[(l / 2) ..])
        f <> s)

let isValid2 n =
    let s = n.ToString().TrimStart '0'

    let mutable i = 0
    let mutable isInvalid = false

    while i < (s.Length / 2) && not isInvalid do
        let s1 = s[..i]
        let sl = s1.Length

        let mutable isDiff = s.Length % sl <> 0

        let mutable j = i + 1

        while not isDiff && (j + sl) <= s.Length do
            let s2 = s[j .. (j + sl - 1)]

            if (s2.Length <> s1.Length) then
                failwith "wtf"

            isDiff <- s1 <> s2

            &j += sl

        isInvalid <- not isDiff
        &i += 1

    not isInvalid


let alg ranges validationFn =
    let mutable allInvalid = []

    for r in ranges do
        let rs, re = ((fst r) |> double, (snd r) |> double)

        let invalid =
            [ for n in rs..re do
                  if not (validationFn n) then
                      yield n ]

        allInvalid <- allInvalid @ [ invalid ]

    allInvalid


let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let inp =
        (inp |> Seq.head).Split ','
        |> Seq.map (fun x ->
            let spl = (x.Split '-')
            (spl[0], spl[1]))
        |> Seq.toList

    let ans1 = alg inp isValid
    printfn $"%d{ans1 |> Seq.collect id |> Seq.sumBy double |> uint64}"

    let ans2 = alg inp isValid2
    printfn $"%d{ans2 |> Seq.collect id |> Seq.sumBy double |> uint64}"
    0
