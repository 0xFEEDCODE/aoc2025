module aoc2025.day02

open System
open aoc2025.util

let isValid (n) =
    let s = n.ToString()
    let s = s.TrimStart '0'

    let l = s |> Seq.length

    let even = (l % 2) = 0

    if not even then
        true
    else
        let x = (s[.. (l / 2) - 1], s[(l / 2) ..])
        fst x <> snd x

let isValid2 (n) =
    let s = n.ToString()
    let s = s.TrimStart '0'

    let mutable i = 0
    let mutable found = false

    while i < (s.Length / 2) && not found do
        let s1 = s[..i]
        let l = s1.Length

        let mutable j = i + 1
        let mutable stop = false

        let mutable t = ""

        if (s.Length % l) = 0 then
            while (j + l) <= s.Length && not stop do
                let s2 = s[(j) .. (j + l - 1)]
                t <- s2

                if (s2.Length <> s1.Length) then
                    failwith ""

                (*
                printf $"%A{(s1, s2)};"
                *)
                let isMatch = s1 = s2
                stop <- not isMatch

                if stop then
                    ()

                &j += l
        else
            stop <- true

        found <- not stop

        if found then
            ()

        &i += 1

    not found





let alg rs =
    let mutable s = []

    for y in rs do
        let r = (double (fst y), double (snd y))

        let invalids =
            [ for x in (fst r) .. (snd r) do
                  if not (isValid2 x) then
                      yield (x) ]

        s <- s @ [ invalids ]

    s


let solve () =
    let io = aocIO
    let sample = false
    let inp = if sample then io.sample () else io.getInput ()

    (*
    let inv = [ 12341234; 77777777; 1212121212; 55; 123123; 1010 ] |> List.map (string)
    let v = [ 101; 0101; 0001010 ] |> Seq.map (string)

    for s in v do
        printfn $"%A{isValid2 s}"
        *)
    
    printfn $"%A{isValid2 2121212118}"

    let inp =
        (inp |> Seq.head).Split ','
        |> Seq.map (fun x ->
            let spl = (x.Split '-')
            (spl[0], spl[1]))
        |> Seq.toList

    printfn $"%A{inp}"

    let ans1 = alg inp
    printfn $"%d{ans1 |> Seq.collect id |> Seq.sumBy (fun x -> double x) |> uint64}"




    (*
    let inv = [ 555;55; 123123; 1010 ] |> Seq.map(string)
    let v = [ 101; 0101; 0001010 ] |> Seq.map(string)
    
    for s in inv do
        printfn $"%A{isValid s}"
        *)

    0
