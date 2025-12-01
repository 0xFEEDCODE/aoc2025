module aoc2025.day01

open aoc2025.util

type Rot =
    | L
    | R


let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let doc =
        inp
        |> Seq.map (fun x ->
            let rot =
                match x[0] with
                | 'L' -> L
                | 'R' -> R

            let dist = x[1..] |> int
            (rot, dist))

    let mutable dial = 50
    let mutable x = 0

    //let doc = doc |> Seq.map(fun (f,s) -> (f, s+100))

    for (r, d) in doc do
        let op =
            match r with
            | L -> (-)
            | R -> (+)

        let mutable nRot = (abs d < 100)?(0, d / 100)

        if nRot > 0 then
            &x += nRot

        let d = d % 100

        let res = (op dial d)

        let res =
            match res with
            | _ when res < 0 ->
                if dial <> 0 then
                    &x += 1

                100 - abs res
            | _ when res >= 100 ->
                if dial <> 0 then
                    &x += 1

                res % 100
            | _ ->
                if dial <> 0 && res = 0 then
                    &x += 1

                res

        dial <- res


        printfn $"%A{(dial, x, nRot)}"

    printfn $"%A{x}"
    ()
