module aoc2025.day09

open aoc2025
open aoc2025.util

let useSampleInput = 0

let calc1SameAxis p1 p2 =
    if p1.y = p2.y then
        Some((max p1.x p2.x) - (min p1.x p2.x))
    else if p1.x = p2.x then
        Some((max p1.y p2.y) - (min p1.y p2.y))
    else
        None

let calcSurface p1 p2 =
    if p1.y = p2.y || p1.x = p2.x then
        (calc1SameAxis p1 p2).Value |> double
    else
        let w,h  =
            let TL_BR =
                if (p1.x < p2.x && p1.y < p2.y) then Some(p1, p2)
                else if (p2.x < p1.x && p2.y < p1.y) then Some(p2, p1)
                else None

            if TL_BR.IsSome then
                let topLeft, bottomRight = TL_BR.Value
                let topRightX = bottomRight.x

                let w = topRightX - topLeft.x + 1
                let h = bottomRight.y - topLeft.y + 1
                
                (w,h)
            else
                let bottomLeft, topRight =
                    if (p1.x < p2.x && p1.y > p2.y) then (p1, p2)
                    else if (p2.x < p1.x && p2.y > p1.y) then (p2, p1)
                    else failwith "wtf"

                let topLeftX = bottomLeft.x

                let w = topRight.x - topLeftX + 1
                let h = bottomLeft.y - topRight.y + 1
                (w,h)
        double w * double h

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let inp =
        inp
        |> Seq.map (fun l ->
            let spl = l.Split "," |> Seq.map int |> Seq.toList
            (spl[0], spl[1]).AsP2DXY())
        |> Seq.toList
        
    let surfaces =
        [ for x in inp do
              yield
                  [ for y in inp do
                        if x <> y then
                            yield ((x, y), calcSurface x y) ] ]
        |> Seq.collect id

    (*
    for s in surfaces do
        printfn $"%A{s}"
    *)
    let ans1  = surfaces |> Seq.maxBy snd
    printfn $"%A{ans1}"
    
    0
