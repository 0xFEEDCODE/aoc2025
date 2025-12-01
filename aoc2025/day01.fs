module aoc2025.day01

open aoc2025.util

[<Literal>]
let RotationSlots = 100

let performOperation (op: int -> int -> int, dist) prevDial = op prevDial (dist % RotationSlots)

let getNewClicks countRotations opAndDistance prevDial =
    let rotationClicks = (snd opAndDistance) / RotationSlots

    countRotations?(rotationClicks, 0)
    + match (performOperation opAndDistance prevDial) with
      | nd when countRotations && nd < 0 && prevDial <> 0 -> 1
      | nd when countRotations && nd > RotationSlots && prevDial <> 0 -> 1
      | 0
      | RotationSlots -> 1
      | _ -> 0

let getNewDial opAndDistance prevDial =
    match (performOperation opAndDistance prevDial) with
    | nd when nd < 0 -> RotationSlots - abs nd
    | nd when nd >= RotationSlots -> nd % RotationSlots
    | nd -> nd

let rotateLock countRotations (accClicks, prevDial) opAndDistance =
    let newDial = getNewDial opAndDistance prevDial

    let newClicks = getNewClicks countRotations opAndDistance prevDial

    (accClicks + newClicks, newDial)

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let doc =
        inp
        |> Seq.map (fun x ->
            let rotOp =
                match x[0] with
                | 'L' -> (-)
                | 'R' -> (+)

            let dist = x[1..] |> int
            (rotOp, dist))

    let dial = 50
    let ans1, _ = ((0, dial), doc) ||> Seq.fold (rotateLock false)
    let ans2, _ = ((0, dial), doc) ||> Seq.fold (rotateLock true)

    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
