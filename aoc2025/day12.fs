module aoc2025.day12


(*
5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
*)

open System
open System.Collections.Generic
open System.Linq
open aoc2025.util

let useSampleInput = 0

let shapeWidth = 3
let shapeLength = 3

let overlaps (a: P2D list) (b: P2D list) =
    a |> Seq.exists b.Contains || b |> Seq.exists a.Contains

let addOfset (bl: P2D list) xOff yOff =
    [ for b in bl do
          yield (b.x + xOff, b.y + yOff).AsP2DXY() ]

let rotate (bl: P2D list) =
    let center = (1, 1).AsP2DXY()
    let mutable rotatedShape = []

    if bl.Contains center then
        rotatedShape <- rotatedShape @ [ center ]

    let nRows = shapeLength - 1
    let nCols = shapeWidth - 1

    for row in 0..nRows do
        let xy = (0, row).AsP2DXY()

        if bl.Contains xy then
            rotatedShape <- rotatedShape @ [ (nRows - row, 0).AsP2DXY() ]

        let xy = (2, row).AsP2DXY()

        if bl.Contains xy then
            rotatedShape <- rotatedShape @ [ (nRows - row, 2).AsP2DXY() ]

    for col in 0..nCols do
        let xy = (col, 0).AsP2DXY()

        if bl.Contains xy then
            rotatedShape <- rotatedShape @ [ (2, col).AsP2DXY() ]

        let xy = (col, 2).AsP2DXY()

        if bl.Contains xy then
            rotatedShape <- rotatedShape @ [ (0, col).AsP2DXY() ]


    (*let gr = Grid.createGrid 12 12

    for i in 0..11 do
        for j in 0..11 do
            gr[i][j] <- '.'

    for s in rotatedShape do
        gr[s.y][s.x] <- '#'

    gr.Print()*)
    rotatedShape |> List.distinct

let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let shapes = Dictionary<int, P2D list>()
    let mutable regions = []

    let mutable parsingShapes = true
    let mutable row = -1
    let mutable index = 0

    for l in inp |> Seq.where (fun x -> x.Length > 0) do

        if parsingShapes && l.Contains 'x' then
            parsingShapes <- false

        if parsingShapes then
            if (Char.IsDigit(l[0])) then
                index <- Int32.Parse(l[..0])
                row <- 0
                shapes[index] <- []
            else
                let mutable col = 0

                for ch in l do
                    if ch = '#' then
                        shapes[index] <- shapes[index] @ [ (col, row).AsP2DXY() ]

                    col <- col + 1

                row <- row + 1
        else
            let spl = l.Split ':'
            let dim = spl[0].Split 'x' |> Seq.toArray
            let dim = (Int32.Parse(dim[0]), Int32.Parse(dim[1]))

            let shapeQuantity =
                spl[1].Split ' '
                |> Seq.where (fun x -> x.Length > 0)
                |> Seq.map Int32.Parse
                |> Seq.toList

            regions <- regions @ [ (dim, shapeQuantity) ]

    let shapesWithRotations = Dictionary<int, (P2D list) list>()

    for kv in shapes do
        let mutable allRotated = HashSet()
        let mutable rotated = rotate kv.Value

        for i in 0..4 do
            rotated <- rotate rotated
            allRotated.Add(rotated) |> ignore

        shapesWithRotations[kv.Key] <- allRotated |> Seq.toList

    let printShape sh =
        let gr = Grid.createGrid 4 4

        for i in 0..3 do
            for j in 0..3 do
                gr[i][j] <- '.'

        for bl in sh do
            gr[bl.y][bl.x] <- '#'

        gr.Print()

    let createMaps dim shape =
        let (nCols, nRows) = dim

        //printShape shape
        let canFit = Grid.createGrid<int> nRows nCols
        let covers = Grid.createGrid<P2D list> nRows nCols

        let boundary =
            { minX = 0
              maxX = nCols - 1
              minY = 0
              maxY = nRows - 1 }

        let iib = Grid.isInBounds boundary

        for x in 0 .. nCols - 1 do
            for y in 0 .. nRows - 1 do
                let withOffset = addOfset shape x y
                let canFitAtPos = withOffset |> Seq.forall (iib)

                if canFitAtPos then
                    covers[y][x] <- withOffset

                canFit[y][x] <- if canFitAtPos then 1 else 0

        (canFit, covers)

    let mutable nCanFit = 0

    for (dim, shapeQuantities) in regions do

        let mutable options = Dictionary()

        for i in 0 .. (shapeQuantities.Length) - 1 do
            let shapeQuantity = shapeQuantities[i]

            if shapeQuantity > 0 then
                let shapeIdx = i

                let opt = shapesWithRotations[shapeIdx]
                let optsMaps = opt |> List.map (createMaps dim)
                options[i] <- optsMaps

        let q = PriorityQueue()
        q.Enqueue(((0, 0).AsP2DXY(), [], shapeQuantities), 0)

        let incrCpos (cpos: P2D) =
            let (maxX, maxY) = dim
            let (maxX, maxY) = (maxX - 1, maxY - 1)

            let incr =
                if (cpos.x >= maxX && cpos.y >= maxY) then None
                else if cpos.x >= maxX then Some((0, cpos.y + 1))
                else if cpos.x < maxX then Some((cpos.x + 1, cpos.y))
                else failwith "wtf"

            incr |> Option.map _.AsP2DXY()

        let mutable canFit = false

        let nSquares = (fst dim) * (snd dim)

        while q.Count > 0 do
            let (cpos, agglomeration, sq) = q.Dequeue()

            let squaresLeft =
                [ for i in 0 .. (sq.Length) - 1 do
                      yield sq[i] * shapes[i].Length ]
                |> Seq.sum

            if sq |> Seq.forall (fun x -> x = 0) then
                q.Clear()
                canFit <- true
            else if (agglomeration.Length + squaresLeft) < nSquares then
                let mutable newPos = incrCpos cpos

                while (newPos.IsSome && agglomeration |> Seq.contains newPos.Value) do
                    newPos <- incrCpos newPos.Value

                if newPos.IsSome then
                    q.Enqueue((newPos.Value, agglomeration, sq), squaresLeft)

                    for i in 0 .. (sq.Length) - 1 do
                        if sq[i] > 0 then

                            for (canFit, covers) in options[i] do
                                if
                                    canFit[cpos.y][cpos.x] = 1
                                    && (not (overlaps (covers[cpos.y][cpos.x]) agglomeration))
                                then
                                    let newPoints = covers[cpos.y][cpos.x]
                                    let newAgglo = agglomeration @ newPoints

                                    let mutable newSq =
                                        [ for j in 0 .. (sq.Length) - 1 do
                                              yield if j = i then (sq[j] - 1) else sq[j] ]

                                    let prio = newSq |> Seq.sum

                                    q.Enqueue((newPos.Value, newAgglo, newSq), prio)

        printfn $"%A{((dim, shapeQuantities, canFit))}"

        if canFit then
            nCanFit <- nCanFit + 1

        printfn $"%A{nCanFit}"


    (*
    printfn $"%A{shapesWithRotations}"
    x (4, 4) shapes[0]
    *)
    0
