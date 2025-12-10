module aoc2025.day09

open System.Collections.Generic
open aoc2025.util

let useSampleInput = 0

type P2 = int * int
type RectEdges = P2 * P2 * P2 * P2

let getVol rectEdges =
    let (p1x, p1y), (p2x, p2y), (p3x, p3y), (p4x, p4y) = rectEdges

    let minX = min (min p1x p2x) (min p3x p4x)
    let maxX = max (max p1x p2x) (max p3x p4x)
    let minY = min (min p1y p2y) (min p3y p4y)
    let maxY = max (max p1y p2y) (max p3y p4y)

    let w = double (maxX - minX + 1)
    let h = double (maxY - minY + 1)

    let total = w * h
    total


let rectangles = HashSet<RectEdges>()
let memo = Dictionary<P2, bool>()

let isInOuter p =
    if memo.ContainsKey(p) then
        memo[p]
    else
        let px, py = p

        let result =
            rectangles
            |> Seq.tryFind (fun rect ->

                let ((p1x, p1y), (p2x, p2y), (p3x, p3y), (p4x, p4y)) = rect
                let minX = min (min p1x p2x) (min p3x p4x)
                let maxX = max (max p1x p2x) (max p3x p4x)
                let minY = min (min p1y p2y) (min p3y p4y)
                let maxY = max (max p1y p2y) (max p3y p4y)

                let tl_x, tl_y = (minX, minY)
                let tr_x, tr_y = (maxX, minY)

                let bl_x, bl_y = (minX, maxY)
                let br_x, br_y = (maxX, maxY)

                py = tl_y && px >= tl_x && px <= tr_x
                || py = bl_y && px >= bl_x && px <= br_x
                || px = tl_x && py >= tl_y && py <= bl_y
                || px = br_x && py >= tr_y && py <= br_y)
            |> Option.isSome

        if result = true then
            memo[p] <- true

        result


let solve () =
    let io = aocIO

    let inp =
        if useSampleInput = 1 then
            io.readSampleInput ()
        else
            io.getInput ()

    let redTiles =
        inp
        |> Seq.map (fun l ->
            let spl = l.Split "," |> Seq.map int |> Seq.toList
            (spl[0], spl[1]))
        |> Seq.toList

    let redTilePairs =
        [ for p1 in redTiles do
              let (p1x, p1y) = p1

              for p2 in redTiles do
                  if p1 <> p2 then
                      let (p2x, p2y) = p2

                      if p1x <> p2x && p1y <> p2y then

                          let sorted = [ p1; p2 ] |> List.sortBy id
                          let sortedTuple = (sorted[0], sorted[1])

                          if p1y = p2y || p1x = p2x then () else yield sortedTuple ]
        |> List.distinct


    let greenTiles =
        [ for gt in redTiles do
              let (gtx, gty) = gt
              let sameY = redTiles |> Seq.where (fun (tx, ty) -> tx <> gtx && ty = gty)
              let sameX = redTiles |> Seq.where (fun (tx, ty) -> tx = gtx && ty <> gty)

              try
                  let (closestXLeft, _) =
                      sameY |> Seq.where (fun (tx, _) -> tx < gtx) |> Seq.maxBy fst

                  for x in closestXLeft..gtx do
                      yield (x, gty)
              with _ ->
                  ()

              try
                  let (closestXRight, _) =
                      sameY |> Seq.where (fun (tx, _) -> tx > gtx) |> Seq.minBy fst

                  for x in gtx..closestXRight do
                      yield (x, gty)
              with _ ->
                  ()

              try
                  let (_, closestYTop) = sameX |> Seq.where (fun (_, ty) -> ty < gty) |> Seq.maxBy fst

                  for y in closestYTop..gty do
                      yield (gtx, y)
              with _ ->
                  ()

              try
                  let (_, closestYBot) = sameX |> Seq.where (fun (_, ty) -> ty > gty) |> Seq.minBy fst

                  for y in gty..closestYBot do
                      yield (gtx, y)
              with _ ->
                  () ]

        |> List.distinct

    let borders =
        [ for gt in redTiles do
              let (gtx, gty) = gt
              let sameY = redTiles |> Seq.where (fun (tx, ty) -> tx <> gtx && ty = gty)
              let sameX = redTiles |> Seq.where (fun (tx, ty) -> tx = gtx && ty <> gty)

              let p1 = gt.AsP2DXY()

              try
                  let (closestXLeft, _) =
                      sameY |> Seq.where (fun (tx, _) -> tx < gtx) |> Seq.maxBy fst

                  yield Line(p1, (closestXLeft, gty).AsP2DXY())
              with _ ->
                  ()

              try
                  let (closestXRight, _) =
                      sameY |> Seq.where (fun (tx, _) -> tx > gtx) |> Seq.minBy fst

                  yield Line(p1, (closestXRight, gty).AsP2DXY())
              with _ ->
                  ()

              try
                  let (_, closestYTop) = sameX |> Seq.where (fun (_, ty) -> ty < gty) |> Seq.maxBy fst
                  yield Line(p1, (gtx, closestYTop).AsP2DXY())

              with _ ->
                  ()

              try
                  let (_, closestYBot) = sameX |> Seq.where (fun (_, ty) -> ty > gty) |> Seq.minBy fst

                  yield Line(p1, (gtx, closestYBot).AsP2DXY())
              with _ ->
                  () ]

        |> List.distinct

    printfn $"%A{(borders |> Seq.length)}"
    let mutable largest = double 0

    let cross (v: Line) (h: Line) =
        let s = min v.s.y v.e.y
        let e = max v.s.y v.e.y

        let r1 = h.s.y > s && h.s.y < e

        let s = min h.s.x h.e.x
        let e = max h.s.x h.e.x

        let r2 = v.s.x > s && v.s.x < e

        r1 && r2

    let crosses (l1: Line) (l2: Line) =
        let r =
            match (l1.GetOrientation(), l2.GetOrientation()) with
            | (Vertical, Horizontal) -> cross l1 l2
            | (Horizontal, Vertical) -> cross l2 l1
            | _ -> false

        r

    let pr = 0
    let getLines rect =
        let ((p1x, p1y), (p2x, p2y), (p3x, p3y), (p4x, p4y)) = rect
        let minX = min (min p1x p2x) (min p3x p4x)
        let maxX = max (max p1x p2x) (max p3x p4x)
        let minY = min (min p1y p2y) (min p3y p4y)
        let maxY = max (max p1y p2y) (max p3y p4y)

        let tl_x, tl_y = (minX, minY)
        let tr_x, tr_y = (maxX, minY)

        let bl_x, bl_y = (minX, maxY)
        let br_x, br_y = (maxX, maxY)

        let topLine = Line((tl_x, tl_y).AsP2DXY(), (tr_x, tr_y).AsP2DXY())
        let botLine = Line((bl_x, bl_y).AsP2DXY(), (br_x, br_y).AsP2DXY())
        let leftLine = Line((tl_x, tl_y).AsP2DXY(), (bl_x, bl_y).AsP2DXY())
        let rightLine = Line((tr_x, tr_y).AsP2DXY(), (br_x, br_y).AsP2DXY())
        [ topLine; botLine; leftLine; rightLine ]

    let anyCross rect =
        let r =
            getLines rect
            |> List.map (fun line -> borders |> Seq.tryFind (fun b -> crosses b line) |> Option.isSome)

        r |> Seq.tryFind (fun x -> x = true) |> Option.isSome

    let pc (e1, e2) =
        let e3 = (fst e1, snd e2)
        let e4 = (fst e2, snd e1)

        let rect = [ e1; e2; e3; e4 ] |> List.sortBy id
        let sortedEdges = (rect[0], rect[1], rect[2], rect[3])

        if pr = 1 then
            let gr = Grid.createGrid 13 13

            for x in 0 .. ((gr |> Seq.head |> Seq.length) - 1) do
                for y in 0 .. ((gr |> Seq.length) - 1) do
                    gr[y][x] <- '.'

            for (x, y) in redTiles do
                gr[y][x] <- '#'

            for b in borders do
                if b.GetOrientation() = Horizontal then
                    for x in b.s.x .. b.e.x do
                        gr[b.s.y][x] <- 'o'
                else
                    for y in b.s.y .. b.e.y do
                        gr[y][b.s.x] <- 'o'

            for (x, y) in rect do
                gr[y][x] <- 'X'


        if not (anyCross sortedEdges) then

            (*
            let nonIntersect =
                lines
                |> Seq.where (fun l1 -> borders |> Seq.forall (fun b -> not (b.IntersectsStrict l1)))

            let isValid =
                if not (nonIntersect |> Seq.isEmpty) then
                    let res =
                        nonIntersect
                        |> Seq.forall (fun p ->
                            [p.s;p.e] |> Seq.forall(fun p -> 
                                let left = redTiles |> Seq.tryFind (fun (x, y) -> x <= p.x)
                                let right = redTiles |> Seq.tryFind (fun (x, y) -> x >= p.x)
                                let top = redTiles |> Seq.tryFind (fun (x, y) -> y <= p.y)
                                let bot = redTiles |> Seq.tryFind (fun (x, y) -> y >= p.y)
                                left.IsSome && right.IsSome && top.IsSome && bot.IsSome)
                            )
                    res
                else
                    true
                    *)

            
            if
                rect
                |> Seq.forall (fun e -> greenTiles |> List.contains e || (isInOuter e) || redTiles |> List.contains e)
            then
                rectangles.Add(sortedEdges) |> ignore
                let volume = getVol sortedEdges

                if (volume > largest) then
                    largest <- volume
                    printfn $"New largest: %d{(largest |> uint64)}"
    
    

    let getNewEdgesAndUpdateRectangles () = redTilePairs |> List.iter pc

    let mutable temp = 0

    let mutable i = 0

    // stabilize
    while (i = 0 || temp <> (rectangles |> Seq.length)) do
        printfn $"RT Pairs %d{redTilePairs |> Seq.length}"
        printfn $"Rectangles %d{rectangles |> Seq.length}"
        temp <- rectangles |> Seq.length
        getNewEdgesAndUpdateRectangles ()


        printfn $"Largest found: %d{(largest |> uint64)}"

        &i += 1

    for rect in rectangles do
        let volume = getVol rect

        if (volume > largest) then
            largest <- volume

    printfn $"Largest found: %d{(largest |> uint64)}"


    (*
    for rect in rectangles do
        let (e1,e2,e3,e4) = rect
        for (x,y) in [e1;e2;e3;e4] do
            gr[y][x] <- '#'
            *)


    printfn $"stabilized"
    printfn $"%d{(largest |> uint64)}"
