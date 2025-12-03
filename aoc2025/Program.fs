open System.Diagnostics
open aoc2025

let sw = Stopwatch()

sw.Start()
day03.solve ()

sw.Stop()
printfn $"Time taken - %A{sw.Elapsed}"
