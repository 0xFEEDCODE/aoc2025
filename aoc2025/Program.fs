open System.Diagnostics
open aoc2025

let sw = Stopwatch()

sw.Start()
day06.solve ()

sw.Stop()
printfn $"Time taken - %A{sw.Elapsed}"
