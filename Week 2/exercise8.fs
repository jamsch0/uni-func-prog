// Week 2 - Exercise 8
//
// Write a program that mimics the Linux command line utility 'rev' with no
// options specified.
//
// $ rev example1.txt
// xof nworb kciuq eht
//
// $ rev example2.txt
// revo depmuj
// god yzal eht

open System
open System.IO

let usage = "usage: rev [file...]"

let reverseLine line =
    String(Array.ofSeq line |> Array.rev)

let reverseLines path =
     File.ReadAllLines(path) |> Seq.map reverseLine

let printLines path =
    reverseLines path |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "%s" usage
        1
    else
        Array.iter printLines args
        0
