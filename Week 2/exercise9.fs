// Week 2 - Exercise 9
//
// Write a program that mimics the Linux command line utility 'tac' with no
// options specified.
//
// $ tac example1.txt
// the quick brown fox
//
// $ tac example2.txt
// the lazy dog
// jumped over
//
// $ tac example1.txt example2.txt
// the quick brown fox
// the lazy dog
// jumped over

open System.IO

let usage = "usage: tac [file...]"

let reverseLines path =
    File.ReadAllLines(path) |> Seq.rev

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
