// Week 2 - Exercise 10
//
// Write a program that mimics the Linux command line utility 'wc' with no
// options specified. Remember according to the 'wc' man file: wc prints
// newline, word, and byte counts for each FILE, and a total line if more than
// one FILE is specified. A word is a non-zero-length sequence of characters
// delimited by whitespace.
//
// Example use of 'wc' with one argument:
// $ wc example1.txt
//  0  4 19 example1.txt
//
// Example use of 'wc' with two arguments:
// $ wc example1.txt example2.txt
//  0  4 19 example1.txt
//  1  5 28 example2.txt
//  1  9 47 total

open System
open System.IO

let usage = "usage: wc [file...]"

let countBytes path =
    (File.ReadAllBytes path).Length

let countWordsAndLines path =
    let lines = File.ReadAllLines path
    let words = Array.collect (fun (line : String) -> line.Split([|' '; '\n'; '\r'; '\t'|], StringSplitOptions.RemoveEmptyEntries)) lines

    (lines.Length, words.Length)

let getCountsForFile path =
    let (lines, words) = countWordsAndLines path
    let bytes = countBytes path
    (lines, words, bytes)

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "%s" usage
        1
    else
        let counts = Array.map getCountsForFile args
        Array.zip counts args |> Array.iter (fun ((lines, words, bytes), path) -> printfn "%d %d %d %s" lines words bytes path)

        if args.Length > 1 then
            let (lines, words, bytes) = Array.fold (fun (lines1, words1, bytes1) (lines2, words2, bytes2) ->
                                                        (lines1 + lines2, words1 + words2, bytes1 + bytes2)) (0, 0, 0) counts
            printfn "%d %d %d %s" lines words bytes "total"

        0
