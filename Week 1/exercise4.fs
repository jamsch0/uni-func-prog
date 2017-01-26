// Week 1 - Exercise 4
//
// Write a program that prompts for an input called 'n' which represents an int
// and does the following:
//
// - if n is evenly divisible by 3 then it will print "Bing" to the command
//   line
// - if n is evenly divisible by 5 then it will print "Bang" to the command
//   line
// - if n is evenly divisible by 3 and 5 it will print "BingBang" to the
//   command line
// - if n is not evenly divisible by either 3 or 5 then print out the value of
//   n to the command line

open System

let prompt = "Please input an integer: "

let bing n =
    if n % 3 = 0 then
        "Bing"
    else
        ""

let bang n =
    if n % 5 = 0 then
        "Bang"
    else
        ""

let bingBang n =
    String.concat "" [bing n; bang n]

let printN n =
    let s = bingBang n

    if String.length s > 0 then
        printfn "%s" s
    else
        printfn "%d" n

let rec input() =
    printf "%s" prompt
    match Console.ReadLine() |> System.Int32.TryParse with
    | (true, n) -> n
    | _ -> input()

[<EntryPoint>]
let main _ =
    input() |> printN
    0
