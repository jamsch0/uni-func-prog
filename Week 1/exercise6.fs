// Week 1 - Exercise 6
//
// Write a program that prompts the user for two floating point numbers, 'x'
// and 'y' as input and then multiplies them together.

open System

let prompt = "Please input a number: "

let rec input() =
    printf "%s" prompt
    match Console.ReadLine() |> System.Double.TryParse with
    | (true, n) -> n
    | _ -> input()

[<EntryPoint>]
let main _ =
    let x = input()
    let y = input()
    
    printfn "%f" (x * y)
    0
