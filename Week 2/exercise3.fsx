// Week 2 - Exercise 3
//
// Write a function that /recursively/ prompts for an int input using
// Console.ReadLine(). If the given string provided is valid 32-bit signed
// integer; if the input string can be converted to a valid 32-bit int it
// it returns with that int value, if not it tells the user that there was an
// input error and then prompts for the input again.

open System

let prompt = "Please input an integer: "
let error = "Not a valid integer!"

let rec getInt() =
    printf "%s" prompt
    match Console.ReadLine() |> System.Int32.TryParse with
    | (true, n) -> n
    | _ -> 
        printfn "%s" error
        getInt()
