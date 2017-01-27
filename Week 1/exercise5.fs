// Week 1 - Exercise 5
//
// Write a simple version of the 'echo' Linux command line utility in F#.
//
// Example usage:
// echo inputstring1
//
// Example result:
// inputstring1
//
// If you have difficulty remembering how 'echo' works as a utility then see
// The JavaScript Linux emulator to re-familiarise yourself. Think carefully
// about how this program works from the perspective of the user.

open System

let substituteEnvVar (arg : String) =
    if arg.StartsWith "%" && arg.EndsWith "%" then
        match Environment.GetEnvironmentVariable(arg.Substring(1, arg.Length - 2)) with
        | value when isNull value -> arg
        | value -> value
    else
        arg

[<EntryPoint>]
let main args =
    Array.map substituteEnvVar args |> String.concat " " |> printfn "%s"
    0
