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

[<EntryPoint>]
let main args =
    String.concat " " args |> printfn "%s"
    0
