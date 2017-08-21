module UG3FuncProg.Project

open ParseResult
open ProgrammingLanguage
open System.IO

let usage = "Usage: parser <FILE>"

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        printfn "%s" usage
        1
    else
        match parser (File.ReadAllText (args.[0])) with
        | Okay (ast, _) ->
            printfn "%A" ast
            0
        | Err (err, name) | FatalErr (err, name) ->
            let err = if err = char -1 then
                          "end of file"
                      else
                          string err
            
            printfn "Unexpected '%s'! Expecting: \"%s\"" err name
            1
