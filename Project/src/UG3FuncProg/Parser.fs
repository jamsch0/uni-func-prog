module Parser

open ParseResult
open System

type Parser<'T, 'E> =
    { name: string; func: string -> ParseResult<'T * string, 'E * string> }

let parser name func =
    { name = name; func = func }

let rename name parser =
    { name = name; func = parser.func }

let parse parser input =
    parser.func input

let rec parseMany parser input =
    match parse parser input with
    | Okay (value, remaining) ->
        parseMany parser remaining
        |> ParseResult.map (fun (values, remaining) ->
            (value::values, remaining)
        )
    | Err _ -> Okay ([], input)
    | FatalErr err -> FatalErr err

// Monad-y functions

let unit value =
    parser "" (fun input -> Okay (value, input))

let bind f p =
    parser p.name (fun input ->
        parse p input |> ParseResult.bind (fun (value, remaining) ->
            parse (f value) remaining
        )
    )

let map f p =
    parser p.name (fun input ->
        parse p input |> ParseResult.map (fun (value, remaining) ->
            (f value, remaining)
        )
    )

let mapErr f p =
    parser p.name (fun input ->
        parse p input |> ParseResult.mapErr f
    )

let rec fold f acc parsers =
    match parsers with
    | [] -> unit acc
    | head::tail -> f head (fold f acc tail)

// Parser combinator functions

let complete p =
    parser p.name (fun input ->
        match parse p input with
        | Okay (value, remaining) -> Okay (value, remaining)
        | Err (err, name) | FatalErr (err, name) -> FatalErr (err, name)
    )

let optional p =
    parser p.name (fun input ->
        match parse p input with
        | Okay (value, remaining) -> Okay (Some value, remaining)
        | Err _ -> Okay (None, input)
        | FatalErr err -> FatalErr err
    )

let peek p =
    parser p.name (fun input ->
        match parse p input with
        | Okay (value, _) -> Okay (value, input)
        | Err err -> Err err
        | FatalErr err -> FatalErr err
    )

let both p q =
    parser (p.name + q.name) (fun input ->
        parse p input |> ParseResult.bind (fun (valueP, remaining) ->
            parse q remaining |> ParseResult.map (fun (valueQ, remaining) ->
                ((valueP, valueQ), remaining)
            )
        )
    )

let either p q =
    parser (sprintf "%s or %s" p.name q.name) (fun input ->
        parse p input |> ParseResult.either Okay (fun _ -> parse q input)
    )

let preceded opening p =
    both opening p |> map (fun (_, value) -> value)

let terminated p closing =
    both p closing |> map (fun (value, _) -> value)

let delimited opening p closing =
    preceded opening p |> terminated <| closing

let separated p separator q =
    both p <| preceded separator q

let all parsers =
    parsers |> fold (fun a b -> both a b |> map List.Cons) []

let any parsers =
    parsers |> List.reduce either
    |> rename (sprintf "any of %s" (parsers |> List.map (fun p -> p.name) |> String.concat ", "))

let many p =
    parser (sprintf "zero or more of %s" p.name) (parseMany p)

let many1 p =
    parser (sprintf "one or more of %s" p.name) (fun input ->
        match parse p input with
        | Okay (value, remaining) ->
            parseMany p remaining |> ParseResult.map (fun (values, remaining) ->
                (value::values, remaining)
            )
        | Err err -> Err err
        | FatalErr err -> FatalErr err
    )

let separatedList p separator =
    both (many (terminated p separator)) (optional p) |> map (function
        | (values, Some value) -> List.append values [value]
        | (values, None) -> values
    )

// Parser functions

let toString p =
    p |> map (Array.ofList >> String)

let empty =
    let name = "end of file"
    
    parser name (fun input ->
        if String.IsNullOrEmpty (input) then
            Okay (char -1, "")
        else
            Err (input.[0], name)
    )

let anyCharacter =
    let name = "any character"
    
    parser name (fun input ->
        if String.IsNullOrEmpty (input) then
            Err (char -1, name)
        else
            Okay (input.[0], input.[1..])
    )

let character c =
    let name = sprintf "'%c'" c
    
    parser name (fun input ->
        if String.IsNullOrEmpty (input) then
            Err (char -1, name)
        else
            let first = input.[0]
            
            if first = c then
                Okay (first, input.[1..])
            else
                Err (first, name)
    )

let notCharacter c =
    let name = sprintf "not '%c'" c
    
    parser name (fun input ->
        if String.IsNullOrEmpty (input) then
            Err (char -1, name)
        else
            let first = input.[0]
            
            if first <> c then
                Okay (first, input.[1..])
            else
                Err (first, name)
    )

let whitespace =
    [' '; '\t'; '\n'; '\r'] |> List.map character |> any
    |> rename " "

let digit =
    ['0'..'9'] |> List.map character |> any
    |> rename "digit"

let alpha =
    ['a'..'z'] |> List.append ['A'..'Z'] |> List.map character |> any
    |> rename "alpha"

let alphanumeric =
    either alpha digit
    |> rename "alphanumeric"

let string s =
    let name = sprintf "\"%s\"" s
    
    List.ofSeq s |> List.map character |> all |> toString
    |> rename name |> mapErr (fun (err, _) -> (err, name))
