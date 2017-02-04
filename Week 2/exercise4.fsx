// Week 2 - Exercise 4
//
// Implement the logical 'and' function (&&) on the previously defined BoolCustom type.

type BoolCustom =
    | TRUE
    | FALSE

let notCustom x =
    match x with
    | TRUE -> FALSE
    | FALSE -> TRUE

let andCustom x y =
    match (x, y) with
    | (TRUE, TRUE) -> TRUE
    | _ -> FALSE
