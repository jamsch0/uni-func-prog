// Week 4 - Exercise 3
//
// Define a function that finds the last element of a list.

let rec last ls =
    match ls with
    | [] -> None
    | [x] -> Some(x)
    | x :: xs -> last xs
