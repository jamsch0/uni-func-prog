// Week 4 - Exercise 4
//
// Define a function that obtains the length of a list.

let rec length ls =
    match ls with
    | [] -> 0
    | x :: xs -> 1 + length xs
