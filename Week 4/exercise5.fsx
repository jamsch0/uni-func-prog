// Week 4 - Exercise 5
//
// Define a function that retrieves the kth element of a list, where k is a
// positive integer.

let rec get ls k =
    match ls with
    | [] -> None
    | x :: xs ->
        if k = 0 then
            Some(x)
        else
            get xs (k - 1)
