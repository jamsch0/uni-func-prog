// Week 3 - Exercise 7
//
// Rewrite the 'any' function previously defined using a valid implementation
// of fold.

let rec fold f ls z =
    match ls with
    | [] -> z
    | x :: xs -> f x (fold f xs z)

let any p ls =
    fold (fun x xs -> xs || p x) ls false
