// Week 3 - Exercise 9
//
// Rewrite the 'all' function previously defined using a valid implementation
// of fold.

let rec fold f ls z =
    match ls with
    | [] -> z
    | x :: xs -> f x (fold f xs z)

let all p ls =
    fold (fun x xs -> xs && p x) ls true
