// Week 3 - Exercise 5
//
// Define a function called filterAsFold that utilises a valid fold
// implementation to implement the 'filter' function.

let rec fold f ls z =
    match ls with
    | [] -> z
    | x :: xs -> f x (fold f xs z)

let filterAsFold p ls =
    fold (fun x xs -> if p x then x::xs else xs) ls []
