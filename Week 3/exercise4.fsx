// Week 3 - Exercise 4
//
// Define a function called mapAsFold that utilises a valid fold implementation
// to implement the 'map' function.

let rec fold f ls z =
    match ls with
    | [] -> z
    | x :: xs -> f x (fold f xs z)

let mapAsFold f ls =
    fold (fun x xs -> f x :: xs) ls []
