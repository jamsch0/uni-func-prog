// Week 3 - Exercise 1
//
// Reduce the following function application manually using expression
// reduction.
//
// map (fun x -> x+2) [1;2;3;4]

let rec map f ls =
    match ls with
    | [] -> []
    | x :: xs -> f x :: map f xs

map (fun x -> x+2) [1;2;3;4]

// let f = (fun x -> x+2) in f 1 :: map f [2;3;4]
// (fun x -> x+2) 1 :: map (fun x -> x+2) [2;3;4]
// 1+2 :: map (fun x -> x+2) [2;3;4]
// 3 :: map (fun x -> x+2) [2;3;4]
// let f = (fun x -> x+2) in 3 :: f 2 :: map f [3;4]
// 3 :: (fun x -> x+2) 2 :: map (fun x -> x+2) [3;4]
// 3 :: 2+2 :: map (fun x -> x+2) [3;4]
// 3 :: 4 :: map (fun x -> x+2) [3;4]
// let f = (fun x -> x+2) in 3 :: 4 :: f 3 :: map f [4]
// 3 :: 4 :: (fun x -> x+2) 3 :: map (fun x -> x+2) [4]
// 3 :: 4 :: 3+2 :: map (fun x -> x+2) [4]
// 3 :: 4 :: 5 :: map (fun x -> x+2) [4]
// let f = (fun x -> x+2) in 3 :: 4 :: 5 :: f 6 :: map f []
// 3 :: 4 :: 5 :: (fun x -> x+2) 4 :: map (fun x -> x+2) []
// 3 :: 4 :: 5 :: 4+2 :: map (fun x -> x+2) []
// 3 :: 4 :: 5 :: 6 :: map (fun x -> x+2) []
// 3 :: 4 :: 5 :: 6 :: []
// [3;4;5;6]
