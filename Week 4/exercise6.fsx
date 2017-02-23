// Week 4 - Exercise 6
//
// Write a function which converts our custom list type (previously defined)
// into an ordinary list.

type 'a CustomList =
    | Empty
    | Cons of 'a * 'a CustomList

let rec convert (ls: 'a CustomList) =
    match ls with
    | Empty -> []
    | Cons(x, xs) -> x :: convert xs
