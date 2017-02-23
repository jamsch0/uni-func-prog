// Week 3 - Exercise 8
//
// Define a function (without using fold) called 'all' that checks elements of
// the list against a predicate, if all of the list elements result in a true
// return value from the predicate then the function should return 'true'. In
// other words, the function checks whether a predicate matches all items in
// the list.

let rec all p ls =
    match ls with
    | [] -> true
    | x::xs -> p x && all p xs