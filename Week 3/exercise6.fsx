// Week 3 - Exercise 6
//
// Define a function (without using fold) called 'any' that checks elements of
// the list against a predicate, if any of the list elements result in a true
// return value from the predicate then the function should return 'true'. In
// other words, the function checks whether a predicate matches any item in the
// list.

let rec any p ls =
    match ls with
    | [] -> false
    | x::xs -> p x || any p xs