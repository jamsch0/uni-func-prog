// Week 1 - Exercise 2
//
// The logical implication operator has the following truth table:
// 
//  p | q | p -> q
// ---+---+--------
//  T | T | T
//  T | F | F
//  F | T | T
//  F | F | T
//
// Write an F# function called implies that will have the following type
// signature:
//
// val implies : p:bool -> q:bool -> bool
//
// You should use match expressions to pattern match on input values. You may
// also use 'if' expressions if you wish.

let implies p q =
    match (p, q) with
    | (true, false) -> false
    | _ -> true
