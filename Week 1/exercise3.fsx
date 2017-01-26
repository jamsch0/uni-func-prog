// Week 1 - Exercise 3
//
// The following is the truth table for a half-adder circuit:
//
//  Input | Output
// -------+-------
//  A | B | C | S
// ---+---+---+---
//  0 | 0 | 0 | 0
//  0 | 1 | 0 | 1
//  1 | 0 | 0 | 1
//  1 | 1 | 1 | 0
//
// Implement this function in F#.
// Note that this function returns two values, but functions should return only
// one value! Is there any way that we could return the pair of values
// together?

let half_adder a b =
    match (a, b) with
    | (true, true) -> (true, false)
    | (true, false) -> (false, true)
    | _ -> (a, b)
