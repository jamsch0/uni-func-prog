// Week 3 - Exercise 2
//
// Define a function called applyTwice which applies the function f to the
// input x, then applies f to the result of that function.
//
// Example usage:
// applyTwice System.Math.Sqrt 256.0;;
// val it : float = 4.0

let applyTwice f x =
    f (f x)
