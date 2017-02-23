// Week 4 - Exercise 2
//
// Using the definition of 'multiplier' create a new function using partial
// application called 'multiplierFive' that multiplies its argument 'y' by 5.

let multiplier (x: bigint) (y: bigint) =
    x * y

let multiplierFive y =
    multiplier 5I y
