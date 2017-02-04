// Week 2 - Exercise 6
//
// Make a function called GCD which works out the largest integer which evenly
// divides the int parameters x and y.
//
// gcd(x, y) = {x                       if y = 0
//             {gcd(y, remainder(x, y)) if x >= y and y > 0
//
// Remember that we can use the % (modulus) operator to calculate the remainder
// when dividing one number by another.
//
// Example expression reduction (just the function applications):
// gcd 518 444
// gcd 444 (518 % 444)
// gcd 444 (74)
// gcd 74 (444 % 74)
// gcd 74 (0)
// 74

let rec gcd x y =
    if y = 0 then
        x
    else
        gcd y (x % y)
