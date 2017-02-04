// Week 2 - Exercise 7
//
// Rewrite the example solution from the "How many days in a month? (with leap
// year case covered)" section of last week's lab to make use of a
// Discriminated union when matching on Months. This should allow you to
// protect against incorrect inputs to the function by leveraging the type
// system.

type Month =
    | JAN
    | FEB
    | MAR
    | APR
    | MAY
    | JUN
    | JUL
    | AUG
    | SEP
    | OCT
    | NOV
    | DEC

let daysOfMonth month leapYear =
    match month with
    | FEB when leapYear -> 29
    | FEB -> 28
    | APR | JUN | SEP | NOV -> 30
    | _ -> 31
