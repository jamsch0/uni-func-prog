// Week 4 - Exercise 10
//
// There is a famous Computer Science puzzle called the 'Towers of Hanoi'.
//
//      -+-            |             |
//     --+--           |             |
//    ---+---          |             |
//   ----+----         |             |
//  -----+-----        |             |
// ======+====== ======+====== ======+======
//       A             B             C
//
// The goal of the puzzle is to move all the disks from the leftmost peg to the
// rightmost peg. The rules of the puzzle are as follows:
//
// - Move only one disk at a time.
// - A larger disk may not be placed on top of a smaller disk.
// - All disks, except the one being moved, must be on a peg.
//
// Exercise 10.1
// Write a function in F# that works out the minimum amount of moves needed to
// solve the Tower of Hanoi (given 'n' amount of discs).
// This should make use of the formula: 2^n - 1
//
// Exercise 10.2
// Write a recursive function in F# that attempts to solve the puzzle by
// stating the list of moves required. For example:
//
// Move disc from peg a to peg b
// Move disc from peg a to peg c
// etc.

let minMoves n =
    (pown 2 n) - 1

let rec towerOfHanoi n a b c =
    if n > 0 then
        towerOfHanoi (n - 1) a c b
        printfn "Move disc from peg %c to peg %c" a c
        towerOfHanoi (n - 1) b a c
