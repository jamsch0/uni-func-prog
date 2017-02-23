// Week 4 - Exerise 9
//
// Define a function that takes in an input of type Shape and calculates the
// relevant area for that shape.

open System

type radius = decimal
type length = decimal

type Shape =
    | Circle of radius
    | Square of length

let area s =
    match s with
    | Circle(r) -> decimal(Math.PI) * (r * r)
    | Square(l) -> l * l
