// Week 4 - Exercise 8
//
// Define a function that will have the following type:
//
// val getTotal : x:Cart -> float
//
// That takes in an instance of the 'Cart' type and returns the total price.

type Cart =
    (int * (int * float * string)) list

let getTotal cart =
    List.fold (fun total (amount, (id, cost, name)) -> total + (cost * float(amount))) 0.0 cart
