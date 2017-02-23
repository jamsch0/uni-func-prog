// Week 3 - Exercise 3
//
// Reduce the following expression using expression reduction:
//
// applyTwice (fun x -> x*2) 2;;

let applyTwice f x =
    f (f x)

applyTwice (fun x -> x*2) 2

// let f = (fun x -> x*2) in (fun x -> f (f x)) 2;;

// (fun x -> (fun x -> x*2) ((fun x -> x*2) x)) 2;;

// let x = 2 in (fun x -> x*2) ((fun x -> x*2) x);;

// (fun x -> x*2) (2*2);;

// (fun x -> x*2) 4;;

// 4*2;;

// 8;;
