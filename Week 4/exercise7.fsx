// Week 4 - Exercise 7
//
// Define an appropriate type abbreviation for Cart that appropriately models
// the following:
//
// - The amount of an item bought
// - The item being purchased
//   - The Item ID
//   - The Item name
//   - The Item price

type Cart =
    (int * (int * float * string)) list
