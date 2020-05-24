module Iterative

let factorial n =
    let rec fact_iter product accumulator =
        match product with
        | 1 -> accumulator
        | _ -> fact_iter (product - 1) (accumulator * product)
    fact_iter n 1

let fibonacci n =
    let rec fib_iter product current previous =
        if (product <= 0) then
            current
        else
            fib_iter (product - 1) (current + previous) current
    fib_iter n 1 0
