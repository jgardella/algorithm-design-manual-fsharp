module Algorithms.Sorting

let swap (arr : 'T []) (i1 : int) (i2 : int) =
    let item1 = arr.[i1]
    arr.[i1] <- arr.[i2]
    arr.[i2] <- item1