module Algorithms.Sorting.SelectionSort

let private swap (arr : 'T []) (i1 : int) (i2 : int) =
    let item1 = arr.[i1]
    arr.[i1] <- arr.[i2]
    arr.[i2] <- item1

/// Performs an in-place selection sort of the provided array.
let sort (arr : 'T []) =
    for i = 0 to arr.Length - 1 do
        let mutable min = i
        for j = i + 1 to arr.Length - 1 do
            if arr.[j] < arr.[min] then min <- j
        swap arr i min
