module Algorithms.Sorting.SelectionSort

open Utils

/// Performs an in-place selection sort of the provided array.
let sort (arr : 'T []) =
    for i = 0 to arr.Length - 1 do
        let mutable min = i
        for j = i + 1 to arr.Length - 1 do
            if arr.[j] < arr.[min] then min <- j
        swap arr i min
