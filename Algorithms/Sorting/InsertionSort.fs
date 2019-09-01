module Algorithms.Sorting.InsertionSort

open Utils

/// Performs an in-place insertion sort of the provided array.
let sort (arr : 'T []) =
    for i = 1 to arr.Length - 1 do
        let mutable j = i
        while j > 0 && arr.[j] < arr.[j - 1] do
            swap arr j (j - 1)
            j <- j - 1