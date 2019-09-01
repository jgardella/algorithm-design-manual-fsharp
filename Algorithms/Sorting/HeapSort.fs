module Algorithms.Sorting.HeapSort

open DataStructures.Heap

/// Sorts an array in-place using heap sort.
let sort (arr : 'T []) =
    let heap = Heap(arr)
    for i = 0 to arr.Length - 1 do
        arr.[i] <- heap.PopMin()