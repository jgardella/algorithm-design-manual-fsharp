module Algorithms.Sorting.QuickSort

open Utils

let private rand = System.Random(int System.DateTime.UtcNow.Ticks)

let private pivot (low : int) (high : int) =
    rand.Next(low, high + 1)

let private partition (arr :'T []) (low : int) (high : int) =
    let pivot = pivot low high
    swap arr pivot high
    let mutable firstHigh = low
    for i = low to high - 1 do
        if arr.[i] < arr.[high] then
            swap arr i firstHigh
            firstHigh <- firstHigh + 1

    swap arr high firstHigh
    firstHigh

let rec private helper (arr : 'T []) (low : int) (high : int) =
    if high - low > 0 then
        let pivot = partition arr low high
        printfn "partition=%A" arr
        helper arr low (pivot - 1)
        helper arr (pivot + 1) high

/// Sorts the provided array in-place using quicksort.
let sort (arr : 'T []) =
    helper arr 0 (arr.Length - 1)