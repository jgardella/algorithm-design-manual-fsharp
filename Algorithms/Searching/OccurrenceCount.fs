module Algorithms.Searching.OccurrenceCount

let rec private helper (arr : 'T []) (item : 'T) (low : int) (high : int) (comparator : 'T -> 'T -> bool) =
    let median = (low + high) / 2
    if low > high then
        low
    elif comparator arr.[median] item then
        helper arr item low (median - 1) comparator
    else
        helper arr item (median + 1) high comparator

/// Finds the number of occurrences of the provided item in the given sorted array,
/// using a modified binary search.
let count (arr : 'T []) (item : 'T) =
    let lowBoundary = helper arr item 0 (arr.Length - 1) (>)
    let highBoundary = helper arr item 0 (arr.Length - 1) (<)
    printfn "low=%d|high=%d" lowBoundary highBoundary
    highBoundary - lowBoundary