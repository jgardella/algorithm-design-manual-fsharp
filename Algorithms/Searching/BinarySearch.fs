module Algorithms.Searching.BinarySearch

let rec private helper (arr : 'T []) (item : 'T) (low : int) (high : int) =
    let median = low + (high - low / 2)
    if low > high then
        None
    else 
        match arr.[median] with
        | x when x > item -> helper arr item low (median - 1)
        | x when x < item -> helper arr item (median + 1) high
        | _ -> Some median

/// Tries to find the index of the element in a sorted array using binary search.
let search (arr : 'T []) (item : 'T)  =
    helper arr item 0 (arr.Length - 1)
