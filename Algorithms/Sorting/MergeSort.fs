module Algorithms.Sorting.MergeSort

let rec private merge (xs : 'T list) (ys : 'T list) =
    match (xs, ys) with
    | (x :: xTail, y :: _) when x < y -> 
        x :: (merge xTail ys)
    | (_ :: _, y :: yTail) -> 
        y :: (merge xs yTail) 
    | (xs , []) -> xs
    | ([], ys) -> ys

/// Returns a copy of the list which has been sorted using merge sort.
let rec sort (xs : 'T list) =
    match xs with
    | []
    | [_] -> xs
    | _ ->
        let (fst, snd) = List.splitAt (List.length xs / 2) xs
        merge (sort fst) (sort snd)
