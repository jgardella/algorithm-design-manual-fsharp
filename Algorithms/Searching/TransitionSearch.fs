module Algorithms.Searching.TransitionSearch

let rec private findTransition (arr : int []) (low : int) (high : int) =
    let median = (low + high) / 2
    if low > high then
        low
    elif arr.[median] = 1 then
        findTransition arr low (median - 1)
    else
        findTransition arr (median + 1) high

let private findHigh (arr : int []) =
    let rec helper (n : int) =
        if arr.[n] = 1 then
            n
        else
            helper (n * 2)

    helper 1

/// Finds the transition point of an unbounded array containing a block of 0s
/// followed by a block of 1s by using one-sided binary search.
let transitionSearch (arr : int []) =
    printfn "arr=%A" arr
    let high = findHigh arr
    printfn "high=%d" high
    findTransition arr 0 high