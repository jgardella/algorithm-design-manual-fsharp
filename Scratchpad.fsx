#load "references.fsx"

open Algorithms.Sorting
open Algorithms.Searching

let arr = [|9; 5; 7; 6; 5; 4; 5; 2; 1|]
QuickSort.sort arr
let idx = OccurrenceCount.count arr 5