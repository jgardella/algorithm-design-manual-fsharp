#load "references.fsx"

open Algorithms.Sorting
open Algorithms.Searching

let arr = [|9; 8; 7; 6; 5; 4; 3; 2; 1|]
QuickSort.sort arr
let idx = BinarySearch.search arr 0