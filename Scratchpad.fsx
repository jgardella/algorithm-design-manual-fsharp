#load "references.fsx"

open Algorithms.Searching

Array.append (Array.create 500 0) (Array.create 1500 1)
|> TransitionSearch.transitionSearch