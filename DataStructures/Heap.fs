module DataStructures.Heap

open Utils

let private parent (n : int) = 
    n / 2

let private youngChild (n : int) =
    n * 2

let private oldChild (n : int) = 
    n * 2 + 1

type Heap<'T when 'T : comparison> (capacity : int) =
    let (arr : 'T []) = Array.zeroCreate (capacity + 1)
    let mutable last = 0

    /// Initializes a heap based on the given array.
    /// The heap will have capacity equivalent to the length of the given array.
    new (initArr : 'T []) as this =
        Heap(initArr.Length)
        then initArr |> Array.iter this.Insert

    member private this.BubbleUp (n : int) =
        if n > 1 then
            let parentIdx = parent n
            if arr.[n] < arr.[parentIdx] then
                swap arr n parentIdx
                this.BubbleUp parentIdx

    member private this.BubbleDown (n : int) =
        printfn "n=%d|youngChild=%d|oldChild=%d" n (youngChild n) (oldChild n)
        let minIndex =
            [|
                n
                youngChild n
                oldChild n
            |] 
            |> Array.choose (fun i -> 
                Array.tryItem i arr |> Option.map (fun value -> (i, value)))
            |> Array.minBy snd
            |> fst

        if minIndex <> n && minIndex <= last then
            swap arr n minIndex
            this.BubbleDown minIndex

    /// Inserts an item into the heap.
    member this.Insert (item : 'T) =
        last <- last + 1
        arr.[last] <- item
        this.BubbleUp last

    /// Pops the minimum element from the heap.
    member this.PopMin () =
        let min = arr.[1]
        arr.[1] <- arr.[last]
        last <- last - 1
        this.BubbleDown 1
        printfn "%A" arr
        min