module Exercises.Chapter3

open DataStructures

/// 3-1. Determine whether a string has balanced, properly nested parentheses.
module Ch3_1 =
    let isBalanced (input : string) =
        let (finalStack, isBalanced) = 
            input.ToCharArray()
            |> Array.fold (fun (stack, balanced) value ->
                if not balanced then 
                    (stack, balanced)
                else
                    match value with
                    | '(' -> (Stack.push value stack, balanced)
                    | ')' ->
                        let (popped, newStack) = Stack.tryPop stack
                        popped
                        |> Option.map (fun popped -> (newStack, popped = '('))
                        |> Option.defaultValue (newStack, false)
                    | _ -> (stack, balanced)) (Stack.empty, true)

        isBalanced && Stack.isEmpty finalStack