/// Simple implementation of a stack.
module DataStructures.Stack

type Stack<'T> = 
    private { list : 'T list }

/// Creates an empty stack.
let empty = { Stack.list = [] } 

/// Pushes a value to the top of the stack.
let push (value : 'T) (stack : Stack<'T>) =
    { stack with list = value :: stack.list }

/// Tries to peek the value on the top of the stack.
let tryPeek (stack : Stack<'T>) =
    (List.tryHead stack.list, stack)

/// Tries to pop a value from the top of the stack.
let tryPop (stack : Stack<'T>) =
    (List.tryHead stack.list, { stack with list = List.tail stack.list })

/// Returns true if the stack is empty.
let isEmpty (stack : Stack<'T>) =
    List.isEmpty stack.list