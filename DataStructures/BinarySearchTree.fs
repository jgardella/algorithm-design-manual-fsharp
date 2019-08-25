// Implementation of a Binary Search Tree.
namespace DataStructures

type BST<'T> = 
    private 
    | Node of value : 'T * left : BST<'T> * right : BST<'T>
    | Nil

module BST =

    // An empty BST.
    let empty = BST.Nil

    // Tries to find the provided value in the BST.
    let rec tryFind (queryValue : 'T) (bst : BST<'T>) =
        match bst with
        | Node (value, _, _) when value = queryValue -> Some queryValue
        | Node (value, left, _) when value > queryValue -> tryFind queryValue left
        | Node (value, _, right) when value < queryValue -> tryFind queryValue right
        | _ -> None

    // Tries to find the minimum value in the BST.
    let rec tryFindMin (bst : BST<'T>) =
        match bst with
        | Node (value, Nil, _) -> Some value
        | Node (_, left, _) -> tryFindMin left
        | Nil -> None

    // Tries to find the maximum value in the BST.
    let rec tryFindMax (bst : BST<'T>) =
        match bst with
        | Node (value, _, Nil) -> Some value
        | Node (_, _, right) -> tryFindMin right
        | Nil -> None

    // Returns a new tree with the provided value inserted (if it doesn't already exist).
    let rec insert (insertValue : 'T) (bst : BST<'T>) =
        match bst with
        | Node (value, _, _) as node when value = insertValue -> node
        | Node (value, left, right) when insertValue > value ->
            Node (value, insert insertValue left, right)
        | Node (value, left, right) when insertValue < value ->
            Node (value, left, insert insertValue right)
        | _ ->
            Node (insertValue, Nil, Nil)

    // An in-order traversal with a side-effecting function.
    let rec iterInOrder (f : 'T -> unit) (bst : BST<'T>) =
        match bst with
        | Nil -> ()
        | Node (value, left, right) ->
            iterInOrder f left
            f value
            iterInOrder f right

    // A pre-order traversal with a side-effecting function.
    let rec iterPreOrder (f : 'T -> unit) (bst : BST<'T>) =
        match bst with
        | Nil -> ()
        | Node (value, left, right) ->
            f value
            iterInOrder f left
            iterInOrder f right

    // A post-order traversal with a side-effecting function.
    let rec iterPostOrder (f : 'T -> unit) (bst : BST<'T>) =
        match bst with
        | Nil -> ()
        | Node (value, left, right) ->
            iterInOrder f left
            iterInOrder f right
            f value

    /// Returns a new BST with the provided value deleted from the tree.
    let rec delete (deleteValue : 'T) (bst : BST<'T>) =
        let rec deleteMin (bst : BST<'T>) =
            match bst with
            | Nil -> (None, Nil)
            | Node (minValue, Nil, _) -> (Some minValue, Nil)
            | Node (value, left, right) ->
                let (minValue, newLeft) = deleteMin left
                (minValue, Node (value, newLeft, right))

        match bst with
        | Nil -> Nil
        | Node (value, left, right) when value > deleteValue -> Node (value, delete deleteValue left, right)
        | Node (value, left, right) when value < deleteValue -> Node (value, left, delete deleteValue right)
        | Node (value, Nil, Nil) when value = deleteValue -> Nil
        | Node (value, left, Nil) when value = deleteValue -> left
        | Node (_, left, right) ->
            let (minValue, newLeft) = deleteMin left
            match minValue with
            | Some minValue ->
                Node (minValue, newLeft, right)
            | None ->
                right
            