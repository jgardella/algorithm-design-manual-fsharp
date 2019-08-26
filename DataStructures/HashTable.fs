// An implementation of a mutable HashTable.
namespace DataStructures

type HashTable<'Key, 'Value> =
    private {
        arr : (('Key * 'Value) list) []
        capacity : int
    }

module HashTable =

    let empty (capacity : int)=
        { HashTable.arr = Array.create capacity []
          capacity = capacity }

    let insert (key : 'Key) (value : 'Value) (table : HashTable<'Key, 'Value>) =
        let index = (hash key) % table.capacity
        table.arr.[index] <- (key, value) :: table.arr.[index]

    let tryFind (targetKey : 'Key) (table : HashTable<'Key, 'Value>) =
        let index = (hash targetKey) % table.capacity
        table.arr.[index]
        |> List.tryFind (fun (key, _) -> targetKey = key)
        |> Option.map snd

    let remove (targetKey : 'Key) (table : HashTable<'Key, 'Value>) =
        let index = (hash targetKey) % table.capacity
        table.arr.[index] <- table.arr.[index] |> List.filter (fun (key, _) -> targetKey = key)