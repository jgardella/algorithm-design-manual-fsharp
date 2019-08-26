#load "references.fsx"

open DataStructures

let hashTable = HashTable.empty 100

HashTable.insert "a" 1 hashTable
HashTable.insert "a" 1 hashTable
HashTable.insert "b" 2 hashTable
HashTable.insert "c" 3 hashTable

let (value : int option) = HashTable.tryFind "a" hashTable