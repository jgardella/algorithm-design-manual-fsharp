// A simple, functional implementation of a queue.
module DataStructures.Queue

type Queue<'T> = 
    private 
        { pushList : 'T list 
          popList : 'T list }

let empty =
    { Queue.pushList = []
      popList = [] }

let enqueue (value : 'T) (queue : Queue<'T>) =
    { queue with pushList = value :: queue.pushList }

let rec tryDequeue (queue : Queue<'T>) =
    match (queue.pushList, queue.popList) with
    | ([], []) ->
        (None, queue)
    | (_, popHead :: popRest) ->
        (Some popHead, { queue with popList = popRest })
    | (pushList, []) ->
        tryDequeue { queue with pushList = []; popList = List.rev pushList}