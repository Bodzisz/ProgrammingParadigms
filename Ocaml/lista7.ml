(* Kacper Wójcicki *)

(* Zad 1 a) *)
module type QUEUE_FUN =
sig
 type 'a t
 exception Empty of string
 val empty: unit -> 'a t
 val enqueue: 'a * 'a t -> 'a t
 val dequeue: 'a t -> 'a t
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
end;;

module ListQueue : QUEUE_FUN =
  struct
    type 'a t = 'a list

    exception Empty of string

    let empty () = []

    let enqueue (element, queue) = 
      queue @ [element]

    let dequeue = function
      | [] -> []
      | head :: tail -> tail

    let first = function
      | [] -> raise(Empty "Queue is empty")
      | head :: tail -> head

    let isEmpty queue = 
      queue = []
  end;;


let queue = ListQueue.empty();;
ListQueue.isEmpty(ListQueue.enqueue(1, queue)) = false;;
ListQueue.isEmpty(queue) = true;;
ListQueue.dequeue(ListQueue.enqueue(1, ListQueue.enqueue(2, queue))) = 
  ListQueue.enqueue(1, ListQueue.dequeue(ListQueue.enqueue(2, queue)));;
ListQueue.dequeue(ListQueue.enqueue(1, ListQueue.empty())) = ListQueue.empty();;
ListQueue.dequeue(ListQueue.empty()) = ListQueue.empty();;
ListQueue.first(ListQueue.enqueue(1, ListQueue.enqueue(2, queue))) = 
  ListQueue.first(ListQueue.enqueue(2, queue));;
ListQueue.first(ListQueue.enqueue(1, ListQueue.empty())) = 1;;
try ListQueue.first(ListQueue.empty()) with 
  | ListQueue.Empty e -> e = "Queue is empty"
  | Failure e -> false

(* Zad 1 b) *)
module DoubleListQueue : QUEUE_FUN =
  struct
    type 'a t = 'a list * 'a list

    exception Empty of string

    let empty () = ([], [])

    let enqueue (element, queue) = 
      match queue with
      | ([], []) -> ([element], [])
      | (firstList, secondList) -> (firstList, element :: secondList)
      
    let dequeue = function
      | (_, []) -> ([], [])
      | ([_], secondList) -> (List.rev secondList, [])
      | (_ :: tail, secondList) -> (tail, secondList)

    let first = function
      | ([], []) -> raise(Empty "Queue is empty")
      | (head :: _, _) -> head

    let isEmpty queue = 
      queue = ([], [])
  end;;

let queue = DoubleListQueue.empty();;
DoubleListQueue.isEmpty(DoubleListQueue.enqueue(1, queue)) = false;;
DoubleListQueue.isEmpty(queue) = true;;
DoubleListQueue.dequeue(DoubleListQueue.enqueue(1, DoubleListQueue.enqueue(2, queue))) = 
  DoubleListQueue.enqueue(1, DoubleListQueue.dequeue(DoubleListQueue.enqueue(2, queue)));;
DoubleListQueue.dequeue(DoubleListQueue.enqueue(1, DoubleListQueue.empty())) = DoubleListQueue.empty();;
DoubleListQueue.dequeue(DoubleListQueue.empty()) = DoubleListQueue.empty();;
DoubleListQueue.first(DoubleListQueue.enqueue(1, DoubleListQueue.enqueue(2, queue))) = 
  DoubleListQueue.first(DoubleListQueue.enqueue(2, queue));;
DoubleListQueue.first(DoubleListQueue.enqueue(1, DoubleListQueue.empty())) = 1;;
try DoubleListQueue.first(DoubleListQueue.empty()) with 
  | DoubleListQueue.Empty e -> e = "Queue is empty"
  | Failure e -> false