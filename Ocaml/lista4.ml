(* Kacper Wójcicki *)

(* Zad  2 *)
let myFunction f a = f a;;

(* Zad  3 *)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tt = Node(1,
 Node(2,
 Node(4,
 Empty,
Empty
 ),
 Empty
 ),
 Node(3,
 Node(5,
 Empty,
Node(6,
 Empty,
Empty
 )
 ),
 Empty
 )
 );; 


let breadthBT bt =
  let rec breadthBTIn nodes resultList =
    if nodes = [] then List.rev(resultList)
    else 
      match (List.hd nodes) with
      Empty -> breadthBTIn (List.tl nodes) (resultList)
      | Node(value, leftChild, rightChild) -> breadthBTIn (List.tl nodes @ [leftChild; rightChild]) (value :: resultList) in
  breadthBTIn [bt] [];;
  
breadthBT tt = [1; 2; 3; 4; 5; 6];;

