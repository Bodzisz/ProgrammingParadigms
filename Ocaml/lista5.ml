(* Kacper Wójcicki *)

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;
let rec ltake = function
 (0, _) -> []
 | (_, LNil) -> []
 | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);;
let rec toLazyList = function
 [] -> LNil
 | x :: xs -> LCons(x, lazy (toLazyList xs));;

let ltl = function
  LNil -> failwith "ltl"
  | LCons (_, lazy t) -> t;;

let lhd = function
  LNil -> failwith "lhd"
  | LCons (x, _) -> x;;

(* Zad 1 *)
let rec lrepeat k llist = 
  let rec lrepeatIn inCounter element ltail = 
    match inCounter with
    | 1 -> LCons(element, ltail)
    | _ -> LCons(element, lazy(lrepeatIn (inCounter-1) element ltail)) in
  match llist with
  | LNil -> LNil
  | LCons(head, lazy ltail) -> lrepeatIn k head (lazy(lrepeat k ltail));;

ltake(15,(lrepeat 3 (lfrom 3)));;
