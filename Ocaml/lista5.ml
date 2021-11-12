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

(* Zad 1 *)
let rec lrepeat k llist = 
  let rec lrepeatIn inCounter element ltail = 
    match inCounter with
    | 0 -> LCons(element, ltail)
    | _ -> LCons(element, lazy(lrepeatIn (inCounter-1) element ltail)) in
  match llist with
  | LNil -> LNil
  | LCons(x, xf) -> lrepeatIn (k) (x) (lrepeat (k) (xf));;



  
ltake(15,(lrepeat 3 (lfrom 3)));;