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
    | 1 -> LCons(element, ltail)
    | _ -> LCons(element, lazy(lrepeatIn (inCounter-1) element ltail)) in
  match llist with
  | LNil -> LNil
  | LCons(head, lazy ltail) -> lrepeatIn k head (lazy(lrepeat k ltail));;

ltake(15, lrepeat 3 (lfrom 3)) = [3; 3; 3; 4; 4; 4; 5; 5; 5; 6; 6; 6; 7; 7; 7];;
ltake(15, lrepeat 3 (LNil)) = [];;
ltake(12, lrepeat 4 (toLazyList ['a';'b';'c'])) = ['a';'a';'a';'a';'b';'b';'b';'b';'c';'c';'c';'c'];;

(* Zad 2 *)
let lfib =
  let rec lfibIn actual next =
    LCons(actual, lazy(lfibIn next (actual + next))) in
  lfibIn 0 1;;

ltake(10, lfib) = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34];;
