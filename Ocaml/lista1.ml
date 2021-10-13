(* Kacper Wójcicki *)


(* Zadanie 1 *)
let rec flatten2 xs rs = 
  if xs = [] then rs
  else flatten2 (List.tl xs) (rs @ List.hd xs);;

let flatten1 xs = flatten2 xs [];;

flatten1 [[5;6];[1;2;3]] = [5; 6; 1; 2; 3];;
flatten1 [[]] = [];;
flatten1 [[['a'; 'b']];[['c'; 'd']]] = [['a'; 'b'];['c'; 'd']]

(* Zadanie 2 *)

let rec countRec x xs r = 
  if xs = [] then r
  else if List.hd xs = x then countRec (x) (List.tl xs) (r+1)
  else countRec (x) (List.tl xs) (r);;

let count x xs = countRec x xs 0;;

count ('a') (['a';'l';'a']) = 2;;
count (1) ([2;3;4]) = 0;;
count (1) ([]) = 0;;

(* Zadanie 3 *)

let rec replicateRec x n xs = 
  if n = 0 then xs
  else replicateRec (x) (n-1) (x::xs);;

let replicate x n = replicateRec x n [];;

replicate 'a' 3 = ['a';'a';'a'];;
replicate 'a' 0 = [];;

(* Zadanie 4 *)

let rec sqrListRec xs rs = 
  if xs = [] then List.rev rs
  else sqrListRec (List.tl xs) ((List.hd xs * List.hd xs) :: rs);;

let sqrList xs = sqrListRec xs [];;

sqrList [1;2;3;4] = [1;4;9;16];;

(* Zadanie 5 *)

let palindrome xs = 
  if List.rev xs = xs then true
  else false;;

  palindrome ['k';'a';'j';'a';'k'] = true;;

(* Zadanie 6 *)

let rec listLengthRec xs r = 
  if xs = [] then r
  else listLengthRec (List.tl xs) (r+1);;


let listLength xs = listLengthRec xs 0;;

listLength [1;2;3] = 3;;
listLength [] = 0;;