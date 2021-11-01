(* Kacper Wójcicki *)

(* Zad  2 *)
let curry3 f x y z = f(x, y ,z);;
let curry3v2 = function f -> function x -> function y -> function z -> f (x,y,z);; 

let uncurry3 f(x, y, z) = f x y z;;
let uncurry3v2 = function f -> function (x,y,z) -> f x y z ;;

(* Zad  3 *)
let sumProd xs =
  List.fold_left (fun acc x -> (x + fst(acc), x * snd(acc))) (0,1) xs;;

sumProd [1;2;3;4] = (10, 24);;
sumProd [] = (0, 1);;

