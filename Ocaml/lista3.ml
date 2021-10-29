(* Kacper Wójcicki *)

let f1 x = x 2 2;;

let f2 x y z = x ( y ^ z );;

(* Zad  2 *)
let curry3 f x y z = f(x, y ,z);;
let curry3v2 = function f -> function x -> function y -> function z -> f (x,y,z);; 


let uncurry3 f(x, y, z) = f x y z;;

let uncurry3v2 = function f -> function (x,y,z) -> f x y z ;;

