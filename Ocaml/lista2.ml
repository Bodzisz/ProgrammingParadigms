(* Kacper Wójcicki *)

(* Zad 2 *)
let rec fib n = 
  if n < 0 then raise(Failure "Invalid argument!")
  else if n = 0 then 0
  else if n = 1 then 1
  else fib(n-1) + fib(n-2);;

fib(4) = 3;;

let fib2 n = 
  let rec fibRec n x1 x2 = 
    if n = 0 then x2
    else fibRec(n-1)(x2)(x1+x2) in
  if n < 0 then raise(Failure "Invalid argument!")
  else fibRec(n)(1)(0);;

fib2(4) = 3;;

(*fib(44);;
fib2(44);;*)


(* Zad 3 *)
let root3 (a, epsilon) = 
  let rec root3Rec (a, i, res, epsilon) = 
    if abs_float(res *. res *. res -. a) <= epsilon *. abs_float(a) then res
    else
      if i = 0 then
        if a > 1. then root3Rec(a, i+1, a/.3., epsilon)
        else root3Rec(a, i+1, a, epsilon)
      else root3Rec(a, i+1, res +. (a /. (res *. res) -. res)/.3., epsilon) in
  root3Rec(a, 0, 0., epsilon);;

root3(8.0, 0.00000000001) < 2.00 +. 0.00000000001;;
root3(8.0, 0.00000000001) > 2.00 -. 0.00000000001;;

(* Zad 4 *)
let matchA xs = 
  let[_; _; x; _; _] = xs in
    x;;

let matchB xs = 
  let[_; (x, _)] = xs in
  x;;

matchA([-2;-1;0;1;2]) = 0;;
matchB([(1,2);(0,1)]) = 0;;

(* Zad 5 *)
let rec initSegment xs ys = 
  match (xs,ys) with 
  ([], _) -> true
  | (_,[]) -> false
  | (a, b) -> if List.hd a = List.hd b then initSegment(List.tl a)(List.tl b) else false;;

initSegment([1;2;3])([1;2;3;4;5;6]) = true;;
initSegment([1;2;3;4;5;6])([1;2;3]) = false;;
initSegment([])([1;2;3]) = true;;
initSegment([1;2;3])([]) = false;;

(* Zad 6 *)
let replaceNth xs n x =
  let rec replaceIn(xs, n, x, i, rs) =
    if n = i then rs @ [x] @ (List.tl xs)
    else  replaceIn(List.tl xs, n, x, i+1, rs @ [(List.hd xs)]) in
  if n >= List.length xs then raise(Failure "Invalid argument!")
  else replaceIn(xs, n, x, 0, []);;

replaceNth([1;2;3;56;5])(3)(4) = [1;2;3;4;5];;
replaceNth(["My";"name";"is";"Tomek"])(3)("Kacper") = ["My";"name";"is";"Kacper"];;
replaceNth([0;2;3;4])(0)(1) = [1;2;3;4];;


