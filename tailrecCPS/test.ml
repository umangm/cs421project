(* true *)
5;;

(* true *)
let f = 5;;

(* true *)
let rec f x = f 5;;

(* true *)
let rec f x = x + x;;

(* true *)
let rec f x = hd x;;

(* true *)
let rec f x = if (x=0) then 0 else f (x-1);;

let rec f x = let f = 5 in f + f;;

(* false *)
let rec f x = if (x=0) then 0 else x*(f (x-1));;

(* true *)
let rec f x = if (x=0) then 0+0+0+0 else (f (x-1));;

(* false *)
let rec f x = if (x=0) then 0 else (f (x-1))*x;;

(* true *)
let rec f x = let x_0=(x=0) in if x_0 then 0 else (f (x-1));;

(* false *)
let rec f x = let f_x_1=(f (x-1)) in if x = 0 then 0 else f_x_1;;

(* true *) (**Check**)
let rec f x = let f = f in if (x=0) then 0 else (f (x-1));;

(* true *)
let rec f x = let g = f in if (x=0) then 0 else g (x-1);;

(* false *)
let rec f x = let g = (f (x-1)) in if (x=0) then 0 else g;;

(* true *)
let rec f x = let g = x-1 in if (x=0) then 0 else f g;;

(* true *)
let rec f x = (fun x -> x * x) x ;;

(* true *)
let rec f x = (fun x -> x * (f x)) x ;;

(* true *)
let rec f x = (fun y -> 0) x;;

(* true *)
let rec f x = let rec g y = y*y in if (x=0) then (g 0) else (f (x-1));;

(* true *)
let rec f x = let rec f y = 0 in if (x=0) then (f 0) else (f (x-1));;

(* true *)
let rec f x = let rec g f = 0 in if (x=0) then (g 0) else (f (x-1));;

(* true *)
let rec f x = let rec g y = (f y) in if (x=0) then 0 else (f (x-1));;

(* false *)
let rec f x = let g = (f x) + (f x) in if (x=0) then 0 else (f (x-1));;

(* false *)
let rec f x = let g = (f x) + (f x) in if (x=0) then 0 else g;;

(* true *)
let rec f x = let rec g y = (f y) + (f y) in if (x=0) then 0 else g x;;

(* true *)
let rec f x = let rec g y = (f y) in if (x=0) then 0 else (g (x-1));;

(* false *)
let rec f x = if (x=0) then 0 else f(f (x-1));;

