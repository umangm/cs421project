(* true *)
5;;

(* true *)
let f = 5;;

(* true *)
let rec f x = f 5;;

(* true *)
let rec f x = if (x=0) then 0 else (f (x-1));;

(* false *)
let rec f x = if (x=0) then 0 else x*(f (x-1));;

(* false *)
let rec f x = if (x=0) then 0 else (f (x-1))*x;;

(* true *)
let rec f x = let x_0=(x=0) in if x_0 then 0 else (f (x-1));;

(* false *)
let rec f x = let f_x_1=(f (x-1)) in if x_0 then 0 else f_x_1;;