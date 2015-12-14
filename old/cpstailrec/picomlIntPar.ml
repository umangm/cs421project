(*
 picomlIntPar.ml - DO NOT EDIT
 *)

open Mp7common
open Picomlparse
open Student

let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let rec string_of_exp_cps ext_cps = 
    match ext_cps with VarCPS (k,x) -> "VarCPS: " ^ paren_string_of_cps_cont k ^ " " ^ x 
    | ConstCPS (k,c) -> "ConstCPS: " ^ paren_string_of_cps_cont k ^ " " ^ string_of_const c 
    | MonOpAppCPS (k,m,r, exncont) -> 
       "MonOpAppCPS: " ^ paren_string_of_cps_cont k ^ "(" ^  string_of_mon_op m ^ " " ^ r ^ ")" 
    | BinOpAppCPS (k,b,r,s, exncont) -> 
       "BinOpAppCPS: " ^paren_string_of_cps_cont k ^ "(" ^ r ^ " " ^ string_of_bin_op b ^ " " ^ s ^")" 
    | IfCPS (b,e1,e2) -> "IfCPS: " ^ "IF "^b^" THEN "^ string_of_exp_cps e1 ^" ELSE "^string_of_exp_cps e2 
    | AppCPS (k,r,s, exncont) -> "AppCPS: " ^ "("^r ^ " " ^ s ^ " " ^ paren_string_of_cps_cont k ^ ")"  
    | FunCPS (k, x, i, j, e) ->  "FunCPS: " ^ (paren_string_of_cps_cont k) ^ " (" ^ (string_of_funk x e i j) ^ ")" 
    | FixCPS (k,f,x,i,j, e) -> "FixCPS: " ^ paren_string_of_cps_cont k ^ 
                            "(FIX "^ f ^". " ^ (string_of_funk x e i j) ^ ")" 
and string_of_funk x e i j = 
     "FUN " ^ x  ^ " -> " ^ "fn " ^ (string_of_int i) ^ ", " ^ (string_of_int j) ^ " => " ^ string_of_exp_cps e 
and 
   string_of_cps_cont k = 
    match k with External -> "<external>" 
    | ContVarCPS i -> "_k" ^ (string_of_int i) 
    | FnContCPS (x, e) -> "FN " ^ x ^ " -> " ^ string_of_exp_cps e 
    | ExnMatch exncont -> "<some exception>" 
and 
  paren_string_of_cps_cont k = 
   match k with FnContCPS _ -> "(" ^ string_of_cps_cont k ^ ")" 
   | _ -> string_of_cps_cont k

let convert_f e = 
    match e
    with VarCPS (k, _) -> 
        (match k 
        with FnContCPS (f, _) -> f
        | _ -> raise (Failure "exception convert_f FnContCPS"))
    | _ -> raise (Failure "exception convert_f VarCPS")

let rec check_cps_tail_rec_f f original_k x k e = 
    match e
    with ConstCPS (k', c) -> cont_tail_recursive original_k k' f
    | VarCPS (k', v) -> cont_tail_recursive original_k k' f
    | MonOpAppCPS (k', mono_op, o1, exk) -> cont_tail_recursive original_k k' f
    | BinOpAppCPS (k', bin_op, o1, o2, exk) -> (print_string "BinOp\n"; cont_tail_recursive original_k k' f)
    | IfCPS (b, e1, e2) -> (check_cps_tail_rec_f f original_k x k e1) && (check_cps_tail_rec_f f original_k x k e2)
    | AppCPS (k', e1, e2, exk) -> 
        (print_string ("AppCPS:\n"^e1^", "^e2^", f: "^f^"\n");
        if (e1 = f)
            then 
                (
                if (k'=original_k) 
                    then (print_string "true\n"; true)
                    else (print_string "false\n";false)
                )
        else
            cont_tail_recursive original_k k' f
        )
    | FunCPS (kappa, x, k, ek, e) -> true
    | FixCPS (kappa, f, x, k, ek, e) -> true
    
and cont_tail_recursive original_k k f = 
    match k
    with ContVarCPS i -> true
    | External -> true
    | FnContCPS (x, e) -> 
        if (x=f)
            then true
        else
            (check_cps_tail_rec_f f original_k x k e) (*TODO x ?*)
    | ExnMatch ek -> true

let check_tail_recursion dec =
match dec
with Anon e -> true
| Let (x,e) -> true
| LetRec (f,x,e) ->
let (i,j) = (next_index(),next_index()) in
let ecps2 = cps_exp e (ContVarCPS i) (ExnContVarCPS j) in
print_string ((string_of_exp_cps ecps2)^"\n");
check_cps_tail_rec_f (convert_f ecps2) (ContVarCPS i) x (ContVarCPS i) ecps2


let rec eval_exp_cps env ecps =
match one_step_exp_cps_eval env ecps
with Failed -> (print_string ("This shouldn't happen; it should mean there is a free variable\n of some type without a corresponding value, or a type checking error."); raise (Failure "compiler error"))
| UncaughtException n ->
(print_string ("Exception no. "^string_of_int n^" raised.\n"); None)
| Final v -> Some v
| Intermediate(env', ecps') -> eval_exp_cps env' ecps'

let rec eval_dec (dec, env) = 
match dec
    with Anon e ->
(match eval_exp_cps env (cps_exp e External EmptyExnContCPS)
 with None -> None
 | Some v -> Some (None, v))
    | Let (x,e) -> 
(match eval_exp_cps env (cps_exp e External EmptyExnContCPS)
 with None -> None
 | Some v -> Some (Some x, v))
    | LetRec (f,x,e) ->
    let (i,j) = (next_index(),next_index()) in
    let ecps2 = cps_exp e (ContVarCPS i) (ExnContVarCPS j) in
Some(Some f, (CPSRecClosureVal(f, x, i, j, ecps2, env)))


    let _ =
    (if is_interactive
     then print_endline "\nWelcome to the Student parser \n"
     else ());
    let rec loop gamma mem = 
    try
    let lexbuf = Lexing.from_channel stdin
    in (if is_interactive 
            then (print_string "> "; flush stdout)
            else ());
    (try
     let dec = main (fun lb -> match Picomllex.token lb with 
         | EOF -> raise Picomllex.EndInput
         | r -> r)
     lexbuf 
     in match infer_dec gather_dec_ty_substitution gamma dec with
     | None          -> (print_string "\ndoes not type check\n";
         loop gamma mem)

     | Some (Proof(hyps,judgement)) ->
     (
      match check_tail_recursion dec 
      with true -> print_string "Tail Recursive!\n"; loop gamma mem
      | false -> print_string "Not Tail Recursive!\n"; loop gamma mem
     )  

     with Failure s -> (print_newline();
         print_endline s;
         print_newline();
         loop gamma mem)
     | Parsing.Parse_error ->
     (print_string "\ndoes not parse\n";
      loop gamma mem))
    with Picomllex.EndInput -> exit 0
in (loop [] [])
