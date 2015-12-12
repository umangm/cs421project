(*
  picomlIntPar.ml - DO NOT EDIT
*)

open Mp7common
open Picomlparse
open Student
#use "mp7.ml"

let is_interactive = 0 = (Sys.command "[ -t 0 ]")

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
