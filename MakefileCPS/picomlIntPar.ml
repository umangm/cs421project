(*
 picomlIntPar.ml - DO NOT EDIT
 *)

open Definitions
open Picomlparse
open CheckTailRecCPS

let is_interactive = 0 = (Sys.command "[ -t 0 ]");;


let rec convert_f_exp e name_of_f = 
(match e
 (*with  ConstCPS (k, c) -> print_string "\nConstCPS \n"; raise (Failure "exception convert_f ContsCPS")*)
 with  ConstCPS (k, c) -> 
 print_string "\nConstCPS \n";
 convert_f_cont k name_of_f
 | VarCPS (k, g) -> 
 print_string "\nVarCPS \n";
 (match k 
  with FnContCPS (number_of_f, e') -> let rec_list = convert_f_exp e' name_of_f 
  in 
  if (g = name_of_f) then (number_of_f :: rec_list) else rec_list
  | _ -> []
 )
 | MonOpAppCPS (k, _, _, _) -> 
 print_string "\nMonOpAppCPS \n";
 convert_f_cont k name_of_f
 | BinOpAppCPS (k , _, _, _, _) -> 
 print_string "\nBinOpAppCPS \n";
 convert_f_cont k name_of_f
 | IfCPS (b, e1, e2) ->
 print_string "\nIfCPS \n";
(convert_f_exp e1 name_of_f)@(convert_f_exp e2 name_of_f)
    | AppCPS (k, _, _, _) ->
    print_string "\nAppCPS \n";
    convert_f_cont k name_of_f
    | FunCPS (k, _, _, _, _) ->
    print_string "\nFunCPS \n";
    convert_f_cont k name_of_f
    | FixCPS (k, _, _, _, _, _) ->
    print_string "\nFixCPS \n";
    convert_f_cont k name_of_f
    )
    and 
    convert_f_cont k name_of_f =
    (
     match k
     with FnContCPS (_, e') -> convert_f_exp e' name_of_f
     | _ -> []
    ) ;;


    let rec check_cps_tail_rec_f flist original_k x k e = 
    match e
    with ConstCPS (k', c) -> cont_tail_recursive original_k k' flist
    | VarCPS (k', v) -> print_string "\nVarCPS\n"; cont_tail_recursive original_k k' flist
    | MonOpAppCPS (k', mono_op, o1, exk) ->print_string "\nMonOpAppCPS\n";  cont_tail_recursive original_k k' flist
    | BinOpAppCPS (k', bin_op, o1, o2, exk) -> print_string "BinOp\n"; cont_tail_recursive original_k k' flist
| IfCPS (b, e1, e2) -> (check_cps_tail_rec_f flist original_k x k e1) && (check_cps_tail_rec_f flist original_k x k e2)
    | AppCPS (k', e1, e2, exk) -> 
    (
     (*print_string ("AppCPS:\n"^e1^", "^e2^", f: "^f^"\n");*)
     if ( List.exists (fun x -> x = e1) flist)
     then 
     (
      if (k'=original_k) 
      then (print_string "true\n"; true)
      else (print_string "false\n";false)
     )
     else
     cont_tail_recursive original_k k' flist
    )
    | FunCPS (kappa, x, k, ek, e) -> true
    | FixCPS (kappa, f, x, k, ek, e) -> true

    and cont_tail_recursive original_k k flist = 
    match k
    with ContVarCPS i -> true
    | External -> true
    | FnContCPS (x, e) -> 
    (*
     if (x=f)
     then true
     else

     *)
(check_cps_tail_rec_f flist original_k x k e) (*TODO x ?*)
    | ExnMatch ek -> true ;;

    let check_tail_recursion dec =
    match dec
    with Anon e -> true
    | Let (x,e) -> true
    | LetRec (f,x,e) ->
    let (i,j) = (next_index(),next_index()) in
    let ecps2 = cps_exp e (ContVarCPS i) (ExnContVarCPS j) in
    print_string ((string_of_exp_cps ecps2)^"\n");
(*let (f, expcps) = convert_f ecps2*)
    let flist = convert_f_exp ecps2 f
    in
(*check_cps_tail_rec_f f (ContVarCPS i) x (ContVarCPS i) expcps*)
    check_cps_tail_rec_f flist (ContVarCPS i) x (ContVarCPS i) ecps2 ;;

(*
    let rec eval_exp_cps env ecps =
    match one_step_exp_cps_eval env ecps
    with Failed -> (print_string ("This shouldn't happen; it should mean there is a free variable\n of some type without a corresponding value, or a type checking error."); raise (Failure "compiler error"))
    | UncaughtException n ->
    (print_string ("Exception no. "^string_of_int n^" raised.\n"); None)
    | Final v -> Some v
    | Intermediate(env', ecps') -> eval_exp_cps env' ecps';;

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
    Some(Some f, (CPSRecClosureVal(f, x, i, j, ecps2, env))) ;;

*)


    let _ =
    (
     if is_interactive
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
     in (loop [] []) ;;
