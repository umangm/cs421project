(*
 tailRecPicoMLInt.ml
 *)

open Definitions
open TailRecPicoMLparse
open TailRecPicoMLlex
open CheckTailRec
open CheckTailRecCPS

let is_interactive = true;;

(* default values *)
let is_cps_arg = ref false
let is_direct_arg = ref false

 
let usage = "usage: " ^ Sys.argv.(0) ^ " [-c] [-d]"
 
let speclist = [
    ("-c", Arg.Set is_cps_arg, ": CPS Only");
    ("-d", Arg.Set is_direct_arg, ": Direct style Only");
  ]

let _ =
    Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
    Printf.printf " %b %b\n" !is_cps_arg !is_direct_arg;

    print_endline "\nWelcome to the PicoML Tail-resursion Checker \n";
    let rec loop gamma mem = 
    try
        let lexbuf = Lexing.from_channel stdin
        in 
        (print_string "> "; flush stdout);
        (
        try
            let dec = main 
                (fun lb -> match token lb 
                    with 
                    | EOF -> raise EndInput
                    | r -> r
                ) lexbuf 
            in 
            match infer_dec gather_dec_ty_substitution gamma dec 
            with 
            | None -> 
                (
                print_string "\ndoes not type check\n";
                loop gamma mem
                )
            | Some (Proof(hyps,judgement)) ->
                (
                
                let is_direct_true = (check_tail_recursion_direct dec ) in 
                let is_cps_true = (check_tail_recursion_cps dec ) in 

                let is_direct_only = ( (not !is_cps_arg) &&  !is_direct_arg ) in 
                let is_cps_only = (!is_cps_arg && (not !is_direct_arg) ) in 
                

                if is_direct_only
                    then (print_direct_result is_direct_true)
                else if is_cps_only
                    then (print_cps_result is_cps_true)
                else (* do both *)
                    ((print_direct_result is_direct_true);(print_cps_result is_cps_true);(print_match is_direct_true is_cps_true))
                ; 
                loop gamma mem)
        with Failure s -> 
            (
            print_newline();
            print_endline s;
            print_newline();
            loop gamma mem
            )
        | Parsing.Parse_error ->
            (
            print_string "\ndoes not parse\n";
            loop gamma mem
            )
        )
    with EndInput -> 
        exit 0

in (loop [] []) ;;
