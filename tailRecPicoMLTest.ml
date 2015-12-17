(*
tailRecPicoMLTest.ml
 *)

open Definitions
open TailRecPicoMLparse
open TailRecPicoMLlex
open CheckTailRec
open CheckTailRecCPS

let check str =
    try
        let lexbuf = Lexing.from_string str
        in 
        (
        try
            let dec = main 
                (fun lb -> match token lb 
                    with 
                    | EOF -> raise EndInput
                    | r -> r
                ) lexbuf 
            in 
            match infer_dec gather_dec_ty_substitution [] dec 
            with 
            | None -> 
                (
                print_string "\ndoes not type check\n"
                )
            | Some (Proof(hyps,judgement)) ->

                let is_direct_true = (check_tail_recursion_direct dec ) in 
                let is_cps_true = (check_tail_recursion_cps dec ) in 
                ((print_direct_result is_direct_true);(print_cps_result is_cps_true);(print_match is_direct_true is_cps_true))
                
        with Failure s -> 
            (
            print_newline();
            print_endline s;
            print_newline()
            )
        | Parsing.Parse_error ->
            (
            print_string "\ndoes not parse\n";
            )
        )
    with EndInput -> 
        exit 0
;;


let read_file filename = 
    let lines = ref [] in
        let chan = open_in filename in
            try
                while true; do
                    let line = input_line chan
                    in
                    print_string line;
                    print_newline (); 
                    check line ;
                    lines := line :: !lines;
                done; !lines
            with End_of_file ->
        close_in chan;
  List.rev !lines ;;

read_file "testing.txt";;

