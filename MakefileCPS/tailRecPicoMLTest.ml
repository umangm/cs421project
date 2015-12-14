(*
 picomlIntPar.ml - DO NOT EDIT
 *)

open Definitions
open TailRecPicoMLparse
open TailRecPicoMLlex
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
                (
                match check_tail_recursion dec 
                with true -> print_string "Tail Recursive!\n"
                | false -> print_string "Not Tail Recursive!\n"
                )  
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

