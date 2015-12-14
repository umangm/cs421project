(*
tailRecPicoMLTest.ml
*)

open Definitions
open TailRecPicoMLparse
open TailRecPicoMLlex
open CheckTailRec

(*
(* Try to detect if something is getting piped in *)
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

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
          let dec = main (fun lb -> match token lb with 
                                    | EOF -> raise EndInput
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
  with EndInput -> exit 0
 in (loop [] [])
 *)

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

