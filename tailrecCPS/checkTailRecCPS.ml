(*
checkTailRecCPS.ml - DO NOT EDIT
 *)

open Definitions

let rec convert_f_exp e name_of_f = 
    (* Return all variable numbers that correspond to f. *)
    (
    match e
    with  ConstCPS (k, c) -> 
        convert_f_cont k name_of_f
    | VarCPS (k, g) -> 
        (*
        print_string "\nVarCPS \n";
        *)
            (
            match k 
            with FnContCPS (number_of_f, e') -> 
                let rec_list = convert_f_exp e' name_of_f 
                in 
                if (g = name_of_f) 
                    (* Found a matched f. Add the number to retured list. *)
                    then (number_of_f :: rec_list) 
                    else rec_list
            | _ -> []
            )
    | MonOpAppCPS (k, _, _, _) -> 
        (*
        print_string "\nMonOpAppCPS \n";
        *)
        convert_f_cont k name_of_f
    | BinOpAppCPS (k , _, _, _, _) -> 
        (*
        print_string "\nBinOpAppCPS \n";
        *)
        convert_f_cont k name_of_f
    | IfCPS (b, e1, e2) ->
        (*
        print_string "\nIfCPS \n";
        *)
        (convert_f_exp e1 name_of_f)@(convert_f_exp e2 name_of_f)
    | AppCPS (k, _, _, _) ->
        (*
        print_string "\nAppCPS \n";
        *)
        convert_f_cont k name_of_f
    | FunCPS (k, _, _, _, _) ->
        (*
        print_string "\nFunCPS \n";
        *)
        convert_f_cont k name_of_f
    | FixCPS (k, _, _, _, _, _) ->
        (*
        print_string "\nFixCPS \n";
        *)
        convert_f_cont k name_of_f
    )

and 

convert_f_cont k name_of_f =
    (
    match k
    with FnContCPS (_, e') -> 
        convert_f_exp e' name_of_f
    | _ -> []
    ) ;;


let rec check_cps_tail_rec_f flist original_k x k e = 
    match e
    with ConstCPS (k', c) -> 
        (* It's impossilbe to call f here, so go deeper to find if there are any f's. *)
        cont_tail_recursive original_k k' flist
    | VarCPS (k', v) -> 
        (* Similarly, go deeper to find f. *)
        cont_tail_recursive original_k k' flist
    | MonOpAppCPS (k', mono_op, o1, exk) ->
        (* Similarly, go deeper to find f. *)
        cont_tail_recursive original_k k' flist
    | BinOpAppCPS (k', bin_op, o1, o2, exk) -> 
        (* Similarly, go deeper to find f. *)
        cont_tail_recursive original_k k' flist
    | IfCPS (b, e1, e2) -> 
        (* Similarly, go deeper to find f. *)
        (check_cps_tail_rec_f flist original_k x k e1) && (check_cps_tail_rec_f flist original_k x k e2)
    | AppCPS (k', e1, e2, exk) -> 
        (
        (* if e1 matches one of variable numbers corresponding to f ... *)
        if ( List.exists (fun x -> x = e1) flist)
            then 
                (
                (* And if the returned k is the original ...*)
                if (k'=original_k) 
                    then true (* then this is a tail call. *)
                    else false
                )
            else
                (* Else just go deeper to find if there are any f's. *)
                cont_tail_recursive original_k k' flist
        )
    | FunCPS (kappa, x, k, ek, e) -> true
    | FixCPS (kappa, f, x, k, ek, e) -> true

and cont_tail_recursive original_k k flist = 
    match k
    with ContVarCPS i -> true
    | External -> true
    | FnContCPS (x, e) -> 
        check_cps_tail_rec_f flist original_k x k e
    | ExnMatch ek -> true ;;


let check_tail_recursion dec =
    match dec
    with Anon e -> true
    | Let (x,e) -> true
    | LetRec (f,x,e) ->
        let (i,j) = (next_index(),next_index()) 
        in
            let ecps2 = cps_exp e (ContVarCPS i) (ExnContVarCPS j) 
            in

            let flist = convert_f_exp ecps2 f
                in
                check_cps_tail_rec_f flist (ContVarCPS i) x (ContVarCPS i) ecps2 ;;
