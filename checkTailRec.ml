open Definitions;;

let rec check_rec_f f e =
    (* check if there is AppExp(f, ...) in e *)
    match e 
    with ConstExp c -> false
    | VarExp v -> false
    | MonOpAppExp (mon_op, e1) -> check_rec_f f e1
    | BinOpAppExp (bin_op, e1, e2) -> (check_rec_f f e1) || (check_rec_f f e2)
    | IfExp (e1, e2, e3) ->
        (check_rec_f f e1) || (check_rec_f f e2) || (check_rec_f f e3)
    | LetInExp (s, e1, e2) ->
        if (check_rec_f f e1) 
            then true
            else
                (
                if (s=f) then false else ( (check_rec_f f e1) || (check_rec_f f e2) )
                )
    | FunExp (s, e1) -> false
    | AppExp (e1, e2) -> 
        (
        match e1
        with VarExp v -> if (v=f) then true else false
        | _ -> (check_rec_f f e1) || (check_rec_f f e2)
        )
    | LetRecInExp (g, x, e1, e2) -> 
        if (g=f) then false else (check_rec_f f e2)
    | RaiseExp e1 -> (check_rec_f f e1)
    | TryWithExp (e0, n1opt, e1, nopt_e_lst) ->
        (check_rec_f f e0) || (check_rec_f_lst f ((n1opt, e1) :: nopt_e_lst) )

and check_rec_f_lst f nopt_e_lst = 
    match nopt_e_lst  
    with [] -> true
    | ((nnopt, en)::rest) -> ((check_rec_f f en) || (check_rec_f_lst f rest));;

let rec check_tail_rec_f f e =
    match e
    with ConstExp c -> true (* it is not recursive, so it is tail-recrusive  *)
    | VarExp v -> true (* it is not recursive, so it is tail-recrusive  *)
    | MonOpAppExp (mon_op, e1) -> 
        (* if f is not called in e1, then it is not recursive, and therefore tail-recrusive  *)
        not (check_rec_f f e1) 
    | BinOpAppExp (bin_op, e1, e2) -> 
        (* if f is not called in e1 or e2, then it is not recursive, and therefore tail-recrusive  *)
        (not (check_rec_f f e1)) && (not (check_rec_f f e2))
    | AppExp(e1, e2) -> if (check_rec_f f e2)
        then (* if f is called in e2, that call is not a tail call. So it is not tail-recursive.  *)
            false 
        else 
            check_tail_rec_f f e1
    | IfExp (e1, e2, e3) -> 
            (* if f is called in e1, that call is not a tail call. So it is not tail-recursive. *)
            (not (check_rec_f f e1)) &&
            (check_tail_rec_f f e2) && 
            (check_tail_rec_f f e3)

    | FunExp (x, e1) -> 
        (* Even if there is a cell to f in e1, 
        it is still tail-recursive because that call is not available in the body of f. *)
        true 

    | LetInExp (x, e1, e2) ->
        if (x = f) 
            then 
                (* if x is f, then we don't have to worry about whether there is f in e2. 
                Because even there is, that f is not the original f itself. 
                In this case, we only need to make sure f is not called in e1
                since f in e1 is not a tail call. *)
                (not (check_rec_f f e1))
            else 
                (* if f is not called in e', then whethere it is tail-recursive is determined by e2. *)
                ( (not (check_rec_f f e1)) && (check_tail_rec_f f e2))
    | LetRecInExp (g, x, e1, e2) ->
        if (g = f) 
            then 
                (* if g is f, then we don't have to worry about whether there is f in e2. 
                Because even there is, that f is not the original f itself.  *)
                true 
        else if (not (g=f) && (x=f)) 
                then 
                    (* if x is f, then all f's in e1 is just x, not the original f. *)
                    (check_tail_rec_f f e2)
        else 
            (* g is just like another function, like the FunExp case. *)
            ( (check_tail_rec_f f e2))
    
    | TryWithExp (e', n1opt, e1, nopt_e_lst) ->
        (
        if (check_rec_f f e')
            then (* if f is called in e', that call is not a tail call. So it is not tail-recursive. *)
                false
            else let lst = ((n1opt, e1)::nopt_e_lst)
                in
                (* All e's in the list has to be tail-recursive in order to make TryWith to be tail-recursive. *)
                (List.fold_right (fun (intop, h) -> fun t -> (check_tail_rec_f f h) && t) lst true) 
        )
    | _ -> false ;;


let check_tail_recursion_direct dec =
    match dec
    with (Anon e) -> true
    | Let (s, e) -> true
    | LetRec (f, x, e) ->
        check_tail_rec_f f e ;;
