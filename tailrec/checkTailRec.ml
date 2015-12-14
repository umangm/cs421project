open Definitions;;

let rec check_let_in_meaningful x e =
    match e
    with ConstExp c -> false
    | VarExp v -> if (v = x) then true else false
    | MonOpAppExp (mon_op, e1) -> check_let_in_meaningful x e1
    | BinOpAppExp (bin_op, e1, e2) -> (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2) 
    | IfExp (e1, e2, e3) ->
        (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2)  || (check_let_in_meaningful x e3) 
    | LetInExp (s, e1, e2) ->
        if (check_let_in_meaningful x e1)
            then (check_let_in_meaningful s e2)
            else (
                if (x=s) 
                    then false
                    else check_let_in_meaningful x e2
                )
    | FunExp (s, e1) -> if (s=x) then false else (check_let_in_meaningful x e1)
    | AppExp (e1, e2) -> 
        (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2)
    | LetRecInExp (g, y, e1, e2) ->
        if ((g=x) || (y=x)) 
            then false 
            else if (check_let_in_meaningful x e1) 
                then (check_rec_f g e2)
                else (check_let_in_meaningful x e2)
    | RaiseExp e1 -> (check_let_in_meaningful x e1)
    | TryWithExp (e0, n1opt, e1, nopt_e_lst) ->
        (check_let_in_meaningful x e0) || (check_let_in_meaningful_lst x ((n1opt,e1)::nopt_e_lst) )
and check_let_in_meaningful_lst x nopt_e_lst = 
    match nopt_e_lst 
    with [] -> false
    | (nopt, en)::rest -> (check_let_in_meaningful x en) || (check_let_in_meaningful_lst x rest)
and check_rec_f f e =
    match e 
    with ConstExp c -> false
    | VarExp v -> false
    | MonOpAppExp (mon_op, e1) -> check_rec_f f e1
    | BinOpAppExp (bin_op, e1, e2) -> (check_rec_f f e1) || (check_rec_f f e2)
    | IfExp (e1, e2, e3) ->
        (check_rec_f f e1) || (check_rec_f f e2) || (check_rec_f f e3)
    | LetInExp (s, e1, e2) ->
        if (check_rec_f f e1) 
            then (check_let_in_meaningful s e2)
            else
                (
                if (s=f) then false else ( (check_rec_f f e1) || (check_rec_f f e2) )
                )
    | FunExp (s, e1) -> if (s=f) then false else (check_rec_f f e1)
    | AppExp (e1, e2) -> 
        (
        match e1
        with VarExp v -> if (v=f) then true else false
        | _ -> (check_rec_f f e1) || (check_rec_f f e2)
        )
    | LetRecInExp (g, x, e1, e2) -> 
        if ( (g=f) || (x=f)) 
            then false 
            else if (check_rec_f f e1) 
                then (check_rec_f g e2)
                else (check_rec_f f e2)
    | RaiseExp e1 -> (check_rec_f f e1)
    | TryWithExp (e0, n1opt, e1, nopt_e_lst) ->
        (check_rec_f f e0) || (check_rec_f_lst f ((n1opt, e1) :: nopt_e_lst) )

and check_rec_f_lst f nopt_e_lst = 
    match nopt_e_lst 
    with [] -> true
    | ((nnopt, en)::rest) -> ((check_rec_f f en) || (check_rec_f_lst f rest));;

let rec check_tail_rec_f f e =
    match e
    with ConstExp c -> true
    | VarExp v -> true
    | MonOpAppExp (mon_op, e1) -> not (check_rec_f f e1)
    | BinOpAppExp (bin_op, e1, e2) -> (not (check_rec_f f e1)) && (not (check_rec_f f e2))
    | AppExp(e1, e2) -> if (check_rec_f f e2)
        then false
        else check_tail_rec_f f e1
    | IfExp (e1, e2, e3) -> 
            (not (check_rec_f f e1)) &&
            (check_tail_rec_f f e2) && 
            (check_tail_rec_f f e3)
    | FunExp (x, e1) -> true
    | LetInExp (x, e1, e2) ->
        if (x = f) 
            then (not (check_rec_f f e1))
            else ( (not (check_rec_f f e1)) && (check_tail_rec_f f e2))
    | LetRecInExp (g, x, e1, e2) ->
        if (g = f) then true
        else if (not (g=f) && (x=f)) then (check_tail_rec_f f e2)
        else ( (check_tail_rec_f f e2))
    (* Before fix:
        if (g = f) then true
        else if (not (g=f) && (x=f)) then (check_tail_rec_f f e2)
        else ( (not (check_rec_f f e1)) && (check_tail_rec_f f e2))
    *)
    | TryWithExp (e', n1opt, e1, nopt_e_lst) ->
        (
        if (check_rec_f f e')
            then false
            else let lst = ((n1opt, e1)::nopt_e_lst)
                in
                (List.fold_right (fun (intop, h) -> fun t -> (check_tail_rec_f f h) && t) lst true) 
        )
    | _ -> false ;;


let check_tail_recursion dec =
    match dec
    with (Anon e) -> true
    | Let (s, e) -> true
    | LetRec (f, x, e) ->
        check_tail_rec_f f e ;;
