open Mp6common;;

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
        with VarExp v -> if (e1=f) then true else false
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
    with AppExp(e1, e2) ->
        (
        match (check_rec_f f e2)
        with true -> false
        | false ->
            (
            match e1
            with VarExp g -> if (g = f) then true
            | _ -> (check_tail_rec_f f e1)
            )
        )
    | IfExp (e1, e2, e3) ->
        (
        match (check_rec_f f e1)
        with true -> false
        | false ->
            (
            let (e2_tail, e3_tail) = (check_tail_rec_f f e2, check_tail_rec_f f e3)
            in
            match e2_tail
            with true -> if e3_tail then true else (check_rec_f f e3)
            | false -> if (not e3_tail) then false else (check_rec_f f e2)
            )
        )
    | FunExp (x, e1) -> 
        if (x = f) then false else (check_tail_rec_f f e1)
    | LetInExp (x, e1, e2) ->
        if (x = f) 
            then false 
            else ( (not (check_rec_f f e1)) && (check_tail_rec_f f e2))
    | LetRecIn (g, x, e1, e2) ->
        if ((x = f) || (g = f))
            then false
            else ( (not (check_rec_f f e1)) && (check_tail_rec_f f e2))
    | TryWithExp (e', n1opt, e1, nopt_e_lst) ->
        (
        if (check_rec_f f e')
            then false
            else let lst = ((n1opt, e1)::nopt_e_lst)
                in
                (
                (check_list_for_either lst f)
                and
                (check_list_for_atleast lst f)
                )
        )
    | _ -> false
and check_list_for_either lst f = 
    match lst
    with [] -> true
    | ((nopt, e)::nopt_e_lst) -> ((check_tail_rec_f f e) || (not (check_rec_f f e)) ) && (check_list_for_either nopt_e_lst f)

and check_list_for_atleast lst f = 
    match lst
    with [] -> false
    | ((nopt, e)::nopt_e_lst) -> if (check_tail_rec_f f e) then true else (check_list_for_atleast nopt_e_lst f)


let check_tail_recursion (dec, m) =
    match dec
    with (Anon e) -> false
    | Let (s, e) -> false
    | LetRec (f, x, e) ->
        check_tail_rec_f f e ;;

