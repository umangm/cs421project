(* File: mp7-sol.ml *)

open Mp7common;;

let rec get_first_match_updateexn lst i =
    match lst
    with [] -> None
    | ((io, e)::rem_lst) ->
        (
        match io
        with None -> Some e
        | Some i' -> if i = i' then Some e else get_first_match_updateexn rem_lst i
        );;

let rec app_cont_to_value env k v =
    match k
    with ContVarCPS ki ->
        (
        match lookup_cont env ki
        with None -> raise (Failure "Failed to lookup ContVarCPS in memory")
        | Some (k', env') -> app_cont_to_value env' k' v
        )
    | External -> Final v
    | FnContCPS (y, e) -> Intermediate ((ValueBinding (y, v))::env, e)
    | ExnMatch ek ->
        (
        match v
        with IntVal i ->  
            (
            match ek
            with ExnContVarCPS eki ->
                (
                match lookup_exn_cont env eki
                with None -> raise (Failure "Failed to lookup ExnContVarCPS")
                | Some (ek', env') -> app_cont_to_value env' (ExnMatch ek') v
                )
            | EmptyExnContCPS -> UncaughtException i
            | UpdateExnContCPS (io_e_lst, ek') -> 
                (
                match get_first_match_updateexn io_e_lst i
                with None -> app_cont_to_value env (ExnMatch ek') v
                | Some e -> one_step_exp_cps_eval env e
                )
            )
        | _ -> raise (Failure "v must be an IntVal")
        )
and
one_step_exp_cps_eval env exp = 
    match exp
    with ConstCPS (k, c) -> Final (const_to_val c)
    | VarCPS (k, x) -> 
        (
        match lookup_value env x
        with None -> raise (Failure "variable x not found in env")
        | Some v -> Final v
        )
    | MonOpAppCPS (k, mono_op, x, e) -> 
        (
        match lookup_value env x
        with None -> raise (Failure "variable x not found in env: monop")
        | Some v ->
            (
            match monOpApply mono_op v
            with Value v' -> Final v'
            | Exn i -> UncaughtException i
            )
        )
    | BinOpAppCPS (k, bin_op, x, y, e) ->
        (
        match lookup_value env x
        with None -> raise (Failure "variable x not found in env: monop")
        | Some v1 ->
            (
            match lookup_value env y
            with None -> raise (Failure "variable y not found in env: monop")
            | Some v2 ->
                (
                match binOpApply bin_op v1 v2
                with Value v' -> Final v'
                | Exn i -> UncaughtException i
                )
            )
        )
    | IfCPS (b, exp1, exp2) ->
        (
        match lookup_value env b 
        with None -> raise (Failure "Boolean not ond in memory")
        | Some v ->
            (
            match v
            with BoolVal true -> Intermediate (env, exp1)
            | BoolVal false -> Intermediate (env, exp2)
            | _ -> raise (Failure "if condision must evaluate to boolean val")
            )
        )
    | FunCPS (kappa, x, k, ek, e) -> Final (CPSClosureVal (x, k, ek, e, env))
    | FixCPS (kappa, f, x, k, ek, e) -> Final (CPSRecClosureVal (f, x, k, ek, e, env))
    | _ -> raise (Failure "Not implemented yet")
