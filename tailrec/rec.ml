open Mp6common;;

(*
let const_to_val c = 
    match c
    with UnitConst -> UnitVal
    | TrueConst -> TrueVal
    | FalseConst -> FalseVal
    | IntConst n -> IntVal n
    | FloatConst f -> FloatVal f
    | StringConst s -> StringVal s
    | NilConst -> ListVal [];;

let monOpApply op v = 
    match op 
    with HdOp  ->
        (
        match v
        with ListVal vlst ->
            (
                match vlst
                with [] -> Exn 0
                | (vl::vs) -> vl
            )
        | _ -> raise (Failure "hd takes value of type ListVal as argument")
        )
    | TlOp ->
        (
        match v
        with ListVal vlst ->
            (
                match vlst
                with [] -> Exn 0
                | (vl::vs) -> ListVal vs
            )
        | _ -> raise (Failure "tl takes value of type ListVal as argument")
        )
    | PrintOp ->
        (
        match v
        with StringVal s ->
            print_string s;
            UnitVal
        | _ -> raise (Failure "print_string takes value of type string as argument")
        )
    | IntNegOp ->
        (
        match v
        with IntVal n -> IntVal (-n)
        | _ -> raise (Failure "~ takes value of type int as argument")
        )
    | FstOp ->
        (
        match v
        with PairVal (v1, v2) -> v1
        | _ -> raise (Failure "FstOp takes value of type pair as argument")
        )

    | SndOp ->
        (
        match v
        with PairVal (v1, v2) -> v2
        | _ -> raise (Failure "SndOp takes value of type pair as argument")
        );;

let binOpApply binop (v1,v2) =
    match binop
    with IntPlusOp ->
        (
        match (v1, v2) 
        with (IntVal vl1, IntVal vl2) -> IntVal (vl1 + vl2)
        | _ -> raise (Failure "Both arguments to IntPlusOp must be of type int")
        )
    | IntMinusOp ->
        (
        match (v1, v2) 
        with (IntVal vl1, IntVal vl2) -> IntVal (vl1 - vl2)
        | _ -> raise (Failure "Both arguments to IntPMinusOp must be of type int")
        )
    | IntTimesOp ->
        (
        match (v1, v2) 
        with (IntVal vl1, IntVal vl2) -> IntVal (vl1 * vl2)
        | _ -> raise (Failure "Both arguments to IntTimesOp must be of type int")
        )
    | IntDivOp ->
        (
        match (v1, v2) 
        with (IntVal vl1, IntVal vl2) -> if vl2 = 0 then Exn 0 else IntVal (vl1 / vl2)
        | _ -> raise (Failure "Both arguments to IntDivOp must be of type int")
        )
    | ModOp ->
        (
        match (v1, v2) 
        with (IntVal vl1, IntVal vl2) -> IntVal (vl1 mod vl2)
        | _ -> raise (Failure "Both arguments to ModOp must be of type int")
        )
    | FloatPlusOp ->
        (
        match (v1, v2) 
        with (FloatVal vl1, FloatVal vl2) -> FloatVal (vl1 +. vl2)
        | _ -> raise (Failure "Both arguments to FloatPlusOp must be of type float")
        )
    | FloatMinusOp ->
        (
        match (v1, v2) 
        with (FloatVal vl1, FloatVal vl2) -> FloatVal (vl1 -. vl2)
        | _ -> raise (Failure "Both arguments to FloatMinusOp must be of type float")
        )
    | FloatTimesOp ->
        (
        match (v1, v2) 
        with (FloatVal vl1, FloatVal vl2) -> FloatVal (vl1 *. vl2)
        | _ -> raise (Failure "Both arguments to FloatTimesOp must be of type float")
        )
    | FloatDivOp ->
        (
        match (v1, v2) 
        with (FloatVal vl1, FloatVal vl2) -> if vl2 = 0. then Exn 0 else FloatVal (vl1 /. vl2)
        | _ -> raise (Failure "Both arguments to FloatDivOp must be of type float")
        )
    | ExpoOp ->
        (
        match (v1, v2) 
        with (FloatVal vl1, FloatVal vl2) -> FloatVal (vl1 ** vl2)
        | _ -> raise (Failure "Both arguments to ExpoOp must be of type float")
        )
    | ConcatOp ->
        (
        match (v1, v2) 
        with (StringVal vl1, StringVal vl2) -> StringVal (vl1 ^ vl2)
        | _ -> raise (Failure "Both arguments to ConcatOp must be of type string")
        )
    | ConsOp ->
        (
        match v2
        with ListVal vl2 -> ListVal (v1::vl2)
        | _ -> raise (Failure "The second argument to ConsOp must be of type ListVal")
        )
    | CommaOp -> PairVal (v1, v2)
    | EqOp -> if (v1 = v2) then TrueVal else FalseVal
    | GreaterOp -> if (v1 > v2) then TrueVal else FalseVal;;



let rec eval_exp (exp, m) = 
    match exp
    with ConstExp c -> const_to_val c
    | VarExp v -> 
        (
        match (lookup_env m v)
        with None -> raise (Failure "Not found")
        | Some vl -> 
            (
            match vl
            with RecVarVal (g, y, e, m')-> Closure (y, e, sum_env (make_env g (RecVarVal (g, y, e, m'))) m')
            | _ -> vl
            )
        )
    | MonOpAppExp (mon_op, e) ->
        let v = eval_exp (e, m)
        in
        (
        match v
        with Exn n -> Exn n
        | _ -> monOpApply mon_op v
        )
    | BinOpAppExp (bin_op, e1, e2) ->
        let (v1, v2) = (eval_exp (e1, m), eval_exp (e2, m))
        in
        (
        match (v1, v2)
        with (Exn n, _) -> Exn n
        | (_, Exn n) -> Exn n
        | _ -> binOpApply bin_op (v1, v2)
        )
    | IfExp (e1, e2, e3) ->
        (
        match eval_exp (e1, m)
        with TrueVal -> eval_exp (e2, m)
        | FalseVal -> eval_exp (e3, m)
        | Exn n -> Exn n
        | _ -> raise (Failure "Boolean guard in IfExp must evaluate to one of true, false or exception.")
        )
    | LetInExp (s, e1, e2) ->
        let v1 = eval_exp (e1, m)
        in
        (
        match v1
        with Exn n -> Exn n
        | _ -> eval_exp (e2, sum_env (make_env s v1) m)
        )
    | FunExp (s, e) -> Closure (s, e, m) 
    | AppExp (e1, e2) -> 
        (
        match (eval_exp (e1, m), eval_exp (e2, m))
        with (Exn n, _) -> Exn n
        | (_, Exn n) -> Exn n
        | (Closure (s, e', m'), v') -> eval_exp (e', sum_env (make_env s v') m') 
        | _ -> raise (Failure "The first expression in an app expression must evaluate to a closure or an exception")
        )
    | LetRecInExp (f, x, e1, e2) -> eval_exp (e2, sum_env (make_env f (RecVarVal (f, x, e1, m))) m)
    | RaiseExp e1 -> 
        let v1 = eval_exp (e1, m)
        in
        (
        match v1
        with Exn i -> Exn i
        | IntVal n -> Exn n
        | _ -> raise (Failure "Argument of RaiseExp should evaluate to int")
        )
    | TryWithExp (e, n1opt, e1, nopt_e_lst) ->
        let v = eval_exp (e, m)
        in
        (
        match v
        with Exn j -> getFirstMatch ((n1opt, e1)::nopt_e_lst) m j
        | _ -> v
        )

and getFirstMatch lst m j =
    match lst
    with [] -> Exn j
    | ((None, e)::tl) -> eval_exp (e, m)
    | ( ((Some n_i), e_i)::tl ) -> if (n_i = j) then eval_exp (e_i, m) else getFirstMatch tl m j;;

*)

let rec check_let_in_meaningful x e =
    match e2 
    with ConstExp c -> false
    | VarExp v -> if (v = x) then true else false
    | MonOpAppExp (mon_op, e1) -> check_let_in_meaningful x e1
    | BinOpAppExp (bin_op, e1, e2) -> (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2) 
    | IfExp (e1, e2, e3) ->
        (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2)  || (check_let_in_meaningful x e3) 
    | LetInExp (s, e1, e2) -> (*TODO*)
        if (check_let_in_meaningful x e1)
            then (check_let_in_meaningful s e2)
        else 
            (check_let_in_meaningful x e2)
    | FunExp (s, e1) -> if (s=x) then false else (check_let_in_meaningful x e1)
    | AppExp (e1, e2) -> 
        (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2)
    | LetRecInExp (g, y, e1, e2) ->
        if (g=x)||(y=x) then false else (check_let_in_meaningful x e1) || (check_let_in_meaningful x e2)
    | RaiseExp e1 -> (check_let_in_meaningful x e1)
    | TryWithExp (e0, n1opt, e1, nopt_e_lst) -> (*TODO*)
        (check_let_in_meaningful x e0)||(check_let_in_meaningful x e1)||(check_let_in_meaningful_help x nopt_e_lst)
and check_let_in_meaningful_help x nopt_e_lst = 
    match nopt_e_lst 
    with [] -> false
    | (nnopt, en)::rest ->
        (check_let_in_meaningful x en)||(check_let_in_meaningful_help x rest)

let rec check_rec_f f e =
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
    | FunExp (s, e1) -> if (s=f) then false else (check_rec_f f e1)
    | AppExp (e1, e2) -> 
        (
        match e1
        with VarExp v -> if (e1=f) then true else false
        | _ -> (check_rec_f f e1) || (check_rec_f f e2)
        )
    | LetRecInExp (g, x, e1, e2) -> 
        if (g=f)||(x=f) then true else (check_rec_f f e1)||(check_rec_f f e2)
    | RaiseExp e1 -> (check_rec_f f e1)
    | TryWithExp (e0, n1opt, e1, nopt_e_lst) ->
        (check_rec_f f e0)||(check_rec_f f e1)||(check_rec_f_help f nopt_e_lst)
and check_rec_f_help f nopt_e_lst = 
    match nopt_e_lst 
    with (nnopt, en)::rest ->
        (check_rec_f f en)||(check_rec_f_help f rest)


(*    
 *  | _ -> raise (Failure "Not implemented yet.")
 *)

