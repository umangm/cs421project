open Mp6common;;

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

let eval_dec (dec, m) =
    match dec
    with (Anon e) -> 
        let v = eval_exp (e, m)
        in
        ((None, v), m)
    | Let (s, e) ->  
        let v = eval_exp (e, m)
        in
        (
        match s
        with "_" -> ((None, v), [])
        | _ ->
            (
            match v
            with Exn n -> ((None, v), [])
            | _ ->((Some s,v), sum_env (make_env s v) m)
            )
        )
    | LetRec (f, x, e) ->
            (
                (Some f, RecVarVal (f, x, e, m)),
                sum_env (make_env f (RecVarVal (f, x, e, m))) m
            );;

(*    
 *  | _ -> raise (Failure "Not implemented yet.")
 *)

