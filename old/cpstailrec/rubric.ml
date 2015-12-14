(*
 * This file will be preprocessed to generate the actual OCaml file.
 *)
open Grader
open Test

(*
 * use a timeout of 4 seconds
 *)

let mptest weight pair = compare (=) 4 weight pair

open Mp7common
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2015 MP7"

(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)

let parse s = Picomlparse.main Picomllex.token (Lexing.from_string s)

let rubric = [
"app_cont_to_value"^" "^"[]"^" "^"External"^" "^"(IntVal 6)", mptest 1 (ss_pair3 Solution.app_cont_to_value Student.app_cont_to_value [] External (IntVal 6));

"one_step_exp_cps_eval"^" "^"[]"^" "^"(ConstCPS(External, IntConst 2))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [] (ConstCPS(External, IntConst 2)));

"one_step_exp_cps_eval"^" "^"[ValueBinding(\"x\", IntVal 2)]"^" "^"(VarCPS(External, \"x\"))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [ValueBinding("x", IntVal 2)] (VarCPS(External, "x")))
                         ;

"one_step_exp_cps_eval"^" "^"[ValueBinding(\"x\", IntVal 2)]"^" "^"(MonOpAppCPS(External, IntNegOp, \"x\", EmptyExnContCPS))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [ValueBinding("x", IntVal 2)] (MonOpAppCPS(External, IntNegOp, "x", EmptyExnContCPS)))
                                                         ;

"one_step_exp_cps_eval"^" "^"[ValueBinding(\"b\", IntVal 3);ValueBinding(\"a\", IntVal 2)]"^" "^"(BinOpAppCPS(External, IntPlusOp, \"a\", \"b\", EmptyExnContCPS))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [ValueBinding("b", IntVal 3);ValueBinding("a", IntVal 2)] (BinOpAppCPS(External, IntPlusOp, "a", "b", EmptyExnContCPS)))

                                                               ;

"one_step_exp_cps_eval"^" "^"[ValueBinding(\"a\", BoolVal true)]"^" "^"(IfCPS(\"a\", ConstCPS(External,IntConst 1), ConstCPS(External,IntConst 0)))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [ValueBinding("a", BoolVal true)] (IfCPS("a", ConstCPS(External,IntConst 1), ConstCPS(External,IntConst 0))))
                                                                            ;

"one_step_exp_cps_eval"^" "^"[]"^" "^"(FunCPS (External, \"x\", 1, 0, VarCPS (FnContCPS (\"a\", ConstCPS (FnContCPS (\"b\", BinOpAppCPS (ContVarCPS 1, IntPlusOp, \"a\", \"b\", ExnContVarCPS 0)), IntConst 5)), \"x\")))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [] (FunCPS (External, "x", 1, 0, VarCPS (FnContCPS ("a", ConstCPS (FnContCPS ("b", BinOpAppCPS (ContVarCPS 1, IntPlusOp, "a", "b", ExnContVarCPS 0)), IntConst 5)), "x"))))






           ;


"one_step_exp_cps_eval"^" "^"[]"^" "^"(let e1 = IfExp (BinOpAppExp (EqOp, VarExp \"n\", ConstExp (IntConst 0)), ConstExp (IntConst 1), AppExp (VarExp \"f\", BinOpAppExp (IntMinusOp, VarExp \"n\", ConstExp (IntConst 1)))) in let e1cps = cps_exp e1 (ContVarCPS 1) (ExnContVarCPS 0) in (FixCPS(FnContCPS(\"f\",VarCPS(External, \"f\")),\"f\",\"n\",1,0,e1cps)))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [] (let e1 = IfExp (BinOpAppExp (EqOp, VarExp "n", ConstExp (IntConst 0)), ConstExp (IntConst 1), AppExp (VarExp "f", BinOpAppExp (IntMinusOp, VarExp "n", ConstExp (IntConst 1)))) in let e1cps = cps_exp e1 (ContVarCPS 1) (ExnContVarCPS 0) in (FixCPS(FnContCPS("f",VarCPS(External, "f")),"f","n",1,0,e1cps))))





                                                                     ;

"one_step_exp_cps_eval"^" "^"[ValueBinding(\"plus5\", (CPSClosureVal (\"x\", 1, 0, VarCPS (FnContCPS (\"a\", ConstCPS (FnContCPS (\"b\", BinOpAppCPS (ContVarCPS 1, IntPlusOp, \"a\", \"b\", ExnContVarCPS 0)), IntConst 5)), \"x\"), []))); ValueBinding(\"c\",IntVal 2)]"^" "^"(AppCPS(External, \"plus5\", \"c\", EmptyExnContCPS))", mptest 1 (ss_pair2 Solution.one_step_exp_cps_eval Student.one_step_exp_cps_eval [ValueBinding("plus5", (CPSClosureVal ("x", 1, 0, VarCPS (FnContCPS ("a", ConstCPS (FnContCPS ("b", BinOpAppCPS (ContVarCPS 1, IntPlusOp, "a", "b", ExnContVarCPS 0)), IntConst 5)), "x"), []))); ValueBinding("c",IntVal 2)] (AppCPS(External, "plus5", "c", EmptyExnContCPS)))
]

(* This list is for extra credit problems *)

let extra_rubric = []

let _ = Main.main rubric extra_rubric rubric_title rubric_version
