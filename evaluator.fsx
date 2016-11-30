type term =
  | TmN of int
  | TmBool of bool
  | TmPlus
  | TmMinus
  | TmMult
  | TmDiv
  | TmLt
  | TmLtOrE
  | TmEqual
  | TmGt
  | TmGtOrE
  | TmDiff
  | TmOp of term * term * term
  | TmApp of term * term
  | TmNil
  | TmVar of string
  | TmIf of term * term * term
  | TmLet of term * term * term
  | TmLetRec of term * term * term
  | TmFun of term * term
  | TmList of term * term
  | TmIsEmpty of term
  | TmHd of term
  | TmTl of term
  | TmRaise
  | TmTry of term * term

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies
let rec replaceTerm expression ident newTerm =
  match expression with
  | TmN _ -> expression
  | TmBool _ -> expression
  | TmVar t1 when (t1 = ident)-> newTerm
  | TmVar _ -> expression
  | TmOp (t1, t2, t3) -> TmOp (t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmFun (TmVar t1, t2) when(t1 = ident) -> TmFun (TmVar t1, t2)
  | TmFun (TmVar t1, t2) when(t1 <> ident) -> TmFun (TmVar t1, replaceTerm t2 ident newTerm)
  | TmIf ( t1 , t2 , t3 ) -> TmIf (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmApp (t1, t2) -> TmApp (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm)
  | TmLet (TmVar t1, t2, t3) when(t1 = ident) -> TmLet (TmVar ident, replaceTerm t2 ident newTerm, t3)
  | TmLet (TmVar t1, t2, t3) when(t1 <> ident) -> TmLet (TmVar t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmLetRec (TmVar t1, t2, t3) when(t1 <> ident) -> TmLetRec (TmVar t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | _ -> expression

let isValue t =
  match t with
  | TmBool _ -> true
  | TmN _ -> true
  | TmFun (_, _) -> true
  | TmNil -> true
  | TmList (_, _) -> true
  | _ -> false

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t =
  match t with
(* CASO TmList ( t1 , t2 ) *)
  | TmList (t1, TmNil) ->
    TmList (t1, TmNil)
  | TmList (t1, t2) ->
    TmList (t1, t2)
(* CASO Hd t1 *)
  | TmHd (TmList (t1, t2)) when (isValue t1) ->
    t1
  | TmHd TmNil ->
    TmRaise
  | TmHd (TmList (t1, t2)) ->
    let t1' = step t1 in
    TmHd (TmList (t1', t2))
(* CASO Hd t1 *)
  | TmTl (TmList (t1, t2)) when (isValue t2)->
    t2
  | TmTl TmNil ->
    TmRaise
  | TmTl (TmList (t1, t2)) ->
    let t2' = step t2 in
    TmTl (TmList (t1, t2'))
(* CASO isEmtpy t1 *)
  | TmIsEmpty TmNil ->
    TmBool true
  | TmIsEmpty (TmList (t1, t2)) ->
    TmBool false
(* CASO IF ( t1 , t2 , t3 ) *)
  | TmIf ( TmBool true , t2 , t3 ) -> (* regra E−IfTrue *)
      t2
  | TmIf ( TmBool false , t2 , t3 ) -> (* regra E−False *)
    t3
  | TmIf ( TmRaise , t2 , t3 ) -> (* regra E−False *)
    TmRaise
  | TmIf ( t1 , t2 , t3 ) -> (* regra E−If *)
    let t1' = step t1 in
    TmIf ( t1', t2 , t3 )
(* CASO + ( t1, t2 ) *)
  | TmOp (  TmPlus, TmN t2, TmN t3 ) ->
    TmN (t2 + t3)
(* CASO - ( t1, t2 ) *)
  | TmOp (  TmMinus, TmN t2, TmN t3 ) ->
    TmN (t2 - t3)
(* CASO * ( t1, t2 ) *)
  | TmOp (  TmMult, TmN t2, TmN t3 ) ->
    TmN (t2 * t3)
(* CASO < ( t1, t2 ) *)
  | TmOp (  TmLt, TmN t2, TmN t3 ) ->
    TmBool (t2 < t3)
(* CASO <= ( t1, t2 ) *)
  | TmOp (  TmLtOrE, TmN t2, TmN t3 ) ->
    TmBool (t2 <= t3)
(* CASO > ( t1, t2 ) *)
  | TmOp (  TmGt, TmN t2, TmN t3 ) ->
    TmBool (t2 > t3)
(* CASO >= ( t1, t2 ) *)
  | TmOp (  TmGtOrE, TmN t2, TmN t3 ) ->
    TmBool (t2 >= t3)
(* CASO != ( t1, t2 ) *)
  | TmOp (  TmDiff, TmN t2, TmN t3 ) ->
    TmBool (t2 <> t3)
(* CASO == ( t1, t2 ) *)
  | TmOp (  TmEqual, TmN t2, TmN t3 ) ->
    TmBool (t2 = t3)
(* CASO div ( t1, t2 ) *)
  | TmOp (  TmDiv, TmN t2, TmN t3 ) when (t3 <> 0) ->
    TmN (t2 / t3)
  | TmOp (  TmDiv, TmN t2, TmN t3 ) when (t3 = 0) ->
    TmRaise
(* CASO try ( t1, t2 ) *)
  | TmTry ( t1, t2) when (isValue t1) ->
    t1
  | TmTry ( TmRaise, t2)  ->
    t2
  | TmTry ( t1, t2)  ->
    let t1' = step t1 in
    TmTry (t1', t2)
(* CASO op t1 t2 *)
  | TmOp (  t1, TmN t2, t3 ) ->
    let t3' = step t3 in
    TmOp (t1, TmN t2, t3')
  | TmOp (  t1, t2, t3 ) ->
    let t2' = step t2 in
    TmOp (t1, t2', t3)
  | TmOp (  _, TmRaise, _ ) ->
    TmRaise
  | TmOp (  _, _, TmRaise ) ->
    TmRaise
(* CASO t1 t2 *)
  | TmApp (TmFun (TmVar t1, t2), t3) when (isValue t3) ->
    replaceTerm t2 t1 t3
  | TmApp (TmFun (TmVar t1, t2), t3) ->
    let t3' = step t3 in
    TmApp (TmFun (TmVar t1, t2), t3')
  | TmApp (t1, TmRaise) when (isValue t1) ->
    TmRaise
  | TmApp (TmRaise, _) ->
    TmRaise
  | TmApp (t1, t2) ->
    let t1' = step t1 in
    TmApp (t1' , t2)
(* CASO  TmLet *)
  | TmLet (TmVar t1, t2, t3) when (isValue t2) ->
    replaceTerm t3 t1 t2
  | TmLet (TmVar t1, t2, t3) ->
    let t2' = step t2 in
    TmLet (TmVar t1, t2', t3)
(* CASO  TmLetRec *)
  | TmLetRec (TmVar t1, TmFun (t2, t3), t4) ->
    replaceTerm t4 t1 (TmFun (t2, TmLetRec (TmVar t1, TmFun (t2, t3), t3)))
(* CASO Nenhuma regra se aplique ao termo *)
  | _ ->
    raise NoRuleApplies

(* Implementacao de EVAL *)
let rec eval t =
  try let t' = step t
      in eval t'
  with NoRuleApplies -> t

(* ASTs para teste *)
let t1 = eval (TmOp (TmPlus, TmN 10, TmN 8) )
let t2 = eval (TmOp (TmPlus, TmOp (TmPlus, TmN 10, TmN 4), TmN 2) )
let t3 = eval (TmOp (TmMinus, TmOp (TmPlus, TmN 10, TmN 4), TmN 3) )
let t4 = eval (TmApp ((TmFun (TmVar "x", (TmOp (TmPlus, TmVar "x", TmN 2) ) ) ), TmN 5))
let t5 = eval (TmLet (TmVar "foo", TmFun (TmVar "y", TmOp (TmPlus, TmN 5, TmVar "y")), TmApp (TmVar "foo", TmN 10)))
(*
let x = 2 in
  let foo = (fn y => x+y) in
    let x = 5 in
      foo (10)
*)
let t6 = eval (TmLet (TmVar "x", TmN 2, TmLet (TmVar "foo", TmFun (TmVar "y", TmOp (TmPlus, TmVar "x", TmVar "y")) , TmLet (TmVar "x", TmN 5, TmApp (TmVar "foo", TmN 10)))))
let t7 = eval (TmLet (TmVar "x",
                       TmN 2,
                       TmLet (TmVar "foo",
                              TmFun (TmVar "y", TmOp (TmPlus, TmVar "x", TmVar "y")),
                              TmApp (TmVar "foo", TmN 10)
                              )
                       )
)

let t8 = eval (TmLetRec (TmVar "fat",
                          TmFun (TmVar "x", TmIf (TmOp (TmEqual, TmVar "x", TmN 0),
                                                  TmN 1,
                                                  TmOp (TmMult, TmVar "x", TmApp (TmVar "fat", TmOp (TmMinus, TmVar "x", TmN 1))))),
                          TmApp (TmVar "fat", TmN 5)
                           ))
let t9 = eval(TmHd (TmList (TmN 4, TmList (TmN 5, TmNil))))
let t10 = eval (TmOp (TmDiv, TmN 10, TmN 0) )

match t1 with
  | TmN c1 -> printf "t1 = %i\n" c1
  | _ -> printf "nan"
match t2 with
  | TmN c1 -> printf "t2 = %i\n" c1
  | _ -> printf "nan"
match t3 with
  | TmN c1 -> printf "t3 = %i\n" c1
  | _ -> printf "nan"
match t4 with
  | TmN c1 -> printf "t4 = %i\n" c1
  | _ -> printf "nan"
match t5 with
  | TmN c1 -> printf "t5 = %i\n" c1
  | _ -> printf "nan"
match t6 with
  | TmN c1 -> printf "t6 = %i\n" c1
  | _ -> printf "nan"
match t7 with
  | TmN c1 -> printf "t7 = %i\n" c1
  | _ -> printf "nan"
match t8 with
  | TmN c1 -> printf "t8 = %i\n" c1
  | _ -> printf "nan"
match t9 with
  | TmN c1 -> printf "t9 = %i\n" c1
  | _ -> printf "nan"
match t10 with
  | TmN c1 -> printf "t10 = %i\n" c1
  | TmRaise -> printf "t10 = raise\n"
  | _ -> printf "nan"
