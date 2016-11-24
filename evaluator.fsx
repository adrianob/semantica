type term =
  | TmN of int
  | TmBool of bool
  | TmPlus
  | TmMinus
  | TmMult
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
  | TmN t1 -> expression
  | TmBool _ -> expression
  | TmVar t1 when (t1 = ident)-> newTerm
  | TmVar _ -> expression
  | TmOp (t1, t2, t3) -> TmOp (t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmIf ( t1 , t2 , t3 ) -> TmIf (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmApp (t1, t2) -> TmApp (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm)
  | TmLet (TmVar ident, t2, t3) -> TmLet (TmVar ident, replaceTerm t2 ident newTerm, t3)
  | TmLet (t1, t2, t3) -> TmLet (t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
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
  | TmHd (TmList (t1, t2)) ->
    t1
(* CASO Hd t1 *)
  | TmTl (TmList (t1, t2)) ->
    t2
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
(* CASO op t1 t2 *)
  | TmOp (  t1, TmN t2, t3 ) ->
    let t3' = step t3 in
    TmOp (t1, TmN t2, t3')
  | TmOp (  t1, t2, t3 ) ->
    let t2' = step t2 in
    TmOp (t1, t2', t3)
(* CASO t1 t2 *)
  | TmApp (TmFun (TmVar t1, t2), t3) when (isValue t3) ->
    replaceTerm t2 t1 t3
  | TmApp (TmFun (TmVar t1, t2), t3) ->
    let t3' = step t3 in
    TmApp (TmFun (TmVar t1, t2), t3')
  | TmApp (t1, t2) ->
    let t1' = step t1 in
    TmApp (t1' , t2)
(* CASO  TmLet *)
  | TmLet (TmVar t1, t2, t3) when (isValue t2) ->
    replaceTerm t3 t1 t2
  | TmLet (TmVar t1, t2, t3) ->
    let t2' = step t2 in
    TmLet (TmVar t1, t2', t3)
(* CASO Nenhuma regra se aplique ao termo *)
  | _ ->
    raise NoRuleApplies
(* Implementacao de EVAL *)
let rec eval t =
  try let t' = step t
      in eval t'
  with NoRuleApplies -> t
(* ASTs para teste *)
let t10 = eval (TmN 10)
let t11 = eval (TmOp (TmPlus, TmN 10, TmN 2) )
let t12 = eval (TmOp (TmPlus, TmOp (TmPlus, TmN 10, TmN 4), TmN 2) )
let t13 = eval (TmOp (TmMinus, TmOp (TmPlus, TmN 10, TmN 4), TmN 2) )
let t18 = eval (TmApp ((TmFun (TmVar "x", (TmOp (TmPlus, TmVar "x", TmN 2) ) ) ), TmN 5))
let t19 = TmOp (TmPlus, TmVar "x", TmN 2)

let test = eval (replaceTerm t19 "x" (TmN 3))
match test with
  | TmN c1 -> printf "%i\n" c1
  | _ -> printf "nan"
match t18 with
  | TmN c1 -> printf "%i" c1
  | _ -> printf "nan"
