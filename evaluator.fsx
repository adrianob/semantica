type term =
  | TmN of int
  | TmTrue
  | TmFalse
  | TmPlus of term * term
  | TmMinus of term * term
  | TmApp of term * term
  | TmNil
  | TmVar of string
  | TmIf of term * term * term
  | TmLet of term * term * term
  | TmFun of term * term
  | TmIsEmpty of term
  | TmHd of term
  | TmTl of term
  | TmTry of term * term

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies
let rec replaceTerm expression ident newTerm =
  match expression with
  | TmN t1 -> expression
  | TmTrue -> expression
  | TmFalse -> expression
  | TmVar ident -> newTerm
  | TmPlus (t1, t2) -> TmPlus (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm)
  | TmMinus (t1, t2) -> TmMinus (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm)
  | TmIf ( t1 , t2 , t3 ) -> TmIf (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | TmApp (t1, t2) -> TmApp (replaceTerm t1 ident newTerm, replaceTerm t2 ident newTerm)
  | TmLet (TmVar ident, t2, t3) -> TmLet (TmVar ident, replaceTerm t2 ident newTerm, t3)
  | TmLet (t1, t2, t3) -> TmLet (t1, replaceTerm t2 ident newTerm, replaceTerm t3 ident newTerm)
  | _ -> expression
let isValue t =
  match t with
  | TmTrue -> true
  | TmFalse -> true
  | TmN _ -> true
  | TmFun (_, _) -> true
  | _ -> false
(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t =
  match t with
(* CASO IF ( t1 , t2 , t3 ) *)
  TmIf ( TmTrue , t2 , t3 ) -> (* regra E−IfTrue *)
      t2
  | TmIf ( TmFalse , t2 , t3 ) -> (* regra E−False *)
    t3
  | TmIf ( t1 , t2 , t3 ) -> (* regra E−If *)
    let t1' = step t1 in
    TmIf ( t1', t2 , t3 )
(* CASO + ( t1, t2 ) *)
  | TmPlus ( TmN t1, TmN t2 )  ->
    TmN (t1 + t2)
  | TmPlus ( TmN t1, t2 )  ->
    let t2' = step t2 in
    TmPlus (TmN t1, t2')
  | TmPlus ( t1, t2 )  ->
    let t1' = step t1 in
    TmPlus (t1', t2)
(* CASO - ( t1, t2 ) *)
  | TmMinus ( TmN t1, TmN t2 )  ->
    TmN (t1 - t2)
  | TmMinus ( TmN t1, t2 )  ->
    let t2' = step t2 in
    TmMinus (TmN t1, t2')
  | TmMinus ( t1, t2 )  ->
    let t1' = step t1 in
    TmMinus (t1', t2)
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
let t11 = eval (TmPlus (TmN 10, TmN 2) )
let t12 = eval (TmPlus (TmPlus (TmN 10, TmN 4), TmN 2) )
let t13 = eval (TmMinus (TmPlus (TmN 10, TmN 4), TmN 2) )
let t18 = eval (TmApp ((TmFun (TmVar "x", (TmPlus (TmVar "x", TmN 2) ) ) ), TmN 5))
let t19 = TmPlus (TmVar "x", TmN 2)

(*printf "%b\n" (eval t1 = TmTrue)*)
let test = eval (replaceTerm t19 "x" (TmN 3))
match test with
  | TmN c1 -> printf "%i\n" c1
  | _ -> printf "nan"
match t18 with
  | TmN c1 -> printf "%i" c1
  | _ -> printf "nan"
