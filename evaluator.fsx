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
  | TmFun of term * term
  | TmZero
  | TmSucc of term
  | TmIsEmpty of term
  | TmHd of term
  | TmTl of term
  | TmTry of term * term
  | TmIsZero of term

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies
(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isnumericval t =
  match t with
  TmZero -> true
  | TmSucc ( t1 ) -> isnumericval t1
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
(* CASO SUCC ( t1 ) *)
  | TmSucc ( t1 ) -> (* regra E−Succ *)
    let t1' = step t1 in
    TmSucc ( t1')
(* CASO ISZERO ( t1 ) *)
  | TmIsZero ( TmZero ) -> (* regra E−IsZeroZero *)
    TmTrue
  | TmIsZero ( TmSucc ( nv1 )) when ( isnumericval nv1 ) -> (* regra E−IsZeroSucc *)
    TmFalse
  | TmIsZero ( t1 ) -> (* regra E−IsZero *)
    let t1' = step t1 in
    TmIsZero ( t1')
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
  | TmApp (TmFun (TmVar t1, t2), TmN t3) ->
    t2 (*TODO substituir var*)
  | TmApp (TmFun (TmVar t1, t2), TmTrue) ->
    t2 (*TODO substituir var*)
  | TmApp (TmFun (TmVar t1, t2), TmFalse) ->
    t2 (*TODO substituir var*)
  | TmApp (TmFun (TmVar t1, t2), TmFun (t3, t4)) ->
    t2 (*TODO substituir var*)
  | TmApp (TmFun (TmVar t1, t2), t3) ->
    let t3' = step t3 in
    TmApp (TmFun (TmVar t1, t2), t3')
  | TmApp (t1, t2) ->
    let t1' = step t1 in
    TmApp (t1' , t2)
(* CASO Nenhuma regra se aplique ao termo *)
  | _ ->
    raise NoRuleApplies
(* Implementacao de EVAL *)
let rec eval t =
  try let t' = step t
      in eval t'
  with NoRuleApplies -> t
(* ASTs para teste *)
let t1 = TmIsZero ( TmZero )
let t2 = TmZero
let t3 = TmSucc ( TmZero )
let tif = TmIf ( t1 , t2 , t3 )
let t4 = TmIsZero ( TmSucc ( TmZero ))
let t5 = TmIsZero ( TmFalse )
let t10 = eval (TmN 10)
let t11 = eval (TmPlus (TmN 10, TmN 2) )
let t12 = eval (TmPlus (TmPlus (TmN 10, TmN 4), TmN 2) )
let t13 = eval (TmMinus (TmPlus (TmN 10, TmN 4), TmN 2) )
printf "%b" (eval t1 = TmTrue)
match t10 with
  | TmN c1 -> printf "%i" c1
  | _ -> printf "nan"
match t13 with
  | TmN c1 -> printf "%i" c1
  | _ -> printf "nan"
