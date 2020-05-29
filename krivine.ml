module Coeff : sig
  type t [@@deriving show]

  val r_coeff_0 : t

  val r_coeff_1 : t

  val r_coeff : t

  val add : t -> t -> t

  val mul : t -> t -> t

  val coeff_handler : 'a -> 'a
end = struct
  type t = float [@@deriving show]

  let r_coeff_0 = 0.0

  let r_coeff_1 = 1.0

  let r_coeff = 0.1

  let coeff_handler x = x

  let add r1 r2 = r1 +. r2

  let mul r1 r2 = r1 *. r2
end

type nat = Z | S of nat [@@deriving show]

type expr =
  | Var of string
  | App of expr * expr
  | Let of string * expr * expr
  | Case of expr * expr * string * expr
  | Obs of expr
  | Coeff of expr
  | Succ of expr
  | Lambda of string * expr
  | Fix of string * expr
  | Exp of expr
  | Nat of nat
[@@deriving show]

type configure = expr * env * stack [@@deriving show]

and closure = expr * env [@@deriving show]

and env = Env of (string * closure) list [@@deriving show]

and stack =
  | EmptyS
  | ClosureS of closure * stack
  | CoeffS of stack
  | SuccS of stack
  | LetS of string * expr * env * stack
  | CaseS of expr * string * expr * env * stack
[@@deriving show]

type result = configure * Coeff.t [@@deriving show]

let search_env name = function Env env -> List.assoc name env

let add_env name closure = function Env env -> Env ((name, closure) :: env)

let rec calc_quantity = function
  | EmptyS ->
      Coeff.r_coeff_1
  | CoeffS stack ->
      Coeff.mul (calc_quantity stack) Coeff.r_coeff
  | ClosureS (_, stack)
  | SuccS stack
  | LetS (_, _, _, stack)
  | CaseS (_, _, _, _, stack) ->
      calc_quantity stack

let is_value = function Lambda (_, _) | Nat _ | Exp _ -> true | _ -> false

let rec eval quantity = function
  | Var x, (Env env_l as env), stack when List.mem_assoc x env_l ->
      let new_expr, new_env = search_env x env in
      eval quantity (new_expr, new_env, stack)
  | Lambda (name, expr), env, ClosureS (c, stack) ->
      eval quantity (expr, add_env name c env, stack)
  | App (e1, e2), env, stack ->
      eval quantity (e1, env, ClosureS ((e2, env), stack))
  | Let (name, e1, e2), env, stack ->
      eval quantity (e1, env, LetS (name, e2, env, stack))
  | Exp e1, env, LetS (x, e2, _, stack) ->
      eval quantity (e2, add_env x (e1, env) env, stack)
  | Succ e, env, stack ->
      eval quantity (e, env, SuccS stack)
  | Nat n, env, SuccS stack ->
      eval quantity (Nat (S n), env, stack)
  | Case (e, z_expr, x, s_expr), env, stack ->
      eval quantity (e, env, CaseS (z_expr, x, s_expr, env, stack))
  | Nat Z, _, CaseS (z_expr, _, _, env2, stack) ->
      eval quantity (z_expr, env2, stack)
  | Nat (S n), env, CaseS (_, x, s_expr, env2, stack) ->
      eval quantity (s_expr, add_env x (Nat (S n), env) env2, stack)
  | Fix (x, e), env, stack ->
      eval quantity (e, add_env x (Fix (x, e), env) env, stack)
  | Coeff e, env, stack ->
      eval quantity (e, env, CoeffS stack)
  | Obs e, env, stack ->
      eval (Coeff.add (calc_quantity stack) quantity) (e, env, stack)
  | e, env, CoeffS stack when is_value e ->
      eval quantity (Coeff.coeff_handler e, env, stack)
  | config ->
      (config, quantity)

let exp_fix_1 =
  App
    ( Fix
        ( "f"
        , Lambda
            ( "a"
            , Case
                ( Coeff (Obs (Var "a"))
                , App (Coeff (Var "f"), Succ (Coeff (Obs (Var "a"))))
                , "x"
                , App
                    ( App
                        ( Lambda ("x", Lambda ("y", Var "x"))
                        , Coeff (Obs (Var "a")) )
                    , Coeff (Obs (Var "a")) ) ) ) )
    , Nat Z )

let exp_fix_2 =
  App
    ( Fix
        ( "f"
        , Lambda
            ( "a"
            , Case
                ( Coeff (Obs (Var "a"))
                , App (Coeff (Var "f"), Succ (Coeff (Obs (Var "a"))))
                , "x"
                , App
                    ( App
                        ( App
                            ( Lambda ("x", Lambda ("y", Lambda ("z", Var "x")))
                            , Coeff (Obs (Var "a")) )
                        , Coeff (Obs (Var "a")) )
                    , Coeff (Obs (Var "a")) ) ) ) )
    , Nat Z )

let exp = exp_fix_1

let _ =
  eval Coeff.r_coeff_0 (exp, Env [], EmptyS)
  |> show_result
  |> fun x -> x ^ "\n" |> print_string
