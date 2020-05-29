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

let coeff_handler expr = expr

let search_env name = function Env env -> List.assoc name env

let add_env name closure = function Env env -> Env ((name, closure) :: env)

let rec eval = function
  | Var x, (Env env_l as env), stack when List.mem_assoc x env_l ->
      let new_expr, new_env = search_env x env in
      eval (new_expr, new_env, stack)
  | Lambda (name, expr), env, ClosureS (c, stack) ->
      eval (expr, add_env name c env, stack)
  | App (e1, e2), env, stack ->
      eval (e1, env, ClosureS ((e2, env), stack))
  | Let (name, e1, e2), env, stack ->
      eval (e1, env, LetS (name, e2, env, stack))
  | Exp e1, env, LetS (x, e2, _, stack) ->
      eval (e2, add_env x (e1, env) env, stack)
  | Succ e, env, stack ->
      eval (e, env, SuccS stack)
  | Nat n, env, SuccS stack ->
      eval (Nat (S n), env, stack)
  | Case (e, z_expr, x, s_expr), env, stack ->
      eval (e, env, CaseS (z_expr, x, s_expr, env, stack))
  | Nat Z, _, CaseS (z_expr, _, _, env2, stack) ->
      eval (z_expr, env2, stack)
  | Nat (S n), env, CaseS (_, x, s_expr, env2, stack) ->
      eval (s_expr, add_env x (Nat (S n), env) env2, stack)
  | Fix (x, e), env, stack ->
      eval (e, add_env x (Fix (x, e), env) env, stack)
  | Coeff e, env, stack ->
      eval (e, env, CoeffS stack)
  | Obs e, env, stack ->
      eval (e, env, stack)
  | e, env, CoeffS stack ->
      eval (e, env, stack)
  | config ->
      config

let exp =
  App
    ( Fix
        ( "f"
        , Lambda
            ( "a"
            , Case
                ( Coeff (Obs (Var "a"))
                , App (Coeff (Obs (Var "f")), Succ (Coeff (Obs (Var "a"))))
                , "x"
                , Coeff (Obs (Var "a")) ) ) )
    , Nat Z )

let _ = eval (exp, Env [], EmptyS) |> show_configure |> print_string
