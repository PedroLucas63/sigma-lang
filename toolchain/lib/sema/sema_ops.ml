open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let rec ty_contains_fun (ty : Ty.t) : bool =
  match ty with
  | Ty.Fun _ -> true
  | Ty.Tuple tys -> List.exists ty_contains_fun tys
  | Ty.Array (t, _) | Ty.DynArray t -> ty_contains_fun t
  | _ -> false

let rec eval_const_int (env : env) (e : Typed_ast.expr) : int64 option =
  match e.desc with
  | Typed_ast.TExprInt n -> Some n
  | Typed_ast.TExprUnary (Ast.UnPos, sub) -> eval_const_int env sub
  | Typed_ast.TExprUnary (Ast.UnNeg, sub) -> Option.map Int64.neg (eval_const_int env sub)
  | Typed_ast.TExprIdent name ->
    (match Hashtbl.find_opt env name with
     | Some { is_const = true; const_int; _ } -> const_int
     | _ -> None)
  | _ -> None

let check_unary_type (op : Ast.unop) (ty : Ty.t) : Ty.t option =
  match op, ty with
  | (Ast.UnPos | Ast.UnNeg), (Ty.Int | Ty.Float | Ty.Double | Ty.Decimal) -> Some ty
  | Ast.UnNot, (Ty.Bool | Ty.Int) -> Some ty
  | _ -> None

let check_binary_type (op : Ast.binop) (lty : Ty.t) (rty : Ty.t) : Ty.t option =
  match lty, rty, op with
  | Ty.Tuple _, Ty.Tuple _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  | Ty.Array _, Ty.Array _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  | Ty.DynArray _, Ty.DynArray _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  | Ty.User _, Ty.User _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  | Ty.Array (t1, n1), Ty.Array (t2, n2), Ast.BinAdd when Ty.equal t1 t2 ->
    Some (Ty.Array (t1, n1 + n2))
  | Ty.Bool, Ty.Bool, (Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinEq | Ast.BinNe) -> Some Ty.Bool
  | Ty.Int, Ty.Int, (Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinShl | Ast.BinShr) -> Some Ty.Int
  | Ty.Char, Ty.Char, Ast.BinAdd -> Some Ty.String
  | Ty.Char, Ty.Char, Ast.BinSub -> Some Ty.Int
  | Ty.Char, Ty.Char, (Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe) -> Some Ty.Bool
  | Ty.Char, Ty.String, Ast.BinAdd -> Some Ty.String
  | Ty.Char, Ty.Int, Ast.BinAdd -> Some Ty.Char
  | Ty.Char, Ty.Int, Ast.BinSub -> Some Ty.Char
  | Ty.Char, Ty.Int, Ast.BinMul -> Some Ty.String
  | Ty.String, Ty.Char, Ast.BinAdd -> Some Ty.String
  | Ty.String, Ty.String, Ast.BinAdd -> Some Ty.String
  | Ty.String, Ty.String, (Ast.BinEq | Ast.BinNe) -> Some Ty.Bool
  | Ty.String, Ty.Int, Ast.BinMul -> Some Ty.String
  | Ty.Decimal, Ty.Decimal, (Ast.BinAdd | Ast.BinSub | Ast.BinMul | Ast.BinDiv | Ast.BinPow | Ast.BinMod)
  | Ty.Decimal, Ty.Int, (Ast.BinAdd | Ast.BinSub | Ast.BinMul | Ast.BinDiv | Ast.BinPow | Ast.BinMod)
  | Ty.Int, Ty.Decimal, (Ast.BinAdd | Ast.BinSub | Ast.BinMul | Ast.BinDiv | Ast.BinPow | Ast.BinMod) ->
    Some Ty.Decimal
  | Ty.Decimal, Ty.Decimal, (Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe)
  | Ty.Decimal, Ty.Int, (Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe)
  | Ty.Int, Ty.Decimal, (Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe) ->
    Some Ty.Bool
  | Ty.Decimal, Ty.Decimal, Ast.BinIntDiv
  | Ty.Decimal, Ty.Int, Ast.BinIntDiv
  | Ty.Int, Ty.Decimal, Ast.BinIntDiv ->
    Some Ty.Int
  | (Ty.Int | Ty.Float | Ty.Double), (Ty.Int | Ty.Float | Ty.Double), _ ->
    let wider =
      if lty = Ty.Double || rty = Ty.Double then Ty.Double
      else if lty = Ty.Float || rty = Ty.Float then Ty.Float
      else Ty.Int
    in
    (match op with
     | Ast.BinAdd | Ast.BinSub | Ast.BinMul | Ast.BinPow | Ast.BinMod -> Some wider
     | Ast.BinDiv -> Some (if wider = Ty.Int then Ty.Float else wider)
     | Ast.BinIntDiv -> Some Ty.Int
     | Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe -> Some Ty.Bool
     | Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinShl | Ast.BinShr -> None)
  | _ -> None

let rec is_zero_literal (e : Typed_ast.expr) : bool =
  match e.desc with
  | Typed_ast.TExprInt 0L -> true
  | Typed_ast.TExprFloat f -> f = 0.0
  | Typed_ast.TExprDouble d -> d = 0.0
  | Typed_ast.TExprDecimal m -> Decimal.is_zero m
  | Typed_ast.TExprUnary (_, sub) -> is_zero_literal sub
  | _ -> false

let binop_of_assign_op (op : Ast.assign_op) : Ast.binop option =
  match op with
  | Ast.AssignEq -> None
  | Ast.AssignAdd -> Some Ast.BinAdd
  | Ast.AssignSub -> Some Ast.BinSub
  | Ast.AssignMul -> Some Ast.BinMul
  | Ast.AssignDiv -> Some Ast.BinDiv
  | Ast.AssignIntDiv -> Some Ast.BinIntDiv
  | Ast.AssignMod -> Some Ast.BinMod
  | Ast.AssignPow -> Some Ast.BinPow
  | Ast.AssignAnd -> Some Ast.BinAnd
  | Ast.AssignOr -> Some Ast.BinOr
  | Ast.AssignXor -> Some Ast.BinXor
  | Ast.AssignShl -> Some Ast.BinShl
  | Ast.AssignShr -> Some Ast.BinShr

let validate_format_spec (spec : Ast.format_spec) (ty : Ty.t) : (unit, Diagnostic.t) result =
  let is_numeric =
    match ty with
    | Ty.Int | Ty.Float | Ty.Double | Ty.Decimal -> true
    | _ -> false
  in
  let supports_prec =
    match ty with
    | Ty.Float | Ty.Double | Ty.Decimal | Ty.String -> true
    | _ -> false
  in
  if spec.sign_plus && not is_numeric then
    Error
      (Diagnostic.error
         ~hint:"the '+' format flag is only valid for numeric types ('int', 'float', 'double', 'decimal')"
         ~code:"E0104" ~span:spec.span
         (Printf.sprintf "format flag '+' cannot be applied to value of type '%s'" (Ty.to_string ty)))
  else if spec.zero_pad && not is_numeric then
    Error
      (Diagnostic.error
         ~hint:"zero-padding ('0width') is only valid for numeric types; use explicit fill and alignment like '${s:0>5}' for non-numeric types"
         ~code:"E0104" ~span:spec.span
         (Printf.sprintf "zero-padding format '0' cannot be applied to value of type '%s'" (Ty.to_string ty)))
  else if spec.precision <> None && not supports_prec then
    Error
      (Diagnostic.error
         ~hint:"precision '.N' is only supported on 'float', 'double', 'decimal' (decimal places) and 'string' (max characters)"
         ~code:"E0104" ~span:spec.span
         (Printf.sprintf "precision specifier cannot be applied to value of type '%s'" (Ty.to_string ty)))
  else Ok ()
