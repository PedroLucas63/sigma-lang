open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

type symbol_info = {
  ty : Ty.t;
  decl_span : Span.t;
  is_const : bool;
  const_int : int64 option;
}

type env = (string, symbol_info) Hashtbl.t

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
  (* tuple and array structural equality *)
  | Ty.Tuple _, Ty.Tuple _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  | Ty.Array _, Ty.Array _, (Ast.BinEq | Ast.BinNe) when Ty.equal lty rty -> Some Ty.Bool
  (* array concatenation *)
  | Ty.Array (t1, n1), Ty.Array (t2, n2), Ast.BinAdd when Ty.equal t1 t2 ->
    Some (Ty.Array (t1, n1 + n2))
  (* logical & equality operations on bool *)
  | Ty.Bool, Ty.Bool, (Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinEq | Ast.BinNe) -> Some Ty.Bool
  (* bitwise operations on int *)
  | Ty.Int, Ty.Int, (Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinShl | Ast.BinShr) -> Some Ty.Int
  (* char & string operations *)
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
  (* exact base-10 decimal hierarchy (only mixes with int or decimal) *)
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
  (* IEEE-754 binary hierarchy: int < float < double *)
  | (Ty.Int | Ty.Float | Ty.Double), (Ty.Int | Ty.Float | Ty.Double), _ ->
    let wider =
      if lty = Ty.Double || rty = Ty.Double then Ty.Double
      else if lty = Ty.Float || rty = Ty.Float then Ty.Float
      else Ty.Int
    in
    (match op with
     | Ast.BinAdd | Ast.BinSub | Ast.BinMul | Ast.BinPow | Ast.BinMod ->
       Some wider
     | Ast.BinDiv ->
       Some (if wider = Ty.Int then Ty.Float else wider)
     | Ast.BinIntDiv ->
       Some Ty.Int
     | Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe ->
       Some Ty.Bool
     | Ast.BinAnd | Ast.BinOr | Ast.BinXor | Ast.BinShl | Ast.BinShr ->
       None)
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

let rec check_expr (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
  match e.desc with
  | Ast.ExprInt n ->
    Ok { Typed_ast.desc = Typed_ast.TExprInt n; ty = Ty.Int; span = e.span }
  | Ast.ExprFloat f ->
    Ok { Typed_ast.desc = Typed_ast.TExprFloat f; ty = Ty.Float; span = e.span }
  | Ast.ExprDouble d ->
    Ok { Typed_ast.desc = Typed_ast.TExprDouble d; ty = Ty.Double; span = e.span }
  | Ast.ExprDecimal m ->
    Ok { Typed_ast.desc = Typed_ast.TExprDecimal m; ty = Ty.Decimal; span = e.span }
  | Ast.ExprBool b ->
    Ok { Typed_ast.desc = Typed_ast.TExprBool b; ty = Ty.Bool; span = e.span }
  | Ast.ExprChar c ->
    Ok { Typed_ast.desc = Typed_ast.TExprChar c; ty = Ty.Char; span = e.span }
  | Ast.ExprString s ->
    Ok { Typed_ast.desc = Typed_ast.TExprString s; ty = Ty.String; span = e.span }
  | Ast.ExprInterp parts ->
    let rec check_parts acc = function
      | [] -> Ok (List.rev acc)
      | Ast.InterpLit s :: rest ->
        check_parts (Typed_ast.TInterpLit s :: acc) rest
      | Ast.InterpExpr ie :: rest ->
        (match check_expr env ie with
         | Error err -> Error err
         | Ok tie -> check_parts (Typed_ast.TInterpExpr tie :: acc) rest)
    in
    (match check_parts [] parts with
     | Ok tparts ->
       Ok { Typed_ast.desc = Typed_ast.TExprInterp tparts; ty = Ty.String; span = e.span }
     | Error err -> Error err)
  | Ast.ExprIdent name ->
    (match Hashtbl.find_opt env name with
     | Some info ->
       Ok { Typed_ast.desc = Typed_ast.TExprIdent name; ty = info.ty; span = e.span }
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;'" name name)
            ~code:"E0102"
            ~span:e.span
            (Printf.sprintf "use of undeclared variable '%s'" name)))
  | Ast.ExprTuple items ->
    (match check_collection_items env items with
     | Error err -> Error err
     | Ok checked ->
       let titems = List.map fst checked in
       let elem_tys = List.concat_map snd checked in
       Ok
         {
           Typed_ast.desc = Typed_ast.TExprTuple titems;
           ty = Ty.Tuple elem_tys;
           span = e.span;
         })
  | Ast.ExprArray items ->
    if items = [] then
      Error
        (Diagnostic.error
           ~hint:"static arrays require at least 1 element (or declare with explicit size like 'var a : int[3];')"
           ~code:"E0108"
           ~span:e.span
           "empty array literal '[]' is not allowed")
    else
      (match check_collection_items env items with
       | Error err -> Error err
       | Ok checked ->
         let titems = List.map fst checked in
         let elem_tys = List.concat_map snd checked in
         match elem_tys with
         | [] ->
           Error
             (Diagnostic.error
                ~code:"E0108"
                ~span:e.span
                "array literal must contain at least 1 element")
         | first_ty :: rest_tys ->
           (match List.find_opt (fun t -> not (Ty.equal first_ty t)) rest_tys with
            | Some bad_ty ->
              Error
                (Diagnostic.error
                   ~hint:"all elements in an array must have the exact same type; use a tuple '(a, b)' for heterogeneous values"
                   ~code:"E0104"
                   ~span:e.span
                   (Printf.sprintf "array elements must all have the same type: expected '%s', found '%s'"
                      (Ty.to_string first_ty)
                      (Ty.to_string bad_ty)))
            | None ->
              Ok
                {
                  Typed_ast.desc = Typed_ast.TExprArray titems;
                  ty = Ty.Array (first_ty, List.length elem_tys);
                  span = e.span;
                }))
  | Ast.ExprIndex (target, idx) ->
    (match check_index_access env ~span:e.span target idx with
     | Error err -> Error err
     | Ok (ttarget, tidx, elem_ty) ->
       Ok
         {
           Typed_ast.desc = Typed_ast.TExprIndex (ttarget, tidx);
           ty = elem_ty;
           span = e.span;
         })
  | Ast.ExprUnary (op, sub) ->
    (match check_expr env sub with
     | Error err -> Error err
     | Ok tsub ->
       (match check_unary_type op tsub.ty with
        | Some res_ty ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TExprUnary (op, tsub);
              ty = res_ty;
              span = e.span;
            }
        | None ->
          Error
            (Diagnostic.error
               ~hint:"unary '+' and '-' are only supported on numeric types ('int', 'float', 'double', 'decimal')"
               ~code:"E0104"
               ~span:e.span
               (Printf.sprintf "invalid unary operator '%s' for operand of type '%s'"
                  (Ast.unop_to_string op)
                  (Ty.to_string tsub.ty)))))
  | Ast.ExprBinary (op, lhs, rhs) ->
    (match check_expr env lhs with
     | Error err -> Error err
     | Ok tlhs ->
       (match check_expr env rhs with
        | Error err -> Error err
        | Ok trhs ->
          (match op, tlhs.ty, trhs.ty with
           | Ast.BinMul, Ty.Array (elem_ty, n), Ty.Int ->
             (match eval_const_int env trhs with
              | None ->
                Error
                  (Diagnostic.error
                     ~hint:"array repetition requires a compile-time constant integer so the static array size is known"
                     ~code:"E0104"
                     ~span:trhs.span
                     "array repetition multiplier must be a compile-time integer constant")
              | Some k when k <= 0L ->
                Error
                  (Diagnostic.error
                     ~hint:"use a positive repetition count >= 1, e.g. 'arr * 3'"
                     ~code:"E0108"
                     ~span:trhs.span
                     (Printf.sprintf "array repetition factor must be positive (> 0), found %Ld" k))
              | Some k ->
                let res_ty = Ty.Array (elem_ty, n * Int64.to_int k) in
                Ok
                  {
                    Typed_ast.desc = Typed_ast.TExprBinary (op, tlhs, trhs);
                    ty = res_ty;
                    span = e.span;
                  })
           | Ast.BinMul, Ty.Int, Ty.Array (elem_ty, n) ->
             (match eval_const_int env tlhs with
              | None ->
                Error
                  (Diagnostic.error
                     ~hint:"array repetition requires a compile-time constant integer so the static array size is known"
                     ~code:"E0104"
                     ~span:tlhs.span
                     "array repetition multiplier must be a compile-time integer constant")
              | Some k when k <= 0L ->
                Error
                  (Diagnostic.error
                     ~hint:"use a positive repetition count >= 1, e.g. '3 * arr'"
                     ~code:"E0108"
                     ~span:tlhs.span
                     (Printf.sprintf "array repetition factor must be positive (> 0), found %Ld" k))
              | Some k ->
                let res_ty = Ty.Array (elem_ty, n * Int64.to_int k) in
                Ok
                  {
                    Typed_ast.desc = Typed_ast.TExprBinary (op, tlhs, trhs);
                    ty = res_ty;
                    span = e.span;
                  })
           | _ ->
             (match check_binary_type op tlhs.ty trhs.ty with
              | None ->
                let hint =
                  if
                    (tlhs.ty = Ty.Decimal && (trhs.ty = Ty.Float || trhs.ty = Ty.Double))
                    || (trhs.ty = Ty.Decimal && (tlhs.ty = Ty.Float || tlhs.ty = Ty.Double))
                  then
                    "mixing binary floating-point ('float'/'double') with exact base-10 'decimal' is forbidden to prevent precision loss"
                  else
                    Printf.sprintf "operator '%s' is not defined for (%s, %s)"
                      (Ast.binop_to_string op)
                      (Ty.to_string tlhs.ty)
                      (Ty.to_string trhs.ty)
                in
                Error
                  (Diagnostic.error
                     ~hint
                     ~code:"E0104"
                     ~span:e.span
                     (Printf.sprintf "invalid binary operation: '%s' between '%s' and '%s'"
                        (Ast.binop_to_string op)
                        (Ty.to_string tlhs.ty)
                        (Ty.to_string trhs.ty)))
              | Some res_ty ->
                if
                  (op = Ast.BinDiv || op = Ast.BinIntDiv || op = Ast.BinMod)
                  && is_zero_literal trhs
                then
                  Error
                    (Diagnostic.error
                       ~code:"E0105"
                       ~span:e.span
                       (Printf.sprintf "division or modulo by zero in '%s' operation"
                          (Ast.binop_to_string op)))
                else
                  Ok
                    {
                      Typed_ast.desc = Typed_ast.TExprBinary (op, tlhs, trhs);
                      ty = res_ty;
                      span = e.span;
                    }))))
  | Ast.ExprChainCmp (e1, op1, e2, op2, e3) ->
    (match check_expr env e1, check_expr env e2, check_expr env e3 with
     | Error err, _, _ | _, Error err, _ | _, _, Error err -> Error err
     | Ok te1, Ok te2, Ok te3 ->
       (match check_binary_type op1 te1.ty te2.ty with
        | None ->
          Error
            (Diagnostic.error
               ~code:"E0104"
               ~span:(Span.merge te1.span te2.span)
               (Printf.sprintf "invalid relational operation: '%s' between '%s' and '%s'"
                  (Ast.binop_to_string op1)
                  (Ty.to_string te1.ty)
                  (Ty.to_string te2.ty)))
        | Some _ ->
          (match check_binary_type op2 te2.ty te3.ty with
           | None ->
             Error
               (Diagnostic.error
                  ~code:"E0104"
                  ~span:(Span.merge te2.span te3.span)
                  (Printf.sprintf "invalid relational operation: '%s' between '%s' and '%s'"
                     (Ast.binop_to_string op2)
                     (Ty.to_string te2.ty)
                     (Ty.to_string te3.ty)))
           | Some _ ->
             Ok
               {
                 Typed_ast.desc = Typed_ast.TExprChainCmp (te1, op1, te2, op2, te3);
                 ty = Ty.Bool;
                 span = e.span;
               })))
  | Ast.ExprAssign (lval, op, rhs) ->
    (match check_lvalue env ~for_op:(Ast.assign_op_to_string op) lval with
     | Error err -> Error err
     | Ok (tlval, lval_ty, lval_label) ->
       (match check_expr env rhs with
        | Error err -> Error err
        | Ok trhs ->
          (match binop_of_assign_op op with
           | None ->
             if Ty.equal lval_ty trhs.ty then
               Ok
                 {
                   Typed_ast.desc = Typed_ast.TExprAssign (tlval, op, trhs);
                   ty = lval_ty;
                   span = e.span;
                 }
             else
               Error
                 (Diagnostic.error
                    ~hint:
                      (Printf.sprintf
                         "Sigma is strongly typed: cannot assign value of type '%s' to %s of type '%s'"
                         (Ty.to_string trhs.ty)
                         lval_label
                         (Ty.to_string lval_ty))
                    ~code:"E0104"
                    ~span:e.span
                    (Printf.sprintf "type mismatch in assignment: %s has type '%s', found '%s'"
                       lval_label
                       (Ty.to_string lval_ty)
                       (Ty.to_string trhs.ty)))
           | Some binop ->
             let op_str = Ast.assign_op_to_string op in
             let eval_res =
               match binop, lval_ty, trhs.ty with
               | Ast.BinMul, Ty.Array (elem_ty, n), Ty.Int ->
                 (match eval_const_int env trhs with
                  | Some k when k > 0L -> Some (Ty.Array (elem_ty, n * Int64.to_int k))
                  | _ -> None)
               | _ -> check_binary_type binop lval_ty trhs.ty
             in
             (match eval_res with
              | None ->
                Error
                  (Diagnostic.error
                     ~hint:
                       (Printf.sprintf "compound operator '%s' is not defined for (%s, %s)"
                          op_str
                          (Ty.to_string lval_ty)
                          (Ty.to_string trhs.ty))
                     ~code:"E0104"
                     ~span:e.span
                     (Printf.sprintf "invalid compound assignment: '%s' between '%s' and '%s'"
                        op_str
                        (Ty.to_string lval_ty)
                        (Ty.to_string trhs.ty)))
              | Some res_ty ->
                if not (Ty.equal res_ty lval_ty) then
                  let hint =
                    if op = Ast.AssignDiv && lval_ty = Ty.Int then
                      "in Sigma, '/' returns 'float'; use '//=' for integer division assignment on 'int'"
                    else
                      Printf.sprintf
                        "operation produces '%s', which cannot be stored back into %s of type '%s'"
                        (Ty.to_string res_ty)
                        lval_label
                        (Ty.to_string lval_ty)
                  in
                  Error
                    (Diagnostic.error
                       ~hint
                       ~code:"E0104"
                       ~span:e.span
                       (Printf.sprintf
                          "type mismatch in compound assignment '%s': result type '%s' does not match %s of type '%s'"
                          op_str
                          (Ty.to_string res_ty)
                          lval_label
                          (Ty.to_string lval_ty)))
                else if
                  (binop = Ast.BinDiv || binop = Ast.BinIntDiv || binop = Ast.BinMod)
                  && is_zero_literal trhs
                then
                  Error
                    (Diagnostic.error
                       ~code:"E0105"
                       ~span:e.span
                       (Printf.sprintf "division or modulo by zero in '%s' operation" op_str))
                else
                  Ok
                    {
                      Typed_ast.desc = Typed_ast.TExprAssign (tlval, op, trhs);
                      ty = lval_ty;
                      span = e.span;
                    }))))
  | Ast.ExprIncDec (inc_op, lval) ->
    let op_str = Ast.incdec_op_to_string inc_op in
    (match check_lvalue env ~for_op:op_str lval with
     | Error err -> Error err
     | Ok (tlval, lval_ty, lval_label) ->
       (match lval_ty with
        | Ty.Int | Ty.Float | Ty.Double | Ty.Decimal | Ty.Char ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TExprIncDec (inc_op, tlval);
              ty = lval_ty;
              span = e.span;
            }
        | Ty.Bool | Ty.String | Ty.Tuple _ | Ty.Array _ ->
          Error
            (Diagnostic.error
               ~hint:"'++' and '--' are supported on numeric types ('int', 'float', 'double', 'decimal') and 'char'"
               ~code:"E0104"
               ~span:e.span
               (Printf.sprintf "invalid operator '%s' for %s of type '%s'"
                  op_str lval_label (Ty.to_string lval_ty)))))

and check_collection_items (env : env) (items : Ast.collection_item list)
    : ((Typed_ast.collection_item * Ty.t list) list, Diagnostic.t) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ast.ItemExpr sub :: rest ->
      (match check_expr env sub with
       | Error err -> Error err
       | Ok tsub ->
         loop ((Typed_ast.TItemExpr tsub, [ tsub.ty ]) :: acc) rest)
    | Ast.ItemSpread (sub, sp) :: rest ->
      (match check_expr env sub with
       | Error err -> Error err
       | Ok tsub ->
         (match tsub.ty with
          | Ty.Tuple elem_tys ->
            loop ((Typed_ast.TItemSpread tsub, elem_tys) :: acc) rest
          | Ty.Array (elem_ty, n) ->
            let tys = List.init n (fun _ -> elem_ty) in
            loop ((Typed_ast.TItemSpread tsub, tys) :: acc) rest
          | _ ->
            Error
              (Diagnostic.error
                 ~hint:"the spread operator '..' can only unpack tuples or arrays, e.g. '[..t, ..l, 50]'"
                 ~code:"E0104"
                 ~span:sp
                 (Printf.sprintf "cannot spread value of type '%s' with '..' (expected a tuple or array)"
                    (Ty.to_string tsub.ty)))))
  in
  loop [] items

and check_index_access (env : env) ~span:_ (target : Ast.expr) (idx : Ast.expr)
    : (Typed_ast.expr * Typed_ast.expr * Ty.t, Diagnostic.t) result =
  match check_expr env target with
  | Error err -> Error err
  | Ok ttarget ->
    (match check_expr env idx with
     | Error err -> Error err
     | Ok tidx ->
       if tidx.ty <> Ty.Int then
         Error
           (Diagnostic.error
              ~hint:"indices must be integer expressions of type 'int' (0-based)"
              ~code:"E0104"
              ~span:tidx.span
              (Printf.sprintf "index must be of type 'int', found '%s'"
                 (Ty.to_string tidx.ty)))
       else
         (match ttarget.ty with
          | Ty.Tuple elem_tys ->
            let len = List.length elem_tys in
            (match eval_const_int env tidx with
             | Some k ->
               if k < 0L || k >= Int64.of_int len then
                 Error
                   (Diagnostic.error
                      ~hint:(Printf.sprintf "valid indices for this tuple are 0..%d" (len - 1))
                      ~code:"E0108"
                      ~span:tidx.span
                      (Printf.sprintf "tuple index %Ld out of bounds for tuple of type '%s' (length %d)"
                         k (Ty.to_string ttarget.ty) len))
               else
                 Ok (ttarget, tidx, List.nth elem_tys (Int64.to_int k))
             | None ->
               let first_ty = List.hd elem_tys in
               if List.for_all (Ty.equal first_ty) elem_tys then
                 Ok (ttarget, tidx, first_ty)
               else
                 Error
                   (Diagnostic.error
                      ~hint:"use a constant integer literal index like 'tuple[0]' for heterogeneous tuples"
                      ~code:"E0104"
                      ~span:tidx.span
                      "indexing a heterogeneous tuple requires a compile-time constant integer index"))
          | Ty.Array (elem_ty, len) ->
            (match eval_const_int env tidx with
             | Some k when k < 0L || k >= Int64.of_int len ->
               Error
                 (Diagnostic.error
                    ~hint:(Printf.sprintf "valid indices for '%s' are 0..%d" (Ty.to_string ttarget.ty) (len - 1))
                    ~code:"E0108"
                    ~span:tidx.span
                    (Printf.sprintf "array index %Ld out of bounds for array of type '%s' (length %d)"
                       k (Ty.to_string ttarget.ty) len))
             | _ ->
               Ok (ttarget, tidx, elem_ty))
          | _ ->
            Error
              (Diagnostic.error
                 ~hint:"indexing '[i]' is only supported on tuples and arrays"
                 ~code:"E0104"
                 ~span:ttarget.span
                 (Printf.sprintf "cannot index value of type '%s' (expected a tuple or array)"
                    (Ty.to_string ttarget.ty)))))

and check_lvalue (env : env) ~for_op (lval : Ast.lvalue)
    : (Typed_ast.lvalue * Ty.t * string, Diagnostic.t) result =
  match lval with
  | Ast.LValIdent (name, name_span) ->
    (match Hashtbl.find_opt env name with
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;' or 'const %s := ...;'" name name name)
            ~code:"E0102"
            ~span:name_span
            (Printf.sprintf "use of undeclared variable '%s'" name))
     | Some info ->
       if info.is_const then
         let msg =
           if for_op = "++" || for_op = "--" then
             Printf.sprintf "cannot mutate constant '%s' with '%s'" name for_op
           else
             Printf.sprintf "cannot assign to constant '%s'" name
         in
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "'%s' was declared as a constant at %s" name (Span.to_string info.decl_span))
              ~code:"E0107"
              ~span:name_span
              msg)
       else
         Ok (Typed_ast.TLValIdent name, info.ty, Printf.sprintf "variable '%s'" name))
  | Ast.LValIndex (base, idx, span) ->
    let rec check_chain_mutable (e : Ast.expr) : Diagnostic.t option =
      match e.desc with
      | Ast.ExprIdent name ->
        (match Hashtbl.find_opt env name with
         | Some info when info.is_const ->
           Some
             (Diagnostic.error
                ~hint:(Printf.sprintf "'%s' was declared as a constant at %s" name (Span.to_string info.decl_span))
                ~code:"E0107"
                ~span
                (Printf.sprintf "cannot mutate element of constant '%s'" name))
         | _ -> None)
      | Ast.ExprIndex (sub_base, _) -> check_chain_mutable sub_base
      | _ -> None
    in
    (match check_index_access env ~span base idx with
     | Error err -> Error err
     | Ok (tbase, tidx, elem_ty) ->
       let rec has_tuple_in_base (te : Typed_ast.expr) : bool =
         match te.ty with
         | Ty.Tuple _ -> true
         | _ ->
           (match te.desc with
            | Typed_ast.TExprIndex (inner, _) -> has_tuple_in_base inner
            | _ -> false)
       in
       if has_tuple_in_base tbase then
         Error
           (Diagnostic.error
              ~hint:"tuples are immutable; overwrite the entire tuple variable instead (e.g. 't = (...);') or use an array '[...]'"
              ~code:"E0107"
              ~span
              "cannot mutate tuple element: tuples are immutable")
       else
         (match check_chain_mutable base with
          | Some err -> Error err
          | None ->
            Ok (Typed_ast.TLValIndex (tbase, tidx), elem_ty, "indexed element")))

let rec resolve_type_expr (te : Ast.type_expr) (init_ty_opt : Ty.t option)
    : (Ty.t, Diagnostic.t) result =
  match te with
  | Ast.TypeNamed (ty_name, ty_span) ->
    (match Ty.of_string ty_name with
     | Some ty -> Ok ty
     | None ->
       Error
         (Diagnostic.error
            ~hint:"supported types: 'int', 'float', 'double', 'decimal', 'bool', 'char', 'string', tuples '(T1, T2)', or arrays 'T[N]'"
            ~code:"E0101"
            ~span:ty_span
            (Printf.sprintf "unknown or unsupported type '%s'" ty_name)))
  | Ast.TypeTuple (sub_tes, _) ->
    let init_subs =
      match init_ty_opt with
      | Some (Ty.Tuple tys) when List.length tys = List.length sub_tes ->
        List.map (fun t -> Some t) tys
      | _ -> List.map (fun _ -> None) sub_tes
    in
    let rec loop acc = function
      | [], [] -> Ok (Ty.Tuple (List.rev acc))
      | sub_te :: r1, sub_init :: r2 ->
        (match resolve_type_expr sub_te sub_init with
         | Error err -> Error err
         | Ok sub_ty -> loop (sub_ty :: acc) (r1, r2))
      | _ -> Ok (Ty.Tuple (List.rev acc))
    in
    loop [] (sub_tes, init_subs)
  | Ast.TypeArray (elem_te, size_opt, sp) ->
    let init_elem_opt, init_len_opt =
      match init_ty_opt with
      | Some (Ty.Array (ie_ty, ilen)) -> (Some ie_ty, Some ilen)
      | _ -> (None, None)
    in
    (match resolve_type_expr elem_te init_elem_opt with
     | Error err -> Error err
     | Ok elem_ty ->
       (match size_opt with
        | Some n ->
          if n <= 0 then
            Error
              (Diagnostic.error
                 ~hint:"specify a positive array size >= 1, e.g. 'int[3]'"
                 ~code:"E0108"
                 ~span:sp
                 (Printf.sprintf "array size must be greater than 0, found %d" n))
          else
            Ok (Ty.Array (elem_ty, n))
        | None ->
          (match init_len_opt, init_ty_opt with
           | Some ilen, _ -> Ok (Ty.Array (elem_ty, ilen))
           | None, None ->
             Error
               (Diagnostic.error
                  ~hint:(Printf.sprintf "provide an initializer like 'var a : %s = [...];' or specify a fixed size like '%s[3]'"
                           (Ast.type_expr_to_string te) (Ty.to_string elem_ty))
                  ~code:"E0106"
                  ~span:sp
                  (Printf.sprintf "array type '%s' with omitted size requires an initializing expression to infer its length"
                     (Ast.type_expr_to_string te)))
           | None, Some bad_init_ty ->
             Error
               (Diagnostic.error
                  ~code:"E0104"
                  ~span:sp
                  (Printf.sprintf "type mismatch: expected array type '%s', found '%s'"
                     (Ast.type_expr_to_string te)
                     (Ty.to_string bad_init_ty))))))

let rec default_expr_for_type (ty : Ty.t) (span : Span.t) : Typed_ast.expr =
  match ty with
  | Ty.Int ->
    { Typed_ast.desc = Typed_ast.TExprInt Ty.default_int_value; ty = Ty.Int; span }
  | Ty.Float ->
    { Typed_ast.desc = Typed_ast.TExprFloat Ty.default_float_value; ty = Ty.Float; span }
  | Ty.Double ->
    { Typed_ast.desc = Typed_ast.TExprDouble Ty.default_float_value; ty = Ty.Double; span }
  | Ty.Decimal ->
    { Typed_ast.desc = Typed_ast.TExprDecimal Ty.default_decimal_value; ty = Ty.Decimal; span }
  | Ty.Bool ->
    { Typed_ast.desc = Typed_ast.TExprBool Ty.default_bool_value; ty = Ty.Bool; span }
  | Ty.Char ->
    { Typed_ast.desc = Typed_ast.TExprChar Ty.default_char_value; ty = Ty.Char; span }
  | Ty.String ->
    { Typed_ast.desc = Typed_ast.TExprString Ty.default_string_value; ty = Ty.String; span }
  | Ty.Tuple tys ->
    let items =
      List.map (fun sub_ty -> Typed_ast.TItemExpr (default_expr_for_type sub_ty span)) tys
    in
    { Typed_ast.desc = Typed_ast.TExprTuple items; ty; span }
  | Ty.Array (elem_ty, n) ->
    let items =
      List.init n (fun _ -> Typed_ast.TItemExpr (default_expr_for_type elem_ty span))
    in
    { Typed_ast.desc = Typed_ast.TExprArray items; ty; span }

type active_loop = {
  loop_id : int;
  label : (string * Span.t) option;
}

let check_loop_label (loop_stack : active_loop list) (label : (string * Span.t) option) : Diagnostic.t option =
  match label with
  | None -> None
  | Some (lbl, lbl_span) ->
    (match
       List.find_opt
         (fun al ->
           match al.label with
           | Some (prev_lbl, _) -> prev_lbl = lbl
           | None -> false)
         loop_stack
     with
     | None -> None
     | Some { label = Some (_, prev_sp); _ } ->
       Some
         (Diagnostic.error
            ~hint:(Printf.sprintf "previously defined on an enclosing loop at %s" (Span.to_string prev_sp))
            ~code:"E0103"
            ~span:lbl_span
            (Printf.sprintf "loop label '%s' is already defined on an enclosing loop" lbl))
     | Some _ -> None)

let rec check_stmt
    (env : env)
    (scope_vars : string list ref)
    (loop_stack : active_loop list)
    (next_loop_id : int ref)
    (s : Ast.stmt) : (Typed_ast.stmt, Diagnostic.t list) result =
  match s.desc with
  | Ast.StmtExpr e ->
    (match check_expr env e with
     | Ok te -> Ok { Typed_ast.desc = Typed_ast.TStmtExpr te; span = s.span }
     | Error err -> Error [ err ])
  | Ast.StmtBlock stmts ->
    let block_vars = ref [] in
    let tstmts = ref [] in
    let errs = ref [] in
    List.iter
      (fun sub ->
        match check_stmt env block_vars loop_stack next_loop_id sub with
        | Ok ts -> tstmts := ts :: !tstmts
        | Error sub_errs -> errs := List.rev_append sub_errs !errs)
      stmts;
    List.iter (Hashtbl.remove env) !block_vars;
    (match List.rev !errs with
     | [] -> Ok { Typed_ast.desc = Typed_ast.TStmtBlock (List.rev !tstmts); span = s.span }
     | errors -> Error errors)
  | Ast.StmtIf { cond; then_branch; else_branch } ->
    let errs = ref [] in
    let tcond_opt =
      match check_expr env cond with
      | Error err ->
        errs := err :: !errs;
        None
      | Ok tc ->
        if tc.ty <> Ty.Bool then begin
          errs :=
            Diagnostic.error
              ~hint:"the condition of an 'if' statement must evaluate to 'bool' (e.g. 'if (x == 10)')"
              ~code:"E0104"
              ~span:tc.span
              (Printf.sprintf "type mismatch in 'if' condition: expected 'bool', found '%s'"
                 (Ty.to_string tc.ty))
            :: !errs;
          None
        end else
          Some tc
    in
    let tthen_opt =
      match check_stmt env scope_vars loop_stack next_loop_id then_branch with
      | Ok ts -> Some ts
      | Error sub_errs ->
        errs := List.rev_append sub_errs !errs;
        None
    in
    let telse_opt =
      match else_branch with
      | None -> Some None
      | Some eb ->
        (match check_stmt env scope_vars loop_stack next_loop_id eb with
         | Ok teb -> Some (Some teb)
         | Error sub_errs ->
           errs := List.rev_append sub_errs !errs;
           None)
    in
    (match List.rev !errs, tcond_opt, tthen_opt, telse_opt with
     | [], Some tc, Some tthen, Some telse ->
       Ok
         {
           Typed_ast.desc =
             Typed_ast.TStmtIf { cond = tc; then_branch = tthen; else_branch = telse };
           span = s.span;
         }
     | errors, _, _, _ -> Error errors)
  | Ast.StmtWhile { label; cond; body } ->
    incr next_loop_id;
    let loop_id = !next_loop_id in
    let errs = ref [] in
    (match check_loop_label loop_stack label with
     | Some err -> errs := err :: !errs
     | None -> ());
    let tcond_opt =
      match check_expr env cond with
      | Error err ->
        errs := err :: !errs;
        None
      | Ok tc ->
        if tc.ty <> Ty.Bool then begin
          errs :=
            Diagnostic.error
              ~hint:"the condition of a 'while' loop must evaluate to 'bool' (e.g. 'while (i < 10)')"
              ~code:"E0104"
              ~span:tc.span
              (Printf.sprintf "type mismatch in 'while' condition: expected 'bool', found '%s'"
                 (Ty.to_string tc.ty))
            :: !errs;
          None
        end else
          Some tc
    in
    let while_scope = ref [] in
    let new_loop_stack = { loop_id; label } :: loop_stack in
    let tbody_opt =
      match check_stmt env while_scope new_loop_stack next_loop_id body with
      | Ok tb -> Some tb
      | Error sub_errs ->
        errs := List.rev_append sub_errs !errs;
        None
    in
    List.iter (Hashtbl.remove env) !while_scope;
    (match List.rev !errs, tcond_opt, tbody_opt with
     | [], Some tc, Some tb ->
       Ok
         {
           Typed_ast.desc =
             Typed_ast.TStmtWhile
               { loop_id; label = Option.map fst label; cond = tc; body = tb };
           span = s.span;
         }
     | errors, _, _ -> Error errors)
  | Ast.StmtFor { label; init; cond; step; body } ->
    incr next_loop_id;
    let loop_id = !next_loop_id in
    let errs = ref [] in
    (match check_loop_label loop_stack label with
     | Some err -> errs := err :: !errs
     | None -> ());
    let for_scope = ref [] in
    let tinit_opt =
      match init with
      | Ast.ForInitVarDecl v ->
        (match check_stmt env for_scope loop_stack next_loop_id { Ast.desc = Ast.StmtVarDecl v; span = s.span } with
         | Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tv; _ } ->
           Some (Typed_ast.TForInitVarDecl tv)
         | Ok _ -> None
         | Error sub_errs ->
           errs := List.rev_append sub_errs !errs;
           None)
      | Ast.ForInitDestruct d ->
        (match check_stmt env for_scope loop_stack next_loop_id { Ast.desc = Ast.StmtDestruct d; span = s.span } with
         | Ok { Typed_ast.desc = Typed_ast.TStmtDestruct td; _ } ->
           Some (Typed_ast.TForInitDestruct td)
         | Ok _ -> None
         | Error sub_errs ->
           errs := List.rev_append sub_errs !errs;
           None)
      | Ast.ForInitExpr e ->
        (match check_expr env e with
         | Ok te -> Some (Typed_ast.TForInitExpr te)
         | Error err ->
           errs := err :: !errs;
           None)
    in
    let tcond_opt =
      match check_expr env cond with
      | Error err ->
        errs := err :: !errs;
        None
      | Ok tc ->
        if tc.ty <> Ty.Bool then begin
          errs :=
            Diagnostic.error
              ~hint:"the condition of a 'for' loop must evaluate to 'bool' (e.g. 'for (var i := 0; i < 10; i++)')"
              ~code:"E0104"
              ~span:tc.span
              (Printf.sprintf "type mismatch in 'for' condition: expected 'bool', found '%s'"
                 (Ty.to_string tc.ty))
            :: !errs;
          None
        end else
          Some tc
    in
    let tstep_opt =
      match check_expr env step with
      | Ok ts -> Some ts
      | Error err ->
        errs := err :: !errs;
        None
    in
    let new_loop_stack = { loop_id; label } :: loop_stack in
    let tbody_opt =
      match check_stmt env for_scope new_loop_stack next_loop_id body with
      | Ok tb -> Some tb
      | Error sub_errs ->
        errs := List.rev_append sub_errs !errs;
        None
    in
    List.iter (Hashtbl.remove env) !for_scope;
    (match List.rev !errs, tinit_opt, tcond_opt, tstep_opt, tbody_opt with
     | [], Some ti, Some tc, Some ts, Some tb ->
       Ok
         {
           Typed_ast.desc =
             Typed_ast.TStmtFor
               {
                 loop_id;
                 label = Option.map fst label;
                 init = ti;
                 cond = tc;
                 step = ts;
                 body = tb;
               };
           span = s.span;
         }
     | errors, _, _, _, _ -> Error errors)
  | Ast.StmtForeach { label; head; collection; body } ->
    incr next_loop_id;
    let loop_id = !next_loop_id in
    let errs = ref [] in
    (match check_loop_label loop_stack label with
     | Some err -> errs := err :: !errs
     | None -> ());
    let tcoll_opt =
      match check_expr env collection with
      | Error err ->
        errs := err :: !errs;
        None
      | Ok tc -> Some tc
    in
    let array_info_opt =
      match tcoll_opt with
      | None -> None
      | Some tc ->
        (match tc.ty with
         | Ty.Array (elem_ty, len) -> Some (tc, elem_ty, len)
         | _ ->
           errs :=
             Diagnostic.error
               ~hint:"'foreach' iterates over static arrays 'T[N]', e.g. 'foreach (var x in [10, 20, 30])'"
               ~code:"E0104"
               ~span:tc.span
               (Printf.sprintf "type mismatch in 'foreach' collection: expected an array 'T[N]', found '%s'"
                  (Ty.to_string tc.ty))
             :: !errs;
           None)
    in
    let foreach_scope = ref [] in
    let thead_opt =
      match array_info_opt with
      | None -> None
      | Some (_, elem_ty, _) ->
        let annot_ok =
          match head.ty_annot with
          | None -> true
          | Some annot ->
            (match resolve_type_expr annot (Some elem_ty) with
             | Error err ->
               errs := err :: !errs;
               false
             | Ok annot_ty ->
               if Ty.equal annot_ty elem_ty then true
               else begin
                 errs :=
                   Diagnostic.error
                     ~code:"E0104"
                     ~span:(Ast.type_expr_span annot)
                     (Printf.sprintf "type mismatch in 'foreach' binding: expected '%s', found array element of type '%s'"
                        (Ty.to_string annot_ty)
                        (Ty.to_string elem_ty))
                   :: !errs;
                 false
               end)
        in
        if not annot_ok then None
        else
          (match head.binding with
           | Ast.ForeachVar (name, name_span) ->
             if name = "_" then
               Some
                 (Typed_ast.TForeachVar
                    { name = None; name_span; is_const = head.is_const; ty = elem_ty })
             else begin
               (match Hashtbl.find_opt env name with
                | None -> ()
                | Some prev ->
                  let entity = if prev.is_const then "constant" else "variable" in
                  errs :=
                    Diagnostic.error
                      ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                      ~code:"E0103"
                      ~span:name_span
                      (Printf.sprintf "%s '%s' is already declared in this scope" entity name)
                    :: !errs);
               if !errs = [] then begin
                 Hashtbl.replace env name
                   { ty = elem_ty; decl_span = name_span; is_const = head.is_const; const_int = None };
                 foreach_scope := name :: !foreach_scope;
                 Some
                   (Typed_ast.TForeachVar
                      { name = Some name; name_span; is_const = head.is_const; ty = elem_ty })
               end else
                 None
             end
           | Ast.ForeachDestruct names ->
             let seen_in_pattern : (string, Span.t) Hashtbl.t = Hashtbl.create 8 in
             List.iter
               (fun (name, name_span) ->
                 if name <> "_" then begin
                   (match Hashtbl.find_opt env name with
                    | None -> ()
                    | Some prev ->
                      let entity = if prev.is_const then "constant" else "variable" in
                      errs :=
                        Diagnostic.error
                          ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                          ~code:"E0103"
                          ~span:name_span
                          (Printf.sprintf "%s '%s' is already declared in this scope" entity name)
                        :: !errs);
                   (match Hashtbl.find_opt seen_in_pattern name with
                    | None -> Hashtbl.replace seen_in_pattern name name_span
                    | Some prev_sp ->
                      errs :=
                        Diagnostic.error
                          ~hint:(Printf.sprintf "first bound in this pattern at %s" (Span.to_string prev_sp))
                          ~code:"E0103"
                          ~span:name_span
                          (Printf.sprintf "duplicate variable '%s' in destructuring pattern" name)
                        :: !errs)
                 end)
               names;
             let sub_tys_opt =
               match elem_ty with
               | Ty.Tuple tys -> Some tys
               | Ty.Array (sub_ty, n) -> Some (List.init n (fun _ -> sub_ty))
               | _ -> None
             in
             (match sub_tys_opt with
              | None ->
                errs :=
                  Diagnostic.error
                    ~hint:"destructuring in 'foreach' requires an array of tuples or nested arrays, e.g. '[(10, 20), (30, 40)]'"
                    ~code:"E0104"
                    ~span:s.span
                    (Printf.sprintf "cannot destructure 'foreach' element of type '%s' (expected a tuple or array)"
                       (Ty.to_string elem_ty))
                  :: !errs;
                None
              | Some sub_tys ->
                let pat_len = List.length names in
                let val_len = List.length sub_tys in
                if pat_len <> val_len then begin
                  errs :=
                    Diagnostic.error
                      ~hint:(Printf.sprintf "provide %d binding(s) in the pattern (use '_' to ignore elements)" val_len)
                      ~code:"E0104"
                      ~span:s.span
                      (Printf.sprintf "destructuring arity mismatch in 'foreach': pattern has %d elements, but element type '%s' has %d elements"
                         pat_len (Ty.to_string elem_ty) val_len)
                    :: !errs;
                  None
                end else if !errs = [] then begin
                  let bindings =
                    List.map2
                      (fun (name, name_span) sub_ty ->
                        if name = "_" then
                          { Typed_ast.name = None; name_span; ty = sub_ty }
                        else begin
                          Hashtbl.replace env name
                            { ty = sub_ty; decl_span = name_span; is_const = head.is_const; const_int = None };
                          foreach_scope := name :: !foreach_scope;
                          { Typed_ast.name = Some name; name_span; ty = sub_ty }
                        end)
                      names sub_tys
                  in
                  Some
                    (Typed_ast.TForeachDestruct
                       { bindings; is_const = head.is_const; ty = elem_ty })
                end else
                  None))
    in
    let new_loop_stack = { loop_id; label } :: loop_stack in
    let tbody_opt =
      match check_stmt env foreach_scope new_loop_stack next_loop_id body with
      | Ok tb -> Some tb
      | Error sub_errs ->
        errs := List.rev_append sub_errs !errs;
        None
    in
    List.iter (Hashtbl.remove env) !foreach_scope;
    (match List.rev !errs, array_info_opt, thead_opt, tbody_opt with
     | [], Some (tcoll, elem_ty, len), Some thead, Some tbody ->
       Ok
         {
           Typed_ast.desc =
             Typed_ast.TStmtForeach
               {
                 loop_id;
                 label = Option.map fst label;
                 head = thead;
                 collection = tcoll;
                 elem_ty;
                 len;
                 body = tbody;
               };
           span = s.span;
         }
     | errors, _, _, _ -> Error errors)
  | Ast.StmtBreak label_opt ->
    (match label_opt with
     | None ->
       (match loop_stack with
        | [] ->
          Error
            [
              Diagnostic.error
                ~hint:"use 'break;' inside a 'while', 'for', or 'foreach' loop"
                ~code:"E0109"
                ~span:s.span
                "'break' cannot be used outside of a loop";
            ]
        | top :: _ ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TStmtBreak { target_loop_id = top.loop_id; label = None };
              span = s.span;
            })
     | Some (lbl, lbl_span) ->
       if loop_stack = [] then
         Error
           [
             Diagnostic.error
               ~hint:"use 'break <label>;' inside an enclosing labeled loop"
               ~code:"E0109"
               ~span:s.span
               (Printf.sprintf "'break %s' cannot be used outside of a loop" lbl);
           ]
       else
         (match
            List.find_opt
              (fun al ->
                match al.label with
                | Some (l, _) -> l = lbl
                | None -> false)
              loop_stack
          with
          | Some target ->
            Ok
              {
                Typed_ast.desc =
                  Typed_ast.TStmtBreak { target_loop_id = target.loop_id; label = Some lbl };
                span = s.span;
              }
          | None ->
            Error
              [
                Diagnostic.error
                  ~hint:(Printf.sprintf "annotate an enclosing loop with '%s: for (...) { ... }'" lbl)
                  ~code:"E0109"
                  ~span:lbl_span
                  (Printf.sprintf "unknown or inactive loop label '%s' in 'break %s'" lbl lbl);
              ]))
  | Ast.StmtContinue label_opt ->
    (match label_opt with
     | None ->
       (match loop_stack with
        | [] ->
          Error
            [
              Diagnostic.error
                ~hint:"use 'continue;' inside a 'while', 'for', or 'foreach' loop"
                ~code:"E0109"
                ~span:s.span
                "'continue' cannot be used outside of a loop";
            ]
        | top :: _ ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TStmtContinue { target_loop_id = top.loop_id; label = None };
              span = s.span;
            })
     | Some (lbl, lbl_span) ->
       if loop_stack = [] then
         Error
           [
             Diagnostic.error
               ~hint:"use 'continue <label>;' inside an enclosing labeled loop"
               ~code:"E0109"
               ~span:s.span
               (Printf.sprintf "'continue %s' cannot be used outside of a loop" lbl);
           ]
       else
         (match
            List.find_opt
              (fun al ->
                match al.label with
                | Some (l, _) -> l = lbl
                | None -> false)
              loop_stack
          with
          | Some target ->
            Ok
              {
                Typed_ast.desc =
                  Typed_ast.TStmtContinue { target_loop_id = target.loop_id; label = Some lbl };
                span = s.span;
              }
          | None ->
            Error
              [
                Diagnostic.error
                  ~hint:(Printf.sprintf "annotate an enclosing loop with '%s: for (...) { ... }'" lbl)
                  ~code:"E0109"
                  ~span:lbl_span
                  (Printf.sprintf "unknown or inactive loop label '%s' in 'continue %s'" lbl lbl);
              ]))
  | Ast.StmtPrint { newline; args } ->
    let targs = ref [] in
    let errs = ref [] in
    List.iter
      (fun arg ->
        match check_expr env arg with
        | Ok targ -> targs := targ :: !targs
        | Error err -> errs := err :: !errs)
      args;
    (match List.rev !errs with
     | [] ->
       Ok
         {
           Typed_ast.desc = Typed_ast.TStmtPrint { newline; args = List.rev !targs };
           span = s.span;
         }
     | errors -> Error errors)
  | Ast.StmtAssert { actual; expected; message } ->
    let errs = ref [] in
    let tactual_opt =
      match check_expr env actual with
      | Ok ta -> Some ta
      | Error e ->
        errs := e :: !errs;
        None
    in
    let texpected_opt =
      match expected with
      | None -> None
      | Some exp_e ->
        (match check_expr env exp_e with
         | Ok te -> Some te
         | Error e ->
           errs := e :: !errs;
           None)
    in
    let tmsg_opt =
      match message with
      | None -> None
      | Some msg_e ->
        (match check_expr env msg_e with
         | Error e ->
           errs := e :: !errs;
           None
         | Ok tm ->
           if tm.ty = Ty.String then
             Some tm
           else begin
             errs :=
               Diagnostic.error
                 ~hint:"the optional 3rd argument of 'assert' is a diagnostic description string"
                 ~code:"E0104"
                 ~span:tm.span
                 (Printf.sprintf "type mismatch in 'assert' message: expected 'string', found '%s'"
                    (Ty.to_string tm.ty))
               :: !errs;
             None
           end)
    in
    (match List.rev !errs, tactual_opt, expected, texpected_opt with
     | _ :: _ as errors, _, _, _ -> Error errors
     | [], Some ta, None, _ ->
       if ta.ty = Ty.Bool then
         let ttrue =
           { Typed_ast.desc = Typed_ast.TExprBool true; ty = Ty.Bool; span = ta.span }
         in
         Ok
           {
             Typed_ast.desc =
               Typed_ast.TStmtAssert { actual = ta; expected = ttrue; message = tmsg_opt };
             span = s.span;
           }
       else
         Error
           [
             Diagnostic.error
               ~hint:"pass an expected value as second argument, e.g. 'assert(actual, expected);'"
               ~code:"E0104"
               ~span:ta.span
               (Printf.sprintf "single-argument 'assert' expects a 'bool' condition, found '%s'"
                  (Ty.to_string ta.ty));
           ]
     | [], Some ta, Some _, Some te ->
       if Ty.equal ta.ty te.ty then
         Ok
           {
             Typed_ast.desc =
               Typed_ast.TStmtAssert { actual = ta; expected = te; message = tmsg_opt };
             span = s.span;
           }
       else
         Error
           [
             Diagnostic.error
               ~hint:"both arguments in 'assert(actual, expected)' must have the exact same type"
               ~code:"E0104"
               ~span:s.span
               (Printf.sprintf
                  "type mismatch in 'assert': actual value has type '%s', expected value has type '%s'"
                  (Ty.to_string ta.ty)
                  (Ty.to_string te.ty));
           ]
     | _ -> Error [])
  | Ast.StmtAssertError { expected_code; snippet; message; _ } ->
    let errs = ref [] in
    let tmsg_opt =
      match message with
      | None -> None
      | Some msg_e ->
        (match check_expr env msg_e with
         | Error e ->
           errs := e :: !errs;
           None
         | Ok tm ->
           if tm.ty = Ty.String then
             Some tm
           else begin
             errs :=
               Diagnostic.error
                 ~hint:"the optional 3rd argument of 'assert_error' is a diagnostic description string"
                 ~code:"E0104"
                 ~span:tm.span
                 (Printf.sprintf "type mismatch in 'assert_error' message: expected 'string', found '%s'"
                    (Ty.to_string tm.ty))
               :: !errs;
             None
           end)
    in
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       let snippet_src = Source.of_string ~filename:"<assert_error>" ~content:snippet in
       let diags =
         match Lexer.tokenize snippet_src with
         | Error lex_errs -> lex_errs
         | Ok tokens ->
           (match Parser.parse tokens with
            | Error parse_errs -> parse_errs
            | Ok ast ->
              (match check_program ast with
               | Error sema_errs -> sema_errs
               | Ok _ -> []))
       in
       let actual_codes = List.map (fun (d : Diagnostic.t) -> d.code) diags in
       let passed = List.exists (fun c -> c = expected_code) actual_codes in
       Ok
         {
           Typed_ast.desc =
             Typed_ast.TStmtAssertError
               { expected_code; actual_codes; passed; message = tmsg_opt };
           span = s.span;
         })
  | Ast.StmtDestruct d ->
    let errs = ref [] in
    let seen_in_pattern : (string, Span.t) Hashtbl.t = Hashtbl.create 8 in
    List.iter
      (fun (name, name_span) ->
        if name <> "_" then begin
          (match Hashtbl.find_opt env name with
           | None -> ()
           | Some prev ->
             let entity = if prev.is_const then "constant" else "variable" in
             errs :=
               Diagnostic.error
                 ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                 ~code:"E0103"
                 ~span:name_span
                 (Printf.sprintf "%s '%s' is already declared in this scope" entity name)
               :: !errs);
          (match Hashtbl.find_opt seen_in_pattern name with
           | None -> Hashtbl.replace seen_in_pattern name name_span
           | Some prev_sp ->
             errs :=
               Diagnostic.error
                 ~hint:(Printf.sprintf "first bound in this pattern at %s" (Span.to_string prev_sp))
                 ~code:"E0103"
                 ~span:name_span
                 (Printf.sprintf "duplicate variable '%s' in destructuring pattern" name)
               :: !errs)
        end)
      d.names;
    if d.is_const && Option.is_none d.init then
      errs :=
        Diagnostic.error
          ~code:"E0106"
          ~span:s.span
          "constant destructuring declaration requires an initializing expression"
        :: !errs;
    let resolved_init =
      match d.init with
      | None -> None
      | Some init_expr ->
        (match check_expr env init_expr with
         | Ok te -> Some te
         | Error err ->
           errs := err :: !errs;
           None)
    in
    let init_ty_opt = Option.map (fun (te : Typed_ast.expr) -> te.ty) resolved_init in
    let resolved_annot =
      match d.ty_annot with
      | None -> None
      | Some annot ->
        (match resolve_type_expr annot init_ty_opt with
         | Ok ty -> Some ty
         | Error err ->
           errs := err :: !errs;
           None)
    in
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       (match resolved_annot, resolved_init with
        | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
          Error
            [
              Diagnostic.error
                ~code:"E0104"
                ~span:init_te.span
                (Printf.sprintf "type mismatch in destructuring: expected '%s', found '%s'"
                   (Ty.to_string annot_ty)
                   (Ty.to_string init_te.ty));
            ]
        | _ ->
          let final_ty, final_init =
            match resolved_annot, resolved_init with
            | Some annot_ty, Some init_te -> (annot_ty, init_te)
            | None, Some init_te -> (init_te.ty, init_te)
            | Some annot_ty, None -> (annot_ty, default_expr_for_type annot_ty s.span)
            | None, None -> (Ty.Int, default_expr_for_type Ty.Int s.span)
          in
          let elem_tys_opt =
            match final_ty with
            | Ty.Tuple tys -> Some tys
            | Ty.Array (elem_ty, n) -> Some (List.init n (fun _ -> elem_ty))
            | _ -> None
          in
          (match elem_tys_opt with
           | None ->
             Error
               [
                 Diagnostic.error
                   ~hint:"destructuring requires a tuple '(a, b)' or array '[a, b]' on the right-hand side"
                   ~code:"E0104"
                   ~span:final_init.span
                   (Printf.sprintf "cannot destructure value of type '%s' (expected a tuple or array)"
                      (Ty.to_string final_ty));
               ]
           | Some elem_tys ->
             let pat_len = List.length d.names in
             let val_len = List.length elem_tys in
             if pat_len <> val_len then
               Error
                 [
                   Diagnostic.error
                     ~hint:(Printf.sprintf "provide %d binding(s) in the pattern (use '_' to ignore elements)" val_len)
                     ~code:"E0104"
                     ~span:s.span
                     (Printf.sprintf "destructuring arity mismatch: pattern has %d elements, but '%s' has %d elements"
                        pat_len (Ty.to_string final_ty) val_len);
                 ]
             else
               let bindings =
                 List.map2
                   (fun (name, name_span) elem_ty ->
                     if name = "_" then
                       { Typed_ast.name = None; name_span; ty = elem_ty }
                     else begin
                       Hashtbl.replace env name
                         { ty = elem_ty; decl_span = name_span; is_const = d.is_const; const_int = None };
                       scope_vars := name :: !scope_vars;
                       { Typed_ast.name = Some name; name_span; ty = elem_ty }
                     end)
                   d.names elem_tys
               in
               Ok
                 {
                   Typed_ast.desc =
                     Typed_ast.TStmtDestruct
                       { bindings; is_const = d.is_const; ty = final_ty; init = final_init };
                   span = s.span;
                 })))
  | Ast.StmtVarDecl v ->
    let errs = ref [] in
    (match Hashtbl.find_opt env v.name with
     | None -> ()
     | Some prev ->
       let entity = if prev.is_const then "constant" else "variable" in
       errs :=
         Diagnostic.error
           ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
           ~code:"E0103"
           ~span:v.name_span
           (Printf.sprintf "%s '%s' is already declared in this scope" entity v.name)
         :: !errs);
    if v.is_const && Option.is_none v.init then
      errs :=
        Diagnostic.error
          ~hint:(Printf.sprintf "provide a value, e.g. 'const %s := <value>;'" v.name)
          ~code:"E0106"
          ~span:v.name_span
          (Printf.sprintf "constant '%s' requires an initializing expression" v.name)
        :: !errs;
    let resolved_init =
      match v.init with
      | None -> None
      | Some init_expr ->
        (match check_expr env init_expr with
         | Ok te -> Some te
         | Error err ->
           errs := err :: !errs;
           None)
    in
    let init_ty_opt = Option.map (fun (te : Typed_ast.expr) -> te.ty) resolved_init in
    let resolved_annot =
      match v.ty_annot with
      | None -> None
      | Some annot ->
        (match resolve_type_expr annot init_ty_opt with
         | Ok ty -> Some ty
         | Error err ->
           errs := err :: !errs;
           None)
    in
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       (match resolved_annot, resolved_init with
        | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
          let entity = if v.is_const then "constant" else "variable" in
          Error
            [
              Diagnostic.error
                ~hint:
                  (Printf.sprintf
                     "Sigma is strongly typed: cannot assign value of type '%s' to %s '%s' of type '%s'"
                     (Ty.to_string init_te.ty)
                     entity
                     v.name
                     (Ty.to_string annot_ty))
                ~code:"E0104"
                ~span:init_te.span
                (Printf.sprintf "type mismatch: expected '%s', found '%s'"
                   (Ty.to_string annot_ty)
                   (Ty.to_string init_te.ty));
            ]
        | _ ->
          let final_ty, final_init, was_default_initialized =
            match resolved_annot, resolved_init with
            | Some annot_ty, Some init_te ->
              (annot_ty, init_te, false)
            | None, Some init_te ->
              (init_te.ty, init_te, false)
            | Some annot_ty, None ->
              (annot_ty, default_expr_for_type annot_ty v.name_span, true)
            | None, None ->
              (Ty.Int, default_expr_for_type Ty.Int v.name_span, true)
          in
          let const_int =
            if v.is_const && final_ty = Ty.Int then eval_const_int env final_init else None
          in
          Hashtbl.replace env v.name
            { ty = final_ty; decl_span = v.name_span; is_const = v.is_const; const_int };
          scope_vars := v.name :: !scope_vars;
          let tdecl =
            {
              Typed_ast.name = v.name;
              name_span = v.name_span;
              is_const = v.is_const;
              ty = final_ty;
              init = final_init;
              was_default_initialized;
            }
          in
          Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tdecl; span = s.span }))

and check_program (prog : Ast.program) : (Typed_ast.program, Diagnostic.t list) result =
  let env : env = Hashtbl.create 16 in
  let root_scope = ref [] in
  let next_loop_id = ref 0 in
  let tstmts = ref [] in
  let all_errors = ref [] in
  List.iter
    (fun stmt ->
      match check_stmt env root_scope [] next_loop_id stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  match List.rev !all_errors with
  | [] -> Ok (List.rev !tstmts)
  | errs -> Error errs
