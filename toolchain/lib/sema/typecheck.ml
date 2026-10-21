open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

type symbol_info = {
  ty : Ty.t;
  decl_span : Span.t;
}

type env = (string, symbol_info) Hashtbl.t

let check_unary_type (op : Ast.unop) (ty : Ty.t) : Ty.t option =
  match op, ty with
  | (Ast.UnPos | Ast.UnNeg), (Ty.Int | Ty.Float | Ty.Double | Ty.Decimal) -> Some ty
  | Ast.UnNot, (Ty.Bool | Ty.Int) -> Some ty
  | _ -> None

let check_binary_type (op : Ast.binop) (lty : Ty.t) (rty : Ty.t) : Ty.t option =
  match lty, rty, op with
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
                 })))
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
  | Ast.ExprAssign (name, name_span, op, rhs) ->
    (match Hashtbl.find_opt env name with
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;'" name name)
            ~code:"E0102"
            ~span:name_span
            (Printf.sprintf "use of undeclared variable '%s'" name))
     | Some info ->
       (match check_expr env rhs with
        | Error err -> Error err
        | Ok trhs ->
          (match binop_of_assign_op op with
           | None ->
             if Ty.equal info.ty trhs.ty then
               Ok
                 {
                   Typed_ast.desc = Typed_ast.TExprAssign (name, op, trhs);
                   ty = info.ty;
                   span = e.span;
                 }
             else
               Error
                 (Diagnostic.error
                    ~hint:
                      (Printf.sprintf
                         "Sigma is strongly typed: cannot assign value of type '%s' to variable '%s' of type '%s'"
                         (Ty.to_string trhs.ty)
                         name
                         (Ty.to_string info.ty))
                    ~code:"E0104"
                    ~span:e.span
                    (Printf.sprintf "type mismatch in assignment: variable '%s' has type '%s', found '%s'"
                       name
                       (Ty.to_string info.ty)
                       (Ty.to_string trhs.ty)))
           | Some binop ->
             let op_str = Ast.assign_op_to_string op in
             (match check_binary_type binop info.ty trhs.ty with
              | None ->
                Error
                  (Diagnostic.error
                     ~hint:
                       (Printf.sprintf "compound operator '%s' is not defined for (%s, %s)"
                          op_str
                          (Ty.to_string info.ty)
                          (Ty.to_string trhs.ty))
                     ~code:"E0104"
                     ~span:e.span
                     (Printf.sprintf "invalid compound assignment: '%s' between '%s' and '%s'"
                        op_str
                        (Ty.to_string info.ty)
                        (Ty.to_string trhs.ty)))
              | Some res_ty ->
                if not (Ty.equal res_ty info.ty) then
                  let hint =
                    if op = Ast.AssignDiv && info.ty = Ty.Int then
                      "in Sigma, '/' returns 'float'; use '//=' for integer division assignment on 'int'"
                    else
                      Printf.sprintf
                        "operation produces '%s', which cannot be stored back into variable '%s' of type '%s'"
                        (Ty.to_string res_ty)
                        name
                        (Ty.to_string info.ty)
                  in
                  Error
                    (Diagnostic.error
                       ~hint
                       ~code:"E0104"
                       ~span:e.span
                       (Printf.sprintf
                          "type mismatch in compound assignment '%s': result type '%s' does not match variable '%s' of type '%s'"
                          op_str
                          (Ty.to_string res_ty)
                          name
                          (Ty.to_string info.ty)))
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
                      Typed_ast.desc = Typed_ast.TExprAssign (name, op, trhs);
                      ty = info.ty;
                      span = e.span;
                    }))))
  | Ast.ExprIncDec (inc_op, name, name_span) ->
    (match Hashtbl.find_opt env name with
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;'" name name)
            ~code:"E0102"
            ~span:name_span
            (Printf.sprintf "use of undeclared variable '%s'" name))
     | Some info ->
       let op_str = Ast.incdec_op_to_string inc_op in
       (match info.ty with
        | Ty.Int | Ty.Float | Ty.Double | Ty.Decimal | Ty.Char ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TExprIncDec (inc_op, name);
              ty = info.ty;
              span = e.span;
            }
        | Ty.Bool | Ty.String ->
          Error
            (Diagnostic.error
               ~hint:"'++' and '--' are supported on numeric types ('int', 'float', 'double', 'decimal') and 'char'"
               ~code:"E0104"
               ~span:e.span
               (Printf.sprintf "invalid operator '%s' for variable '%s' of type '%s'"
                  op_str name (Ty.to_string info.ty)))))

let resolve_type_annot (ty_name, ty_span) : (Ty.t, Diagnostic.t) result =
  match Ty.of_string ty_name with
  | Some ty -> Ok ty
  | None ->
    Error
      (Diagnostic.error
         ~hint:"supported types: 'int', 'float', 'double', 'decimal', 'bool', 'char', 'string'"
         ~code:"E0101"
         ~span:ty_span
         (Printf.sprintf "unknown or unsupported type '%s'" ty_name))

let default_expr_for_type (ty : Ty.t) (span : Span.t) : Typed_ast.expr =
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

let rec check_stmt (env : env) (scope_vars : string list ref) (s : Ast.stmt)
    : (Typed_ast.stmt, Diagnostic.t list) result =
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
        match check_stmt env block_vars sub with
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
      match check_stmt env scope_vars then_branch with
      | Ok ts -> Some ts
      | Error sub_errs ->
        errs := List.rev_append sub_errs !errs;
        None
    in
    let telse_opt =
      match else_branch with
      | None -> Some None
      | Some eb ->
        (match check_stmt env scope_vars eb with
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
  | Ast.StmtVarDecl v ->
    let errs = ref [] in
    (match Hashtbl.find_opt env v.name with
     | None -> ()
     | Some prev ->
       errs :=
         Diagnostic.error
           ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
           ~code:"E0103"
           ~span:v.name_span
           (Printf.sprintf "variable '%s' is already declared in this scope" v.name)
         :: !errs);
    let resolved_annot =
      match v.ty_annot with
      | None -> None
      | Some annot ->
        (match resolve_type_annot annot with
         | Ok ty -> Some ty
         | Error err ->
           errs := err :: !errs;
           None)
    in
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
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       (match resolved_annot, resolved_init with
        | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
          Error
            [
              Diagnostic.error
                ~hint:
                  (Printf.sprintf
                     "Sigma is strongly typed: cannot assign value of type '%s' to variable '%s' of type '%s'"
                     (Ty.to_string init_te.ty)
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
          Hashtbl.replace env v.name { ty = final_ty; decl_span = v.name_span };
          scope_vars := v.name :: !scope_vars;
          let tdecl =
            {
              Typed_ast.name = v.name;
              name_span = v.name_span;
              ty = final_ty;
              init = final_init;
              was_default_initialized;
            }
          in
          Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tdecl; span = s.span }))

let check_program (prog : Ast.program) : (Typed_ast.program, Diagnostic.t list) result =
  let env : env = Hashtbl.create 16 in
  let root_scope = ref [] in
  let tstmts = ref [] in
  let all_errors = ref [] in
  List.iter
    (fun stmt ->
      match check_stmt env root_scope stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  match List.rev !all_errors with
  | [] -> Ok (List.rev !tstmts)
  | errs -> Error errs
