open Sigma_common
open Ast_types
open Ast_str

let format_params (params : fun_param list) : string =
  String.concat ", "
    (List.map
       (fun (p : fun_param) ->
         param_mode_prefix p.param_mode ^ p.param_name ^ ": " ^ type_expr_to_string p.param_type)
       params)

let format_label = function
  | None -> ""
  | Some (l, _) -> l ^ ": "

let rec pp_collection_item fmt = function
  | ItemExpr e -> pp_expr fmt e
  | ItemSpread (e, _) -> Format.fprintf fmt "..%a" pp_expr e

and pp_lvalue fmt = function
  | LValIdent (name, _) -> Format.fprintf fmt "%s" name
  | LValIndex (base, idx, _) -> Format.fprintf fmt "%a[%a]" pp_expr base pp_expr idx
  | LValField (base, field, _) -> Format.fprintf fmt "%a.%s" pp_expr base field

and pp_expr fmt (e : expr) =
  match e.desc with
  | ExprInt n -> Format.fprintf fmt "%Ld" n
  | ExprFloat f -> Format.fprintf fmt "%.6g" f
  | ExprDouble d -> Format.fprintf fmt "%.15gd" d
  | ExprDecimal m -> Format.fprintf fmt "%sm" (Decimal.to_string m)
  | ExprBool b -> Format.fprintf fmt "%B" b
  | ExprChar c -> Format.fprintf fmt "'%s'" c
  | ExprString s -> Format.fprintf fmt "%S" s
  | ExprInterp parts ->
    Format.fprintf fmt "\"";
    List.iter
      (function
        | InterpLit s -> Format.fprintf fmt "%s" s
        | InterpExpr (ie, _) -> Format.fprintf fmt "${%a}" pp_expr ie)
      parts;
    Format.fprintf fmt "\""
  | ExprIdent id -> Format.fprintf fmt "%s" id
  | ExprInput { prompt; mode } ->
    let fn = if Option.is_some prompt then "prompt" else "input" in
    let prompt_s =
      match prompt with
      | None -> ""
      | Some pe -> Format.asprintf "%a" pp_expr pe
    in
    let mode_s =
      match mode with
      | InputLine -> ""
      | InputTypes tys -> String.concat ", " (List.map fst tys)
      | InputMask parts ->
        let buf = Buffer.create 16 in
        Buffer.add_char buf '"';
        List.iter
          (function
            | MaskLit s -> Buffer.add_string buf s
            | MaskType (t, _) -> Buffer.add_string buf ("${" ^ t ^ "}"))
          parts;
        Buffer.add_char buf '"';
        Buffer.contents buf
    in
    let args_s =
      if prompt_s <> "" && mode_s <> "" then prompt_s ^ ", " ^ mode_s
      else if prompt_s <> "" then prompt_s
      else mode_s
    in
    Format.fprintf fmt "%s(%s)" fn args_s
  | ExprTuple items ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_collection_item)
      items
  | ExprArray items ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_collection_item)
      items
  | ExprIndex (base, idx) ->
    Format.fprintf fmt "%a[%a]" pp_expr base pp_expr idx
  | ExprField (base, field) ->
    Format.fprintf fmt "%a.%s" pp_expr base field
  | ExprStructInit (tname, args) ->
    let pp_arg fmt (a : call_arg) =
      match a.arg_name with
      | None -> pp_expr fmt a.arg_value
      | Some (n, _) -> Format.fprintf fmt "%s = %a" n pp_expr a.arg_value
    in
    Format.fprintf fmt "%s { %a }" tname
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_arg)
      args
  | ExprNew { new_name; new_is_ctor; new_args } ->
    let pp_arg fmt (a : call_arg) =
      match a.arg_name with
      | None -> pp_expr fmt a.arg_value
      | Some (n, _) -> Format.fprintf fmt "%s = %a" n pp_expr a.arg_value
    in
    let open_c, close_c = if new_is_ctor then ("(", ")") else (" { ", " }") in
    Format.fprintf fmt "new %s%s%a%s" new_name open_c
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_arg)
      new_args close_c
  | ExprCall (callee, args) ->
    let pp_arg fmt (a : call_arg) =
      let prefix =
        match a.arg_name with
        | None -> ""
        | Some (n, _) -> n ^ "="
      in
      let mode_prefix =
        match a.arg_mode with
        | ArgVal -> ""
        | ArgRef -> "ref "
        | ArgOut -> "out "
        | ArgOutVar { var_name; var_type = None; _ } -> Printf.sprintf "out var %s " var_name
        | ArgOutVar { var_name; var_type = Some te; _ } ->
          Printf.sprintf "out var %s: %s " var_name (type_expr_to_string te)
      in
      match a.arg_mode with
      | ArgOutVar _ -> Format.fprintf fmt "%s%s" prefix (String.trim mode_prefix)
      | _ -> Format.fprintf fmt "%s%s%a" prefix mode_prefix pp_expr a.arg_value
    in
    Format.fprintf fmt "%a(%a)" pp_expr callee
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_arg)
      args
  | ExprLambda lam ->
    let kw = if lam.lam_is_act then "act" else "fun" in
    let params_s = format_params lam.lam_params in
    let ret_s =
      match lam.lam_ret with
      | None -> ""
      | Some te -> ": " ^ type_expr_to_string te
    in
    Format.fprintf fmt "%s(%s)%s => { %a }" kw params_s ret_s
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
      lam.lam_body
  | ExprUnary (op, sub) ->
    Format.fprintf fmt "(%s %a)" (unop_to_string op) pp_expr sub
  | ExprBinary (op, lhs, rhs) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr lhs (binop_to_string op) pp_expr rhs
  | ExprChainCmp (e1, op1, e2, op2, e3) ->
    Format.fprintf fmt "(%a %s %a %s %a)"
      pp_expr e1 (binop_to_string op1)
      pp_expr e2 (binop_to_string op2)
      pp_expr e3
  | ExprAssign (lval, op, rhs) ->
    Format.fprintf fmt "(%a %s %a)" pp_lvalue lval (assign_op_to_string op) pp_expr rhs
  | ExprIncDec (PreInc, lval) ->
    Format.fprintf fmt "(++%a)" pp_lvalue lval
  | ExprIncDec (PreDec, lval) ->
    Format.fprintf fmt "(--%a)" pp_lvalue lval
  | ExprIncDec (PostInc, lval) ->
    Format.fprintf fmt "(%a++)" pp_lvalue lval
  | ExprIncDec (PostDec, lval) ->
    Format.fprintf fmt "(%a--)" pp_lvalue lval

and pp_stmt fmt (s : stmt) =
  match s.desc with
  | StmtVarDecl v ->
    let kw = if v.is_const then "const" else "var" in
    let ty_s =
      match v.ty_annot with
      | None -> ""
      | Some te -> ": " ^ type_expr_to_string te
    in
    (match v.init, v.ty_annot with
     | Some init, None -> Format.fprintf fmt "%s %s := %a;" kw v.name pp_expr init
     | Some init, Some _ -> Format.fprintf fmt "%s %s%s = %a;" kw v.name ty_s pp_expr init
     | None, _ -> Format.fprintf fmt "%s %s%s;" kw v.name ty_s)
  | StmtDestruct d ->
    let kw = if d.is_const then "const" else "var" in
    let names_s = String.concat ", " (List.map fst d.names) in
    let ty_s =
      match d.ty_annot with
      | None -> ""
      | Some te -> ": " ^ type_expr_to_string te
    in
    (match d.init, d.ty_annot with
     | Some init, None -> Format.fprintf fmt "%s (%s) := %a;" kw names_s pp_expr init
     | Some init, Some _ -> Format.fprintf fmt "%s (%s)%s = %a;" kw names_s ty_s pp_expr init
     | None, _ -> Format.fprintf fmt "%s (%s)%s;" kw names_s ty_s)
  | StmtBlock stmts ->
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
      stmts
  | StmtIf { cond; then_branch; else_branch = None } ->
    Format.fprintf fmt "if (%a) %a" pp_expr cond pp_stmt then_branch
  | StmtIf { cond; then_branch; else_branch = Some eb } ->
    Format.fprintf fmt "if (%a) %a else %a" pp_expr cond pp_stmt then_branch pp_stmt eb
  | StmtWhile { label; cond; body } ->
    Format.fprintf fmt "%swhile (%a) %a" (format_label label) pp_expr cond pp_stmt body
  | StmtFor { label; init; cond; step; body } ->
    let pp_for_init fmt = function
      | ForInitVarDecl v -> pp_stmt fmt { desc = StmtVarDecl v; span = s.span }
      | ForInitDestruct d -> pp_stmt fmt { desc = StmtDestruct d; span = s.span }
      | ForInitExpr e -> Format.fprintf fmt "%a;" pp_expr e
    in
    Format.fprintf fmt "%sfor (%a %a; %a) %a"
      (format_label label) pp_for_init init pp_expr cond pp_expr step pp_stmt body
  | StmtForeach { label; head; collection; body } ->
    let kw = if head.is_const then "const" else "var" in
    let bind_s =
      match head.binding with
      | ForeachVar (name, _) -> name
      | ForeachDestruct names -> "(" ^ String.concat ", " (List.map fst names) ^ ")"
    in
    let ty_s =
      match head.ty_annot with
      | None -> ""
      | Some te -> " : " ^ type_expr_to_string te
    in
    Format.fprintf fmt "%sforeach (%s %s%s in %a) %a"
      (format_label label) kw bind_s ty_s pp_expr collection pp_stmt body
  | StmtBreak None -> Format.fprintf fmt "break;"
  | StmtBreak (Some (l, _)) -> Format.fprintf fmt "break %s;" l
  | StmtContinue None -> Format.fprintf fmt "continue;"
  | StmtContinue (Some (l, _)) -> Format.fprintf fmt "continue %s;" l
  | StmtPrint { newline; args } ->
    let fn_name = if newline then "println" else "print" in
    Format.fprintf fmt "%s(%a);" fn_name
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
      args
  | StmtAssert { actual; expected; message } ->
    (match expected, message with
     | None, None -> Format.fprintf fmt "assert(%a);" pp_expr actual
     | Some exp, None -> Format.fprintf fmt "assert(%a, %a);" pp_expr actual pp_expr exp
     | Some exp, Some msg ->
       Format.fprintf fmt "assert(%a, %a, %a);" pp_expr actual pp_expr exp pp_expr msg
     | None, Some msg ->
       Format.fprintf fmt "assert(%a, %a);" pp_expr actual pp_expr msg)
  | StmtAssertError { expected_code; snippet; message; _ } ->
    (match message with
     | None -> Format.fprintf fmt "assert_error(%S, %S);" expected_code snippet
     | Some msg -> Format.fprintf fmt "assert_error(%S, %S, %a);" expected_code snippet pp_expr msg)
  | StmtFunDecl fd ->
    let params_s = format_params fd.fd_params in
    (match fd.fd_ret with
     | Some ret ->
       Format.fprintf fmt "fun %s(%s): %s { %a }" fd.fd_name params_s (type_expr_to_string ret)
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
         fd.fd_body
     | None ->
       Format.fprintf fmt "act %s(%s) { %a }" fd.fd_name params_s
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
         fd.fd_body)
  | StmtTypeDecl td ->
    let kw =
      match td.td_kind with
      | KindClass -> "class"
      | KindStruct -> "struct"
      | KindRecord -> "record"
    in
    let pp_vis fmt = function
      | None -> () | Some VisPublic -> Format.fprintf fmt "public "
      | Some VisProtected -> Format.fprintf fmt "protected "
      | Some VisPrivate -> Format.fprintf fmt "private " | Some VisInternal -> Format.fprintf fmt "internal "
    in
    let pp_acc fmt (a : accessor_spec) =
      let k = match a.acc_kind with AccGet -> "get" | AccSet -> "set" | AccInit -> "init" in
      Format.fprintf fmt "%a%s;" pp_vis a.acc_vis k
    in
    let pp_accs fmt = function
      | None -> ()
      | Some accs -> Format.fprintf fmt " { %a }" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_acc) accs
    in
    let pp_field fmt (f : field_decl) =
      match f.field_default with
      | None ->
        Format.fprintf fmt "%a%s: %s%a" pp_vis f.field_vis f.field_name
          (type_expr_to_string f.field_type) pp_accs f.field_accessors
      | Some d ->
        Format.fprintf fmt "%a%s: %s%a = %a" pp_vis f.field_vis f.field_name
          (type_expr_to_string f.field_type) pp_accs f.field_accessors pp_expr d
    in
    (match td.td_kind with
     | KindRecord ->
       Format.fprintf fmt "%a%s %s(%a) {}" pp_vis td.td_vis kw td.td_name
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_field)
         td.td_fields
     | KindClass | KindStruct ->
       Format.fprintf fmt "%a%s %s { %a }" pp_vis td.td_vis kw td.td_name
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_field)
         td.td_fields)
  | StmtMultiAssign { ma_targets; ma_rhs } ->
    let pp_t fmt = function
      | TargetDiscard _ -> Format.fprintf fmt "_"
      | TargetLVal lv -> pp_lvalue fmt lv
    in
    Format.fprintf fmt "(%a) = %a;"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_t)
      ma_targets pp_expr ma_rhs
  | StmtReturn None -> Format.fprintf fmt "return;"
  | StmtReturn (Some e) -> Format.fprintf fmt "return %a;" pp_expr e
  | StmtExpr e -> Format.fprintf fmt "%a;" pp_expr e

let pp_program fmt prog =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
    pp_stmt fmt prog
