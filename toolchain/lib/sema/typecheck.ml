open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

type const_kind =
  | NotConst
  | DeepConst
  | ShallowConst

type symbol_info = {
  ty : Ty.t;
  decl_span : Span.t;
  is_const : bool;
  const_kind : const_kind;
  is_ref_param : bool;
  const_int : int64 option;
}

type env = (string, symbol_info) Hashtbl.t

(* Contexto de função durante a checagem: retorno esperado da função atual
   ([Some Ty.Void] em [act]), nomes de escopo externo que podem ser
   escondidos (shadowing) por parâmetros/locais e o escopo raiz do programa. *)
let current_fun : Ty.t option ref = ref None
let fn_outer : (string, unit) Hashtbl.t option ref = ref None
let root_scope_ref : string list ref option ref = ref None
let current_scope_vars : string list ref option ref = ref None

(* Nomes dos parâmetros de cada função declarada ou valor parcial (chave: span ou identificador), usados
   para resolver argumentos nomeados em chamadas. *)
let fun_param_names : (Span.t, string list) Hashtbl.t = Hashtbl.create 16
let var_param_names : (string, string list) Hashtbl.t = Hashtbl.create 16
let expr_param_names : (Span.t, string list) Hashtbl.t = Hashtbl.create 16

(* Conflito de redeclaração no mesmo escopo; parâmetros e locais de funções
   podem esconder nomes declarados fora do corpo da função. *)
let find_conflict (env : env) (name : string) : symbol_info option =
  match Hashtbl.find_opt env name with
  | None -> None
  | Some prev ->
    (match !fn_outer with
     | Some outer when Hashtbl.mem outer name ->
       Hashtbl.remove outer name;
       None
     | _ -> Some prev)

let rec ty_contains_fun (ty : Ty.t) : bool =
  match ty with
  | Ty.Fun _ -> true
  | Ty.Tuple tys -> List.exists ty_contains_fun tys
  | Ty.Array (t, _) -> ty_contains_fun t
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
         ~code:"E0104"
         ~span:spec.span
         (Printf.sprintf "format flag '+' cannot be applied to value of type '%s'"
            (Ty.to_string ty)))
  else if spec.zero_pad && not is_numeric then
    Error
      (Diagnostic.error
         ~hint:"zero-padding ('0width') is only valid for numeric types; use explicit fill and alignment like '${s:0>5}' for non-numeric types"
         ~code:"E0104"
         ~span:spec.span
         (Printf.sprintf "zero-padding format '0' cannot be applied to value of type '%s'"
            (Ty.to_string ty)))
  else if spec.precision <> None && not supports_prec then
    Error
      (Diagnostic.error
         ~hint:"precision '.N' is only supported on 'float', 'double', 'decimal' (decimal places) and 'string' (max characters)"
         ~code:"E0104"
         ~span:spec.span
         (Printf.sprintf "precision specifier cannot be applied to value of type '%s'"
            (Ty.to_string ty)))
  else Ok ()

let resolve_input_type (ty_name : string) (sp : Span.t) : (Ty.t, Diagnostic.t) result =
  match Ty.of_string ty_name with
  | Some ty -> Ok ty
  | None ->
    Error
      (Diagnostic.error
         ~hint:"supported input types are 'int', 'float', 'double', 'decimal', 'bool', 'char', and 'string'"
         ~code:"E0101"
         ~span:sp
         (Printf.sprintf "unknown or unsupported input type '%s'" ty_name))

let rec resolve_type_expr (te : Ast.type_expr) (init_ty_opt : Ty.t option)
    : (Ty.t, Diagnostic.t) result =
  match te with
  | Ast.TypeNamed (ty_name, ty_span) ->
    (match Ty.of_string ty_name with
     | Some ty -> Ok ty
     | None when ty_name = "void" ->
       Error
         (Diagnostic.error
            ~hint:"'void' only appears as the return type of a function type, e.g. 'void(int)' for an 'act'"
            ~code:"E0113"
            ~span:ty_span
            "'void' cannot be used as the type of a value")
     | None ->
       Error
         (Diagnostic.error
            ~hint:"supported types: 'int', 'float', 'double', 'decimal', 'bool', 'char', 'string', tuples '(T1, T2)', or arrays 'T[N]'"
            ~code:"E0101"
            ~span:ty_span
            (Printf.sprintf "unknown or unsupported type '%s'" ty_name)))
  | Ast.TypeFun (ret_te, param_tes, _) ->
    let ret_res =
      match ret_te with
      | Ast.TypeNamed ("void", _) -> Ok Ty.Void
      | _ -> resolve_type_expr ret_te None
    in
    (match ret_res with
     | Error err -> Error err
     | Ok ret_ty ->
       let rec params acc = function
         | [] -> Ok (List.rev acc)
         | (pmode, pte) :: rest ->
           let ty_pmode =
             match pmode with
             | Ast.ParamVal -> Ty.ParamVal
             | Ast.ParamRef -> Ty.ParamRef
             | Ast.ParamOut -> Ty.ParamOut
           in
           (match resolve_type_expr pte None with
            | Error err -> Error err
            | Ok pty -> params ({ Ty.pty; pmode = ty_pmode } :: acc) rest)
       in
       (match params [] param_tes with
        | Error err -> Error err
        | Ok ptys -> Ok (Ty.Fun (ptys, ret_ty))))
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
  | Ty.Fun _ | Ty.Void ->
    (* Inalcançável: declarações sem inicializador de tipo função são rejeitadas (E0106). *)
    { Typed_ast.desc = Typed_ast.TExprInt 0L; ty = Ty.Int; span }

let rec check_expr (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
  match check_expr_any env e with
  | Ok (te : Typed_ast.expr) when te.ty = Ty.Void ->
    Error
      (Diagnostic.error
         ~hint:"'act' procedures do not produce a value; call them as a standalone statement, e.g. 'my_act(...);'"
         ~code:"E0113"
         ~span:e.span
         "the result of an 'act' call cannot be used as a value")
  | r -> r

and check_expr_any (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
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
      | Ast.InterpExpr (ie, fmt_opt) :: rest ->
        (match check_expr env ie with
         | Error err -> Error err
         | Ok tie ->
           (match fmt_opt with
            | None -> check_parts (Typed_ast.TInterpExpr (tie, None) :: acc) rest
            | Some spec ->
              (match validate_format_spec spec tie.ty with
               | Error err -> Error err
               | Ok () ->
                 check_parts (Typed_ast.TInterpExpr (tie, Some spec) :: acc) rest)))
    in
    (match check_parts [] parts with
     | Ok tparts ->
       Ok { Typed_ast.desc = Typed_ast.TExprInterp tparts; ty = Ty.String; span = e.span }
     | Error err -> Error err)
  | Ast.ExprInput { prompt; mode } ->
    let prompt_res =
      match prompt with
      | None -> Ok None
      | Some ({ Ast.desc = Ast.ExprIdent name; span = p_span } as p_expr) ->
        if Hashtbl.find_opt env name = None && Ty.of_string name <> None then
          Error
            (Diagnostic.error
               ~hint:(Printf.sprintf "use 'input(%s)' to read without a prompt, or 'prompt(\"message: \", %s)' to display a prompt first" name name)
               ~code:"E0104"
               ~span:p_span
               (Printf.sprintf "first argument of 'prompt(...)' must be a prompt message of type 'string', found type name '%s'" name))
        else
          (match check_expr env p_expr with
           | Error err -> Error err
           | Ok tp ->
             if tp.ty = Ty.String then Ok (Some tp)
             else
               Error
                 (Diagnostic.error
                    ~hint:"the first argument to 'prompt(...)' must be a 'string' expression"
                    ~code:"E0104"
                    ~span:tp.span
                    (Printf.sprintf "prompt message must be of type 'string', found '%s'"
                       (Ty.to_string tp.ty))))
      | Some p_expr ->
        (match check_expr env p_expr with
         | Error err -> Error err
         | Ok tp ->
           if tp.ty = Ty.String then Ok (Some tp)
           else
             Error
               (Diagnostic.error
                  ~hint:"the first argument to 'prompt(...)' must be a 'string' expression"
                  ~code:"E0104"
                  ~span:tp.span
                  (Printf.sprintf "prompt message must be of type 'string', found '%s'"
                     (Ty.to_string tp.ty))))
    in
    (match prompt_res with
     | Error err -> Error err
     | Ok tprompt ->
       let mode_res =
         match mode with
         | Ast.InputLine -> Ok (Typed_ast.TInputLine, Ty.String)
         | Ast.InputTypes ty_list ->
           let rec resolve_all acc = function
             | [] ->
               let tys = List.rev acc in
               let res_ty =
                 match tys with
                 | [ single ] -> single
                 | _ -> Ty.Tuple tys
               in
               Ok (Typed_ast.TInputTypes tys, res_ty)
             | (ty_name, sp) :: rest ->
               (match resolve_input_type ty_name sp with
                | Error err -> Error err
                | Ok ty -> resolve_all (ty :: acc) rest)
           in
           resolve_all [] ty_list
         | Ast.InputMask parts ->
           let rec check_mask acc_parts acc_tys prev_was_type = function
             | [] ->
               let tparts = List.rev acc_parts in
               let tys = List.rev acc_tys in
               let res_ty =
                 match tys with
                 | [ single ] -> single
                 | _ -> Ty.Tuple tys
               in
               Ok (Typed_ast.TInputMask tparts, res_ty)
             | Ast.MaskLit s :: rest ->
               check_mask (Typed_ast.TMaskLit s :: acc_parts) acc_tys false rest
             | Ast.MaskType (ty_name, sp) :: rest ->
               if prev_was_type then
                 Error
                   (Diagnostic.error
                      ~hint:"place a delimiter between placeholders, e.g. '\"${int} ${int}\"' or '\"${int}-${int}\"'"
                      ~code:"E0002"
                      ~span:sp
                      "adjacent type placeholders in input mask require a literal separator")
               else
                 (match resolve_input_type ty_name sp with
                  | Error err -> Error err
                  | Ok ty ->
                    check_mask
                      (Typed_ast.TMaskType ty :: acc_parts)
                      (ty :: acc_tys)
                      true
                      rest)
           in
           check_mask [] [] false parts
       in
       (match mode_res with
        | Error err -> Error err
        | Ok (tmode, res_ty) ->
          Ok
            {
              Typed_ast.desc = Typed_ast.TExprInput { prompt = tprompt; mode = tmode };
              ty = res_ty;
              span = e.span;
            }))
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
  | Ast.ExprCall (callee, args) ->
    (match check_expr env callee with
     | Error err -> Error err
     | Ok tcallee ->
       (match tcallee.ty with
        | Ty.Fun (ptys, ret) -> check_call env e tcallee ptys ret callee args
        | other ->
          Error
            (Diagnostic.error
               ~hint:"only values of function type ('fun'/'act') can be called"
               ~code:"E0112"
               ~span:callee.span
               (Printf.sprintf "cannot call a value of type '%s'" (Ty.to_string other)))))
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
        | Ty.Bool | Ty.String | Ty.Tuple _ | Ty.Array _ | Ty.Fun _ | Ty.Void ->
          Error
            (Diagnostic.error
               ~hint:"'++' and '--' are supported on numeric types ('int', 'float', 'double', 'decimal') and 'char'"
               ~code:"E0104"
               ~span:e.span
               (Printf.sprintf "invalid operator '%s' for %s of type '%s'"
                  op_str lval_label (Ty.to_string lval_ty)))))

and check_call (env : env) (e : Ast.expr) (tcallee : Typed_ast.expr) (ptys : Ty.fun_param list)
    (ret : Ty.t) (callee : Ast.expr) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  let n = List.length ptys in
  let ptys_a = Array.of_list ptys in
  let names_opt =
    match callee.desc with
    | Ast.ExprIdent name ->
      (match Hashtbl.find_opt var_param_names name with
       | Some pnames -> Some pnames
       | None ->
         (match Hashtbl.find_opt env name with
          | Some info -> Hashtbl.find_opt fun_param_names info.decl_span
          | None -> None))
    | _ -> Hashtbl.find_opt expr_param_names callee.span
  in
  let used = Array.make (max n 1) false in
  let next_pos = ref 0 in
  let seen_named = ref false in
  let call_err ~hint span msg = Error (Diagnostic.error ~hint ~code:"E0110" ~span msg) in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (a : Ast.call_arg) :: rest ->
      let idx_res =
        match a.arg_name with
        | None ->
          if !seen_named then
            call_err ~hint:"pass positional arguments first, then named ones, e.g. 'f(1, b=2)'"
              a.arg_value.span "positional argument after a named argument"
          else begin
            let i = !next_pos in
            incr next_pos;
            if i >= n then
              call_err
                ~hint:(Printf.sprintf "the callee accepts at most %d more argument(s)" n)
                a.arg_value.span
                (Printf.sprintf "too many arguments: expected at most %d, found more" n)
            else Ok i
          end
        | Some (nm, nsp) ->
          seen_named := true;
          (match names_opt with
           | None ->
             call_err
               ~hint:"named arguments are only available when calling a declared 'fun'/'act' directly by its name"
               nsp
               (Printf.sprintf "cannot use named argument '%s' here" nm)
           | Some names ->
             let rec find_idx i = function
               | [] -> None
               | x :: tl -> if x = nm then Some i else find_idx (i + 1) tl
             in
             (match find_idx 0 names with
              | None ->
                call_err
                  ~hint:(Printf.sprintf "parameters are: %s" (String.concat ", " names))
                  nsp
                  (Printf.sprintf "unknown parameter name '%s'" nm)
              | Some i -> Ok i))
      in
      (match idx_res with
       | Error err -> Error err
       | Ok i ->
         if used.(i) then
           call_err ~hint:"each parameter can be bound only once" a.arg_value.span
             (Printf.sprintf "parameter #%d is bound more than once" (i + 1))
         else begin
           used.(i) <- true;
           let expected_param = ptys_a.(i) in
           let param_name =
             match names_opt with
             | Some names when i < List.length names -> List.nth names i
             | _ -> Printf.sprintf "#%d" (i + 1)
           in
           let mode_check =
             match expected_param.pmode, a.arg_mode with
             | Ty.ParamVal, Ast.ArgVal -> Ok ()
             | Ty.ParamVal, Ast.ArgRef ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is not marked 'ref'; remove 'ref'" param_name))
             | Ty.ParamVal, (Ast.ArgOut | Ast.ArgOutVar _) ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is not marked 'out'; remove 'out'" param_name))
             | Ty.ParamRef, Ast.ArgRef -> Ok ()
             | Ty.ParamRef, Ast.ArgVal ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is marked 'ref'; argument must be passed with 'ref'" param_name))
             | Ty.ParamRef, (Ast.ArgOut | Ast.ArgOutVar _) ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is marked 'ref', but argument was passed with 'out'" param_name))
             | Ty.ParamOut, (Ast.ArgOut | Ast.ArgOutVar _) -> Ok ()
             | Ty.ParamOut, Ast.ArgVal ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is marked 'out'; argument must be passed with 'out'" param_name))
             | Ty.ParamOut, Ast.ArgRef ->
               Error
                 (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                    (Printf.sprintf "parameter '%s' is marked 'out', but argument was passed with 'ref'" param_name))
           in
           match mode_check with
           | Error err -> Error err
           | Ok () ->
             match a.arg_mode with
             | Ast.ArgOutVar { var_name; var_span; var_type } ->
               let conflict =
                 match find_conflict env var_name with
                 | Some prev -> Some prev
                 | None ->
                   (match !current_scope_vars with
                    | Some sc when List.mem var_name !sc ->
                      Hashtbl.find_opt env var_name
                    | _ -> None)
               in
               (match conflict with
                | Some prev ->
                  Error
                    (Diagnostic.error
                       ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                       ~code:"E0103"
                       ~span:var_span
                       (Printf.sprintf "'%s' is already declared in this scope" var_name))
                | None ->
                  let type_check =
                    match var_type with
                    | None -> Ok ()
                    | Some te ->
                      (match resolve_type_expr te None with
                       | Error err -> Error err
                       | Ok annot_ty ->
                         if Ty.equal annot_ty expected_param.pty then Ok ()
                         else
                           Error
                             (Diagnostic.error
                                ~hint:"explicit type annotation on 'out var' must match the parameter type"
                                ~code:"E0104"
                                ~span:(Ast.type_expr_span te)
                                (Printf.sprintf "type mismatch for 'out var %s': parameter expects '%s', found '%s'"
                                   var_name (Ty.to_string expected_param.pty) (Ty.to_string annot_ty))))
                  in
                  (match type_check with
                   | Error err -> Error err
                   | Ok () ->
                     Hashtbl.replace env var_name
                       {
                         ty = expected_param.pty;
                         decl_span = var_span;
                         is_const = false;
                         const_kind = NotConst;
                         is_ref_param = false;
                         const_int = None;
                       };
                     (match !current_scope_vars with
                      | Some sc -> sc := var_name :: !sc
                      | None -> ());
                     let targ =
                       {
                         Typed_ast.desc = Typed_ast.TExprIdent var_name;
                         ty = expected_param.pty;
                         span = var_span;
                       }
                     in
                     let tca =
                       {
                         Typed_ast.tca_idx = i;
                         tca_expr = targ;
                         tca_mode = expected_param.pmode;
                         tca_is_out_var = true;
                       }
                     in
                     loop (tca :: acc) rest))
             | Ast.ArgRef | Ast.ArgOut ->
               (match a.arg_value.desc with
                | Ast.ExprIdent vname ->
                  (match Hashtbl.find_opt env vname with
                   | None ->
                     Error
                       (Diagnostic.error ~code:"E0101" ~span:a.arg_value.span
                          (Printf.sprintf "unknown variable '%s'" vname))
                   | Some info when info.is_const ->
                     Error
                       (Diagnostic.error ~code:"E0115" ~span:a.arg_value.span
                          (Printf.sprintf "cannot pass constant '%s' as '%s' argument" vname
                             (if a.arg_mode = Ast.ArgRef then "ref" else "out")))
                   | Some info ->
                     if not (Ty.equal info.ty expected_param.pty) then
                       Error
                         (Diagnostic.error
                            ~hint:"Sigma is strongly typed: arguments must have exactly the parameter type"
                            ~code:"E0104"
                            ~span:a.arg_value.span
                            (Printf.sprintf "type mismatch in argument #%d: expected '%s', found '%s'"
                               (i + 1) (Ty.to_string expected_param.pty) (Ty.to_string info.ty)))
                     else
                       let targ =
                         {
                           Typed_ast.desc = Typed_ast.TExprIdent vname;
                           ty = info.ty;
                           span = a.arg_value.span;
                         }
                       in
                       let tca =
                         {
                           Typed_ast.tca_idx = i;
                           tca_expr = targ;
                           tca_mode = expected_param.pmode;
                           tca_is_out_var = false;
                         }
                       in
                       loop (tca :: acc) rest)
                | _ ->
                  Error
                    (Diagnostic.error ~code:"E0115" ~span:a.arg_value.span
                       (Printf.sprintf "argument of '%s' must be a variable"
                          (if a.arg_mode = Ast.ArgRef then "ref" else "out"))))
             | Ast.ArgVal ->
               (match check_expr env a.arg_value with
                | Error err -> Error err
                | Ok targ ->
                  if Ty.equal targ.ty expected_param.pty then
                    let tca =
                      {
                        Typed_ast.tca_idx = i;
                        tca_expr = targ;
                        tca_mode = expected_param.pmode;
                        tca_is_out_var = false;
                      }
                    in
                    loop (tca :: acc) rest
                  else
                    Error
                      (Diagnostic.error
                         ~hint:"Sigma is strongly typed: arguments must have exactly the parameter type"
                         ~code:"E0104"
                         ~span:targ.span
                         (Printf.sprintf "type mismatch in argument #%d: expected '%s', found '%s'"
                            (i + 1) (Ty.to_string expected_param.pty) (Ty.to_string targ.ty))))
         end)
  in
  match loop [] args with
  | Error err -> Error err
  | Ok targs ->
    let remaining = List.filteri (fun i _ -> not used.(i)) ptys in
    let remaining_names =
      match names_opt with
      | Some names when List.length names = n ->
        Some (List.filteri (fun i _ -> not used.(i)) names)
      | _ -> None
    in
    Option.iter (Hashtbl.replace expr_param_names e.span) remaining_names;
    let full = remaining = [] in
    let ty = if full then ret else Ty.Fun (remaining, ret) in
    Ok
      {
        Typed_ast.desc = Typed_ast.TExprCall { callee = tcallee; args = targs; full };
        ty;
        span = e.span;
      }

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
       if info.const_kind = ShallowConst then
         let msg =
           if for_op = "++" || for_op = "--" then
             Printf.sprintf "cannot mutate parameter '%s' with '%s'; parameters are immutable by default (use 'ref %s' to allow mutation)" name for_op name
           else
             Printf.sprintf "cannot reassign parameter '%s'; parameters are immutable by default (use 'ref %s' to allow reassignment)" name name
         in
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "use 'ref %s' in the function signature to allow modifying the caller's variable" name)
              ~code:"E0107"
              ~span:name_span
              msg)
       else if info.const_kind = DeepConst then
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
         | Some info when info.const_kind = DeepConst ->
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

let rec stmt_always_returns (s : Typed_ast.stmt) : bool =
  match s.desc with
  | Typed_ast.TStmtReturn _ -> true
  | Typed_ast.TStmtBlock ss -> List.exists stmt_always_returns ss
  | Typed_ast.TStmtIf { then_branch; else_branch = Some eb; _ } ->
    stmt_always_returns then_branch && stmt_always_returns eb
  | _ -> false

let resolve_fun_signature (fd : Ast.fun_decl)
    : (Typed_ast.typed_param list * Ty.t, Diagnostic.t list) result =
  let errs = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let params =
    List.filter_map
      (fun (p : Ast.fun_param) ->
        let pname = p.param_name in
        let psp = p.param_span in
        if Hashtbl.mem seen pname then
          errs :=
            Diagnostic.error
              ~code:"E0103"
              ~span:psp
              (Printf.sprintf "duplicate parameter '%s'" pname)
            :: !errs;
        Hashtbl.replace seen pname ();
        let pmode =
          match p.param_mode with
          | Ast.ParamVal -> Ty.ParamVal
          | Ast.ParamRef -> Ty.ParamRef
          | Ast.ParamOut -> Ty.ParamOut
        in
        match resolve_type_expr p.param_type None with
        | Ok ty -> Some { Typed_ast.tp_name = pname; tp_mode = pmode; tp_ty = ty }
        | Error err ->
          errs := err :: !errs;
          None)
      fd.fd_params
  in
  let ret =
    match fd.fd_ret with
    | None -> Some Ty.Void
    | Some (Ast.TypeNamed ("void", sp)) ->
      errs :=
        Diagnostic.error
          ~hint:"use 'act' to declare a procedure without a return value"
          ~code:"E0113"
          ~span:sp
          "a 'fun' must declare a value return type, not 'void'"
        :: !errs;
      None
    | Some te ->
      (match resolve_type_expr te None with
       | Ok ty -> Some ty
       | Error err ->
         errs := err :: !errs;
         None)
  in
  match List.rev !errs, ret with
  | [], Some r -> Ok (params, r)
  | es, _ -> Error es

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

let rec collect_used_expr (used : (string, unit) Hashtbl.t) (e : Typed_ast.expr) : unit =
  match e.desc with
  | Typed_ast.TExprIdent id -> Hashtbl.replace used id ()
  | Typed_ast.TExprUnary (_, sub) -> collect_used_expr used sub
  | Typed_ast.TExprBinary (_, l, r) ->
    collect_used_expr used l;
    collect_used_expr used r
  | Typed_ast.TExprChainCmp (a, _, b, _, c) ->
    collect_used_expr used a;
    collect_used_expr used b;
    collect_used_expr used c
  | Typed_ast.TExprAssign (lval, _, r) ->
    (match lval with
     | Typed_ast.TLValIdent id -> Hashtbl.replace used id ()
     | Typed_ast.TLValIndex (arr, idx) ->
       collect_used_expr used arr;
       collect_used_expr used idx);
    collect_used_expr used r
  | Typed_ast.TExprIncDec (_, lval) ->
    (match lval with
     | Typed_ast.TLValIdent id -> Hashtbl.replace used id ()
     | Typed_ast.TLValIndex (arr, idx) ->
       collect_used_expr used arr;
       collect_used_expr used idx)
  | Typed_ast.TExprCall { callee; args; _ } ->
    collect_used_expr used callee;
    List.iter (fun (arg : Typed_ast.typed_call_arg) -> collect_used_expr used arg.tca_expr) args
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.iter
      (function
        | Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub ->
          collect_used_expr used sub)
      items
  | Typed_ast.TExprIndex (sub, idx) ->
    collect_used_expr used sub;
    collect_used_expr used idx
  | Typed_ast.TExprInterp parts ->
    List.iter
      (function
        | Typed_ast.TInterpLit _ -> ()
        | Typed_ast.TInterpExpr (sub, _) -> collect_used_expr used sub)
      parts
  | Typed_ast.TExprInput { prompt; _ } ->
    Option.iter (collect_used_expr used) prompt
  | Typed_ast.TExprInt _ | Typed_ast.TExprFloat _ | Typed_ast.TExprDouble _
  | Typed_ast.TExprDecimal _ | Typed_ast.TExprBool _ | Typed_ast.TExprChar _
  | Typed_ast.TExprString _ -> ()

let rec collect_used_stmt
    (declared : (string, unit) Hashtbl.t)
    (used : (string, unit) Hashtbl.t)
    (s : Typed_ast.stmt) : unit =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    collect_used_expr used v.init;
    Hashtbl.replace declared v.name ()
  | Typed_ast.TStmtDestruct d ->
    collect_used_expr used d.init;
    List.iter
      (fun (b : Typed_ast.destruct_binding) ->
        Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
      d.bindings
  | Typed_ast.TStmtFunDecl inner_fd ->
    Hashtbl.replace declared inner_fd.tf_name ();
    List.iter (fun (cap_name, _) -> Hashtbl.replace used cap_name ()) inner_fd.tf_captures
  | Typed_ast.TStmtBlock stmts ->
    List.iter (collect_used_stmt declared used) stmts
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    collect_used_expr used cond;
    collect_used_stmt declared used then_branch;
    Option.iter (collect_used_stmt declared used) else_branch
  | Typed_ast.TStmtWhile { cond; body; _ } ->
    collect_used_expr used cond;
    collect_used_stmt declared used body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    (match init with
     | Typed_ast.TForInitVarDecl v ->
       collect_used_expr used v.init;
       Hashtbl.replace declared v.name ()
     | Typed_ast.TForInitDestruct d ->
       collect_used_expr used d.init;
       List.iter
         (fun (b : Typed_ast.destruct_binding) ->
           Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
         d.bindings
     | Typed_ast.TForInitExpr e -> collect_used_expr used e);
    collect_used_expr used cond;
    collect_used_expr used step;
    collect_used_stmt declared used body
  | Typed_ast.TStmtForeach { head; collection; body; _ } ->
    collect_used_expr used collection;
    (match head with
     | Typed_ast.TForeachVar { name; _ } ->
       Option.iter (fun n -> Hashtbl.replace declared n ()) name
     | Typed_ast.TForeachDestruct { bindings; _ } ->
       List.iter
         (fun (b : Typed_ast.destruct_binding) ->
           Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
         bindings);
    collect_used_stmt declared used body
  | Typed_ast.TStmtPrint { args; _ } ->
    List.iter (collect_used_expr used) args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    collect_used_expr used actual;
    collect_used_expr used expected;
    Option.iter (collect_used_expr used) message
  | Typed_ast.TStmtAssertError { message; _ } ->
    Option.iter (collect_used_expr used) message
  | Typed_ast.TStmtReturn eo ->
    Option.iter (collect_used_expr used) eo
  | Typed_ast.TStmtExpr e ->
    collect_used_expr used e
  | Typed_ast.TStmtBreak _ | Typed_ast.TStmtContinue _ -> ()

let compute_captures
    (env : env)
    (params : Typed_ast.typed_param list)
    (self_name : string)
    (body : Typed_ast.stmt list) : ((string * Ty.t) list, Diagnostic.t list) result =
  let declared : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let used : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (collect_used_stmt declared used) body;
  let is_param (name : string) : bool =
    List.exists (fun (tp : Typed_ast.typed_param) -> tp.tp_name = name) params
  in
  let is_global (name : string) : bool =
    match !root_scope_ref with
    | Some r -> List.mem name !r
    | None -> false
  in
  let caps = ref [] in
  let cap_errs = ref [] in
  Hashtbl.iter
    (fun name () ->
      if name <> self_name
         && not (is_param name)
         && not (Hashtbl.mem declared name)
         && not (is_global name)
      then
        match Hashtbl.find_opt env name with
        | Some info ->
          if info.is_ref_param then
            cap_errs :=
              Diagnostic.error
                ~code:"E0118"
                ~span:info.decl_span
                (Printf.sprintf "cannot capture 'ref' or 'out' parameter '%s' in a closure" name)
              :: !cap_errs
          else
            caps := (name, info.ty) :: !caps
        | None -> ())
    used;
  if !cap_errs <> [] then Error (List.rev !cap_errs)
  else Ok (List.sort (fun (a, _) (b, _) -> String.compare a b) !caps)

module OutCheck = struct
  module SSet = Set.Make(String)

  let rec assigned_in_expr (e : Typed_ast.expr) (acc : SSet.t) : SSet.t =
    match e.desc with
    | Typed_ast.TExprAssign (Typed_ast.TLValIdent name, _, rhs) ->
      let acc' = assigned_in_expr rhs acc in
      SSet.add name acc'
    | Typed_ast.TExprAssign (Typed_ast.TLValIndex (base, idx), _, rhs) ->
      let acc1 = assigned_in_expr base acc in
      let acc2 = assigned_in_expr idx acc1 in
      assigned_in_expr rhs acc2
    | Typed_ast.TExprCall { args; _ } ->
      List.fold_left
        (fun s (arg : Typed_ast.typed_call_arg) ->
          let s' =
            match arg.tca_mode, arg.tca_expr.desc with
            | Ty.ParamOut, Typed_ast.TExprIdent nm -> SSet.add nm s
            | _ -> s
          in
          assigned_in_expr arg.tca_expr s')
        acc args
    | Typed_ast.TExprUnary (_, sub) -> assigned_in_expr sub acc
    | Typed_ast.TExprBinary (_, lhs, rhs) ->
      assigned_in_expr rhs (assigned_in_expr lhs acc)
    | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
      assigned_in_expr e3 (assigned_in_expr e2 (assigned_in_expr e1 acc))
    | Typed_ast.TExprIncDec (_, Typed_ast.TLValIdent name) ->
      SSet.add name acc
    | Typed_ast.TExprIncDec (_, Typed_ast.TLValIndex (base, idx)) ->
      assigned_in_expr idx (assigned_in_expr base acc)
    | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
      List.fold_left
        (fun s item ->
          match item with
          | Typed_ast.TItemExpr sub -> assigned_in_expr sub s
          | Typed_ast.TItemSpread sub -> assigned_in_expr sub s)
        acc items
    | Typed_ast.TExprIndex (base, idx) ->
      assigned_in_expr idx (assigned_in_expr base acc)
    | Typed_ast.TExprInterp parts ->
      List.fold_left
        (fun s part ->
          match part with
          | Typed_ast.TInterpLit _ -> s
          | Typed_ast.TInterpExpr (sub, _) -> assigned_in_expr sub s)
        acc parts
    | Typed_ast.TExprInput { prompt = Some p; _ } -> assigned_in_expr p acc
    | _ -> acc

  let check_fun (fn_name : string) (fn_span : Span.t) (params : Typed_ast.typed_param list) (body : Typed_ast.stmt list) : Diagnostic.t list =
    let required =
      List.filter_map
        (fun (p : Typed_ast.typed_param) ->
          if p.tp_mode = Ty.ParamOut then Some p.tp_name else None)
        params
    in
    if required = [] then []
    else
      let errors = ref [] in
      let check_assigned span current =
        List.iter
          (fun p ->
            if not (SSet.mem p current) then
              errors :=
                Diagnostic.error
                  ~code:"E0117"
                  ~span
                  (Printf.sprintf "out parameter '%s' must be assigned before returning from function '%s'" p fn_name)
                :: !errors)
          required
      in
      let rec check_stmts (stmts : Typed_ast.stmt list) (curr : SSet.t) : SSet.t =
        List.fold_left check_stmt_flow curr stmts
      and check_stmt_flow (curr : SSet.t) (s : Typed_ast.stmt) : SSet.t =
        match s.desc with
        | Typed_ast.TStmtReturn _ ->
          check_assigned s.span curr;
          curr
        | Typed_ast.TStmtExpr e ->
          assigned_in_expr e curr
        | Typed_ast.TStmtBlock ss ->
          check_stmts ss curr
        | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
          let curr_cond = assigned_in_expr cond curr in
          let then_out = check_stmt_flow curr_cond then_branch in
          (match else_branch with
           | Some eb ->
             let else_out = check_stmt_flow curr_cond eb in
             if stmt_always_returns then_branch && stmt_always_returns eb then
               curr_cond
             else if stmt_always_returns then_branch then
               else_out
             else if stmt_always_returns eb then
               then_out
             else
               SSet.inter then_out else_out
           | None ->
             curr_cond)
        | Typed_ast.TStmtWhile { cond; body; _ } ->
          let curr_cond = assigned_in_expr cond curr in
          ignore (check_stmt_flow curr_cond body);
          curr_cond
        | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
          let curr_init =
            match init with
            | Typed_ast.TForInitExpr e -> assigned_in_expr e curr
            | _ -> curr
          in
          let curr_cond = assigned_in_expr cond curr_init in
          ignore (check_stmt_flow curr_cond body);
          ignore (assigned_in_expr step curr_cond);
          curr_cond
        | Typed_ast.TStmtForeach { collection; body; _ } ->
          let curr_coll = assigned_in_expr collection curr in
          ignore (check_stmt_flow curr_coll body);
          curr_coll
        | _ -> curr
      in
      let final_assigned = check_stmts body SSet.empty in
      if not (List.exists stmt_always_returns body) then begin
        List.iter
          (fun p ->
            if not (SSet.mem p final_assigned) then
              errors :=
                Diagnostic.error
                  ~code:"E0117"
                  ~span:fn_span
                  (Printf.sprintf "out parameter '%s' must be assigned before end of function '%s'" p fn_name)
                :: !errors)
          required
      end;
      List.rev !errors
end

let rec check_stmt
    (env : env)
    (scope_vars : string list ref)
    (loop_stack : active_loop list)
    (next_loop_id : int ref)
    (s : Ast.stmt) : (Typed_ast.stmt, Diagnostic.t list) result =
  let saved_scope = !current_scope_vars in
  current_scope_vars := Some scope_vars;
  let res = check_stmt_inner env scope_vars loop_stack next_loop_id s in
  current_scope_vars := saved_scope;
  res

and check_stmt_inner
    (env : env)
    (scope_vars : string list ref)
    (loop_stack : active_loop list)
    (next_loop_id : int ref)
    (s : Ast.stmt) : (Typed_ast.stmt, Diagnostic.t list) result =
  match s.desc with
  | Ast.StmtExpr e ->
    (match check_expr_any env e with
     | Ok te -> Ok { Typed_ast.desc = Typed_ast.TStmtExpr te; span = s.span }
     | Error err -> Error [ err ])
  | Ast.StmtReturn eo ->
    let ret_err msg hint span =
      Error [ Diagnostic.error ~hint ~code:"E0111" ~span msg ]
    in
    (match !current_fun with
     | None ->
       ret_err "'return' used outside of a function"
         "'return' can only appear inside the body of a 'fun' or 'act'" s.span
     | Some ret_ty ->
       (match eo with
        | None ->
          if ret_ty = Ty.Void then
            Ok { Typed_ast.desc = Typed_ast.TStmtReturn None; span = s.span }
          else
            ret_err
              (Printf.sprintf "'return' without a value in a function returning '%s'"
                 (Ty.to_string ret_ty))
              "provide a value, e.g. 'return <expr>;'" s.span
        | Some e ->
          if ret_ty = Ty.Void then
            ret_err "an 'act' cannot return a value"
              "use 'return;' or declare a 'fun' with a return type" e.span
          else
            (match check_expr env e with
             | Error err -> Error [ err ]
             | Ok te ->
               if Ty.equal te.ty ret_ty then
                 Ok { Typed_ast.desc = Typed_ast.TStmtReturn (Some te); span = s.span }
               else
                 ret_err
                   (Printf.sprintf "type mismatch in 'return': expected '%s', found '%s'"
                      (Ty.to_string ret_ty) (Ty.to_string te.ty))
                   "the returned value must have exactly the declared return type" te.span)))
  | Ast.StmtFunDecl fd ->
    let conflict_errs =
      match find_conflict env fd.fd_name with
      | None -> []
      | Some prev ->
        [
          Diagnostic.error
            ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
            ~code:"E0103"
            ~span:fd.fd_name_span
            (Printf.sprintf "'%s' is already declared in this scope" fd.fd_name);
        ]
    in
    (match resolve_fun_signature fd with
     | Error es -> Error (conflict_errs @ es)
     | Ok (params, ret) ->
       if conflict_errs <> [] then Error conflict_errs
       else begin
         let fty =
           Ty.Fun
             (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) params,
              ret)
         in
         Hashtbl.replace env fd.fd_name
           {
             ty = fty;
             decl_span = fd.fd_name_span;
             is_const = true;
             const_kind = DeepConst;
             is_ref_param = false;
             const_int = None;
           };
         scope_vars := fd.fd_name :: !scope_vars;
         Hashtbl.replace fun_param_names fd.fd_name_span
           (List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) params);
         let body_env : env = Hashtbl.copy env in
         let outer : (string, unit) Hashtbl.t = Hashtbl.create 16 in
         Hashtbl.iter (fun k _ -> Hashtbl.replace outer k ()) env;
         let saved_fun = !current_fun in
         let saved_outer = !fn_outer in
         current_fun := Some ret;
         fn_outer := Some outer;
         List.iter2
           (fun (p : Ast.fun_param) (tp : Typed_ast.typed_param) ->
             Hashtbl.remove outer tp.tp_name;
             let is_const = tp.tp_mode = Ty.ParamVal in
             let const_kind = if is_const then ShallowConst else NotConst in
             let is_ref_param = tp.tp_mode = Ty.ParamRef || tp.tp_mode = Ty.ParamOut in
             Hashtbl.replace body_env tp.tp_name
               {
                 ty = tp.tp_ty;
                 decl_span = p.param_span;
                 is_const;
                 const_kind;
                 is_ref_param;
                 const_int = None;
               })
           fd.fd_params params;
         let body_scope = ref [] in
         let tbody = ref [] in
         let berrs = ref [] in
         List.iter
           (fun sub ->
             match check_stmt body_env body_scope [] next_loop_id sub with
             | Ok ts -> tbody := ts :: !tbody
             | Error es -> berrs := List.rev_append es !berrs)
           fd.fd_body;
         current_fun := saved_fun;
         fn_outer := saved_outer;
         match List.rev !berrs with
         | _ :: _ as errors -> Error errors
         | [] ->
           let tbody = List.rev !tbody in
           let out_errs = OutCheck.check_fun fd.fd_name fd.fd_name_span params tbody in
           if out_errs <> [] then Error out_errs
           else if ret <> Ty.Void && not (List.exists stmt_always_returns tbody) then
             Error
               [
                 Diagnostic.error
                   ~hint:"add a 'return <expr>;' on every path that reaches the end of the function"
                   ~code:"E0111"
                   ~span:fd.fd_name_span
                   (Printf.sprintf "function '%s' may reach its end without returning a value"
                      fd.fd_name);
               ]
           else
             match compute_captures env params fd.fd_name tbody with
             | Error es -> Error es
             | Ok captures ->
               Ok
                 {
                   Typed_ast.desc =
                     Typed_ast.TStmtFunDecl
                       {
                         Typed_ast.tf_name = fd.fd_name;
                         tf_name_span = fd.fd_name_span;
                         tf_params = params;
                         tf_ret = ret;
                         tf_captures = captures;
                         tf_body = tbody;
                       };
                   span = s.span;
                 }
       end)
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
               (match find_conflict env name with
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
                   {
                     ty = elem_ty;
                     decl_span = name_span;
                     is_const = head.is_const;
                     const_kind = (if head.is_const then DeepConst else NotConst);
                     is_ref_param = false;
                     const_int = None;
                   };
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
                   (match find_conflict env name with
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
                            {
                              ty = sub_ty;
                              decl_span = name_span;
                              is_const = head.is_const;
                              const_kind = (if head.is_const then DeepConst else NotConst);
                              is_ref_param = false;
                              const_int = None;
                            };
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
          (match find_conflict env name with
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
                         {
                           ty = elem_ty;
                           decl_span = name_span;
                           is_const = d.is_const;
                           const_kind = (if d.is_const then DeepConst else NotConst);
                           is_ref_param = false;
                           const_int = None;
                         };
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
    (match find_conflict env v.name with
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
    (match v.ty_annot with
     | Some annot when Option.is_none v.init ->
       (match resolve_type_expr annot None with
        | Ok ty when ty_contains_fun ty ->
          errs :=
            Diagnostic.error
              ~hint:"initialize it with a function, e.g. 'var f : int(int) = my_fun;'"
              ~code:"E0106"
              ~span:v.name_span
              (Printf.sprintf "variable '%s' of function type requires an initializing expression" v.name)
            :: !errs
        | _ -> ())
     | _ -> ());
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
            {
              ty = final_ty;
              decl_span = v.name_span;
              is_const = v.is_const;
              const_kind = (if v.is_const then DeepConst else NotConst);
              is_ref_param = false;
              const_int;
            };
          (match v.init with
           | Some { Ast.desc = Ast.ExprIdent orig_name; _ } ->
             (match Hashtbl.find_opt var_param_names orig_name with
              | Some pnames -> Hashtbl.replace var_param_names v.name pnames
              | None ->
                (match Hashtbl.find_opt env orig_name with
                 | Some info ->
                   (match Hashtbl.find_opt fun_param_names info.decl_span with
                    | Some pnames -> Hashtbl.replace var_param_names v.name pnames
                    | None -> ())
                 | None -> ()))
           | Some ie ->
             (match Hashtbl.find_opt expr_param_names ie.span with
              | Some pnames -> Hashtbl.replace var_param_names v.name pnames
              | None -> ())
           | None -> ());
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
  let saved_root = !root_scope_ref in
  let saved_fun = !current_fun in
  let saved_outer = !fn_outer in
  root_scope_ref := Some root_scope;
  current_fun := None;
  fn_outer := None;
  List.iter
    (fun stmt ->
      match check_stmt env root_scope [] next_loop_id stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  root_scope_ref := saved_root;
  current_fun := saved_fun;
  fn_outer := saved_outer;
  match List.rev !all_errors with
  | [] -> Ok (List.rev !tstmts)
  | errs -> Error errs
