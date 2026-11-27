open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

let generic_types : (string, Ast.type_decl) Hashtbl.t = Hashtbl.create 16
let generic_funs : (string, Ast.fun_decl) Hashtbl.t = Hashtbl.create 16
let generic_methods : (string * string, Ast.method_decl) Hashtbl.t = Hashtbl.create 16
let instantiated_types : (string, Ty.t) Hashtbl.t = Hashtbl.create 16
let instantiated_funs : (string, string * Ty.t) Hashtbl.t = Hashtbl.create 16
let pending_stmts : Typed_ast.stmt list ref = ref []
let inst_span_counter : int ref = ref 100000
let unique_inst_span (sp : Span.t) : Span.t =
  incr inst_span_counter;
  { sp with Span.start_pos = { sp.start_pos with Span.offset = !inst_span_counter } }
let instantiate_type_hook : (Span.t -> string -> Ty.t list -> (Ty.t, Diagnostic.t) result) ref =
  ref (fun sp name _ -> Error (Diagnostic.error ~code:"E0101" ~span:sp (Printf.sprintf "unknown generic type '%s'" name)))

let clear_all () =
  Hashtbl.clear generic_types;
  Hashtbl.clear generic_funs;
  Hashtbl.clear generic_methods;
  Hashtbl.clear instantiated_types;
  Hashtbl.clear instantiated_funs;
  pending_stmts := []

let snapshot () =
  (Hashtbl.copy generic_types, Hashtbl.copy generic_funs,
   Hashtbl.copy generic_methods, Hashtbl.copy instantiated_types,
   Hashtbl.copy instantiated_funs, !pending_stmts)

let restore (gt, gf, gm, it, ifn, ps) =
  clear_all ();
  Hashtbl.iter (Hashtbl.replace generic_types) gt;
  Hashtbl.iter (Hashtbl.replace generic_funs) gf;
  Hashtbl.iter (Hashtbl.replace generic_methods) gm;
  Hashtbl.iter (Hashtbl.replace instantiated_types) it;
  Hashtbl.iter (Hashtbl.replace instantiated_funs) ifn;
  pending_stmts := ps

let rec mangle_ty (ty : Ty.t) : string =
  match ty with
  | Ty.Int -> "int"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Decimal -> "decimal"
  | Ty.Bool -> "bool"
  | Ty.Char -> "char"
  | Ty.String -> "string"
  | Ty.Void -> "void"
  | Ty.User (_, n) | Ty.WildcardUser (_, n) -> n
  | Ty.Array (inner, n) -> Printf.sprintf "arr%d_%s" n (mangle_ty inner)
  | Ty.DynArray inner -> "dyn_" ^ mangle_ty inner
  | Ty.Tuple tys -> "tup_" ^ String.concat "_" (List.map mangle_ty tys)
  | Ty.Fun (ps, r) ->
    "fn_" ^ String.concat "_" (List.map (fun p -> mangle_ty p.Ty.pty) ps) ^ "_" ^ mangle_ty r

let mangle_name (base : string) (tys : Ty.t list) : string =
  match tys with
  | [] -> base
  | _ -> base ^ "__" ^ String.concat "_" (List.map mangle_ty tys)

let rec ty_to_ast_type_expr (sp : Span.t) (ty : Ty.t) : Ast.type_expr =
  match ty with
  | Ty.Int -> Ast.TypeNamed ("int", sp)
  | Ty.Float -> Ast.TypeNamed ("float", sp)
  | Ty.Double -> Ast.TypeNamed ("double", sp)
  | Ty.Decimal -> Ast.TypeNamed ("decimal", sp)
  | Ty.Bool -> Ast.TypeNamed ("bool", sp)
  | Ty.Char -> Ast.TypeNamed ("char", sp)
  | Ty.String -> Ast.TypeNamed ("string", sp)
  | Ty.Void -> Ast.TypeNamed ("void", sp)
  | Ty.User (_, n) -> Ast.TypeNamed (n, sp)
  | Ty.WildcardUser (_, n) -> Ast.TypeGeneric (n, [ Ast.TypeWildcard sp ], sp)
  | Ty.Array (inner, n) -> Ast.TypeArray (ty_to_ast_type_expr sp inner, Some n, sp)
  | Ty.DynArray inner -> Ast.TypeArray (ty_to_ast_type_expr sp inner, None, sp)
  | Ty.Tuple tys -> Ast.TypeTuple (List.map (ty_to_ast_type_expr sp) tys, sp)
  | Ty.Fun (ps, r) ->
    let pmode = function
      | Ty.ParamVal -> Ast.ParamVal
      | Ty.ParamRef -> Ast.ParamRef
      | Ty.ParamOut -> Ast.ParamOut
    in
    Ast.TypeFun
      (ty_to_ast_type_expr sp r,
       List.map (fun (p : Ty.fun_param) -> (pmode p.pmode, ty_to_ast_type_expr sp p.pty)) ps,
       sp)

let check_duplicate_type_params (tps : Ast.type_param list) : (unit, Diagnostic.t) result =
  let seen = Hashtbl.create 8 in
  let rec loop = function
    | [] -> Ok ()
    | (tp : Ast.type_param) :: rest ->
      if Hashtbl.mem seen tp.tp_name then
        Error
          (Diagnostic.error ~code:"E0143" ~span:tp.tp_span
             (Printf.sprintf "duplicate generic type parameter '%s'" tp.tp_name))
      else begin
        Hashtbl.add seen tp.tp_name ();
        loop rest
      end
  in
  loop tps

let check_where_clauses ~resolve_type_expr (wcs : Ast.where_clause list) (bindings : (string * Ty.t) list)
    : (unit, Diagnostic.t) result =
  let is_numeric = function
    | Ty.Int | Ty.Float | Ty.Double | Ty.Decimal -> true
    | _ -> false
  in
  let rec loop = function
    | [] -> Ok ()
    | (wc : Ast.where_clause) :: rest ->
      (match List.assoc_opt wc.wc_param bindings with
       | None ->
         Error
           (Diagnostic.error ~code:"E0141" ~span:wc.wc_param_span
              (Printf.sprintf "unknown type parameter '%s' in where clause" wc.wc_param))
       | Some actual_ty ->
         let ok_res =
           match wc.wc_constraint with
           | Ast.ConstraintNumeric -> Ok (is_numeric actual_ty)
           | Ast.ConstraintClass ->
             Ok (match actual_ty with Ty.User (Ty.KindClass, _) -> true | _ -> false)
           | Ast.ConstraintStruct ->
             Ok (match actual_ty with Ty.User (Ty.KindStruct, _) -> true | _ -> false)
           | Ast.ConstraintRecord ->
             Ok (match actual_ty with Ty.User (Ty.KindRecord, _) -> true | _ -> false)
           | Ast.ConstraintTypes tes ->
             let rec check_any = function
               | [] -> Ok false
               | te :: r ->
                 (match resolve_type_expr te None with
                  | Error e -> Error e
                  | Ok t -> if Ty.equal actual_ty t then Ok true else check_any r)
             in
             check_any tes
         in
         (match ok_res with
          | Error e -> Error e
          | Ok true -> loop rest
          | Ok false ->
            Error
              (Diagnostic.error ~code:"E0142" ~span:wc.wc_span
                 (Printf.sprintf "type '%s' does not satisfy constraint on type parameter '%s'"
                    (Ty.to_string actual_ty) wc.wc_param))))
  in
  loop wcs

let infer_from_type_expr (tp_names : string list) (te : Ast.type_expr) (actual : Ty.t)
    (acc : (string * Ty.t) list) : (string * Ty.t) list =
  let rec walk te ty acc =
    match te, ty with
    | Ast.TypeNamed (n, _), _ when List.mem n tp_names ->
      if List.mem_assoc n acc then acc else (n, ty) :: acc
    | Ast.TypeTuple (tes, _), Ty.Tuple tys when List.length tes = List.length tys ->
      List.fold_left2 (fun a t1 t2 -> walk t1 t2 a) acc tes tys
    | Ast.TypeArray (t1, _, _), (Ty.Array (t2, _) | Ty.DynArray t2) -> walk t1 t2 acc
    | Ast.TypeFun (r1, ps1, _), Ty.Fun (ps2, r2) when List.length ps1 = List.length ps2 ->
      let acc = walk r1 r2 acc in
      List.fold_left2 (fun a (_, pt1) (pt2 : Ty.fun_param) -> walk pt1 pt2.pty a) acc ps1 ps2
    | _ -> acc
  in
  walk te actual acc

let infer_call_type_args (sp : Span.t) (tps : Ast.type_param list) (fparams : Ast.fun_param list)
    (arg_tys : Ty.t list) : (Ty.t list * Ty.t list, Diagnostic.t) result =
  let non_var_tps = List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) tps in
  let tp_names = List.map (fun (tp : Ast.type_param) -> tp.tp_name) non_var_tps in
  let rec match_params ps atys acc var_tys =
    match ps, atys with
    | [ p ], rest when p.Ast.param_variadic -> (acc, rest)
    | p :: pr, a :: ar ->
      let acc' = infer_from_type_expr tp_names p.Ast.param_type a acc in
      match_params pr ar acc' var_tys
    | _, _ -> (acc, var_tys)
  in
  let inferred, var_tys = match_params fparams arg_tys [] [] in
  let rec collect acc = function
    | [] -> Ok (List.rev acc, var_tys)
    | (tp : Ast.type_param) :: rest ->
      (match List.assoc_opt tp.tp_name inferred with
       | Some ty -> collect (ty :: acc) rest
       | None ->
         Error
           (Diagnostic.error ~code:"E0141" ~span:sp
              (Printf.sprintf "cannot infer generic type argument '%s'; specify it explicitly"
                 tp.tp_name)))
  in
  collect [] non_var_tps
