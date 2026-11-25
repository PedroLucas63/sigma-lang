open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let vis_rank = function
  | Ast.VisPublic -> 3
  | Ast.VisInternal -> 2
  | Ast.VisProtected -> 1
  | Ast.VisPrivate -> 0

let resolve_field_accessors ~(kind : Ty.user_type_kind) ~(field_name : string) ~(member_vis : Ast.visibility)
    (accs_opt : Ast.accessor_spec list option)
    : (Ast.visibility option * Ast.visibility option * Ast.visibility option, Diagnostic.t) result =
  match accs_opt with
  | None ->
    (match kind with
     | Ty.KindRecord -> Ok (Some member_vis, None, Some member_vis)
     | Ty.KindClass | Ty.KindStruct -> Ok (Some member_vis, Some member_vis, None))
  | Some accs ->
    let g = ref None in
    let s = ref None in
    let i = ref None in
    let rec loop = function
      | [] -> Ok (!g, !s, !i)
      | (a : Ast.accessor_spec) :: rest ->
        let acc_name =
          match a.acc_kind with Ast.AccGet -> "get" | Ast.AccSet -> "set" | Ast.AccInit -> "init"
        in
        (match a.acc_vis with
         | Some v when vis_rank v > vis_rank member_vis ->
           Error
             (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                (Printf.sprintf "accessor '%s' on '%s' cannot be more accessible ('%s') than the member ('%s')"
                   acc_name field_name (vis_to_string v) (vis_to_string member_vis)))
         | _ ->
           let eff_v = match a.acc_vis with Some v -> v | None -> member_vis in
           (match a.acc_kind with
            | Ast.AccGet ->
              if !g <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'get' accessor on property '%s'" field_name))
              else (g := Some eff_v; loop rest)
            | Ast.AccSet ->
              if kind = Ty.KindRecord then
                Error (Diagnostic.error ~code:"E0125" ~span:a.acc_span
                         ~hint:"use 'init;' instead of 'set;' in immutable records"
                         (Printf.sprintf "immutable record property '%s' cannot declare a 'set' accessor" field_name))
              else if !s <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'set' accessor on property '%s'" field_name))
              else if !i <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "property '%s' cannot declare both 'set' and 'init' accessors" field_name))
              else (s := Some eff_v; loop rest)
            | Ast.AccInit ->
              if !i <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'init' accessor on property '%s'" field_name))
              else if !s <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "property '%s' cannot declare both 'set' and 'init' accessors" field_name))
              else (i := Some eff_v; loop rest)))
    in
    loop accs
