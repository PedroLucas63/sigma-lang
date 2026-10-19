open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sigma_sema
open Sigma_ir
open Sigma_interp
open Sigma_codegen
open Sigma_linter
open Sigma_debugger

let compile_frontend (src : Source.t) :
    (Typed_ast.program * Ir.program, Diagnostic.t list) result =
  match Lexer.tokenize src with
  | Error errs -> Error errs
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error errs -> Error errs
     | Ok ast ->
       (match Typecheck.check_program ast with
        | Error errs -> Error errs
        | Ok typed_ast ->
          let ir = Lower.lower_program typed_ast in
          Ok (typed_ast, ir)))

let run_source (src : Source.t) : (string, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (_typed_ast, ir) ->
    (match Vm.run ir with
     | Ok out -> Ok out
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0900" ~span:Span.dummy msg ])

let build_source ~output_path (src : Source.t) : (unit, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, _ir) ->
    (match Codegen_c.compile_executable ~output_path typed_ast with
     | Ok () -> Ok ()
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0901" ~span:Span.dummy msg ])

let lint_source (src : Source.t) : (Diagnostic.t list, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, _ir) -> Ok (Linter.lint_program typed_ast)

let debug_source (src : Source.t) : (string, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (_typed_ast, ir) ->
    (match Debugger.trace_execution ir with
     | Ok trace -> Ok trace
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0902" ~span:Span.dummy msg ])
