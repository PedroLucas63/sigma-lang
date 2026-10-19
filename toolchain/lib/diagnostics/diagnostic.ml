open Sigma_common

type severity =
  | Error
  | Warning

type t = {
  severity : severity;
  code : string;
  message : string;
  span : Span.t;
  hint : string option;
}

let error ?hint ~code ~span message =
  { severity = Error; code; message; span; hint }

let warning ?hint ~code ~span message =
  { severity = Warning; code; message; span; hint }

let severity_str = function
  | Error -> "error"
  | Warning -> "warning"

let render ?source d =
  let header =
    Printf.sprintf "%s[%s]: %s\n  --> %s"
      (severity_str d.severity)
      d.code
      d.message
      (Span.to_string d.span)
  in
  let snippet =
    match source with
    | None -> ""
    | Some src ->
      (match Source.get_line src d.span.start_pos.line with
       | None -> ""
       | Some line_text ->
         let line_no = d.span.start_pos.line in
         let col_start = max 1 d.span.start_pos.col in
         let len =
           if d.span.end_pos.line = d.span.start_pos.line then
             max 1 (d.span.end_pos.col - col_start)
           else 1
         in
         let pad = String.make (col_start - 1) ' ' in
         let carets = String.make len '^' in
         Printf.sprintf "\n %4d | %s\n      | %s%s" line_no line_text pad carets)
  in
  let hint_str =
    match d.hint with
    | None -> ""
    | Some h -> Printf.sprintf "\n  = help: %s" h
  in
  header ^ snippet ^ hint_str

let pp fmt d = Format.fprintf fmt "%s" (render d)
