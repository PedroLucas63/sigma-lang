type pos = {
  offset : int;
  line : int;
  col : int;
}

type t = {
  file : string;
  start_pos : pos;
  end_pos : pos;
}

let dummy_pos = { offset = 0; line = 1; col = 1 }

let dummy = { file = "<dummy>"; start_pos = dummy_pos; end_pos = dummy_pos }

let make ~file ~start_pos ~end_pos = { file; start_pos; end_pos }

let merge a b =
  let start_pos =
    if a.start_pos.offset <= b.start_pos.offset then a.start_pos else b.start_pos
  in
  let end_pos =
    if a.end_pos.offset >= b.end_pos.offset then a.end_pos else b.end_pos
  in
  { file = a.file; start_pos; end_pos }

let to_string s =
  Printf.sprintf "%s:%d:%d" s.file s.start_pos.line s.start_pos.col

let pp fmt s = Format.fprintf fmt "%s" (to_string s)
