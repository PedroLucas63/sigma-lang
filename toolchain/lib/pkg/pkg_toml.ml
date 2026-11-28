let toml_quote (s : string) : string =
  let buf = Buffer.create (String.length s + 4) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let unquote (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    let inner = String.sub s 1 (len - 2) in
    let buf = Buffer.create (String.length inner) in
    let i = ref 0 in
    let ilen = String.length inner in
    while !i < ilen do
      if inner.[!i] = '\\' && !i + 1 < ilen then begin
        (match inner.[!i + 1] with
         | '"' -> Buffer.add_char buf '"'
         | '\\' -> Buffer.add_char buf '\\'
         | 'n' -> Buffer.add_char buf '\n'
         | 'r' -> Buffer.add_char buf '\r'
         | 't' -> Buffer.add_char buf '\t'
         | c -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
        i := !i + 2
      end else begin
        Buffer.add_char buf inner.[!i];
        incr i
      end
    done;
    Buffer.contents buf
  else
    s

let parse_string_array (s : string) : string list =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then
    let inner = String.trim (String.sub s 1 (len - 2)) in
    if inner = "" then []
    else String.split_on_char ',' inner |> List.map unquote |> List.filter (fun x -> x <> "")
  else []

let strip_comment (line : string) : string =
  let in_quotes = ref false in
  let len = String.length line in
  let idx = ref len in
  let i = ref 0 in
  while !i < len do
    let c = line.[!i] in
    if c = '"' then in_quotes := not !in_quotes
    else if c = '#' && not !in_quotes then (idx := !i; i := len);
    incr i
  done;
  String.trim (String.sub line 0 !idx)
