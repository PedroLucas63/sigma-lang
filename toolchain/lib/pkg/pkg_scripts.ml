type script_target =
  | ScriptCmd of string
  | ScriptEntry of {
      file : string;
      fn_name : string;
    }

let resolve_script_raw ~entry (raw : string) : script_target =
  let trimmed = String.trim raw in
  let has_space = String.contains trimmed ' ' || String.contains trimmed '\t' in
  if not has_space && String.starts_with ~prefix:":" trimmed && String.length trimmed > 1 then
    let fn_name = String.sub trimmed 1 (String.length trimmed - 1) in
    ScriptEntry { file = entry; fn_name }
  else if not has_space then
    match String.rindex_opt trimmed ':' with
    | Some idx when idx > 0 && idx + 1 < String.length trimmed ->
      let file = String.sub trimmed 0 idx in
      let fn_name = String.sub trimmed (idx + 1) (String.length trimmed - idx - 1) in
      if Filename.check_suffix file ".sgm" then ScriptEntry { file; fn_name }
      else ScriptCmd raw
    | _ -> ScriptCmd raw
  else ScriptCmd raw

let default_scripts_app =
  [
    ("start", "sigma run");
    ("build", "sigma build");
    ("test", "sigma test");
    ("test-parity", "sigma test --parity");
    ("lint", "sigma lint");
  ]

let default_scripts_lib =
  [
    ("build", "sigma build --lib");
    ("pack", "sigma pkg pack");
    ("test", "sigma test");
    ("test-parity", "sigma test --parity");
    ("lint", "sigma lint");
  ]
