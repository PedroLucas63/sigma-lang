type t = {
  name : string;
  version : string;
  entry : string;
}

let default ~name =
  { name; version = "0.1.0"; entry = "src/main.sgm" }

let to_toml m =
  Printf.sprintf
    "[package]\nname = %S\nversion = %S\nentry = %S\n"
    m.name m.version m.entry
