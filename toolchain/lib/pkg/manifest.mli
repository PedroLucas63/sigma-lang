(** Gerenciador de pacotes, projetos e manifesto [sigma.toml]. *)

type project_kind =
  | KindApp
  | KindLib

type native_config = {
  sources : string list;
  libs : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
}

type dep_spec =
  | DepSemver of string
  | DepPath of string
  | DepUrl of {
      url : string;
      version : string option;
    }
  | DepGit of {
      git : string;
      tag : string option;
      branch : string option;
      rev : string option;
    }

type t = {
  name : string;
  version : string;
  kind : project_kind;
  description : string;
  authors : string list;
  src_dir : string;
  entry : string;
  test_dir : string;
  scripts : (string * string) list;
  native : native_config;
  dependencies : (string * dep_spec) list;
  mirrors : (string * string * int) list;
}

val default :
  ?version:string ->
  ?kind:project_kind ->
  ?description:string ->
  ?authors:string list ->
  ?src_dir:string ->
  ?entry:string ->
  ?test_dir:string ->
  ?scripts:(string * string) list ->
  ?native:native_config ->
  ?dependencies:(string * dep_spec) list ->
  ?mirrors:(string * string * int) list ->
  name:string ->
  unit ->
  t

val to_toml : t -> string
val parse_toml : string -> (t, string) result
val load : ?path:string -> unit -> (t, string) result
type script_target =
  | ScriptCmd of string
  | ScriptEntry of {
      file : string;
      fn_name : string;
    }

val find_script : string -> t -> string option
val resolve_script : string -> t -> script_target option

(** Cria a estrutura de diretórios e arquivos iniciais ([sigma.toml], [entry], [<test_dir>/main_test.sgm]). *)
val scaffold_project : ?root_dir:string -> t -> (string list, string) result
