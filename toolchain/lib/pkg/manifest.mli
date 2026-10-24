(** Gerenciador de pacotes, projetos e manifesto [sigma.toml]. *)

type t = {
  name : string;
  version : string;
  description : string;
  authors : string list;
  src_dir : string;
  entry : string;
  test_dir : string;
  scripts : (string * string) list;
}

val default :
  ?version:string ->
  ?description:string ->
  ?authors:string list ->
  ?src_dir:string ->
  ?entry:string ->
  ?test_dir:string ->
  ?scripts:(string * string) list ->
  name:string ->
  unit ->
  t

val to_toml : t -> string
val parse_toml : string -> (t, string) result
val load : ?path:string -> unit -> (t, string) result
val find_script : string -> t -> string option

(** Cria a estrutura de diretórios e arquivos iniciais ([sigma.toml], [entry], [<test_dir>/main_test.sgm]). *)
val scaffold_project : ?root_dir:string -> t -> (string list, string) result
