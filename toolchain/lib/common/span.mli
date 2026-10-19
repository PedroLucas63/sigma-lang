(** Localização exata de um trecho de código-fonte [.sgm]. *)

type pos = {
  offset : int;
  line : int;   (** 1-indexed *)
  col : int;    (** 1-indexed *)
}

type t = {
  file : string;
  start_pos : pos;
  end_pos : pos;
}

val dummy : t
val make : file:string -> start_pos:pos -> end_pos:pos -> t
val merge : t -> t -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
