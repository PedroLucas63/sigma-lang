(** Diagnósticos estruturados (erros e avisos) com localização exata. *)

open Sigma_common

type severity =
  | Error
  | Warning

type t = {
  severity : severity;
  code : string;      (** Ex: "E0001", "W0001" *)
  message : string;
  span : Span.t;
  hint : string option;
}

val error : ?hint:string -> code:string -> span:Span.t -> string -> t
val warning : ?hint:string -> code:string -> span:Span.t -> string -> t
val render : ?source:Source.t -> t -> string
val pp : Format.formatter -> t -> unit
