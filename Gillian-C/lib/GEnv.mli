open Gillian.Concrete

type def = FunDef of string | GlobVar of string

val serialize_def : def -> Values.t

val deserialize_def : Values.t -> def

type t = {
  symb : (string, string) Gillian.Utils.PMap.t;
      (** maps symbols to loc names *)
  defs : (string, def) Gillian.Utils.PMap.t;
      (** maps loc names to definitions *)
}

val find_symbol : t -> string -> string
(** Finds a location name given symbol in the global environment *)

val find_def : t -> string -> def
(** Finds a definition given its location name in the global environment *)

val set_symbol : t -> string -> string -> t
(** [set_symbol genv symbol locname ]
    Returns a new global environment where the symbol [symbol] is associated with the location [locname] *)

val set_def : t -> string -> def -> t
(** [set_def genv locname def ]
    Returns a new global environment where the block [locname] is associated with the global definition [def] *)

val empty : t
(** Empty global environment *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for the global environment *)

(** {3 Symbolic things} *)

val substitution : Gillian.Symbolic.Subst.t -> t -> t

val assertions : t -> string list * Gillian.Gil_syntax.Asrt.t list