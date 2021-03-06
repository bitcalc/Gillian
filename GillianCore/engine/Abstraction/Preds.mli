module type S = sig
  (* value type *)
  type vt

  (* substitution type for the value type *)
  type st

  (* preds *)
  type t

  type abs_t = string * vt list

  val length : t -> int

  val init : abs_t list -> t

  val to_list : t -> abs_t list

  val copy : t -> t

  val is_empty : t -> bool

  val extend : t -> abs_t -> unit

  val pop : t -> (abs_t -> bool) -> abs_t option

  val remove_by_name : t -> string -> abs_t option

  val find_pabs_by_name : t -> string -> abs_t list

  val pp : Format.formatter -> t -> unit

  val pp_pabs : Format.formatter -> abs_t -> unit

  val get_pred :
    maintain:bool ->
    t ->
    string ->
    vt list ->
    int list ->
    (vt -> vt -> bool) ->
    abs_t option

  val find : t -> (abs_t -> bool) -> abs_t option

  val get_all : maintain:bool -> (abs_t -> bool) -> t -> abs_t list

  val substitution_in_place : st -> t -> unit

  (** Turns a predicate set into a list of assertions *)
  val to_assertions : t -> Asrt.t list
end

module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st) :
  S with type vt = Val.t and type st = Subst.t

module SPreds : S with type vt = SVal.M.t and type st = SVal.SSubst.t
