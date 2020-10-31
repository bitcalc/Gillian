open Gil_syntax
open Utils.Containers
open Monadic

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment       of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled              of string
  | RemovingNotOwned
  | HoleNotUndefined
  | MemoryNotFreed

val pp_err : err Fmt.t

val err_equal : err -> err -> bool

type 'a or_error = ('a, err) Result.t

type 'a d_or_error = ('a, err) Delayed_result.t

module Range : sig
  type t = Expr.t * Expr.t
end

type t

val pp : t Fmt.t

val empty : t

val freed : t

val is_empty : t -> bool

val lvars : t -> SS.t

val get_bounds : t -> Range.t option or_error

val set_bounds : t -> Range.t option -> t or_error

val rem_bounds : t -> t or_error

val get_single :
  t -> Expr.t -> Chunk.t -> (SVal.t * Perm.t option * t) d_or_error

val set_single : t -> Expr.t -> Chunk.t -> SVal.t -> Perm.t -> t d_or_error

val rem_single : t -> Expr.t -> Chunk.t -> t d_or_error

val get_hole : t -> Expr.t -> Expr.t -> (t * Perm.t option) d_or_error

val set_hole : t -> Expr.t -> Expr.t -> Perm.t -> t d_or_error

val rem_hole : t -> Expr.t -> Expr.t -> t d_or_error

val get_freed : t -> unit or_error

val alloc : Expr.t -> Expr.t -> t

val store : t -> Chunk.t -> Expr.t -> SVal.t -> t d_or_error

val load : t -> Chunk.t -> Expr.t -> (SVal.t * t) d_or_error

val free : t -> Expr.t -> Expr.t -> t d_or_error

(* val get_perm_at : Expr.t -> t -> Perm.t option d_or_error *)

val drop_perm : t -> Expr.t -> Expr.t -> Perm.t -> t d_or_error

val get_perm_at : t -> Expr.t -> Perm.t option d_or_error

val assertions : loc:string -> t -> Asrt.t list

val substitution :
  le_subst:(Expr.t -> Expr.t) -> sval_subst:(SVal.t -> SVal.t) -> t -> t