type 'a t

val resolve : curr_pc:Pc.t -> 'a t -> 'a Branch.t list

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val branch_on :
  Gil_syntax.Formula.t ->
  then_branch:(unit -> 'a t) ->
  else_branch:(unit -> 'a t) ->
  'a t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end