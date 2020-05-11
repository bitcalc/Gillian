module FileLogging : sig
  val enabled : unit -> bool

  val enable : unit -> unit

  val disable : unit -> unit

  val initialized : unit -> bool

  val initialize : unit -> unit

  val get_formatter : unit -> Format.formatter

  val wrap_up : unit -> unit
end

module DBLogging : sig
  val enabled : unit -> bool

  val enable : unit -> unit

  val disable : unit -> unit

  val initialized : unit -> bool

  val initialize : unit -> unit

  val get_db : unit -> (module Sanddb.Database.T with type t = Report_j.t)

  (* FIXME: this is not extendable *)

  val wrap_up : unit -> unit
end
