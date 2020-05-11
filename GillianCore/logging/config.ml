exception Not_initialized

module type S = sig
  type state

  type conf

  val enabled : unit -> bool

  val enable : unit -> unit

  val disable : unit -> unit

  val initialized : unit -> bool

  val initialize : unit -> unit

  val get_conf : unit -> conf

  val set_conf : conf -> unit

  val get_state : unit -> state

  val wrap_up : unit -> unit
end

module Make (P : sig
  type conf

  val default_conf : conf

  type state

  val init_state : conf -> state

  val default_enabled : bool

  val wrap_up : state -> conf -> unit
end) : S with type conf = P.conf and type state = P.state = struct
  type conf = P.conf

  type state = P.state

  let was_init = ref false

  let curr_state = ref None

  let enabled, enable, disable =
    let enabled = ref P.default_enabled in
    ( (fun () -> !enabled),
      (fun () -> enabled := true),
      fun () -> enabled := false )

  let get_conf, set_conf =
    let curr_conf = ref P.default_conf in
    ((fun () -> !curr_conf), fun c -> curr_conf := c)

  let get_state () =
    if !was_init then Option.get !curr_state else raise Not_initialized

  let initialized, initialize =
    ( (fun () -> !was_init),
      fun () ->
        if (not !was_init) && enabled () then
          curr_state := Some (P.init_state (get_conf ()));
        was_init := true )

  let wrap_up () = if initialized () then P.wrap_up (get_state ()) (get_conf ())
end

module FileLogging = struct
  module TYPES = struct
    type conf = { filename : string }

    type state = { out_channel : out_channel; formatter : Format.formatter }
  end

  include Make (struct
    include TYPES

    let default_conf = { filename = "out.log" }

    let init_state { filename; _ } =
      let out_channel = open_out filename in
      let formatter = Format.formatter_of_out_channel out_channel in
      { out_channel; formatter }

    let default_enabled = true

    let wrap_up { out_channel; _ } _ = close_out out_channel
  end)

  let get_formatter () =
    let state = get_state () in
    state.formatter
end

module DBLogging = struct
  module TYPES = struct
    type conf = { filename : string }

    type state = {
      database : (module Sanddb.Database.T with type t = Report_j.t);
    }
  end

  include Make (struct
    include TYPES

    let default_conf = { filename = "db.log" }

    let init_state { filename } =
      if Sys.file_exists filename then Sys.remove "db.log";
      { database = Sanddb.create_json_database filename (module Report_j) }

    let default_enabled = false

    let wrap_up _ _ = ()
  end)

  let get_db () = (get_state ()).database
end
