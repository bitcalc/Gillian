(* type 'b t = < log : 'a. ('a, 'b) Report.t -> unit ; wrap_up : unit -> unit; log_tl : 'b > *)

class ['a, 'b] file_reporter =
  object (self)
    method log (report : ('a, 'b) Report.t) =
      if Config.FileLogging.enabled () then
        match report.content with
        | Debug msgf    -> self#file_debug msgf
        | Phase p       -> self#file_phase p
        | TargetLang tl -> self#file_tl tl

    method fmt = Config.FileLogging.get_formatter ()

    method file_tl (_ : 'b) = ()

    method private file_debug
        : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit =
      fun msgf -> msgf @@ fun fmt -> Format.fprintf self#fmt (fmt ^^ "@,@?")

    method private file_phase phase =
      Format.fprintf self#fmt "*** Phase %s ***@,@?"
        (Report.string_of_phase phase)

    method wrap_up = Config.FileLogging.wrap_up ()

    method initialize = Config.FileLogging.initialize ()
  end

class ['a, 'b] file_and_db_reporter =
  object (self)
    inherit ['a, 'b] file_reporter as super

    method! log (report : ('a, 'b) Report.t) =
      super#log report;
      if Config.DBLogging.enabled () then
        let db_rep : Report_t.t =
          {
            id = Uuidm.to_string report.id;
            title = report.title;
            elapsed_time = report.elapsed_time;
            previous = Option.map Uuidm.to_string report.previous;
            parent = Option.map Uuidm.to_string report.parent;
            content = self#db_serialize_content report.content;
            severity = self#db_serialize_severity report.severity;
          }
        in
        ignore (Sanddb.insert_record (Config.DBLogging.get_db ()) db_rep)

    method private db_serialize_severity : Report.severity -> Report_t.severity
        =
      function
      | Info    -> `Info
      | Log     -> `Log
      | Success -> `Success
      | Error   -> `Error
      | Warning -> `Warning

    method private db_serialize_content (content : ('a, 'b) Report.content) =
      match content with
      | TargetLang tl -> self#db_serialize_tl tl
      | Phase p       -> Format.asprintf "Phase %s" (Report.string_of_phase p)
      | Debug msgf    ->
          let str = ref "" in
          let () =
            msgf @@ fun fmt -> Format.kasprintf (fun s -> str := s) fmt
          in
          !str

    method db_serialize_tl (_ : 'a) = "Cannot report tl-specific content"

    method! wrap_up =
      super#wrap_up;
      Config.DBLogging.wrap_up ()

    method! initialize =
      super#initialize;
      Config.DBLogging.initialize ()
  end

(*
class ['b] database_reporter () =
  object (self)
    val database =
      if Sys.file_exists "db.log" then Sys.remove "db.log";
      Sanddb.create_json_database "db.log" (module Report_j)

    method private serialize_content : 'a. ('a, 'b) Report.content -> string =
      function
      | Debug msgf   ->
          let str = ref "" in
          (msgf @@ fun fmt -> Format.kasprintf (fun s -> str := s) fmt);
          !str
      | Phase phase  ->
          Format.asprintf "Phase %s" @@ Report.string_of_phase phase
      | TargetLang _ -> ""

    method private serialize_severity : Report.severity -> Report_t.severity =
      function
      | Info    -> `Info
      | Log     -> `Log
      | Success -> `Success
      | Error   -> `Error
      | Warning -> `Warning

    method log : 'a. ('a, 'b) Report.t -> unit =
      fun report ->
        let report : Report_t.t =
          {
            id = Uuidm.to_string report.id;
            title = report.title;
            elapsed_time = report.elapsed_time;
            previous = Option.map Uuidm.to_string report.previous;
            parent = Option.map Uuidm.to_string report.parent;
            content = self#serialize_content report.content;
            severity = self#serialize_severity report.severity;
          }
        in
        let _ = Sanddb.insert_record database report in
        ()

    method wrap_up () = ()
  end

type default

let fr : default t = new file_reporter ()

let dr : default t = new database_reporter ()

*)

let init () = (new file_and_db_reporter)#initialize

let wrap_up () = (new file_and_db_reporter)#wrap_up
