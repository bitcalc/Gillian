type severity = Info | Log | Success | Error | Warning

type 'a t = {
  id : Uuidm.t;
  title : string;
  elapsed_time : float;
  previous : Uuidm.t option;
  parent : Uuidm.t option;
  content : (('a, Format.formatter, unit) format -> 'a) -> unit;
  severity : severity;
}

let parents : Uuidm.t Stack.t = Stack.create ()

let current : Uuidm.t option ref = ref Option.none

let seed = Random.State.make_self_init ()

type 'a report_builder = unit -> 'a t

let report_builder title content severity (_unit : unit) =
  let report =
    {
      id = Uuidm.v4_gen seed ();
      title;
      elapsed_time = Sys.time ();
      previous = !current;
      parent = Stack.top_opt parents;
      content;
      severity;
    }
  in
  current := Some report.id;
  report

let info title content = report_builder title content Info

let log title content = report_builder title content Log

let success title content = report_builder title content Success

let error title content = report_builder title content Error

let warning title content = report_builder title content Warning

let enter_node () =
  Stack.push (Option.get !current) parents;
  current := Option.none

let exit_node () = current := Option.some @@ Stack.pop parents