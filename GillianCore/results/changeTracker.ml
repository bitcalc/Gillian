open Containers

let cur_source_files = SourceFiles.make ()

let reset () = SourceFiles.reset cur_source_files

type t = {
  changed_procs : string list;
  new_procs : string list;
  deleted_procs : string list;
  dependents : string list;
}

let pp fmt changes =
  let pp_procs sec fmt = function
    | []    -> Fmt.pf fmt "%s:@\n<none>@\n" sec
    | procs ->
        let newline = Fmt.any "@\n" in
        Fmt.pf fmt "%s:@\n%a@\n" sec (Fmt.list ~sep:newline Fmt.string) procs
  in
  Fmt.pf fmt "%a%a%a%a" (pp_procs "Changed procs") changes.changed_procs
    (pp_procs "New procs") changes.new_procs (pp_procs "Deleted procs")
    changes.deleted_procs
    (pp_procs "Transitive dependents")
    changes.dependents

let to_key_set (table : (string, 'b) Hashtbl.t) : SS.t =
  Hashtbl.fold (fun key _ keys -> SS.add key keys) table SS.empty

let to_list (set : SS.t) : string list =
  SS.fold (fun elem acc -> elem :: acc) set []

let get_changed_files prev_files new_files =
  let rec get_changed paths changed =
    match paths with
    | []           -> changed
    | path :: rest ->
        (* Check if file contents have changed *)
        let prev_hash = SourceFiles.get_contents_hash prev_files path in
        let new_hash = SourceFiles.get_contents_hash new_files path in
        let contents_changed = not (String.equal prev_hash new_hash) in
        let dependents = SourceFiles.get_dependents new_files path in
        let changed =
          if List.length dependents = 0 && contents_changed then path :: changed
          else if contents_changed then dependents @ changed
          else changed
        in
        get_changed rest changed
  in
  let prev_paths = to_key_set prev_files in
  let new_paths = to_key_set new_files in
  let created = to_list (SS.diff new_paths prev_paths) in
  let existing = to_list (SS.inter prev_paths new_paths) in
  let changed = get_changed existing [] in
  (changed, created)

let get_procs_with_path (prog : ('a, 'b) Prog.t) path =
  let string_opt_equal string str_opt =
    match str_opt with
    | Some str -> String.equal string str
    | None     -> false
  in
  Hashtbl.fold
    (fun pname (proc : ('a, 'b) Proc.t) acc ->
      if string_opt_equal path proc.proc_source_path then pname :: acc else acc)
    prog.procs []

let map_concat f list = List.concat (List.map f list)

let get_proc_callers reverse_graph proc_name =
  let proc_id = CallGraph.id_of_proc_name proc_name in
  let caller_ids = CallGraph.get_children reverse_graph proc_id in
  List.map (CallGraph.get_name reverse_graph) caller_ids

let get_changed_procs prog prev_source_files prev_call_graph =
  let changed, created = get_changed_files prev_source_files cur_source_files in
  let changed_files_procs = map_concat (get_procs_with_path prog) changed in
  let new_files_procs = map_concat (get_procs_with_path prog) created in
  (* Distinguish between new procedures and those that existed before *)
  let changed_procs, new_procs =
    List.partition (CallGraph.contains_proc prev_call_graph) changed_files_procs
  in
  let other_changed_procs, other_new_procs =
    List.partition (CallGraph.contains_proc prev_call_graph) new_files_procs
  in
  let changed_procs = changed_procs @ other_changed_procs in
  let new_procs = new_procs @ other_new_procs in
  (* Determine callers of changed procedures that themselves did not change *)
  let reverse_graph = CallGraph.to_reverse_graph prev_call_graph in
  let callers = map_concat (get_proc_callers reverse_graph) changed_procs in
  let changed_procs_set = SS.of_list changed_procs in
  let callers =
    List.filter (fun pname -> not (SS.mem pname changed_procs_set)) callers
  in
  let all_prev_procs = SS.of_list (CallGraph.get_proc_names prev_call_graph) in
  let alL_current_procs = SS.of_list (Prog.get_noninternal_proc_names prog) in
  let deleted_procs = to_list (SS.diff all_prev_procs alL_current_procs) in
  { changed_procs; new_procs; deleted_procs; dependents = callers }
