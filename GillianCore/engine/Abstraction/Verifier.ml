open Containers

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SSubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SSubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) =
struct
  module L = Logging
  module SSubst = SVal.SSubst
  module SAInterpreter =
    GInterpreter.Make (SVal.M) (SVal.SSubst) (SStore) (SPState) (External)
  module Normaliser = Normaliser.Make (SPState)

  let print_success_or_failure success =
    if success then Fmt.pr "%a" (Fmt.styled `Green Fmt.string) "Success\n"
    else Fmt.pr "%a" (Fmt.styled `Red Fmt.string) "Failure\n"

  type t = {
    name : string;
    id : int;
    params : string list;
    pre_state : SPState.t;
    post_up : UP.t;
    flag : Flag.t option;
    spec_vars : SS.t;
  }

  let global_results = VerificationResults.make ()

  let reset () =
    VerificationResults.reset global_results;
    SAInterpreter.reset ()

  let testify
      (preds : (string, Pred.t) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (pre : Asrt.t)
      (posts : Asrt.t list)
      (flag : Flag.t option)
      (label : (string * SS.t) option)
      (to_verify : bool) : t option * (Asrt.t * Asrt.t list) option =
    try
      (* Step 1 - normalise the precondition *)
      match Normaliser.normalise_assertion ~pvars:(SS.of_list params) pre with
      | None                 -> (None, None)
      | Some (ss_pre, subst) -> (
          (* Step 2 - spec_vars = lvars(pre)\dom(subst) -U- alocs(range(subst)) *)
          let lvars = Asrt.lvars pre in
          let subst_dom = SSubst.domain subst None in
          let get_aloc x =
            match (x : Expr.t) with
            | ALoc loc -> Some loc
            | _        -> None
          in
          let alocs =
            SS.of_list
              (List_utils.get_list_somes
                 (List.map get_aloc (SSubst.range subst)))
          in
          let spec_vars = SS.union (SS.diff lvars subst_dom) alocs in

          let pre' = Asrt.star (SPState.to_assertions ss_pre) in

          (* Step 3 - postconditions to symbolic states *)
          L.verbose (fun m ->
              m
                "Processing one postcondition of %s with label %a and \
                 spec_vars: @[<h>%a@].@\n\
                 Original Pre:@\n\
                 %a\n\
                 Symb State Pre:@\n\
                 %a@\n\
                 Posts (%d):@\n\
                 %a"
                name
                Fmt.(
                  option ~none:(any "None") (fun ft (s, e) ->
                      Fmt.pf ft "[ %s; %a ]" s
                        (iter ~sep:comma SS.iter string)
                        e))
                label
                Fmt.(iter ~sep:comma SS.iter string)
                spec_vars Asrt.pp pre SPState.pp ss_pre (List.length posts)
                Fmt.(list ~sep:(any "@\n") Asrt.pp)
                posts);

          let posts =
            List.map (fun p -> SVal.SSubst.substitute_asrt subst true p) posts
          in
          let posts' =
            List.filter (fun p -> Simplifications.admissible_assertion p) posts
          in

          (* the following line is horrific - and suggests bad design - mea culpa - JFS *)
          (* let known_post_vars = match flag with | Some _ -> SS.singleton Names.return_variable | None -> SS.empty in
             let ss_posts = List.map (Normaliser.normalise_assertion None None (Some known_post_vars)) posts in
             let ss_posts = List.map (fun (ss, _) -> ss) (get_list_somes ss_posts) in
             let posts'   = List.map (fun post -> Asrt.star (SPState.to_assertions post)) ss_posts in *)
          match to_verify with
          | false -> (None, Some (pre', posts'))
          | true  -> (
              (* Step 4 - create a unification plan for the postconditions and s_test *)
              (* let known_vars    = SS.add Names.return_variable (SS.union (SS.of_list params) spec_vars) in *)
              let known_vars = SS.add Names.return_variable spec_vars in
              let known_vars =
                SS.union known_vars
                  (Option.fold
                     ~some:(fun (_, existentials) -> existentials)
                     ~none:SS.empty label)
              in
              let simple_posts =
                List.map (fun post -> (post, (label, None))) posts'
              in
              let post_up = UP.init known_vars SS.empty preds simple_posts in
              L.verbose (fun m -> m "END of STEP 4@\n");
              match post_up with
              | Error errs ->
                  let msg =
                    Printf.sprintf
                      "WARNING: testify failed for %s. Cause: post_up.\n" name
                  in
                  Printf.printf "%s" msg;
                  L.verbose (fun m -> m "%s" msg);
                  (None, None)
              | Ok post_up ->
                  let test =
                    {
                      name;
                      id;
                      params;
                      pre_state = ss_pre;
                      post_up;
                      flag;
                      spec_vars;
                    }
                  in
                  let pre' = Asrt.star (SPState.to_assertions ss_pre) in
                  (Some test, Some (pre', posts')) ) )
    with Failure msg ->
      let new_msg =
        Printf.sprintf
          "WARNING: testify failed for %s. Cause: normalisation with msg: %s.\n"
          name msg
      in
      Printf.printf "%s" new_msg;
      L.normal (fun m -> m "%s" new_msg);
      (None, None)

  let testify_sspec
      (preds : (string, Pred.t) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (sspec : Spec.st) : t option * Spec.st option =
    let stest, sspec' =
      testify preds name params id sspec.ss_pre sspec.ss_posts
        (Some sspec.ss_flag)
        (Spec.label_vars_to_set sspec.ss_label)
        sspec.ss_to_verify
    in
    let sspec' =
      Option.map
        (fun (pre, posts) -> { sspec with ss_pre = pre; ss_posts = posts })
        sspec'
    in
    (stest, sspec')

  let testify_spec (preds : (string, Pred.t) Hashtbl.t) (spec : Spec.t) :
      t list * Spec.t =
    match spec.spec_to_verify with
    | false -> ([], spec)
    | true  ->
        let () =
          List.iter
            (fun (sspec : Spec.st) ->
              if sspec.ss_posts = [] then
                failwith
                  ( "Specification without post-condition for function "
                  ^ spec.spec_name ))
            spec.spec_sspecs
        in
        L.verbose (fun m ->
            m
              ( "-------------------------------------------------------------------------@\n"
              ^^ "Creating symbolic tests for procedure %s: %d cases\n"
              ^^ "-------------------------------------------------------------------------"
              )
              spec.spec_name
              (List.length spec.spec_sspecs));
        let tests, sspecs =
          List.split
            (List.mapi
               (testify_sspec preds spec.spec_name spec.spec_params)
               spec.spec_sspecs)
        in
        let tests = List_utils.get_list_somes tests in
        let spec_sspecs = List_utils.get_list_somes sspecs in
        let new_spec = { spec with spec_sspecs } in
        L.verbose (fun m -> m "Simplified SPECS:@\n@[%a@]@\n" Spec.pp new_spec);
        (tests, new_spec)

  let testify_lemma (preds : (string, Pred.t) Hashtbl.t) (lemma : Lemma.t) :
      t list * Lemma.t =
    let test, sspec =
      testify preds lemma.lemma_name lemma.lemma_params 0 lemma.lemma_hyp
        lemma.lemma_concs None None true
    in
    let tests = Option.fold ~some:(fun test -> [ test ]) ~none:[] test in
    match sspec with
    | Some (lemma_hyp, lemma_concs) ->
        (tests, { lemma with lemma_hyp; lemma_concs })
    | None ->
        raise
          (Failure
             (Printf.sprintf "Could not testify lemma %s" lemma.lemma_name))

  let analyse_result (subst : SSubst.t) (test : t) (state : SPState.t) : bool =
    let _ = SPState.simplify state in
    let subst = SSubst.copy subst in

    (* Adding spec vars in the post to the subst - these are effectively the existentials of the post *)
    List.iter
      (fun x ->
        if not (SSubst.mem subst x) then SSubst.add subst x (Expr.LVar x))
      (Var.Set.elements (SPState.get_spec_vars state));

    (* TODO: Understand if this should be done: setup all program variables in the subst *)
    SStore.iter (SPState.get_store state) (fun v value ->
        if not (SSubst.mem subst v) then SSubst.put subst v value);

    (* Option.may (fun v_ret -> SSubst.put subst Names.return_variable v_ret)
       (SStore.get (SState.get_store state) Names.return_variable); *)
    L.verbose (fun m ->
        m "Analyse result: About to unify one postcondition of %s. post: %a"
          test.name UP.pp test.post_up);
    match SPState.unify state subst test.post_up with
    | true  ->
        L.verbose (fun m ->
            m "Analyse result: Postcondition unified successfully");
        VerificationResults.set_result global_results test.name test.id true;
        true
    | false ->
        L.normal (fun m -> m "Analyse result: Postcondition not unifiable.");
        VerificationResults.set_result global_results test.name test.id false;
        false

  let make_post_subst (test : t) (post_state : SPState.t) : SSubst.t =
    let subst_lst =
      List.map
        (fun x ->
          if Names.is_aloc_name x then (x, Expr.ALoc x) else (x, Expr.LVar x))
        (SS.elements test.spec_vars)
    in
    let params_subst_lst = SStore.bindings (SPState.get_store post_state) in
    let subst = SSubst.init (subst_lst @ params_subst_lst) in
    subst

  let analyse_proc_results
      (test : t) (flag : Flag.t) (rets : SAInterpreter.result_t list) : bool =
    let success : bool =
      rets <> []
      && List.fold_left
           (fun ac result ->
             match (result : SAInterpreter.result_t) with
             | ExecRes.RFail (proc, i, state, errs) ->
                 L.verbose (fun m ->
                     m
                       "VERIFICATION FAILURE: Procedure %s, Command %d\n\
                        Spec %s %d\n\
                        @[<v 2>State:@\n\
                        %a@]@\n\
                        @[<v 2>Errors:@\n\
                        %a@]@\n"
                       proc i test.name test.id SPState.pp state
                       Fmt.(list ~sep:(any "@\n") SAInterpreter.pp_err)
                       errs);
                 false
             | ExecRes.RSucc (fl, v, state) ->
                 if Some fl <> test.flag then (
                   L.normal (fun m ->
                       m
                         "VERIFICATION FAILURE: Spec %s %d terminated with \
                          flag %s instead of %s\n"
                         test.name test.id (Flag.str fl) (Flag.str flag));
                   false )
                 else
                   let subst = make_post_subst test state in
                   if analyse_result subst test state then (
                     L.normal (fun m ->
                         m
                           "VERIFICATION SUCCESS: Spec %s %d terminated \
                            successfully\n"
                           test.name test.id);
                     ac )
                   else (
                     L.normal (fun m ->
                         m
                           "VERIFICATION FAILURE: Spec %s %d - post condition \
                            not unifiable\n"
                           test.name test.id);
                     false ))
           true rets
    in
    if rets = [] then (
      L.(
        normal (fun m ->
            m "ERROR: Function %s evaluates to 0 results." test.name));
      exit 1 );
    print_success_or_failure success;
    success

  let analyse_lemma_results (test : t) (rets : SPState.t list) : bool =
    let success : bool =
      rets <> []
      && List.fold_left
           (fun ac final_state ->
             let subst = make_post_subst test final_state in
             if analyse_result subst test final_state then (
               L.normal (fun m ->
                   m
                     "VERIFICATION SUCCESS: Spec %s %d terminated successfully\n"
                     test.name test.id);
               ac )
             else (
               L.normal (fun m ->
                   m
                     "VERIFICATION FAILURE: Spec %s %d - post condition not \
                      unifiable\n"
                     test.name test.id);
               false ))
           true rets
    in
    if rets = [] then (
      L.(
        normal (fun m ->
            m "ERROR: Function %s evaluates to 0 results." test.name));
      exit 1 );
    print_success_or_failure success;
    success

  let verify (prog : UP.prog) (test : t) : bool =
    let state' = SPState.add_pred_defs prog.preds test.pre_state in

    (* Printf.printf "Inside verify with a test for %s\n" test.name; *)
    match test.flag with
    | Some flag ->
        let msg = "Verifying one spec of procedure " ^ test.name ^ "... " in
        L.tmi (fun fmt -> fmt "%s" msg);
        Fmt.pr "%s" msg;
        (* TEST for procedure *)
        let rets =
          SAInterpreter.evaluate_proc
            (fun x -> x)
            prog test.name test.params state'
        in
        L.verbose (fun m ->
            m "Verification: Concluded evaluation: %d obtained results.%a@\n"
              (List.length rets) SAInterpreter.pp_result rets);
        analyse_proc_results test flag rets
    | None      -> (
        let lemma = Prog.get_lemma_exn prog.prog test.name in
        match lemma.lemma_proof with
        | None       ->
            if !Config.lemma_proof then
              raise
                (Failure (Printf.sprintf "Lemma %s WITHOUT proof" test.name))
            else true (* It's already correct *)
        | Some proof ->
            let msg = "Verifying lemma " ^ test.name ^ "... " in
            L.tmi (fun fmt -> fmt "%s" msg);
            Fmt.pr "%s" msg;
            let rets = SAInterpreter.evaluate_lcmds prog proof state' in
            analyse_lemma_results test rets )

  let pred_extracting_visitor =
    object
      inherit [_] Visitors.reduce

      inherit Visitors.Utils.ss_monoid

      method! visit_Pred _ pred_name _ = SS.singleton pred_name

      method! visit_Fold _ pred_name _ _ = SS.singleton pred_name

      method! visit_Unfold _ pred_name _ _ _ = SS.singleton pred_name

      method! visit_GUnfold _ pred_name = SS.singleton pred_name
    end

  let filter_internal_preds (prog : UP.prog) (pred_names : SS.t) =
    SS.filter
      (fun pred_name ->
        let pred = Prog.get_pred_exn prog.prog pred_name in
        not pred.pred_internal)
      pred_names

  let lemma_extracting_visitor =
    object
      inherit [_] Visitors.reduce

      inherit Visitors.Utils.ss_monoid

      method! visit_ApplyLem _ lemma_name _ _ = SS.singleton lemma_name
    end

  let filter_internal_lemmas (prog : UP.prog) (lemma_names : SS.t) =
    SS.filter
      (fun lemma_name ->
        let lemma = Prog.get_lemma_exn prog.prog lemma_name in
        not lemma.lemma_internal)
      lemma_names

  let record_proc_dependencies proc_name (prog : UP.prog) =
    let proc = Prog.get_proc_exn prog.prog proc_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_proc () proc)
    in
    let lemmas_used =
      filter_internal_lemmas prog (lemma_extracting_visitor#visit_proc () proc)
    in
    SS.iter
      (CallGraph.add_proc_pred_use SAInterpreter.call_graph proc_name)
      preds_used;
    SS.iter
      (CallGraph.add_proc_lemma_use SAInterpreter.call_graph proc_name)
      lemmas_used

  let record_lemma_dependencies lemma_name (prog : UP.prog) =
    let lemma = Prog.get_lemma_exn prog.prog lemma_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_lemma () lemma)
    in
    let lemmas_used =
      filter_internal_lemmas prog
        (lemma_extracting_visitor#visit_lemma () lemma)
    in
    SS.iter
      (CallGraph.add_lemma_pred_use SAInterpreter.call_graph lemma_name)
      preds_used;
    SS.iter
      (CallGraph.add_lemma_call SAInterpreter.call_graph lemma_name)
      lemmas_used

  let record_preds_used_by_pred pred_name (prog : UP.prog) =
    let pred = Prog.get_pred_exn prog.prog pred_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_pred () pred)
    in
    SS.iter
      (CallGraph.add_pred_call SAInterpreter.call_graph pred_name)
      preds_used

  let check_previously_verified prev_results cur_verified =
    Option.fold ~none:true
      ~some:(fun res ->
        VerificationResults.check_previously_verified
          ~printer:print_success_or_failure res cur_verified)
      prev_results

  let verify_procs
      ?(prev_results : VerificationResults.t option)
      (prog : (Annot.t, int) Prog.t)
      (pnames_to_verify : SS.t)
      (lnames_to_verify : SS.t) : unit =
    let preds = prog.preds in
    let start_time = Sys.time () in

    (* STEP 1: Get the specs to verify *)
    Printf.printf "Obtaining specs to verify...\n";
    let specs_to_verify =
      List.filter
        (fun (spec : Spec.t) -> SS.mem spec.spec_name pnames_to_verify)
        (Prog.get_specs prog)
    in

    (* STEP 2: Convert specs to symbolic tests *)
    (* Printf.printf "Converting symbolic tests from specs: %f\n" (cur_time -. start_time); *)
    let tests : t list =
      List.concat
        (List.map
           (fun spec ->
             let tests, new_spec = testify_spec preds spec in
             let proc = Prog.get_proc_exn prog spec.spec_name in
             Hashtbl.replace prog.procs proc.proc_name
               { proc with proc_spec = Some new_spec };
             tests)
           specs_to_verify)
    in

    (* STEP 3: Get the lemmas to verify *)
    Printf.printf "Obtaining lemmas to verify...\n";
    let lemmas_to_verify =
      List.filter
        (fun (lemma : Lemma.t) -> SS.mem lemma.lemma_name lnames_to_verify)
        (Prog.get_lemmas prog)
    in

    (* STEP 4: Convert lemmas to symbolic tests *)
    (* Printf.printf "Converting symbolic tests from lemmas: %f\n" (cur_time -. start_time); *)
    let tests' : t list =
      List.concat
        (List.map
           (fun lemma ->
             let tests, new_lemma = testify_lemma preds lemma in
             Hashtbl.replace prog.lemmas lemma.lemma_name new_lemma;
             tests)
           lemmas_to_verify)
    in

    Printf.printf "Obtained %d symbolic tests in total\n"
      (List.length tests + List.length tests');

    L.verbose (fun m ->
        m
          ( "@[-------------------------------------------------------------------------@\n"
          ^^ "UNFOLDED and SIMPLIFIED SPECS and LEMMAS@\n%a@\n%a"
          ^^ "@\n\
              -------------------------------------------------------------------------@]"
          )
          Fmt.(list ~sep:(any "@\n") Spec.pp)
          (Prog.get_specs prog)
          Fmt.(list ~sep:(any "@\n") Lemma.pp)
          (Prog.get_lemmas prog));

    (* STEP 4: Create unification plans for specs and predicates *)
    (* Printf.printf "Creating unification plans: %f\n" (cur_time -. start_time); *)
    match UP.init_prog prog with
    | Error _  -> raise (Failure "Creation of unification plans failed.")
    | Ok prog' ->
        (* STEP 5: Determine static dependencies and add to call graph *)
        List.iter (fun test -> record_proc_dependencies test.name prog') tests;
        List.iter (fun test -> record_lemma_dependencies test.name prog') tests';
        Hashtbl.iter
          (fun pred_name _ -> record_preds_used_by_pred pred_name prog')
          prog'.preds;

        (* STEP 6: Run the symbolic tests *)
        let cur_time = Sys.time () in
        Printf.printf "Running symbolic tests: %f\n" (cur_time -. start_time);
        let success : bool =
          List.fold_left
            (fun ac test -> if verify prog' test then ac else false)
            true (tests' @ tests)
        in
        let end_time = Sys.time () in
        let cur_verified = SS.union pnames_to_verify lnames_to_verify in
        let success =
          success && check_previously_verified prev_results cur_verified
        in
        let msg : string =
          if success then "All specs succeeded:" else "There were failures:"
        in
        let msg : string =
          Printf.sprintf "%s %f%!" msg (end_time -. start_time)
        in
        Printf.printf "%s\n" msg;
        L.normal (fun m -> m "%s" msg)

  let verify_prog
      (prog : (Annot.t, int) Prog.t)
      (incremental : bool)
      (source_files : SourceFiles.t option) : unit =
    let f prog incremental source_files =
      let open ResultsDir in
      let open ChangeTracker in
      if incremental && prev_results_exist () then
        (* Only verify changed procedures and lemmas *)
        let cur_source_files =
          match source_files with
          | Some files -> files
          | None       -> failwith "Cannot use -a in incremental mode"
        in
        let prev_source_files, prev_call_graph, results =
          read_verif_results ()
        in
        let proc_changes, lemma_changes =
          get_verif_changes prog ~prev_source_files ~prev_call_graph
            ~cur_source_files
        in
        let procs_to_prune =
          proc_changes.changed_procs @ proc_changes.deleted_procs
          @ proc_changes.dependent_procs
        in
        let lemmas_to_prune =
          lemma_changes.changed_lemmas @ lemma_changes.deleted_lemmas
          @ lemma_changes.dependent_lemmas
        in
        let () = CallGraph.prune_procs prev_call_graph procs_to_prune in
        let () = CallGraph.prune_lemmas prev_call_graph lemmas_to_prune in
        let () =
          VerificationResults.prune results (procs_to_prune @ lemmas_to_prune)
        in
        let procs_to_verify =
          SS.of_list
            ( proc_changes.changed_procs @ proc_changes.new_procs
            @ proc_changes.dependent_procs )
        in
        let lemmas_to_verify =
          SS.of_list
            ( lemma_changes.changed_lemmas @ lemma_changes.new_lemmas
            @ lemma_changes.dependent_lemmas )
        in
        let () =
          verify_procs ~prev_results:results prog procs_to_verify
            lemmas_to_verify
        in
        let cur_call_graph = SAInterpreter.call_graph in
        let cur_results = global_results in
        let call_graph = CallGraph.merge prev_call_graph cur_call_graph in
        let results = VerificationResults.merge results cur_results in
        let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
        write_verif_results cur_source_files call_graph ~diff results
      else
        (* Analyse all procedures and lemmas *)
        let cur_source_files =
          Option.value ~default:(SourceFiles.make ()) source_files
        in
        let procs_to_verify =
          SS.of_list (Prog.get_noninternal_proc_names prog)
        in
        let lemmas_to_verify =
          SS.of_list (Prog.get_noninternal_lemma_names prog)
        in
        let () = verify_procs prog procs_to_verify lemmas_to_verify in
        let call_graph = SAInterpreter.call_graph in
        write_verif_results cur_source_files call_graph ~diff:"" global_results
    in
    L.with_normal_phase ~title:"Program verification" (fun () ->
        f prog incremental source_files)
end

module From_scratch (SMemory : SMemory.S) (External : External.S) = struct
  module INTERNAL__ = struct
    module SState = SState.Make (SMemory)
  end

  include Make
            (INTERNAL__.SState)
            (PState.Make (SVal.M) (SVal.SSubst) (SStore) (INTERNAL__.SState)
               (Preds.SPreds))
            (External)
end
