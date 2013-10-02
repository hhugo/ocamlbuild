(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Log
open Format
open Outcome

type backtrace =
  | Leaf of Pathname.t
  | Choice of backtrace list
  | Depth of Pathname.t * backtrace
  | Target of string * backtrace
exception Failed of backtrace
exception Circular of Pathname.t * Pathname.t list

let failed target backtrace =
  Resource.Cache.resource_failed target;
  raise (Failed backtrace)

let extract_failed = function
  | Failed trace -> trace
  | exn -> raise exn

let rec pp_repeat f (n, s) =
  if n > 0 then (pp_print_string f s; pp_repeat f (n - 1, s))

let return = Rule.return
let bind = Rule.bind

let sequence
: 'a 'b . ('a, 'b) Outcome.t Rule.action_result list
-> ('a list, 'b) Outcome.t Rule.action_result
=
  let rec seq results = function
    | [] -> return (Good (List.rev results))
    | x::xs ->
      bind x (function
        | Good res -> seq (res::results) xs
        | Bad _ as bad -> return bad)
  in fun res -> seq [] res

let sequential_alternative
:  'a 'b . ('a, 'b) Outcome.t Rule.action_result list
-> ('a, 'b list) Outcome.t Rule.action_result
=
  let rec alt failures = function
    | [] -> return (Bad (List.rev failures))
    | x::xs ->
      bind x (function
        | Good _ as good -> return good
        | Bad fail -> alt (fail::failures) xs)
  in fun res -> alt [] res

(* Targets must be normalized pathnames.
 * Recursive calls are either on input targets
 * or dependencies of these targets (returned by Rule.deps_of_rule).
 *)
let rec indirect_self depth on_the_go_orig target =
  let rules = Rule.get_rules () in
  let on_the_go = target :: on_the_go_orig in

  dprintf 4 "==%a> %a" pp_repeat (depth, "==") Resource.print target;
  if List.mem target on_the_go_orig then
    raise (Circular(target, on_the_go_orig));
  match Resource.Cache.resource_state target with
  | Resource.Cache.Bbuilt ->
    dprintf 5 "%a already built" Resource.print target;
    return (Good ())
  | Resource.Cache.Bcannot_be_built ->
    dprintf 5 "%a already failed" Resource.print target;
    return (Bad (Failed (Leaf target)))
  | Resource.Cache.Bsuspension(s) ->
    dprintf 5 "%a was suspended -> resuming" Resource.print target;
    return begin
      try Good (Resource.Cache.resume_suspension s)
      with exn -> Bad exn
    end
  | Resource.Cache.Bnot_built_yet ->
    if not (Pathname.is_relative target) && Pathname.exists target then
      if Resource.Cache.external_is_up_to_date target
      then return (Good ())
      else begin
        (* perhaps the error can be refined *)
        Resource.Cache.resource_failed target;    
        return (Bad (Failed (Leaf target)))
      end
    else
    if Resource.exists_in_source_dir target then
      return (Good (Resource.Cache.import_in_build_dir target))
    else
      let matching_rules = List.filter_opt (Rule.can_produce target) rules in
      let try_rule r =
        let make_dep dep =
          bind (indirect_self (depth + 1) on_the_go dep) & function
            | Bad _ as bad -> return bad
            | Good _ as good ->
              Resource.Cache.resume_resource dep;
              return good
        in
        bind (sequence (List.map make_dep (Rule.deps_of_rule r))) & function
          | Bad (Failed backtrace as fail) ->
            let () =
              match backtrace with
                | Depth (top_prod, _) ->
                  Resource.Cache.clear_resource_failed top_prod
                | Target _ | Choice _ | Leaf _ -> ()
            in
            return (Bad fail)
          | Bad exn -> return (Bad exn)
          | Good unit_list ->
            List.iter (fun () -> ()) unit_list;
            let indirect_builder choices =
              let build dep =
                bind (indirect_self (depth + 1) on_the_go dep) & function
                  | Bad bad -> return (Bad bad)
                  | Good () -> return (Good dep) 
                    (* indicate which of the possible filenames worked *)
              in
              bind (sequential_alternative (List.map build choices)) & function
                | Good result -> return (Good result)
                | Bad traces ->
                  try return (Bad (Failed
                        (Choice (List.map extract_failed traces))))
                  with exn -> return (Bad exn)
            in
            Rule.unroll_result indirect_builder &
            bind (Rule.indirect_call r) & fun () ->
            (* todo support something akin to Rule.Failed
               for user-decided failure *)
            return (Good ())
      in
      bind (sequential_alternative (List.map try_rule matching_rules)) & function
        | Good good -> return (Good good)
        | Bad [] ->
          Resource.Cache.resource_failed target;
          return (Bad (Failed (Leaf target)))
        | Bad traces ->
          Resource.Cache.resource_failed target;
          try return (Bad (Failed (Depth
                (target, Choice (List.map extract_failed traces)))))
          with exn -> return (Bad exn)

(* Targets must be normalized pathnames.
 * Recursive calls are either on input targets
 * or dependencies of these targets (returned by Rule.deps_of_rule).
 *)
let rec self depth on_the_go_orig target =
  let rules = Rule.get_rules () in
  let on_the_go = target :: on_the_go_orig in

  dprintf 4 "==%a> %a" pp_repeat (depth, "==") Resource.print target;
  if List.mem target on_the_go_orig then raise (Circular(target, on_the_go_orig));
  match Resource.Cache.resource_state target with
  | Resource.Cache.Bbuilt ->
      (dprintf 5 "%a already built" Resource.print target)
  | Resource.Cache.Bcannot_be_built ->
      (dprintf 5 "%a already failed" Resource.print target; failed target (Leaf target))
  | Resource.Cache.Bsuspension(s) ->
      (dprintf 5 "%a was suspended -> resuming" Resource.print target;
       Resource.Cache.resume_suspension s)
  | Resource.Cache.Bnot_built_yet ->
    if not (Pathname.is_relative target) && Pathname.exists target then
      if Resource.Cache.external_is_up_to_date target then ()
      else (* perhaps the error can be refined *) failed target (Leaf target)
    else
    if Resource.exists_in_source_dir target then
      Resource.Cache.import_in_build_dir target
    else
    match List.filter_opt (Rule.can_produce target) rules with
    | [] -> failed target (Leaf target)
    | matching_rules ->
      let rec until_works rs backtraces =
        match rs with
        | [] -> assert false
        | r :: rs ->
            try
              List.iter (force_self (depth + 1) on_the_go) (Rule.deps_of_rule r);
              try
                let rule_action = Rule.indirect_call r in
                Rule.run_action_result
                  (self_firsts (depth + 1) on_the_go)
                  rule_action;
              with Rule.Failed -> raise (Failed (Leaf target))
            with Failed backtrace ->
              if rs = [] then failed target (Depth (target, Choice (backtrace :: backtraces)))
              else
                let () =
                  match backtrace with
                  | Depth (top_prod, _) -> Resource.Cache.clear_resource_failed top_prod
                  | Target _ | Choice _ | Leaf _ -> ()
                in until_works rs (backtrace :: backtraces)
      in until_works matching_rules []

(* Build the first target that is buildable *)
and self_first depth on_the_go already_failed rs =
  match rs with
  | [] -> Bad (Failed (Choice already_failed))
  | r :: rs ->
      try self depth on_the_go r; Good r
      with Failed backtrace -> self_first depth on_the_go (backtrace :: already_failed) rs

(* This variant is the one (once partially applied) called the 'build'
 * function in the rule actions.
 *
 * This one takes a list of list of pathnames to build.
 * This is a parallel conjonction of sequential alternatives.
 * This means that in each sublist of pathnames, the first
 * target that is buildable will be picked. The outer list
 * denotes that one can build each target in parallel.
 *)
and self_firsts depth on_the_go rss =
  let results = List.map (self_first depth on_the_go []) rss in
  let cmds, thunks =
    List.fold_right begin fun res ((acc1, acc2) as acc) ->
      match res with
      | Bad _ -> acc
      | Good res ->
          match Resource.Cache.get_optional_resource_suspension res with
          | None -> acc
          | Some (cmd, thunk) -> (cmd :: acc1, thunk :: acc2)
    end results ([], []) in
  let count = List.length cmds in
  let job_debug = if !Command.jobs = 1 then 10 else 5 in
  if count > 1 then dprintf job_debug ">>> PARALLEL: %d" count;
  let opt_exn = Command.execute_many cmds in
  if count > 1 then dprintf job_debug "<<< PARALLEL";
  begin match opt_exn with
  | Some(res, exn) ->
      List.iter2 (fun res thunk -> if res then thunk ()) res thunks;
      Log.finish ~how:`Error ();
      raise exn
  | None ->
      List.iter (fun thunk -> thunk ()) thunks
  end;
  results
and force_self depth on_the_go x = self depth on_the_go x; Resource.Cache.resume_resource x

let solve = force_self 0 []
let solve_target name rs =
  match self_first 0 [] [] rs with
  | Good res -> Resource.Cache.resume_resource res; res
  | Bad (Failed backtrace) -> raise (Failed (Target (name, backtrace)))
  | Bad exn -> raise exn
