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
open Format
open Log
open Outcome
module Resources = Resource.Resources

exception Exit_rule_error of string
exception Failed

type env = Pathname.t -> Pathname.t

type build_result = (Pathname.t, exn) Outcome.t
type builder = Pathname.t list list -> build_result list
type build_order = (Pathname.t list * (build_result -> unit)) list
type build_wrapper = builder -> builder

type 'a gen_action = env -> 'a action_result
and 'a action_result =
| Direct of (builder -> 'a)
| Final of 'a
| Step of build_order * (unit -> 'a action_result)

let rec map_action_result f = function
  | Final result -> Final (f result)
  | Direct action -> Direct (fun builder -> f (action builder))
  | Step (order, next) ->
    Step (order, (fun () -> map_action_result f (next ())))

let rec run_action_result builder = function
  | Final result -> result
  | Direct action -> action builder
  | Step (order, next) ->
    let deps, checks = List.split order in
    Printf.eprintf "static dep: %s\n%!"
      (String.concat " & " (List.map (String.concat "|") deps));
    let results = builder deps in
    List.iter2 (fun check res -> check res) checks results;
    run_action_result builder (next ())

let final v = Final v
let direct action = Direct action
let build_order order = Step (order, fun () -> Final ())

let rec seq result continuation = match result with
  | Final v -> continuation v
  | Direct action ->
    Direct (fun builder ->
      run_action_result builder (continuation (action builder)))
  | Step (order, next) ->
    Step (order, fun () -> seq (next ()) continuation)

let rec combine action_results =
  let (finals, directs, steps) =
    let rec add (finals, directs, steps) = function
      | Final v -> (v::finals, directs, steps)
      | Direct act -> (finals, act::directs, steps)
      | Step (order, next) -> (finals, directs, (order,next)::steps) in
    List.fold_left add ([], [], []) action_results in
  let add_steps cont =
    if steps = [] then cont []
    else begin
      let deps, nexts = List.split steps in
      Step (List.flatten deps, fun () ->
        seq (combine (List.map (fun next -> next ()) nexts)) cont)
    end in
  let add_directs cont =
    if directs = [] then Final (cont [])
    else begin
      let direct_results build =
        List.map (fun action -> action build) directs in
      Direct (fun build -> cont (direct_results build))
    end in
  add_steps (fun res_steps ->
    add_directs (fun directs ->
      res_steps @ directs @ finals))

type action = env -> builder -> Command.t
type indirect_action = Command.t gen_action

type digest_command = { digest : string; command : Command.t }

type 'a gen_rule =
  { name  : string;
    deps  : Pathname.t list; (* These pathnames must be normalized *)
    prods : 'a list; (* Note that prods also contains stamp *)
    stamp : 'a option;
    doc   : string option;
    code  : digest_command gen_action }

type rule = Pathname.t gen_rule
type rule_scheme = Resource.resource_pattern gen_rule

let name_of_rule r = r.name
let deps_of_rule r = r.deps
let prods_of_rule r = r.prods
let stamp_of_rule r = r.stamp
let doc_of_rule r = r.doc

type 'a rule_printer = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a gen_rule -> unit

let compare _ _ = assert false

let print_rule_name f r = pp_print_string f r.name

let print_resource_list = List.print Resource.print

let print_rule_contents ppelt f r =
  fprintf f "@[<v2>{@ @[<2>name  =@ %S@];@ @[<2>deps  =@ %a@];@ @[<2>prods = %a@];@ @[<2>code  = <fun>@];@ @[<hov 2> doc = %s@]@]@ }"
    r.name print_resource_list r.deps (List.print ppelt)
    r.prods
    (match r.doc with
      | None -> "None"
      | Some doc -> sprintf "Some %S" doc)

let pretty_print ppelt f r =
  fprintf f "@[<hv2>rule %S@ ~deps:%a@ ~prods:%a@ "
    r.name print_resource_list r.deps (List.print ppelt) r.prods;
  begin match r.doc with
    | None -> ()
    | Some doc -> fprintf f "~doc:\"@[<hov>%a@]\"@ " pp_print_text doc
  end;
  fprintf f "<fun>@]"  

let print = print_rule_name

let subst env rule =
  let subst_resources = List.map (Resource.subst env) in
  let subst_resource_patterns = List.map (Resource.subst_pattern env) in
  let finder next_finder p = next_finder (Resource.subst_any env p) in
  let stamp = match rule.stamp with None -> None | Some x -> Some (Resource.subst_pattern env x) in
  let prods = subst_resource_patterns rule.prods in
  { name = sbprintf "%s (%a)" rule.name Resource.print_env env;
    prods = prods;
    deps =
      (* The substition should preserve normalization of pathnames *)
      subst_resources rule.deps; 
    stamp = stamp;
    doc = rule.doc;
    code = (fun env -> rule.code (finder env)) }

exception Can_produce of rule

let can_produce target rule =
  try
    List.iter begin fun resource ->
      match Resource.matchit resource target with
      | Some env -> raise (Can_produce (subst env rule))
      | None -> ()
    end rule.prods; None
  with Can_produce r -> Some r

let digest_prods r =
  List.fold_right begin fun p acc ->
    let f = Pathname.to_string (Resource.in_build_dir p) in
    if sys_file_exists f then (f, Digest.file f) :: acc else acc
  end r.prods []

let digest_deps r dyndeps =
  let buf = Buffer.create 1024 in
  let add_resource r = Buffer.add_string buf (Digest.to_hex (Resource.digest r)) in
  Buffer.add_string buf "deps:";
  List.iter add_resource r.deps;
  Buffer.add_string buf "dyndeps:";
  Resources.iter add_resource dyndeps;
  Digest.to_hex (Digest.string (Buffer.contents buf))

let digest_rule r dyndeps action =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf action.digest;
  let add_resource r = Buffer.add_string buf (Resource.digest r) in
  Buffer.add_string buf "prods:";
  List.iter add_resource r.prods;
  Buffer.add_string buf "deps:";
  List.iter add_resource r.deps;
  Buffer.add_string buf "dyndeps:";
  Resources.iter add_resource dyndeps;
  Digest.string (Buffer.contents buf)

let cached_digest r =
  try Some (Digest_cache.get ("Rule: " ^ r.name))
  with Not_found -> None

let store_digest r digest = Digest_cache.put ("Rule: " ^ r.name) digest

let print_digest f x = pp_print_string f (Digest.to_hex x)

let exists2 find p rs =
  try Some (find p rs) with Not_found -> None

let build_deps_of_tags builder tags =
  match Command.deps_of_tags tags with
  | [] -> []
  | deps -> List.map Outcome.good (builder (List.map (fun x -> [x]) deps))

let build_deps_of_tags_on_cmd command =
  let add_deps tags action =
    let deps = Command.deps_of_tags tags in
    let order = List.map (fun x -> ([x], ignore_good)) deps in
    seq (build_order order) (fun () -> action) in
  Command.fold_tags add_deps command (Final ())

let indirect_call r =
  let dyndeps = ref Resources.empty in
  (*
  let builder rs =
    let results = builder rs in
    List.map begin fun res ->
      match res with
      | Good res' ->
          let () = dprintf 10 "new dyndep for %S(%a): %S" r.name print_resource_list r.prods res' in
          dyndeps := Resources.add res' !dyndeps;
          List.iter (fun x -> Resource.Cache.add_dependency x res') r.prods;
          res
      | Bad _ -> res
    end results in
  *)
  let () = dprintf 5 "start rule %a" print r in
  let action = ref None in
  let compute_action =
    seq (r.code (fun x -> x)) & fun act ->
    action := Some act;
    build_deps_of_tags_on_cmd act.command
  in
  seq compute_action & fun () ->
  let action = match !action with
    | None -> assert false
    | Some act -> act in
  let dyndeps = !dyndeps in
  let () = dprintf 10 "dyndeps: %a" Resources.print dyndeps in
  let (reason, cached) =
    match exists2 List.find (fun r -> not (Resource.exists_in_build_dir r)) r.prods with
    | Some r -> (`cache_miss_missing_prod r, false)
    | _ ->
      begin match exists2 List.find Resource.Cache.resource_has_changed r.deps with
      | Some r -> (`cache_miss_changed_dep r, false)
      | _ ->
        begin match exists2 Resources.find_elt Resource.Cache.resource_has_changed dyndeps with
        | Some r -> (`cache_miss_changed_dyn_dep r, false)
        | _ ->
            begin match cached_digest r with
            | None -> (`cache_miss_no_digest, false)
            | Some d ->
                let rule_digest = digest_rule r dyndeps action in
                if d = rule_digest then (`cache_hit, true)
                else (`cache_miss_digest_changed(d, rule_digest), false)
            end
        end
      end
  in
  let explain_reason l =
    raw_dprintf (l+1) "mid rule %a: " print r;
    match reason with
    | `cache_miss_missing_prod r ->
          dprintf l "cache miss: a product is not in build dir (%a)" Resource.print r
    | `cache_miss_changed_dep r ->
          dprintf l "cache miss: a dependency has changed (%a)" Resource.print r
    | `cache_miss_changed_dyn_dep r ->
          dprintf l "cache miss: a dynamic dependency has changed (%a)" Resource.print r
    | `cache_miss_no_digest ->
          dprintf l "cache miss: no digest found for %S (the command, a dependency, or a product)"
            r.name
    | `cache_hit -> dprintf (l+1) "cache hit"
    | `cache_miss_digest_changed(old_d, new_d) ->
          dprintf l "cache miss: the digest has changed for %S (the command, a dependency, or a product: %a <> %a)"
            r.name print_digest old_d print_digest new_d
  in
  let prod_digests = digest_prods r in
  (if not cached then List.iter Resource.clean r.prods);
  (if !Options.nothing_should_be_rebuilt && not cached then
    (explain_reason (-1);
     let msg = sbprintf "Need to rebuild %a through the rule `%a'" print_resource_list r.prods print r in
     raise (Exit_rule_error msg)));
  explain_reason 3;
  let thunk () =
    try
      if cached then Command.execute ~pretend:true action.command
      else
        begin match r.stamp with
        | Some stamp ->
            reset_filesys_cache ();
            let digest_deps = digest_deps r dyndeps in
            with_output_file stamp (fun oc -> output_string oc digest_deps)
        | None -> ()
        end;
      List.iter (fun r -> Resource.Cache.resource_built r) r.prods;
      (if not cached then
        let new_rule_digest = digest_rule r dyndeps action in
        let new_prod_digests = digest_prods r in
        let () = store_digest r new_rule_digest in
        List.iter begin fun p ->
          let f = Pathname.to_string (Resource.in_build_dir p) in
          (try let digest = List.assoc f prod_digests in
               let new_digest = List.assoc f new_prod_digests in
               if digest <> new_digest then raise Not_found
          with Not_found -> Resource.Cache.resource_changed p)
        end r.prods);
      dprintf 5 "end rule %a" print r
    with exn -> (List.iter Resource.clean r.prods; raise exn)
  in
  begin
    if cached
    then thunk ()
    else List.iter (fun x -> Resource.Cache.suspend_resource x action.command thunk r.prods) r.prods
  end;
  Final ()

let call builder r =
  let dyndeps = ref Resources.empty in
  let builder rs =
    let results = builder rs in
    List.map begin fun res ->
      match res with
      | Good res' ->
          let () = dprintf 10 "new dyndep for %S(%a): %S" r.name print_resource_list r.prods res' in
          dyndeps := Resources.add res' !dyndeps;
          List.iter (fun x -> Resource.Cache.add_dependency x res') r.prods;
          res
      | Bad _ -> res
    end results in
  let () = dprintf 5 "start rule %a" print r in
  let action = run_action_result builder (r.code (fun x -> x)) in
  run_action_result builder (build_deps_of_tags_on_cmd action.command);
  let dyndeps = !dyndeps in
  let () = dprintf 10 "dyndeps: %a" Resources.print dyndeps in
  let (reason, cached) =
    match exists2 List.find (fun r -> not (Resource.exists_in_build_dir r)) r.prods with
    | Some r -> (`cache_miss_missing_prod r, false)
    | _ ->
      begin match exists2 List.find Resource.Cache.resource_has_changed r.deps with
      | Some r -> (`cache_miss_changed_dep r, false)
      | _ ->
        begin match exists2 Resources.find_elt Resource.Cache.resource_has_changed dyndeps with
        | Some r -> (`cache_miss_changed_dyn_dep r, false)
        | _ ->
            begin match cached_digest r with
            | None -> (`cache_miss_no_digest, false)
            | Some d ->
                let rule_digest = digest_rule r dyndeps action in
                if d = rule_digest then (`cache_hit, true)
                else (`cache_miss_digest_changed(d, rule_digest), false)
            end
        end
      end
  in
  let explain_reason l =
    raw_dprintf (l+1) "mid rule %a: " print r;
    match reason with
    | `cache_miss_missing_prod r ->
          dprintf l "cache miss: a product is not in build dir (%a)" Resource.print r
    | `cache_miss_changed_dep r ->
          dprintf l "cache miss: a dependency has changed (%a)" Resource.print r
    | `cache_miss_changed_dyn_dep r ->
          dprintf l "cache miss: a dynamic dependency has changed (%a)" Resource.print r
    | `cache_miss_no_digest ->
          dprintf l "cache miss: no digest found for %S (the command, a dependency, or a product)"
            r.name
    | `cache_hit -> dprintf (l+1) "cache hit"
    | `cache_miss_digest_changed(old_d, new_d) ->
          dprintf l "cache miss: the digest has changed for %S (the command, a dependency, or a product: %a <> %a)"
            r.name print_digest old_d print_digest new_d
  in
  let prod_digests = digest_prods r in
  (if not cached then List.iter Resource.clean r.prods);
  (if !Options.nothing_should_be_rebuilt && not cached then
    (explain_reason (-1);
     let msg = sbprintf "Need to rebuild %a through the rule `%a'" print_resource_list r.prods print r in
     raise (Exit_rule_error msg)));
  explain_reason 3;
  let thunk () =
    try
      if cached then Command.execute ~pretend:true action.command
      else
        begin match r.stamp with
        | Some stamp ->
            reset_filesys_cache ();
            let digest_deps = digest_deps r dyndeps in
            with_output_file stamp (fun oc -> output_string oc digest_deps)
        | None -> ()
        end;
      List.iter (fun r -> Resource.Cache.resource_built r) r.prods;
      (if not cached then
        let new_rule_digest = digest_rule r dyndeps action in
        let new_prod_digests = digest_prods r in
        let () = store_digest r new_rule_digest in
        List.iter begin fun p ->
          let f = Pathname.to_string (Resource.in_build_dir p) in
          (try let digest = List.assoc f prod_digests in
               let new_digest = List.assoc f new_prod_digests in
               if digest <> new_digest then raise Not_found
          with Not_found -> Resource.Cache.resource_changed p)
        end r.prods);
      dprintf 5 "end rule %a" print r
    with exn -> (List.iter Resource.clean r.prods; raise exn)
  in
  if cached
  then thunk ()
  else List.iter (fun x -> Resource.Cache.suspend_resource x action.command thunk r.prods) r.prods

let (get_rules, add_rule, clear_rules) =
  let rules = ref [] in
  (fun () -> !rules),
  begin fun pos r ->
    try
      let _ = List.find (fun x -> x.name = r.name) !rules in
      raise (Exit_rule_error (sbprintf "Rule.add_rule: already exists: (%a)" print r))
    with Not_found ->
      match pos with
      | `bottom -> rules := !rules @ [r]
      | `top -> rules := r :: !rules
      | `after s ->
          rules :=
            List.fold_right begin fun x acc ->
              if x.name = s then x :: r :: acc else x :: acc
            end !rules []
      | `before s ->
          rules :=
            List.fold_right begin fun x acc ->
              if x.name = s then r :: x :: acc else x :: acc
            end !rules []
  end,
  (fun () -> rules := [])

let indirect_rule name
    ?tags ?(prods=[]) ?(deps=[]) ?prod ?dep ?stamp ?(insert = `bottom) ?doc code =
  let () =
    match tags with
      | None -> ()
      | Some _ ->
        Log.eprintf "Warning: your ocamlbuild rule %S uses the ~tags parameter,
                     which is deprecated and ignored."
          name
  in
  let res_add import xs xopt =
    let init =
      match xopt with
      | None -> []
      | Some r -> [import r]
    in
    List.fold_right begin fun x acc ->
      let r = import x in
      if List.mem r acc then
        failwith (sprintf "in rule %s, multiple occurrences of the resource %s" name x)
      else r :: acc
    end xs init
  in
  if prods = [] && prod = None && stamp = None then raise (Exit_rule_error "Can't make a rule that produces nothing");
  let stamp, prods =
    match stamp with
    | None -> None, prods
    | Some stamp ->
        Some (Resource.import_pattern stamp), stamp :: prods
  in
  let prods = res_add Resource.import_pattern prods prod in
  let code env =
    map_action_result (fun cmd -> {
      digest = Command.digest cmd;
      command = cmd;
    }) (code env)
  in
  add_rule insert
  { name  = name;
    deps  = res_add Resource.import (* should normalize *) deps dep;
    stamp = stamp;
    doc = doc;
    prods = prods;
    code  = code }

let rule name ?tags ?prods ?deps ?prod ?dep ?stamp ?insert ?doc code =
  indirect_rule name ?tags ?prods ?deps ?prod ?dep ?stamp ?insert ?doc
    (fun env -> Direct (fun build -> code env build))

module Common_commands = struct
  open Command
  let mv src dest = Cmd (S [A"mv"; P src; Px dest])
  let cp src dest = Cmd (S [A"cp"; P src; Px dest])
  let cp_p src dest = Cmd (S [A"cp"; A"-p"; P src; Px dest])
  let ln_f pointed pointer = Cmd (S [A"ln"; A"-f"; P pointed; Px pointer])
  let ln_s pointed pointer = Cmd (S[A"ln"; A"-s"; P pointed; Px pointer])
  let rm_f x = Cmd (S [A"rm"; A"-f"; Px x])
  let chmod opts file = Cmd (S[A"chmod"; opts; Px file])
  let cmp a b = Cmd (S[A"cmp"; P a; Px b])
end
open Common_commands

let copy_rule name ?insert src dest =
  rule name ?insert ~prod:dest ~dep:src
    begin fun env _ ->
      let src = env src and dest = env dest in
      Shell.mkdir_p (Pathname.dirname dest);
      cp_p src dest
    end

let show_documentation () =
  let pp fmt = Log.raw_dprintf (-1) fmt in
  let rules = get_rules () in
  List.iter
    (fun rule -> pp "%a@\n@\n" (pretty_print Resource.print_pattern) rule)
    rules;
  pp "@."
   

