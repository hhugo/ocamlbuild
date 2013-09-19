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
open Resource

type env = Pathname.t -> Pathname.t

type build_result = (Pathname.t, exn) Outcome.t
type builder = Pathname.t list list -> build_result list
type build_order = (Pathname.t list * (build_result -> unit)) list

type action = env -> builder -> Command.t
type indirect_action = Command.t gen_action
and 'a gen_action = env -> 'a action_result
and 'a action_result

val run_action_result : builder -> 'a action_result -> 'a

val final : 'a -> 'a action_result
val direct : (builder -> 'a) -> 'a action_result
(** in a "direct style" result, the action author is free to write
    a building rule invoking the builder at any time. *)
val seq : 'b action_result -> ('b -> 'a action_result) -> 'a action_result
(** in an "indirect style" result, the action author can write his
    code in inverted, continuation-passing style. This allows the
    build engine to obtain fine-grained information about the dynamic
    dependencies, and schedule compilation better (parallelization,
    etc.). *)
val build_order : build_order -> unit action_result
val combine : 'a action_result list -> 'a list action_result

type 'a gen_rule

type rule = Pathname.t gen_rule
type rule_scheme = resource_pattern gen_rule

type 'a rule_printer = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a gen_rule -> unit

(** This exception can be raised inside the action of a rule to make the
    algorithm skip this rule. *)
exception Failed

val name_of_rule : 'a gen_rule -> string
val deps_of_rule : 'a gen_rule -> Pathname.t list
val prods_of_rule : 'a gen_rule -> 'a list
val doc_of_rule : 'a gen_rule -> string option

val indirect_rule : string ->
  ?tags:string list ->
  ?prods:string list ->
  ?deps:string list ->
  ?prod:string ->
  ?dep:string ->
  ?stamp:string ->
  ?insert:[`top | `before of string | `after of string | `bottom] ->
  Command.t gen_action -> unit

val rule : string ->
  ?tags:string list ->
  ?prods:string list ->
  ?deps:string list ->
  ?prod:string ->
  ?dep:string ->
  ?stamp:string ->
  ?insert:[`top | `before of string | `after of string | `bottom] ->
  ?doc:string ->
  action -> unit

(** [copy_rule name ?insert source destination] *)
val copy_rule : string ->
  ?insert:[`top | `before of string | `after of string | `bottom] ->
  string -> string -> unit

module Common_commands : sig
  val mv : Pathname.t -> Pathname.t -> Command.t
  val cp : Pathname.t -> Pathname.t -> Command.t
  val cp_p : Pathname.t -> Pathname.t -> Command.t
  val ln_f : Pathname.t -> Pathname.t -> Command.t
  val ln_s : Pathname.t -> Pathname.t -> Command.t
  val rm_f : Pathname.t -> Command.t
  val chmod : Command.spec -> Pathname.t -> Command.t
  val cmp : Pathname.t -> Pathname.t -> Command.t
end

val print : Format.formatter -> rule -> unit
val pretty_print : 'a rule_printer

(** For system use only *)

val subst : Resource.env -> rule_scheme -> rule
val can_produce : Pathname.t -> rule_scheme -> rule option
(* val tags_matches : Tags.t -> t -> t option *)
val compare : 'a gen_rule -> 'a gen_rule -> int

val print_rule_name : Format.formatter -> 'a gen_rule -> unit
val print_rule_contents : 'a rule_printer

val get_rules : unit -> rule_scheme list
val clear_rules : unit -> unit

val call : builder -> rule -> unit

val build_deps_of_tags : builder -> Tags.t -> Pathname.t list

val show_documentation : unit -> unit
