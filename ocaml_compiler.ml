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
open Pathname.Operators
open Tools
open Command
open Rule
open Tags.Operators
open Ocaml_utils
open Rule.Common_commands
open Outcome


let library_index = Hashtbl.create 32
let package_index = Hashtbl.create 32
let hidden_packages = ref []

let hide_package_contents package =
  hidden_packages := package :: !hidden_packages

let forpack_flags arg tags =
  if Tags.mem "pack" tags then
    Ocaml_arch.forpack_flags_of_pathname arg
  else N

let ocamlc_c tags arg out =
  let tags = tags++"ocaml"++"byte" in
  Cmd (S [!Options.ocamlc; A"-c"; T(tags++"compile");
          ocaml_ppflags tags; ocaml_include_flags arg; A"-o"; Px out; P arg])

let ocamlc_link flag tags deps out =
  Cmd (S [!Options.ocamlc; flag; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlc_link_lib = ocamlc_link (A"-a")
let ocamlc_link_prog = ocamlc_link N

let ocamlmklib tags deps out =
  Cmd (S [!Options.ocamlmklib; T tags;
          atomize_paths deps; A"-o"; Px (Pathname.remove_extensions out)])

let ocamlmktop tags deps out =
  Cmd( S [!Options.ocamlmktop; T (tags++"mktop");
          atomize_paths deps; A"-o"; Px out])

let byte_lib_linker tags =
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
    ocamlc_link_lib tags

let byte_lib_linker_tags tags = tags++"ocaml"++"link"++"byte"++"library"

let ocamlc_p tags deps out =
  Cmd (S [!Options.ocamlc; A"-pack"; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlopt_c tags arg out =
  let tags = tags++"ocaml"++"native" in
  Cmd (S [!Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname arg;
          T(tags++"compile"); ocaml_ppflags tags; ocaml_include_flags arg;
          A"-o"; Px out (* FIXME ocamlopt bug -o cannot be after the input file *); P arg])

let ocamlopt_link flag tags deps out =
  Cmd (S [!Options.ocamlopt; flag; forpack_flags out tags; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlopt_link_lib = ocamlopt_link (A"-a")
let ocamlopt_link_shared_lib = ocamlopt_link (A"-shared")
let ocamlopt_link_prog = ocamlopt_link N

let ocamlopt_p tags deps out =
  let dirnames = List.union [] (List.map Pathname.dirname deps) in
  let include_flags = List.fold_right ocaml_add_include_flag dirnames [] in
  let mli = Pathname.update_extensions "mli" out in
  let cmd =
    S [!Options.ocamlopt; A"-pack"; forpack_flags out tags; T tags;
       S include_flags; atomize_paths deps;
       A"-o"; Px out] in
  if (*FIXME true ||*) Pathname.exists mli then Cmd cmd
  else
    let rm = S[A"rm"; A"-f"; P mli] in
    Cmd(S[A"touch"; P mli; Sh" ; if "; cmd; Sh" ; then "; rm; Sh" ; else ";
          rm; Sh" ; exit 1; fi"])

let native_lib_linker tags =
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
    ocamlopt_link_lib tags

let native_shared_lib_linker tags =
(* ocamlmklib seems to not support -shared, is this OK?
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
*)
    ocamlopt_link_shared_lib tags

let native_lib_linker_tags tags = tags++"ocaml"++"link"++"native"++"library"


module Indirect = struct
let prepare_compile ml =
  let dir = Pathname.dirname ml in
  let include_dirs = Pathname.include_dirs_of dir in
  let modules = path_dependencies_of ml in
  let module_build_order (mandatory, name) =
    (expand_module include_dirs name ["cmi"],
     fun result -> match mandatory, result with
    | _, Good _ -> ()
    | `mandatory, Bad exn ->
        if not !Options.ignore_auto then raise exn;
        dprintf 3
          "Warning: Failed to build the module %s requested by ocamldep."
          name;
        if not (!Options.recursive || Options.ocamlbuild_project_heuristic ())
        then Log.at_failure ~name:"a module failed to build,
           while recursive traversal was disabled by fragile heuristic;
           hint that having a _tags or myocamlbuild.ml would maybe solve
           the build error"
          (fun `Error ->
            eprintf "Hint:@ Recursive@ traversal@ of@ subdirectories@ \
              was@ not@ enabled@ for@ this@ build,@ as@ the@ working@ \
              directory does@ not@ look@ like@ an@ ocamlbuild@ project@ \
              (no@ '_tags'@ or@ 'myocamlbuild.ml'@ file).@ \
              If@ you@ have@ modules@ in@ subdirectories,@ you@ should@ add@ \
              the@ option@ \"-r\"@ or@ create@ an@ empty@ '_tags'@ file.@\n\
              @\n\
              To@ enable@ recursive@ traversal@ for@ some@ subdirectories@ \
              only,@ you@ can@ use@ the@ following@ '_tags'@ file:@\n\
              @[<v 4>@,\
                true: -traverse@,\
                <dir1> or <dir2>: traverse@,\
              @]"
          );
    | `just_try, Bad _ -> ()
  in build_order (List.map module_build_order modules)

let byte_compile_ocaml_interf mli cmi env (* build *) =
  let mli = env mli and cmi = env cmi in
  seq (prepare_compile mli) & fun () ->
  final (ocamlc_c (tags_of_pathname mli++"interf") mli cmi)

(* given that .cmi can be built from either ocamlc and ocamlopt, this
   "agnostic" rule chooses either compilers depending on whether the
   "native" tag is present. This was requested during PR#4613 as way
   to enable using ocamlbuild in environments where only ocamlopt is
   available, not ocamlc. *)
let compile_ocaml_interf mli cmi env =
  let mli = env mli and cmi = env cmi in
  seq (prepare_compile mli) & fun () ->
  let tags = tags_of_pathname mli++"interf" in
  let comp_c = if Tags.mem "native" tags then ocamlopt_c else ocamlc_c in
  final (comp_c tags mli cmi)

let byte_compile_ocaml_implem ?tag ml cmo env =
  let ml = env ml and cmo = env cmo in
  seq (prepare_compile ml) & fun () ->
  let tags =
    Tags.union (tags_of_pathname ml) (tags_of_pathname cmo)
    ++"implem"+++tag in
  final (ocamlc_c tags ml cmo)

let cache_prepare_link = Hashtbl.create 107
let rec prepare_link tag cmx extensions =
  let key = (tag, cmx, extensions) in
  let dir = Pathname.dirname cmx in
  let include_dirs = Pathname.include_dirs_of dir in
  let ml = Pathname.update_extensions "ml" cmx in
  let mli = Pathname.update_extensions "mli" cmx in
  let modules =
    List.union
      (if Pathname.exists (ml-.-"depends")
       then path_dependencies_of ml else [])
      (if Pathname.exists (mli-.-"depends")
       then path_dependencies_of mli else [])
  in
  let modules =
    if (modules = []) && (Pathname.exists (ml^"pack")) then
      List.map (fun s -> (`mandatory, s)) (string_list_of_file (ml^"pack"))
    else
      modules
  in
  if modules = [] || Hashtbl.mem cache_prepare_link key
  then final ()
  else begin
    let () = Hashtbl.add cache_prepare_link key true in
    let recursive_deps = ref [] in
    let modules_build_order =
      let module_order (mandatory, x) =
        (expand_module include_dirs x extensions,
         fun result -> match mandatory, result with
           | _, Good p -> recursive_deps := p :: !recursive_deps
           | `mandatory, Bad exn -> if not !Options.ignore_auto then raise exn
           | `just_try, Bad _ -> ())
      in List.map module_order modules in
    seq (build_order (modules_build_order)) & fun () ->
    let deps_action_results =
      List.map (fun p -> prepare_link tag p extensions) !recursive_deps in
    seq (combine (deps_action_results)) & fun results ->
    final (List.iter (fun () -> ()) results)
  end

let native_compile_ocaml_implem ?tag ?(cmx_ext="cmx") ml env =
  let ml = env ml in
  let cmi = Pathname.update_extensions "cmi" ml in
  let cmx = Pathname.update_extensions cmx_ext ml in
  seq (prepare_link cmx cmi [cmx_ext; "cmi"]) & fun () ->
  let tags =
    Tags.union (tags_of_pathname ml) (tags_of_pathname cmx)
    ++"implem"+++tag in
  final (ocamlopt_c tags ml cmx)

let libs_of_use_lib tags =
  Tags.fold begin fun tag acc ->
    try let libpath, extern = Hashtbl.find info_libraries tag in
        if extern then acc else libpath :: acc
    with Not_found -> acc
  end tags []

let ignore_good_order targets =
  List.map (fun lib -> lib, ignore_good) targets


let prepare_libs cma_ext a_ext out =
  let out_no_ext = Pathname.remove_extension out in
  let libs1 = List.union (libraries_of out_no_ext) (libs_of_use_lib (tags_of_pathname out)) in
  let () = dprintf 10 "prepare_libs: %S -> %a" out pp_l libs1 in
  let libs = List.map (fun x -> x-.-cma_ext) libs1 in
  let libs2 = List.map (fun lib -> [lib-.-a_ext]) libs1 in
  seq (build_order (ignore_good_order libs2)) & fun () ->
  final libs

module Ocaml_dependencies_input = struct
  let fold_dependencies = Resource.Cache.fold_dependencies
  let fold_libraries f = Hashtbl.fold f library_index
  let fold_packages f = Hashtbl.fold f package_index
end
module Ocaml_dependencies = Ocaml_dependencies.Make(Ocaml_dependencies_input)

let caml_transitive_closure = Ocaml_dependencies.caml_transitive_closure

let link_one_gen linker tagger cmX out env =
  let cmX = env cmX and out = env out in
  let tags = tagger (tags_of_pathname out) in
  final (linker tags [cmX] out)

let link_gen cmX_ext cma_ext a_ext extensions linker tagger cmX out env =
  let cmX = env cmX and out = env out in
  let tags = tagger (tags_of_pathname out) in
  let dyndeps_action =
    let dyndeps = Command.deps_of_tags (tags++"link_with") in
    let results = ref [] in
    let record dep =
      ([dep], fun result -> results := Outcome.good result :: !results) in
    seq (build_order (List.map record dyndeps)) & fun () ->
    final !results in
  seq dyndeps_action & fun dyndeps ->
  let cmi = Pathname.update_extensions "cmi" cmX in
  seq (prepare_link cmX cmi extensions) & fun () ->
  seq (prepare_libs cma_ext a_ext out) & fun libs ->
  let hidden_packages = List.map (fun x -> x-.-cmX_ext) !hidden_packages in
  let deps =
    caml_transitive_closure
      ~caml_obj_ext:cmX_ext ~caml_lib_ext:cma_ext
      ~used_libraries:libs ~hidden_packages (cmX :: dyndeps) in
  let deps = (List.filter (fun l -> not (List.mem l deps)) libs) @ deps in

  (* Hack to avoid linking twice with the standard library. *)
  let stdlib = "stdlib/stdlib"-.-cma_ext in
  let is_not_stdlib x = x <> stdlib in
  let deps = List.filter is_not_stdlib deps in

  if deps = [] then failwith "Link list cannot be empty";
  let () = dprintf 6 "link: %a -o %a" print_string_list deps Pathname.print out in
  final (linker (tags++"dont_link_with") deps out)

let byte_link_gen = link_gen "cmo" "cma" "cma" ["cmo"; "cmi"]

let byte_link = byte_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"program")

let byte_output_obj = byte_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"output_obj")

let byte_library_link = byte_link_gen byte_lib_linker byte_lib_linker_tags

let byte_debug_link_gen =
  link_gen "d.cmo" "d.cma" "d.cma" ["d.cmo"; "cmi"]

let byte_debug_link = byte_debug_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"debug"++"program")

let byte_debug_library_link = byte_debug_link_gen byte_lib_linker
  (fun tags -> byte_lib_linker_tags tags++"debug")

let native_link_gen linker =
  link_gen "cmx" "cmxa" !Options.ext_lib [!Options.ext_obj; "cmi"] linker

let native_link x = native_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"program") x

let native_output_obj x = native_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"output_obj") x

let native_library_link x =
  native_link_gen native_lib_linker native_lib_linker_tags x

let native_profile_link_gen linker =
  link_gen "p.cmx" "p.cmxa" ("p" -.- !Options.ext_lib) ["p" -.- !Options.ext_obj; "cmi"] linker

let native_profile_link x = native_profile_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"profile"++"program") x

let native_profile_library_link x = native_profile_link_gen native_lib_linker
  (fun tags -> native_lib_linker_tags tags++"profile") x

let link_units table extensions cmX_ext cma_ext a_ext linker tagger contents_list cmX env =
  let cmX = env cmX in
  let tags = tagger (tags_of_pathname cmX) in
  let order =
    let singleton x = [x] in
    ignore_good_order (List.map singleton (Command.deps_of_tags tags)) in
  seq (build_order order) & fun () ->
  let dir =
    let dir1 = Pathname.remove_extensions cmX in
    if Resource.exists_in_source_dir dir1 then dir1
    else Pathname.dirname cmX in
  let include_dirs = Pathname.include_dirs_of dir in
  let extension_keys = List.map fst extensions in
  seq (prepare_libs cma_ext a_ext cmX) & fun libs ->
  let to_update = ref [] in
  let make_order module_name =
    (expand_module include_dirs module_name extension_keys,
     function
       | Bad exn -> raise exn
       | Good p -> to_update := p :: !to_update) in
  seq (build_order (List.map make_order contents_list)) & fun () ->
  let module_paths = !to_update in
  let update_order p =
    (* List.iter ignore_good (build [[Pathname.update_extensions ext p]]) *)
    let extension_values = List.assoc (Pathname.get_extensions p) extensions in
    match extension_values with
      | [] -> []
      | [x] -> [[x], ignore_good]
      | _::_::_ -> failwith "Rule.link_units: case not implemented yet" in
  let total_order = List.concat & List.map update_order module_paths in
  seq (build_order total_order) & fun () ->
  Hashtbl.replace table cmX module_paths;
  let hidden_packages = List.map (fun x -> x-.-cmX_ext) !hidden_packages in
  let deps =
    caml_transitive_closure
      ~caml_obj_ext:cmX_ext ~caml_lib_ext:cma_ext
      ~hidden_packages ~pack_mode:true module_paths in
  let full_contents = libs @ module_paths in
  let deps = List.filter (fun x -> List.mem x full_contents) deps in
  let deps = (List.filter (fun l -> not (List.mem l deps)) libs) @ deps in

  (* Hack to avoid linking twice with the standard library. *)
  let stdlib = "stdlib/stdlib"-.-cma_ext in
  let is_not_stdlib x = x <> stdlib in
  let deps = List.filter is_not_stdlib deps in

  final (linker tags deps cmX)

let link_modules = link_units library_index
let pack_modules = link_units package_index

let link_from_file link modules_file cmX env =
  let modules_file = env modules_file in
  let contents_list = string_list_of_file modules_file in
  link contents_list cmX env

let byte_library_link_modules =
  link_modules [("cmo",[])] "cmo" "cma" "cma" byte_lib_linker byte_lib_linker_tags

let byte_library_link_mllib = link_from_file byte_library_link_modules

let byte_toplevel_link_modules =
  link_modules [("cmo",[])] "cmo" "cma" "cma" ocamlmktop
               (fun tags -> tags++"ocaml"++"link"++"byte"++"toplevel")

let byte_toplevel_link_mltop = link_from_file byte_toplevel_link_modules

let byte_debug_library_link_modules =
  link_modules [("d.cmo",[])] "d.cmo" "d.cma" "d.cma" byte_lib_linker
    (fun tags -> byte_lib_linker_tags tags++"debug")

let byte_debug_library_link_mllib = link_from_file byte_debug_library_link_modules

let byte_pack_modules =
  pack_modules [("cmo",["cmi"]); ("cmi",[])] "cmo" "cma" "cma" ocamlc_p
    (fun tags -> tags++"ocaml"++"pack"++"byte")

let byte_pack_mlpack = link_from_file byte_pack_modules

let byte_debug_pack_modules =
  pack_modules [("d.cmo",["cmi"]); ("cmi",[])] "d.cmo" "d.cma" "d.cma" ocamlc_p
    (fun tags -> tags++"ocaml"++"pack"++"byte"++"debug")

let byte_debug_pack_mlpack = link_from_file byte_debug_pack_modules

let native_pack_modules x =
  pack_modules [("cmx",["cmi"; !Options.ext_obj]); ("cmi",[])] "cmx" "cmxa" !Options.ext_lib ocamlopt_p
    (fun tags -> tags++"ocaml"++"pack"++"native") x

let native_pack_mlpack = link_from_file native_pack_modules

let native_profile_pack_modules x =
  pack_modules [("p.cmx",["cmi"; "p" -.- !Options.ext_obj]); ("cmi",[])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) ocamlopt_p
    (fun tags -> tags++"ocaml"++"pack"++"native"++"profile") x

let native_profile_pack_mlpack = link_from_file native_profile_pack_modules

let native_library_link_modules x =
  link_modules [("cmx",[!Options.ext_obj])] "cmx" "cmxa"
     !Options.ext_lib native_lib_linker native_lib_linker_tags x

let native_shared_library_link_modules x =
  link_modules [("cmx",[!Options.ext_obj])] "cmx" "cmxa"
     !Options.ext_lib native_shared_lib_linker
     (fun tags -> native_lib_linker_tags tags++"shared") x

let native_library_link_mllib = link_from_file native_library_link_modules

let native_shared_library_link_mldylib = link_from_file native_shared_library_link_modules

let native_shared_library_tags tags basetags =
  List.fold_left (++) (basetags++"ocaml"++"link"++"native"++"shared"++"library") tags

let native_shared_library_link ?(tags = []) x =
  link_one_gen native_shared_lib_linker
    (native_shared_library_tags tags) x

let native_profile_library_link_modules x =
  link_modules [("p.cmx",["p" -.- !Options.ext_obj])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) native_lib_linker
    (fun tags -> native_lib_linker_tags tags++"profile") x

let native_profile_shared_library_link_modules x =
  link_modules [("p.cmx",["p" -.- !Options.ext_obj])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) native_shared_lib_linker
    (fun tags -> native_lib_linker_tags tags++"shared"++"profile") x

let native_profile_library_link_mllib = link_from_file native_profile_library_link_modules

let native_profile_shared_library_link_mldylib = link_from_file native_profile_shared_library_link_modules
end

let prepare_compile build p =
  run_action_result build (Indirect.prepare_compile p)

let convert f = fun env build ->
  run_action_result build (f env)

let convert1 f = fun x -> convert (f x)
let convert2 f = fun x y -> convert (f x y)
let convert3 f = fun x y z -> convert (f x y z)

let compile_ocaml_interf = convert2 Indirect.compile_ocaml_interf
let byte_compile_ocaml_interf = convert2 Indirect.byte_compile_ocaml_interf
let byte_compile_ocaml_implem ?tag =
  convert2 (Indirect.byte_compile_ocaml_implem ?tag)
let prepare_link = convert2 Indirect.prepare_link
let native_compile_ocaml_implem ?tag ?cmx_ext =
  convert1 (Indirect.native_compile_ocaml_implem ?tag ?cmx_ext)
let prepare_libs = convert2 Indirect.prepare_libs
let byte_link = convert2 Indirect.byte_link
let byte_output_obj = convert2 Indirect.byte_output_obj
let byte_library_link = convert2 Indirect.byte_library_link
let native_link = convert2 Indirect.native_link
let native_output_obj = convert2 Indirect.native_output_obj
let native_library_link = convert2 Indirect.native_library_link
let native_shared_library_link ?tags =
  convert2 (Indirect.native_shared_library_link ?tags)
let native_profile_link = convert2 Indirect.native_profile_link
let native_profile_library_link = convert2 Indirect.native_profile_library_link
let byte_library_link_modules = convert2 Indirect.byte_library_link_modules
let byte_library_link_mllib = convert2 Indirect.byte_library_link_mllib

let byte_debug_link = convert2 Indirect.byte_debug_link
let byte_debug_library_link = convert2 Indirect.byte_debug_library_link
let byte_debug_library_link_modules = convert2 Indirect.byte_debug_library_link_modules
let byte_debug_library_link_mllib = convert2 Indirect.byte_debug_library_link_mllib
let byte_pack_modules = convert2 Indirect.byte_pack_modules
let byte_pack_mlpack = convert2 Indirect.byte_pack_mlpack
let byte_debug_pack_modules = convert2 Indirect.byte_debug_pack_modules
let byte_debug_pack_mlpack = convert2 Indirect.byte_debug_pack_mlpack
let byte_toplevel_link_modules = convert2 Indirect.byte_toplevel_link_modules
let byte_toplevel_link_mltop = convert2 Indirect.byte_toplevel_link_mltop
let native_pack_modules = convert2 Indirect.native_pack_modules
let native_pack_mlpack = convert2 Indirect.native_pack_mlpack
let native_library_link_modules = convert2 Indirect.native_library_link_modules
let native_library_link_mllib = convert2 Indirect.native_library_link_mllib
let native_shared_library_link_modules = convert2 Indirect.native_shared_library_link_modules
let native_shared_library_link_mldylib = convert2 Indirect.native_shared_library_link_mldylib
let native_profile_pack_modules = convert2 Indirect.native_profile_pack_modules
let native_profile_pack_mlpack = convert2 Indirect.native_profile_pack_mlpack
let native_profile_library_link_modules = convert2 Indirect.native_profile_library_link_modules
let native_profile_library_link_mllib = convert2 Indirect.native_profile_library_link_mllib
let native_profile_shared_library_link_modules = convert2 Indirect.native_profile_shared_library_link_modules
let native_profile_shared_library_link_mldylib = convert2 Indirect.native_profile_shared_library_link_mldylib
