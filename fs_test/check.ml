(****************************************************************************)
(*  Copyright (c) 2013, 2014, 2015, Tom Ridge, David Sheets, Thomas Tuerk,  *)
(*  Andrea Giugliano (as part of the SibylFS project)                       *)
(*                                                                          *)
(*  Permission to use, copy, modify, and/or distribute this software for    *)
(*  any purpose with or without fee is hereby granted, provided that the    *)
(*  above copyright notice and this permission notice appear in all         *)
(*  copies.                                                                 *)
(*                                                                          *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL           *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED           *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE        *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL    *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR   *)
(*  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER          *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR        *)
(*  PERFORMANCE OF THIS SOFTWARE.                                           *)
(*                                                                          *)
(*  Meta:                                                                   *)
(*    - Headers maintained using headache.                                  *)
(*    - License source: http://opensource.org/licenses/ISC                  *)
(****************************************************************************)

(*
check: read a trace file, and check that the transitions are allowed
by the spec

    #directory "../../build_spec";;
    #directory "../../src_ext/lem/ocaml-lib/_build";;

    #use "topfind";;
    #require "unix";;
    #require "bigarray";;
    #require "num";;
    #require "str";;
    #require "sha";;

    #load "extract.cma";;
    #load "fs_spec_lib.cma";;
*)

open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
(* open Fs_spec_extras *)
open CheckLib

(******************************************************************************)
(* command line *)

let verbose = ref false
let no_warnings = ref false
let dry_run = ref false
let no_root = ref true
let out_file = ref (None : string option)
let arch = ref None
let files = ref ([]:string list)
let seed = ref None
let sexp_file = ref (None : string option)

let options = Arg.align ([
  ( "-v", 
    Arg.Set verbose,
    " verbose output");  
  ( "-nw", 
    Arg.Set no_warnings,
    " suppress warning messages");  
  ( "-n", 
    Arg.Set dry_run,
    " dry-run (only parse and print)");  
  ( "--dry-run", 
    Arg.Set dry_run,
    " dry-run (only parse and print)");  
  ( "--root", 
    Arg.Clear no_root,
    " run process 1 with superuser permissions");  
  ( "-o", 
    Arg.String (fun s -> out_file := Some s),
    " output file; converts input trace and stores it in given file");  
  ( "-arch", 
    Arg.Symbol (["posix"; "linux"; "mac_os_x"; "freebsd"], (function
    | "linux" -> (arch := Some ARCH_LINUX)
    | "posix" -> (arch := Some ARCH_POSIX)
    | "mac_os_x" -> (arch := Some ARCH_MAC_OS_X)
    | "freebsd" -> (arch := Some ARCH_FREEBSD)
    | _ -> () (* can't happen *))),
    " architecture to simulate");
  ( "--seed",
    Arg.Int (fun s -> seed := Some s),
    " use this integer for random seed");
  ( "--sexp",
    Arg.String (fun s -> sexp_file := Some s),
    " output the result of the check to this file as an S expression");
])

let usage_msg = 
    ("check 0.4\n"
     ^ "Usage: check [OPTION]... [TRACE_FILE]... \n\
Example: check -v example_traces/os_trace1.txt\n\n\
check that the traces stored in the given trace-files are accepted by
the POSIX specification.\n" 
    )

let check_args () =
  let print_err m =
    print_endline ("Error: "^m);
    Arg.usage options usage_msg;
    exit 1
  in
  if List.length (!files) = 0
  then print_err "no trace-file given"
  else if (!out_file <> None) && (List.length (!files) > 1)
  then print_err "if option '-o' is used, only one trace-file can be processed"
  else if (!arch = None)
  then print_err "-arch required"

let () =
  Arg.parse options (fun s -> (files := ((!files) @ [s]))) usage_msg;
  check_args ()

module C = CheckLib.Check_spec

(* check_args enforces *)
let arch = match !arch with Some a -> a | None -> assert false

let spec_initial_state = CheckLib.spec_initial_state arch !no_root

let comment s =
  let lines = Fs_test_util.split [] s '\n' in
  String.concat "\n" (List.map (fun l -> "# "^l) lines)

let write_out o s =
  output_string o s;
  output_string o "\n";
  flush o

let outf o_ty s = match o_ty with
  | CO_error
  | CO_warning -> write_out stderr s; write_out stdout (comment s)
  | CO_verbose
  | CO_dump
  | CO_normal
  | CO_status  -> write_out stdout s
let outf = if !verbose then outf else drop_check_output_fun CO_verbose outf
let outf = if !no_warnings then drop_check_output_fun CO_warning outf else outf

let convert_trace = C.convert_trace arch outf spec_initial_state !dry_run

(* calls can raise Parse_error from Fs_ast, Trace, or CheckLib? *)
let process_file f =
  try
    let tr = Trace.of_file arch f in
    match !out_file with
    | Some outfile -> fst (convert_trace tr Trace.TyTrace)
    | None ->
      if !dry_run
      then C.parse_print_trace_file arch outf f
      else Trace.(match tr with
      | TyScript, tr -> fst (convert_trace (TyScript,tr) TyTrace)
      | TyTrace,  tr ->
        let (check, state_set) as r = C.process_trace spec_initial_state tr in
        (match !sexp_file with
        | None -> ()
        | Some sexp ->
          let oc = open_out sexp in
          Sexplib.Sexp.output_hum oc (CheckLib.sexp_of_d_trace check);
          close_out oc
        );
        C.print_check arch outf r
      )
  with CheckLib.Exec_error msg ->
    outf CO_error ("\nFatal exec error: "^msg);
    exit 1

let multiple_files = (List.length (!files) > 1)

let process_file_print filename =
  (match !seed with
  | None -> Random.self_init ()
  | Some seed -> Random.init seed
  );
  let o_ty = if multiple_files then CO_status else CO_verbose in
  outf o_ty ("# processing file '"^filename^"' ...");
  ignore ((process_file filename) : bool);
  if multiple_files then (outf CO_status "\n");

;;

List.iter process_file_print (!files)
