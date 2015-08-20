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

open Printf

open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
(* open Fs_spec_extras *)
open CheckLib

module System = Fs_test_system

(******************************************************************************)
(* command line *)

let verbose = ref false;;
let no_warnings = ref false;;
let dry_run = ref false;;
let keep_root = ref false;;
let out_file = ref (None : string option);;

let files = ref ([]:string list)
let temp_dir = ref (Filename.get_temp_dir_name ());;
let root_dir_opt = ref (None : string option);;

let arch = ref None;;
let files = ref ([]:string list)


let rec remove_trailing_slashes nd =
  if (String.length nd == 0) then nd else
  begin
    let last_char = String.get nd (String.length nd - 1) in
    if last_char == '/' then
       remove_trailing_slashes (String.sub nd 0 (String.length nd - 1))
    else
       nd
  end

(* Fixes a directory name to be absolute and not end with a slash.
   To make it absolute the current working directory is added if necessary *)
let make_abs_dirname nd = begin
  (* first make it absolute *)
  let nd' = if (String.length nd == 0) then Sys.getcwd() else
    if (String.get nd 0 == '/') then nd else
    (Sys.getcwd() ^ "/" ^ nd) in
  remove_trailing_slashes nd'
end

let options = Arg.align ([
  ( "-r", 
    Arg.String (fun nd -> root_dir_opt := Some (make_abs_dirname nd)),
    (" set root directory"));
  ( "-tmp", 
    Arg.String (fun nd -> temp_dir := make_abs_dirname nd),
    (" set temporary directory"));
  ( "-k", 
    Arg.Set keep_root,
    " do not clean-up root directory afterwards");    
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
  ( "-o", 
    Arg.String (fun s -> out_file := Some s),
    " output file; converts input trace and stores it in given file");  
  ( "-arch",
    Arg.Symbol (["posix"; "linux"; "mac_os_x"; "freebsd"], (function
    | "linux"    -> (arch := Some ARCH_LINUX)
    | "posix"    -> (arch := Some ARCH_POSIX)
    | "mac_os_x" -> (arch := Some ARCH_MAC_OS_X)
    | "freebsd"  -> (arch := Some ARCH_FREEBSD)
    | _ -> () (* can't happen *))),
    " architecture to simulate");
])

let usage_msg = 
    ("posix 0.4\n"
     ^ "Usage: posix [OPTION]... [TRACE_FILE]... \n\
Example: posix -v example_traces/os_trace1.txt\n\n\
check that the traces stored in the given trace-files are accepted by
the underlying file-system implementation of the operating system.\n" 
    )

let check_args () =
  let print_err m =
    prerr_endline ("Error: "^m);
    Arg.usage options usage_msg;
    exit 1
  in

  if List.length !files = 0 then print_err "no trace-file given";
  (match !root_dir_opt with
  | None -> ()
  | Some rd ->
    if Sys.file_exists rd
    then print_err ("root-dir '" ^ rd ^ "' does not exist")
  );

  if ((!out_file <> None) && (List.length (!files) > 1))
  then print_err "if option '-o' is used, only one trace-file can be processed";
  if (!arch = None)
  then print_err "-arch required"
;;

Arg.parse options
  (fun s -> (files := ((!files) @ [s])))
  usage_msg;

check_args ()

;;

(* check_args enforces *)
let arch = match !arch with Some a -> a | None -> assert false

let outf = default_check_output_fun
let outf = if !verbose then outf else drop_check_output_fun CO_verbose outf
let outf = if !no_warnings then drop_check_output_fun CO_warning outf else outf

(* if no root directory is specified, create a temp one *)
let (root_dir, keep_root) = match !root_dir_opt with
  | Some rd -> (rd, true)
  | None -> (
      let rd = Filename.temp_file ~temp_dir:(!temp_dir) "posix-tmp-root_" "" in
      let _ = Syscall.unlink rd in
      let _ = if !keep_root then
                print_endline ("# using root directory '"^rd^"' ...")
              else () in
      (rd, !keep_root)
  )

module C = CheckLib_with_posix.Check_posix

(* the main functionality *)
let process_file_core f =
  let posix_initial_state = Posix_ops.posix_initial_state arch root_dir in
  let convert_trace = C.convert_trace arch outf posix_initial_state !dry_run in
  try
    let tr = Trace.of_file arch f in (* TODO: raises? *)
    match !out_file with
    | Some outfile -> fst (convert_trace tr Trace.TyTrace)
    | None ->
      if !dry_run
      then C.parse_print_trace_file arch outf f
      else Trace.(match tr with
      | TyScript, tr -> fst (convert_trace (TyScript,tr) TyTrace)
      | TyTrace,  tr ->
        let check = C.process_trace posix_initial_state tr in
        C.print_check arch outf check
      )
  with CheckLib.Exec_error msg ->
    outf CO_error ("\nFatal exec error: "^msg);
    exit 1

let process_file filename =
  (* create root dir if necessary *)
  if not (Sys.file_exists root_dir)
  then begin
    try
      Syscall.mkdir root_dir 0o777;
      Syscall.chmod root_dir 0o777
    with Unix.Unix_error (err, syscall, path) ->
      eprintf "%s failed on %s: %s\n%!" syscall path (Unix.error_message err);
      exit 2
  end;

  let signal_handler ?pids sigs = match pids with
    | Some pids -> fun signal ->
      ignore Unix.(sigprocmask SIG_BLOCK sigs);
      printf
        "# %d caught %s; terminating children and resetting global state.\n%!"
        (Unix.getpid ()) (System.string_of_signal signal);
      printf "# %d live children receiving SIGTERM\n%!" (List.length pids);
      List.iter (fun pid -> Unix.kill pid Sys.sigterm) pids;
      List.iter (fun pid ->
        ignore (Unix.waitpid [Unix.WUNTRACED] pid);
        printf "# Child pid %d terminated\n%!" pid;
      ) pids;
      if not keep_root then Posix_ops.rm_recursive root_dir;
      Posix_ops.reset_global_state ();
      exit 1
    | None -> fun signal ->
      ignore Unix.(sigprocmask SIG_BLOCK sigs);
      printf "# %d caught %s; resetting global state.\n%!"
        (Unix.getpid ()) (System.string_of_signal signal);
      Posix_ops.stop_all ();
      Posix_ops.reset_global_state ();
      exit 1
  in

  let cleanup ?status () = match status with
    | Some status -> begin match status with
      | Syscall.WEXITED _ -> ()
      | Syscall.WSIGNALED k ->
        printf "\n# Fatal error: Worker process signalled %s\n"
          (System.string_of_signal k)
      | Syscall.WSTOPPED k ->
        printf "\n# Fatal error: Worker process stopped %d\n" k
    end;
      if not keep_root then Posix_ops.rm_recursive root_dir;
      Posix_ops.reset_global_state ()
    | None ->
      Posix_ops.stop_all ();
      Posix_ops.reset_global_state ();
  in

  (* run in separate process with exception handling etc. *)
  Posix_ops.run_in_child_process signal_handler cleanup (fun () ->
    if process_file_core filename then 1 else 0
  )

let process_file_print filename =
  print_endline ("# processing file '"^filename^"' ...");
  process_file filename;
  print_endline "\n"

;;

let check_file = if (List.length (!files) > 1) || !verbose then
    process_file_print else process_file in
List.iter check_file (!files);
