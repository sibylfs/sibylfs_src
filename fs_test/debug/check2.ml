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

open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
(* open Fs_spec_extras *)
open Checklib_2

(******************************************************************************)
(* command line *)

let no_root = ref true;;
let out_file = ref (None : string option);;
let arch = ref (None : ty_arch option);;
let files = ref ([]:string list)

let options = Arg.align [
    ( "--root", Arg.Clear no_root,
      " check with initial state having process 1 with superuser permissions");  
    ( "-o", Arg.String (fun s -> out_file := Some s),
      " output file");  
    ( "-arch", Arg.Symbol (["posix"; "linux";"mac_os_x";"freebsd"], (function
         | "linux" -> (arch := Some ARCH_LINUX) 
         | "posix" -> (arch := Some ARCH_POSIX)
         | "mac_os_x" -> (arch := Some ARCH_MAC_OS_X)
         | "freebsd"  -> (arch := Some ARCH_FREEBSD)
         | _ -> () (* can't happen *))),
      " architecture to check");
  ]

let usage_msg = 
"debug
Usage: debug [OPTION]... [TRACE_FILE]...
debug is similar to check, but prints extensive debugging information."

let print_usage () = Arg.usage options usage_msg

let check_args () = (
  let print_err m = (
    print_endline ("Error: "^m); 
    print_usage();
    exit(1))
  in
  (match !files with
  | [f] -> ()
  | _ -> (
      print_err "Multiple files provided, but only one can be processed"));
  (if !arch = None then print_err "arch argument required"))


let _ = Arg.parse options (fun s -> (files := (!files)@[s])) usage_msg
let _ = check_args ()

let file = (
  match !files with 
  | [f] -> f 
  | _ -> failwith "check2.ml: impossible: check_args ensures only one file")

let arch = (match !arch with | Some a -> a | _ -> failwith "check2.ml: impossible: arch argument checked not equal to None in check_args")

let debugged_trace_to_string arch dt = (
  let tlines = dt.sequence in
  let tlines = List.rev tlines in
  let possible_error_message : state_set_maybe_error -> string option = (fun ss -> match ss with
    | SE_state_set _ -> None
    | SE_error (es,ss) -> (
        let s = if List.mem E_special_states es then ["Error: special states present"] else [] in
        let s = s@(if List.mem E_no_normal_states es then ["Error: no normal states"] else []) in
        if s = [] then None else Some(String.concat "\n" s)))
  in
  let ss = List.map (fun (x,y) -> (trace_line_to_string arch y, possible_error_message x)) tlines in
  let f1 yx = (match yx with
      | (s,None) -> [s]
      | (s,Some s') -> [s;s'])
  in
  let ss = ss |> List.map f1 |> List.concat in
  let s = String.concat "\n" ss in
  s)

(* now read the file and process it, then write output to file *)
let main arch no_root file out_file = (
  let print_err s = (
    Printf.eprintf "Error: check2.ml: %s\n" s;
    exit(1))
  in
  let open Checklib_2 in
  let trace = 
    file 
    |> file_to_string 
    |> (fun s -> match s with
        | None -> print_err (Printf.sprintf "failed to read file: %s" file)
        | Some s -> 
          s 
          |> string_to_trace arch 
          |> (fun t -> match t with
              | None -> print_err (Printf.sprintf "failed to parse trace file: %s" file)
              | Some t -> t))
  in
  let init_state = 
    Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_init_state arch no_root
    |> (fun x -> Fs_interface.Fs_spec_intf.Fs_types.OS_normal x)
    |> mk_spec_state_or_special
  in
  let state_set = mk_state_set init_state in
  let ct = check_trace arch trace state_set in
  let dt = debug_trace ct in
  let s = debugged_trace_to_string arch dt in
  match out_file with
  | None -> print_endline s
  | Some f -> (
      let success = write_string_to_file s f in
      match success with
      | true -> ()
      | false -> print_err (Printf.sprintf "failed to write file: %s" f)))

let _ = main arch !no_root file !out_file
