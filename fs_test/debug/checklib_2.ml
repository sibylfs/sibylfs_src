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

(* see checklib_2.mli; implementation of simple checking library checklib_2.mli *)
open Sexplib.Std
open Sexplib.Conv

let sexp_to_string t = Sexplib.Sexp.to_string_hum t

(* we wish to enforce that traces always have "normal" labels; this is a restriction on the Trace.line type *)
type line = 
| Comment of string
| Newline
| Label of Fs_interface.Fs_spec_intf.Fs_types.os_label
| Dump_result of (string * Dump.t) sexp_opaque with sexp_of

type arch = Trace.arch

type numbered_line = int option * line with sexp_of

type trace_line = numbered_line with sexp_of (* lines which are not os_labels are treated as id transition, ie state transitions to itself *)

type trace = trace_line list

type spec_state = Fs_interface.Dir_heap_intf.Dir_heap_types.dh_os_state

type spec_state_or_special = Fs_interface.Dir_heap_intf.Dir_heap_types.dh_os_state_or_special
let mk_spec_state_or_special : Fs_interface.Dir_heap_intf.Dir_heap_types.dh_os_state_or_special -> spec_state_or_special = fun x -> x

let sexp_of_spec_state_or_special = Fs_interface.Dir_heap_intf.Dir_heap_types.sexp_of_dh_os_state_or_special

type history = spec_state_or_special list with sexp_of

(* introduce an abstract type for state set, just in case there is a reason to use Lem_support.finset *)
module State_set : sig 
  type t 
  val sexp_of_state_set: t -> Sexplib.Sexp.t
  val state_set_to_history_list: t -> history list
  val history_list_to_state_set: history list -> t
end = struct
  type t = history list with sexp_of (* Lem_support.finset *)
  let sexp_of_state_set = sexp_of_t
  (* let state_set_to_list : state_set -> spec_state_or_special list = fun ss -> ss |> Lem_support.list_from_finset |> List.hd *)
  let state_set_to_history_list : t -> history list = fun x -> x (* Lem_support.list_from_finset *)
  let history_list_to_state_set: history list -> t = fun x -> x (* Lem_support.finset_from_list *)
end

type state_set = State_set.t
include State_set

(* construct an initial state set, with no history *)
let mk_state_set : spec_state_or_special -> state_set = fun x -> [[x]] |> history_list_to_state_set


type 'a full_trace = { sequence: ('a * trace_line) list; initial: 'a } with sexp_of

type checked_trace = state_set full_trace

type error = 
  | E_special_states
  | E_no_normal_states with sexp_of

type state_set_maybe_error = SE_state_set of state_set | SE_error of error list * state_set with sexp_of

type debugged_trace = state_set_maybe_error full_trace with sexp_of



(* FIXME following may leak resources *)
(* copied from ocaml/utils/misc.ml *)
let string_of_ic' ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_substring b buff 0 n; copy())
  in copy()

let read_ic_as_string ic = (
  try
    string_of_ic' ic
  with _ -> failwith "read_ic_as_string")

(* get contents of file as string; return option Some s if successful, None otherwise *)
let read_file_as_string fn = (
  try
    let ic = open_in fn in
    let s = string_of_ic' ic in
    let _ = close_in ic in
    Some s
  with _ -> None)
let (_:string -> string option) = read_file_as_string


let file_to_string = read_file_as_string

(* write a string to a file ; return true if successful, false otherwise *)
let write_string_to_file s fn = (
  try
    let oc = open_out fn in
    let _ = output_string oc s in
    let _ = close_out oc in
    true
  with _ -> false)
let (_:string -> string -> bool) = write_string_to_file

let string_to_trace arch s = 
  let map_line l = (match l with
      | Trace.Comment s -> Comment s
      | Trace.Newline -> Newline
      | Trace.Label (_,Trace.OS_simple_label l) -> Label l
      | Trace.Dump_result (s,d) -> Dump_result (s, d)
      | _ -> raise (Trace.Parse_error "string_to_trace: map_line: unexpected label"))
  in
  let map_numbered_line (n,l) = (n,map_line l) in
  try 
    let t = Trace.of_string arch s in
    match t with 
    (Trace.TyTrace,ls) -> (Some(List.map map_numbered_line ls))
    | _ -> raise (Trace.Parse_error "checklib_2.ml: string_to_trace: file not @type trace")
  with Trace.Parse_error s -> 
    Printf.eprintf "checklib_2.ml: string_to_trace: string not parsed as a trace: %s" s;
    None



let trace_line_to_string arch (n,l0) = (
  match l0 with
  | Comment s -> "#"^s
  | Newline -> "\n"
  | Label l -> (
      let has_pid_FIXME = true in  (* FIXME need to remove this bool argument from Trace.Label *)
      let l1 = Trace.(Label(has_pid_FIXME,OS_simple_label(l))) in
      Trace.numbered_line_to_string arch (n,l1))
(* Fs_interface.Fs_printer_intf.string_of_os_label (arch |> Fs_interface.Fs_spec_intf.Fs_arch.architecture_of_ty_arch) l  *)
  | Dump_result x -> "FIXME: trace_line_to_string, Dump_result")



(* make a transition with a given label *)
let check_line: arch -> state_set -> trace_line -> state_set = (fun arch0 ss0 l ->
    let (line_num_opt,line) = l in
    match line with
    | Comment s -> ss0
    | Newline -> ss0
    | Dump_result (so,_do) -> (Printf.eprintf "checklib_2: dumps not supported yet\n";ss0)
    | Label lbl -> (
        let open Fs_interface.Fs_spec_intf.Fs_types in
        let hs : history list = ss0 |> state_set_to_history_list in
        let rest = List.filter (fun x -> is_OS_normal (List.hd x)) hs in
        (* for every normal state, we get a list(set) of possible successor states *)
        let f1 : history -> history list =  fun h ->
          let s = List.hd h in  (* guaranteed to be OS_normal since we only apply to rest *)
          let s : spec_state = (match s with OS_normal s -> s | _ -> failwith "impossible 67p") in  (* guaranteed to be OS_normal since we only apply to rest *)
          let trans = Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_trans in 
          let ss' : spec_state_or_special Lem_support.finset = trans s lbl in
          let ss' = ss' |> Lem_support.list_from_finset in
          (* but we are tracking histories, not states *)
          let hs' = List.map (fun s' -> s'::h) ss' in
          hs'
        in
        (* get new histories *)
        (* we combine all the results, then perform some additional checks *)
        let hs' = rest |> List.map f1 |> List.concat in
        (hs' |> history_list_to_state_set)))

(* apply check_label repeatedly; ends when no more labels, or empty state_set; accumulates all output *)
let check_trace: arch -> trace -> state_set -> checked_trace = fun arch0 tr0 ss0 ->
  let f1 (ct,ss) line = 
    let ss' = check_line arch0 ss line in
    let ct' = {ct with sequence=(ss',line)::ct.sequence} in
    (ct',ss')
  in
  List.fold_left f1 ({sequence=[];initial=ss0},ss0) tr0 |> fst


let debug_state_set: state_set -> state_set_maybe_error = (fun ss ->
    let hs = ss |> state_set_to_history_list in
    let open Fs_interface.Fs_spec_intf.Fs_types in
    let (special,rest) = List.partition (fun x -> x |> List.hd |> is_OS_special) hs in
    (* check for special states *)
    let es1 = (
      match special with
      | [] -> []
      | x::xs -> [E_special_states])
    in
    (* check for no normal states *)
    let es2 = (
      match rest with 
      | [] -> [E_no_normal_states]
      | x::xs -> [])
    in
    let es = es1@es2 in
    match es with 
    | [] -> SE_state_set(ss)
    | _ -> SE_error(es,ss))


let debug_trace: checked_trace -> debugged_trace = (fun ct ->
    let f1 = fun (x,line) -> (debug_state_set x,line) in
    { sequence=(ct.sequence |> List.map f1); initial=(ct.initial |> debug_state_set) })




(* test

open Fs_interface.Fs_spec_intf.Fs_types

let arch0 = Fs_interface.Fs_spec_intf.Fs_types.ARCH_MAC_OS_X

let f0 = "/tmp/l/github/lemix-tests/artefacts/2014-10-27_artefacts/2014-10-27_mJ4/hfsplus_loop/open/results/exec_open___open_broken_sl_____O_APPEND__O_CLOEXEC__O_CREAT__O_DIRECTORY__O_EXCL__O_EXEC__O_NOFOLLOW__O_TRUNC___0666___close_3-int.trace"
let checked_trace = "/tmp/l/github/lemix-tests/artefacts/2014-10-27_artefacts/2014-10-27_mJ4/hfsplus_loop/open/results/check_exec_open___open_broken_sl_____O_APPEND__O_CLOEXEC__O_CREAT__O_DIRECTORY__O_EXCL__O_EXEC__O_NOFOLLOW__O_TRUNC___0666___close_3-int.trace"

let s0 = f0 |> file_to_string
let (Some tr0) = f0 |> file_to_string |> (fun (Some x) -> x|> string_to_trace arch0)

let no_root0 = true
let s0 = Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_init_state arch0 no_root0
let ss0 = OS_normal s0 |> mk_state_set

let _ = check_line arch0 ss0 (List.hd tr0)

let ct0 = check_trace arch0 tr0 ss0

let dt0 = debug_trace ct0

let _ = dt0 |> sexp_of_debugged_trace |> sexp_to_string |> print_endline

*)
