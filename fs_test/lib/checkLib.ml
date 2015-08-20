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
open Sexplib.Std
open Diff

open Lem_support
open Fs_prelude
open Fs_interface.Fs_spec_intf.Fs_types
(* open Fs_spec_extras *)

open Trace
open Fs_interface.Fs_printer_intf
(* open Fs_dict_wrappers *)
open Fs_interface.Dir_heap_intf.Dir_heap_types

module Fs_printer = Fs_interface.Fs_printer_intf

(******************************************************************************)
(* Auxiliary functions  *)

let mapi f l = begin
  let (r, _) = List.fold_left (fun (acc, n) e -> (f n e :: acc, n+1)) ([], 0) l in
  List.rev r
end

let map f l = List.rev (List.rev_map f l)

let strings_of_eovs = map input_string_of_rv_error_or_value

(** [list_choose l] returns a random element of the list. *)
let list_choose l =
  let len = List.length l in
  if (len = 0) then failwith "list_random on empty list";
  let p = Random.int len in
  List.nth l p

(* a functional version of List.find *)
let list_find_opt p l = try Some (List.find p l) with Not_found -> None

let finset_of_sexp f sexp = finset_from_list (list_of_sexp f sexp)
let sexp_of_finset f finset = sexp_of_list f (list_from_finset finset)

(******************************************************************************)
(* Types        *)

type special_state = monad_special_state * string with sexp

type d_value =
| D_bytes of string diff
| D_stats of Stat.d_t
| D_names of (string list * (string * string) list * string list)
| D_num of int diff
| D_file_perm of file_perm diff
| D_type_error of ret_value diff
with sexp

type d_eov =
| D_error of error diff
| D_value of d_value
| D_value_not_error of (ret_value * error)
| D_error_not_value of (error * ret_value)
with sexp

let is_d_eov_zero = function
  | D_error None
  | D_value (D_bytes None
            | D_names ([],_,[])
            | D_num None
            | D_file_perm None
            | D_type_error None) -> true
  | D_value (D_stats s) -> Stat.is_d_zero s
  | D_error (Some _)
  | D_value (D_bytes (Some _)
            | D_names _
            | D_num (Some _)
            | D_file_perm (Some _)
            | D_type_error (Some _))
  | D_value_not_error _ | D_error_not_value _ -> false

type error_or_ret_value = ret_value error_or_value
with sexp

type d_ret_lbl_error =
| D_ret_lbl_pid of ty_pid diff (* TODO: provoke *)
| D_ret_lbl_errors_prohibited of error list
| D_ret_lbl_unexpected of error_or_ret_value list * error_or_ret_value list
| D_ret_lbl_diff of error_or_ret_value * d_eov * error_or_ret_value
with sexp

type d_ret_lbl = {
  error : d_ret_lbl_error option;
  rvs   : error_or_ret_value list;
  reset : bool;
}
with sexp

type w_return =
| W_return_none
| W_return_choice of (ret_value list * ret_value)
| W_return_mixed_choice of (error_or_ret_value list * ret_value)
with sexp

type bad_state =
| Special of (string * special_state finset)
| Empty   of string
with sexp

type 'a state_check =
| Success of 'a
| Fail    of bad_state
with sexp

type 'os_state checked_os_finset =
| Bad_states     of bad_state
| Mixed_states   of (string * 'os_state finset * special_state finset)
| Normal_states  of 'os_state finset
with sexp

type 'os_state warning =
| W_return of w_return
| W_return_states
| W_dump_failed of string
| W_mixed_states of (string * special_state finset)
with sexp

type 'os_state interp_line = {
  line_no : int option;
  lines   : Trace.line list;
  warns   : 'os_state warning list;
}
with sexp

type 'os_state interp_state_fail = {
  bad_state : bad_state;
  interp    : 'os_state interp_line list;
}
with sexp

type 'os_state interp_success = 'os_state finset * 'os_state interp_line list
with sexp

type 'os_state interp =
| Interp of 'os_state interp_success
| Interp_state_fail of 'os_state interp_state_fail
| Interp_label_fail of (Trace.numbered_line * 'os_state interp_success)
with sexp

type d_lbl =
| D_check_dump_failed of (string * string)
| D_interp_dump_failed
| D_dump of (Dump.d_t * Dump.t)
| D_ret_lbl of d_ret_lbl
| D_ret_lbl_empty of d_ret_lbl_error option (* TODO: provoke *)
| D_bad_state of bad_state
| D_mixed_states of (string * special_state finset) (* TODO: provoke *)
| D_checklib_false_negative of (bad_state * error_or_ret_value list)
| D_checklib_false_positive of bad_state
with sexp

type d_trace_line = {
  trace_ctxt : Trace.numbered_line list;
  trace_line : Trace.numbered_line;
  d_lbls     : d_lbl list;
} with sexp

type d_trace = d_trace_line list with sexp

exception Exec_error of string

let rec string_opt_of_d_ret_lbl ~exec { error; rvs; reset } =
  let check_reset msg =
    if exec && reset
    then
      let rv_s = String.concat ", " (strings_of_eovs rvs) in
      sprintf "%s\n   continuing execution with %s" msg rv_s
    else msg
  in
  match error with
  | None | Some ( D_ret_lbl_pid None
                | D_ret_lbl_errors_prohibited []
                | D_ret_lbl_unexpected ([],_)) -> None
  | Some (D_ret_lbl_pid (Some _)) ->
    Some (check_reset "process ids do not match")
  | Some (D_ret_lbl_errors_prohibited errs) ->
    let spec_result_s = String.concat ", " (map string_of_error errs) in
    Some (check_reset ("errors possible: " ^ spec_result_s))
  | Some (D_ret_lbl_unexpected (missing, spec)) ->
    let result_s = String.concat ", " (strings_of_eovs missing) in
    let spec_result_s = String.concat ", " (strings_of_eovs spec) in
    let msg = sprintf "unexpected results: %s\n   allowed are only: %s"
      result_s spec_result_s
    in
    Some (check_reset msg)
  | Some (D_ret_lbl_diff (missing, _diff, spec)) ->
    let error = Some (D_ret_lbl_unexpected ([missing], [spec])) in
    string_opt_of_d_ret_lbl ~exec { error; rvs; reset }

let string_of_special_states ss =
  let s_ss = map Fs_printer.string_of_os_special_state (
    map (fun s -> OS_special s) (list_from_finset ss)
  ) in
  let ident_s = "\n     - " in
  let ss_s = String.concat "" (List.map (fun s -> ident_s ^ s) s_ss) in
  "special states found:" ^ ss_s

open Checklib_shared_types

let gen_os_state_or_special_2_os_state_or_special = function
  | GOS_normal s0 -> OS_normal s0
  | GOS_special (ss, s) -> OS_special (ss, s)

let os_state_or_special_2_gen_os_state_or_special = function
  | OS_normal s0 -> GOS_normal s0
  | OS_special (ss, s) -> GOS_special (ss, s)

(** [dest_return_label lbl] destructs the return label [lbl]. It
    returns a tuple [(pid, no_error, error_or_value_list)], where
    [pid] is the process id, [no_error] indicates whether errors are
    prohibited and [error_or_value_list] is a list of all the return
    values and errors encountered. *)
let dest_return_label = function
  | OS_simple_label (OS_RETURN (pid, rv)) -> (pid, false, [rv])
  | OS_simple_label _ -> failwith "dest_return_label: no return label"
  | OS_no_error_return pid -> (pid, true, [])
  | OS_multiple_return (pid, eovl) -> (pid, false, Fs_prelude.removeDuplicatesBy (=) eovl)

let get_pid_label = function
  | OS_RETURN (pid, _) -> pid
  | OS_CALL (pid, _) -> pid
  | _ -> failwith "get_pid_label"

let get_pid_extended_label = function
  | OS_simple_label lbl -> get_pid_label lbl
  | OS_no_error_return pid -> pid
  | OS_multiple_return (pid, _) -> pid

(******************************************************************************)
(* output funs *)

type check_output_type = 
  | CO_error 
  | CO_warning
  | CO_status
  | CO_normal
  | CO_verbose
  | CO_dump

type error_severity = ES_fatal | ES_error | ES_warning

type check_output_fun = check_output_type -> string -> unit 

let out_channel_check_output o s =
  output_string o s;
  output_string o "\n";
  flush o

let default_check_output_fun o_ty s = 
  out_channel_check_output
  (match o_ty with
    | CO_error   -> stderr 
    | CO_warning -> stderr 
    | CO_verbose -> stdout
    | CO_dump    -> stdout
    | CO_normal  -> stdout
    | CO_status  -> stdout) s

let empty_check_output_fun _ _ = ()

let drop_check_output_fun oty outf oty' s =
  if oty <> oty' then outf oty' s

let combine_check_output_fun outf1 outf2 oty s =
  outf1 oty s;
  outf2 oty s

let out_channel_check_output_fun oty o outf oty' s =
  if oty = oty'
  then out_channel_check_output o s
  else outf oty' s

let buffer_check_output_fun b otys outf oty' s =
  if (otys oty') then begin
    Buffer.add_string b s;
    Buffer.add_string b "\n"
  end
  else outf oty' s

let buffer_to_file b out_file =
  let f = open_out out_file in
  Buffer.output_buffer f b;
  close_out f

(******************************************************************************)
(* parsing *)

(******************************************************************************)
(* printing *)

let trace_to_file arch ?change_type trace file_name =
  let f = open_out file_name in
  let s = Trace.to_string arch ?change_type trace in
  fprintf f "%s\n" s;
  close_out f


module type Check_result = sig 

  type m_os_state

  (** [process_script initial_state script] interprets [script]
      from initial state [initial_state]. It returns whether errors
      occurred as well as the set of states after processing the
      trace. *)
  val process_script : m_os_state -> Trace.untyped -> m_os_state interp

  (** [process_trace initial_state trace] checks [trace] using
      from initial state [initial_state]. It returns whether errors
      occurred as well as the set of states after processing the
      trace. *)
  val process_trace :
    m_os_state -> Trace.untyped -> d_trace * m_os_state checked_os_finset

  (** [parse_print_trace_file outf filename] is used for dry-runs. It
      parses and then prints the contents of the trace-file.  If
      returns [true] if everything was OK and [false] if errors (e.g.
      parsing errors) occurred. *)
  (* FIXME move to trace parsing files; this doesn't belong here *)
  val parse_print_trace_file : ty_arch -> check_output_fun -> string -> bool

  (** [print_interp arch outf script] prints the results of [script]
      with [outf] for platform [arch]. If any warnings or errors
      occurred during the interpretation, true will be returned along
      with the interpreted trace. *)
  val print_interp :
    ty_arch -> check_output_fun
    -> m_os_state interp -> bool * Trace.numbered_line list

  (** [print_check arch outf check] prints the results of [check] with
      [outf] for platform [arch]. If any errors occurred during the
      check, true will be returned. *)
  val print_check :
    ty_arch -> check_output_fun
    -> d_trace * m_os_state checked_os_finset -> bool

  (** [script_of_trace tr] converts the trace [tr] to type [Trace.TyScript]. *)
  val script_of_trace : Trace.untyped -> Trace.untyped

  (** [convert_trace outf dry_run tr ty] converts the trace [tr] to
      type [ty]. If converting an interp-trace, this means adding
      return values. In order to figure out the correct return values,
      the model is run on the trace. This execution of the model is
      only attempted if the flag [dry_run] is not set. Return bool is
      true if an error occurred. *)
  (* FIXME remove; this is very strange *)
  val convert_trace :
    ty_arch -> check_output_fun -> m_os_state
    -> bool -> Trace.t -> Trace.ty -> bool * Trace.t

end


module Check (M : Os_ops) :
  Check_result with type m_os_state = M.os_state = struct

type m_os_state = M.os_state

(******************************************************************************)
(* The dry-run is simple *)

let parse_print_trace_file arch0 outf filename =
  try
    let (trace_ty, lbls) = Trace.of_file arch0 filename in
    let s = Trace.to_string arch0 (trace_ty, lbls) in
    outf CO_normal s; true
  with
  | Exec_error m ->
    outf CO_error ("\nFatal exec error: " ^ m); false
  | Trace.Parse_error m ->
    outf CO_error ("\nFatal error: "^m); false

(******************************************************************************)
(* process and check results *)

(* we want to work with sets of states, not just one state *)
let os_set_trans (ss : m_os_state finset) lbl =
  Fs_prelude.finset_bigunion_image (fun s -> M.os_trans s lbl) ss

(** [os_return_trans s0 pid eovs] calls [os_set_trans s0 (OS_RETURN
    (pid, eov))] for all errors or values in the list [eovs]. It
    returns the union of all results. This means that
    [os_return_trans] simulates the effect of returning
    non-deterministically one of the values in [oevs] for process
    [pid] in state set [s0].

    FIXME for the spec this is OK; for real-world systems,
    os_trans/os_set_trans will have side effects, so this doesn't make any
    sense in that scenario.
*)
let os_return_trans s0 pid eovs =
  let ret_eov eov = os_set_trans s0 (OS_RETURN (pid, eov)) in

  let (es, vs) = List.partition is_Error eovs in

  (* execute at most one error return, since they are all the same anyhow *)
  let e_ss = match es with
    | [] -> finset_empty ()
    | (e :: _) -> ret_eov e
  in

  (* execute all values, though *)
  List.fold_left (fun ss eov ->
    finset_union (ret_eov eov) ss
  ) e_ss vs

let keep_cwd fn =
  let cwd = Sys.getcwd () in
  let r = fn () in
  Sys.chdir cwd;
  r

let possible_results_for_pid pid ss =
    finset_cleanup (=) (finset_bigunion_image (M.allowed_results_for_pid pid) ss)

(** [select_return_value pid ss] chooses a return value from those
    possible for [pid] in state [ss]. A list of all errors, a single
    value, or the empty list is returned as the second element of a
    tuple. The first element is a list of warnings regarding the
    result list.

    FIXME Trace.os_extended_label allows multiple return values; to
    me, this doesn't make much sense (apart from possibly hand-written
    traces?)

    FIXME this function is too complicated and doesn't make any sense

    FIXME possible_results_for_pid only makes sense for error returns
*)
let select_return_value pid ss =
  (* extract the set of possible results. *)
  let rl = possible_results_for_pid pid ss in

  let errors = List.rev (list_from_finset (finset_bigunion_image (function
    | Error e -> finset_singleton e
    | Value _ -> finset_empty ()
  ) rl)) in
  let values = List.rev (list_from_finset (finset_bigunion_image (function
    | Error _ -> finset_empty ()
    | Value v -> finset_singleton v
  ) rl)) in
  let rl = list_from_finset rl in

  match errors, values with
  | [], []  -> ([W_return_none], [])
  | [], [_] | _, [] -> ([], rl)
  | [], _   ->
    let rv = list_choose values in
    ([W_return_choice (values,rv)], [Value rv])
  | _::_, _::_ ->
    let rv = list_choose values in
    ([W_return_mixed_choice (rl,rv)], [Value rv])

(** [diff_names ns ns_spec] checks whether [ns] of type [name list]
    matches [ns_stat]. Name lists can be returned in any
    order. Therefore, we need to check whether the two lists are
    permutations of each other. *)
let diff_names ns ns_spec = inter_diff String.compare ns ns_spec

(** [diff_error_or_value eov eov_spec] checks whether an error or
    return value [eov] is subsumed by [eov_spec].  Normally, this is
    just an equality test. For some values, like a list of names, it
    might be more clever and e.g. ignore the order. 

    FIXME "subsumes" weakens what the checker is doing; conceptually
    the checker should just check whether the transitions are allowed
    by the spec; using this function makes it unclear what the checker
    is doing.

*)
let diff_error_or_value eov eov_spec = match (eov, eov_spec) with
  | (Error e, Error e_spec) ->
    D_error (diff e e_spec) (* errors are simple *)
  | (Value (RV_bytes bs), Value (RV_bytes bs_spec)) ->
    let bs_s = Abstract_string.to_string bs in
    let bs_spec_s = Abstract_string.to_string bs_spec in
    D_value (D_bytes (diff bs_s bs_spec_s))
  | (Value (RV_stats st), Value (RV_stats st_spec)) ->
    D_value (D_stats (Stat.diff st st_spec))
  | (Value (RV_names ns), Value (RV_names ns_spec)) ->
    D_value (D_names (diff_names ns ns_spec))
  | (Value (RV_num n), Value (RV_num n_spec)) ->
    D_value (D_num (diff n n_spec))
  | (Value (RV_file_perm p), Value (RV_file_perm p_spec)) ->
    D_value (D_file_perm (diff p p_spec))
  | (Value RV_none, Value RV_none) -> D_error None
  | (Value v, Value v_spec) -> (* fallback for everything else *)
    D_value (D_type_error (diff v v_spec))
  | (Value v, Error e) -> D_value_not_error (v, e)
  | (Error e, Value v) -> D_error_not_value (e, v)

(** [diff_return_label dest_lbl dest_lbl_spec] checks whether the
    destructed return label [dest_lbl] is compatible with the
    destructed return label [dest_lbl_spec]. [dest_lbl] represents the
    return value of a concrete implementation and [dest_lbl_spec] of the
    specification. This function checks whether these are
    compatible. It returns a tuple [(diff, rv_opt)]. [diff] is a
    [d_lbl] zero if the labels are compatible. Otherwise [diff]
    contains a [d_lbl] describing why they are not. If [rv_opt] is not
    [None], it contains the return values the execution should
    continue with.

    FIXME too complicated
*)
let diff_return_label dest_lbl dest_lbl_spec =
  let (pid,      no_error,      rvs_trace) = dest_lbl in
  let (pid_spec, no_error_spec, rvs_spec)  = dest_lbl_spec in

  let rv_in_spec eov = List.exists (fun eov_spec ->
    is_d_eov_zero (diff_error_or_value eov eov_spec)
  ) rvs_spec in
  let rv_in_result eov_spec = List.exists (fun eov ->
    is_d_eov_zero (diff_error_or_value eov eov_spec)
  ) rvs_trace in

  (* get the rv to continue execution *)
  let rvs_inter = List.filter rv_in_result rvs_spec in
  let (rvs, reset) = match rvs_inter, rvs_spec with
    | _ :: _, _ ->
      (* randomly choose one accepted by both lbl and lbl_spec *)
      (rvs_inter, false)
    | [], _ :: _ ->
      (* choose one only accepted by lbl_spec *)
      (rvs_spec, true)
    | [], [] ->
      (* no return values are allowed, rely on caller to deal with it *)
      ([], false)
  in

  let error =
    if pid <> pid_spec
    then
      (* the ids of the two return statements don't match. Something
         went seriously(!) wrong. *)
      Some (D_ret_lbl_pid (Some (pid, pid_spec)))
    else if no_error && List.exists is_Error rvs_spec
    then
      (* our concrete trace prohibits any kind of error, however,
         the spec one allows errors *)
      Some (D_ret_lbl_errors_prohibited (List.fold_right (fun eov errs ->
        match eov with Error e -> e::errs | Value _ -> errs
      ) rvs_spec []))
    else if not (List.for_all rv_in_spec rvs_trace)
    then
      (* There is a return value encountered in the concrete label
         that is not allowed by the spec *)
      match rvs_trace, rvs_spec with
      | [res], [spec] ->
        Some (D_ret_lbl_diff (res, diff_error_or_value res spec, spec))
      | _ ->
        let rvs_missing =
          List.filter (fun rv -> not (rv_in_spec rv)) rvs_trace
        in
        Some (D_ret_lbl_unexpected (rvs_missing, rvs_spec))
    else if reset
    then
      (* This case occurs when the trace contained '-' "no error
         return" and the checking has then picked an arbitrary value
         the model allows. It is not strictly an error. *)
      None (* TODO: mark a warning? *)
    else
      (* everything is fine *)
      None
  in
  { error; rvs; reset; }

(** [check_gos_finset location ss] performs some checks on whether the result
    set [ss] is OK.

    It checks that the result set is not empty and that it contains no
    special states. [check_gos_finset] returns a [checked_os_finset].
*)
let check_gos_finset location ss =
  if finset_is_empty ss
  then Bad_states (Empty location)
  else
    let ss_special = finset_bigunion_image (function
      | GOS_normal _  -> finset_empty ()
      | GOS_special s -> finset_from_list [s]
    ) ss in
    let ss_normal = finset_bigunion_image (function
      | GOS_normal s  -> finset_from_list [s]
      | GOS_special _ -> finset_empty ()
    ) ss in
    if finset_is_empty ss_special
    then Normal_states ss_normal
    else
      if finset_is_empty ss_normal
      then Bad_states (Special (location, ss_special))
      else Mixed_states (location, ss_normal, ss_special)

let normalize_checked_set = function
  | Bad_states bad -> [], Fail bad
  | Mixed_states (name, normal, special) ->
    [W_mixed_states (name, special)], Success normal
  | Normal_states normal -> [], Success normal

(** [os_set_interp_extended_return dest_lbl ss] interprets a
    destructed return label [dest_lbl] in state set [ss].

    It checks the currently allowed results in [ss] and compares them
    to the ones expected by [dest_lbl]. If the actual results do not agree
    with the ones in the label, an error message is produced. In any
    case, it tries to choose one of the allowed results and executes
    a simple return label with this result.

    [os_set_interp_extended_return] returns a tuple [(diffs, ss'')],
    where [diffs] is a description of a discrepancy and [ss''] is the
    result set after executing the simple return statement.

    FIXME the notion of allowed results is not sensible; function too
    complicated
*)
let os_set_interp_extended_return dest_lbl ss =
  let location = "os_set_interp_extended_return" in
  let (pid,_,_) = dest_lbl in
  let rvs = list_from_finset (possible_results_for_pid pid ss) in

  let d_ret_lbl = diff_return_label dest_lbl (pid,false,rvs) in
  match d_ret_lbl with
  | { rvs = [] } ->
    ([D_ret_lbl_empty d_ret_lbl.error], Bad_states (Empty location), [])
  | { error=None; rvs } ->
    let ss' = os_return_trans ss pid rvs in
    ([], check_gos_finset (location^".trans") ss', rvs)
  | { rvs } ->
    let ss' = os_return_trans ss pid rvs in
    ([D_ret_lbl d_ret_lbl], check_gos_finset (location^".trans") ss', rvs)

(** [os_interp_lbl lbl ss] interprets the label [lbl] in the state set
    [ss]. It executes it, checks that the result is sensible and
    generates error and warning messages.

    FIXME extremely complicated function

    FIXME executes the label and an OS_TAU!
*)
let os_interp_lbl lbl ss =
  (* run it followed by tau *)
  let ss'  = check_gos_finset "os_interp_lbl.lbl" (os_set_trans ss lbl) in
  match normalize_checked_set ss' with
  | _warns, (Fail _ as fail) -> fail
  | warns, Success ss' ->
    let ss'' = check_gos_finset "os_interp_lbl.tau" (os_set_trans ss' OS_TAU) in
    match normalize_checked_set ss'' with
    | _warns, (Fail _ as fail) -> fail
    | warns', Success ss'' ->
      let warns = warns' @ warns in
      (* lbl is assumed to be OS_CALL (or OS_RETURN?) *)
      let pid  = get_pid_label lbl in

      (* extract the set of possible results. Make sure the label contains
         not more than one non-error value. *)
      let (warns', rvs) = select_return_value pid ss'' in
      let warns' = List.rev_map (fun w -> W_return w) warns' in
      let warns = List.rev_append warns' warns in

      (* If we have multiple possible results, we choose one to continue with.
         We can do this silently, since [create_return_lbl] guarantees that in
         this case we only have errors. *)
      (* FIXME choosing a return value is not right; hopefully this
         only happens with OS_multiple_return - ie with non-standard
         traces *)
      let return = OS_RETURN(pid, list_choose rvs) in
      let ret_lbl = match rvs with
        | [_] -> OS_simple_label return
        | _   -> OS_multiple_return (pid, rvs) (* TODO: []? *)
      in
      let ss''' = os_set_trans ss'' return in
      let ss''' = check_gos_finset "os_interp_lbl.return" ss''' in
      match normalize_checked_set  ss''' with
      | _warns, (Fail _ as fail) -> fail
      | warns', Success ss''' ->
        let warns = warns' @ warns in

        (* Check that after selecting a concrete return value, we really
           only get one state.  This should hold and probably indicates a
           problem with the specification if it is not the case. *)
        match list_from_finset ss''' with
        | []   -> failwith "os_interp_lbl: return broken!" (* can't happen *)
        | [s0] -> (* the expected case of getting exactly one state back *)
          Success (warns, ss''', ret_lbl)
        | l ->
          (* more than one state: we did not expect that. Perhaps there is
             a problem in the spec?  Anyhow, let's just pick one. *)
          let warns = W_return_states::warns in
          Success (warns, finset_singleton (list_choose l), ret_lbl)

let interp_dump ss p =
  (* ss is not empty, otherwise we would have gotten an error before *)
  if finset_is_empty ss then failwith "interp_dump: no states";
  (* we might have multiple states, but would have had a warning before.
     So just choose a state *)
  let s0 : M.os_state = finset_choose ss in
  M.dump_of_path s0 p

let string_of_dump_error = Dump.(function
  | Unix_error (err,call,path) ->
    Printf.sprintf "Unhandled Unix_error %s calling %s on %s"
      (Fs_printer.string_of_error err) call path
  | Unknown s -> s
)

let check_dump ss (p, exp_dump) =
  try let observed_dump = interp_dump ss p in
      let dump = (p, observed_dump) in
      let diff_opt = Dump.diff (p, exp_dump) dump in
      match diff_opt with
      | None      -> []
      | Some diff -> [D_dump (diff, observed_dump)]
  with
  | Dump.Dump_error dump_error ->
    [D_check_dump_failed (string_of_dump_error dump_error, p)]

(** [add_debug_trace_line lines line] adds the trace line [line] at
    the end of the list of line [lines]. If [line] has an input line
    number, and can therefore be identified by the user easily, the
    list [lines] is cleared first. This function is used to ensure
    that always at least one line from the input file is printed when
    giving error messages. *)
let add_debug_trace_line lines (n_opt, lbl) =
  let keep = Trace.(match (n_opt, lbl) with
    | (None, _) -> true
    | (_, Newline) -> true
    | (_, Comment _) -> true
    | _ -> false
  ) in
  (if keep then lines else []) @ [(n_opt, lbl)]

let rec contains_terminal_error = function
  | (D_check_dump_failed _)::_
  | D_interp_dump_failed::_
  | (D_dump _)::_
  | (D_ret_lbl_empty _)::_
  | (D_bad_state _)::_
  | (D_mixed_states _)::_
  | (D_ret_lbl { error=Some (D_ret_lbl_pid _ | D_ret_lbl_unexpected _ ) })::_
  | (D_ret_lbl { error=Some (D_ret_lbl_diff (_,D_error_not_value _,_)) })::_ ->
    true
  | (D_ret_lbl { error=None })::more
  | (D_ret_lbl { error=Some (D_ret_lbl_errors_prohibited _) })::more
  | (D_ret_lbl {
    error=Some (D_ret_lbl_diff (_,(D_error _
                                      | D_value _
                                      | D_value_not_error _),_))
  })::more
  | (D_checklib_false_negative _)::more
  | (D_checklib_false_positive _)::more -> contains_terminal_error more
  | [] -> false


(******************************************************************************)
(* interpreting and checking traces *)

let check_trace is0 lbls =
  let normal_line trace_line = { trace_line; trace_ctxt=[]; d_lbls=[]; } in
  let rec process_lbl (diffs, ss, lines) = function
    | ((_n_opt,lbl) as trace_line) :: lbls ->
      let trace_ctxt = add_debug_trace_line lines trace_line in
      Trace.(match lbl with
      | Comment _ ->
        let diffs = normal_line trace_line :: diffs in
        process_lbl (diffs, ss, lines) lbls
      | Newline   ->
        let diffs = normal_line trace_line :: diffs in
        process_lbl (diffs, ss, lines) lbls
      | Dump _p   ->
        let d_lbls = [D_interp_dump_failed] in
        let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
        process_lbl (diffs, ss, lines) lbls
      | Dump_result (p, dump) ->
        let d_lbls = check_dump ss (p, dump) in
        process_lbl
          ({ trace_ctxt; trace_line; d_lbls; } :: diffs, ss, lines)
          lbls
      | Label (_, (OS_simple_label (OS_RETURN _ as lbl') as lbl)) ->
        let dest_lbl = dest_return_label lbl in
        let ss' = check_gos_finset "check_trace" (os_set_trans ss lbl') in
        let (d_lbls, ss'', rvs) = os_set_interp_extended_return dest_lbl ss in
        begin match ss', ss'' with
        | (Bad_states bad) as state_check, Bad_states _ ->
          let d_lbls = (D_bad_state bad)::d_lbls in
          (List.rev ({ trace_ctxt; trace_line; d_lbls; } :: diffs), state_check)
        | Mixed_states (location, normal, special), Mixed_states _ ->
          let d_lbls = (D_mixed_states (location, special))::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | Normal_states normal, Normal_states _ ->
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | (Normal_states normal | Mixed_states (_, normal, _)), Bad_states bad ->
          let d_lbls = (D_checklib_false_positive bad)::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | Normal_states normal, Mixed_states (loc, _, special) ->
          let bad = Special (loc, special) in
          let d_lbls = (D_checklib_false_positive bad)::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | (Bad_states bad) as spec_states,
          (Mixed_states (_, normal, _) | Normal_states normal) ->
          let d_lbls = (D_checklib_false_negative (bad, rvs))::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          if contains_terminal_error d_lbls
          then (List.rev diffs, spec_states)
          else process_lbl (diffs, normal, lines) lbls
        | (Mixed_states (loc, normal, special)) as spec_states,
          Normal_states _ ->
          let bad = Special (loc, special) in
          let d_lbls = (D_checklib_false_negative (bad, rvs))::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          if contains_terminal_error d_lbls
          then (List.rev diffs, spec_states)
          else process_lbl (diffs, normal, lines) lbls
        end
      | Label (_, (OS_no_error_return _ as lbl))
      | Label (_, (OS_multiple_return (_, _) as lbl)) ->
        let dest_lbl = dest_return_label lbl in
        let (d_lbls, ss'', _rvs) = os_set_interp_extended_return dest_lbl ss in
        begin match ss'' with
        | (Bad_states bad) as state_check ->
          let d_lbls = (D_bad_state bad)::d_lbls in
          (List.rev ({ trace_ctxt; trace_line; d_lbls; } :: diffs), state_check)
        | Mixed_states (location, normal, special) ->
          let d_lbls = (D_mixed_states (location, special))::d_lbls in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | Normal_states normal ->
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        end
      | Label (_, OS_simple_label lbl') ->
        let ss' = os_set_trans ss lbl' in
        begin match check_gos_finset "check_trace" ss' with
        | (Bad_states bad) as state_check ->
          let d_lbls = [D_bad_state bad] in
          (List.rev ({ trace_ctxt; trace_line; d_lbls; } :: diffs), state_check)
        | Mixed_states (location, normal, special) ->
          let d_lbls = [D_mixed_states (location, special)] in
          let diffs = { trace_ctxt; trace_line; d_lbls; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        | Normal_states normal ->
          let diffs = { trace_ctxt; trace_line; d_lbls=[]; } :: diffs in
          process_lbl (diffs, normal, lines) lbls
        end
      )
    | [] -> (List.rev diffs, Normal_states ss)
  in
  let initial_state_set = finset_singleton is0 in
  process_lbl ([], initial_state_set, []) lbls

let interp_trace is0 lbls =
  let rec process_lbl (ss, wtrace) = function
    | (line_no,lbl) :: lbls -> Trace.(match lbl with
      | Comment _ ->
        process_lbl (ss, { line_no; lines=[lbl]; warns = [] } :: wtrace) lbls
      | Newline   ->
        process_lbl (ss, { line_no; lines=[lbl]; warns = [] } :: wtrace) lbls
      | Dump p -> begin
        try
          let dump = interp_dump ss p in
          let line = Dump_result (p, dump) in
          process_lbl (ss, { line_no; lines=[line]; warns = [] } :: wtrace) lbls
        with Dump.Dump_error dump_error ->
          let warns = [W_dump_failed (string_of_dump_error dump_error)] in
          process_lbl (ss, { line_no; lines=[lbl]; warns } :: wtrace) lbls
      end
      | Label (_, OS_simple_label ((OS_CREATE _ | OS_DESTROY _) as lbl')) ->
        let ss'  = os_set_trans ss lbl' in
        let ss'' = check_gos_finset "process_lbl.process" ss' in
        begin match normalize_checked_set ss'' with
        | warns, Success ss'' ->
          let wtrace = { line_no; lines=[lbl]; warns; } :: wtrace in
          process_lbl (ss'', wtrace) lbls
        | warns, Fail bad_state ->
          let interp = List.rev
            ({ line_no; lines=[lbl]; warns; } :: wtrace)
          in Interp_state_fail { bad_state; interp; }
        end
      | Label (had_pid, OS_simple_label ((OS_CALL _) as lbl')) ->
        begin match os_interp_lbl lbl' ss with
        | Success (warns, ss', ret_label) ->
          let lines = lbl
            :: (Label (true, OS_simple_label OS_TAU))
            :: [Label (had_pid, ret_label)]
          in
          process_lbl (ss', { line_no; lines; warns } :: wtrace) lbls
        | Fail bad_state ->
          let interp = List.rev
            ({ line_no; lines=[lbl]; warns = [] } :: wtrace)
          in Interp_state_fail { bad_state; interp; }
        end
      | Dump_result _
      | Label (_, OS_simple_label (OS_RETURN _ | OS_TAU))
      | Label (_, OS_multiple_return _)
      | Label (_, OS_no_error_return _) ->
        Interp_label_fail ((line_no,lbl),(ss, List.rev wtrace))
      )
    | [] -> Interp (ss, List.rev wtrace)
  in
  let initial_state_set = finset_singleton is0 in
  process_lbl (initial_state_set, []) lbls

let print_error arch0 outf ?pre lines es msg =
  let (w_s, co) = match es with
    | ES_error -> ("Error", CO_error)
    | ES_fatal -> ("Fatal error", CO_error)
    | ES_warning -> ("Warning", CO_warning)
  in
  let w_s = "\n" ^ w_s ^ ": " in
  let ws_s = String.make (String.length w_s - 1) ' ' in
  let lines_s = String.concat ("\n"^ ws_s)
    (List.map (Trace.numbered_line_to_string arch0) lines)
  in
  let pre_s = match pre with None -> "" | Some s -> s in
  let msg_s = if msg = "" then msg else "\n   "^msg in
  outf co (w_s ^ pre_s ^ lines_s ^ msg_s)

let print_bad_state_error arch0 outf line = function
  | Special (_location, ss) ->
    let msg = (string_of_special_states ss)^"\n   no normal result states" in
    print_error arch0 outf [line] ES_fatal msg
  | Empty _location ->
    print_error arch0 outf [line] ES_fatal "no result states"

let rvs_to_string rl =
  if List.length rl = 1
  then Fs_printer.string_of_ret_value (List.hd rl)
  else
    let rv_ss = map Fs_printer.string_of_ret_value rl in
    "["^(String.concat ";" rv_ss)^"]"

let print_interp_line arch0 outf = function
  | { line_no; lines; warns; } ->
    let lines = List.mapi (fun i line ->
      let line = if i = 0 then (line_no, line) else (None, line) in
      let change_type = Trace.TyTrace in
      outf CO_normal (Trace.numbered_line_to_string ~change_type arch0 line);
      line
    ) lines in
    let error = List.fold_left (fun error -> function
      | W_mixed_states (_location, ss) ->
        let msg = "\n   continuing with normal result states" in
        print_error arch0 outf [List.hd lines] ES_error
          ((string_of_special_states ss) ^ msg);
        true
      | W_dump_failed err_s ->
        outf CO_error err_s;
        print_error arch0 outf [List.hd lines] ES_error
          "execution of dump failed";
        true
      | W_return_states ->
        print_error arch0 outf [List.hd lines] ES_warning
          "multiple result states after return";
        true
      | W_return W_return_none -> list_choose [] (* TODO: provoke *)
      | W_return (W_return_choice (rvs, rv)) ->
        let msg3 = "        possible results: "^(rvs_to_string rvs) in
        let rv_s = Fs_printer.string_of_ret_value rv in
        let msg2 = "   randomly continuing execution with "^rv_s^"\n" in
        print_error arch0 outf [List.hd lines] ES_warning
          ("more than one possible result value\n"^msg2^msg3);
        error
      | W_return (W_return_mixed_choice _) ->
        print_error arch0 outf [List.hd lines] ES_error
          "errors and values mixed in result";
        error
    ) false warns in
    error, lines

let print_interp arch0 outf = function
  | Interp (_ss, lines) ->
    let error, lines = List.fold_left (fun (error,tr) line ->
      let (error', line) = print_interp_line arch0 outf line in
      (error' || error, List.rev_append line tr)
    ) (false, []) lines in
    if error then outf CO_warning "Warning: trace caused problems";
    error, List.rev lines
  | Interp_label_fail ((line_no, lbl), (_ss, lines)) ->
    let lines = List.fold_left (fun tr line ->
      let (_, line) = print_interp_line arch0 outf line in
      List.rev_append line tr
    ) [] lines in
    let lbl_line = { line_no; lines=[lbl]; warns=[] } in
    let (_, flines) = print_interp_line arch0 outf lbl_line in
    let lbl_type_s = match lbl with
      | Trace.Dump_result _ -> "dump-results"
      | _ -> "extended label"
    in
    print_error arch0 outf flines
      ~pre:(lbl_type_s^" in interp-trace: ") ES_fatal "";
    true, List.rev_append lines flines
  | Interp_state_fail { bad_state; interp } ->
    let lines = List.fold_left (fun tr line ->
      let (_, line) = print_interp_line arch0 outf line in
      List.rev_append line tr
    ) [] interp in
    print_bad_state_error arch0 outf (List.hd lines) bad_state;
    true, lines

let process_script lbls = keep_cwd (fun () -> interp_trace lbls)

let process_trace lbls = keep_cwd (fun () -> check_trace lbls)

let print_check_line arch0 outf = function
  | { trace_ctxt; trace_line; d_lbls=[] } ->
    outf CO_verbose (Trace.numbered_line_to_string arch0 trace_line);
    (false, trace_line)
  | { trace_ctxt; trace_line; d_lbls } ->
    outf CO_verbose (Trace.numbered_line_to_string arch0 trace_line);
    let error = List.fold_left (fun error -> function
    | D_dump (dump_dt, expected) ->
      outf CO_error (Trace.dump_to_string ~ws:8 expected);
      print_error arch0 outf [trace_line] ES_error (Dump.string_of_d_t dump_dt);
      true
    | D_interp_dump_failed ->
      print_error arch0 outf [trace_line] ES_error
        "interpretation of dump failed";
      true
    | D_check_dump_failed (err,_path) ->
      outf CO_error err;
      print_error arch0 outf [trace_line] ES_error "execution of dump failed";
      true
    | D_ret_lbl d_ret_lbl ->
      (match string_opt_of_d_ret_lbl ~exec:true d_ret_lbl with
      | Some s -> print_error arch0 outf [trace_line] ES_error s; true
      | None -> error)
    | D_ret_lbl_empty _ ->
      print_error arch0 outf [trace_line] ES_error "ret_lbl_empty";
      true
    | D_bad_state bad_state ->
      print_bad_state_error arch0 outf trace_line bad_state;
      true
    | D_mixed_states _ ->
      print_error arch0 outf [trace_line] ES_error "mixed_states";
      true
    | D_checklib_false_negative (bad_state, rvs) ->
      print_bad_state_error arch0 outf trace_line bad_state;
      print_error arch0 outf [] ES_error
        "!!! EXCEPT CHECKLIB DOES NOT AGREE !!!";
      print_error arch0 outf [] ES_error
        ("The spec permitted:\n"
         ^(String.concat ", " (strings_of_eovs rvs)));
      true
    | D_checklib_false_positive bad_state ->
      print_bad_state_error arch0 outf trace_line bad_state;
      print_error arch0 outf [trace_line] ES_error
        "!!! EXCEPT THE SPEC DOES NOT AGREE !!!";
      true
    ) false d_lbls in
    (error, trace_line)

let print_check arch0 outf (lines, ss) =
  let error, _tr = List.fold_left (fun (error,tr) line ->
    let (error', line) = print_check_line arch0 outf line in
    (error' || error, line :: tr)
  ) (false, []) lines in
  outf CO_verbose "";
  let error = match ss with
    | Bad_states _ | Mixed_states _ -> true (* TODO: test *)
    | Normal_states _ -> error
  in
  outf CO_normal (
    if error then "# trace not accepted"
    else "# trace accepted"
  );
  error

(******************************************************************************)
(* converting traces *)

(* converting a trace to a script means dropping any tau
   transitions, any returns, and converting dump results to dump commands *)
let script_of_trace (tr : Trace.untyped) : Trace.untyped =
  let is_interp_label = function
    | OS_no_error_return _ -> false
    | OS_multiple_return (_, _) -> false
    | OS_simple_label (OS_RETURN(_, _)) -> false
    | OS_simple_label OS_TAU -> false
    | OS_simple_label (OS_CREATE (_, _, _)) -> true      
    | OS_simple_label (OS_DESTROY _) -> true
    | OS_simple_label (OS_CALL(_, _)) -> true
  in
  let convert_elbl lbls (n_opt, lbl) = Trace.(match lbl with
    | Label (_, lbl') when is_interp_label lbl' -> (n_opt, lbl) :: lbls
    | Label _ -> lbls
    | Dump _ | Newline | Comment _ -> (n_opt, lbl) :: lbls
    | Dump_result (p, _) -> (n_opt, Dump p) :: lbls
  ) in
  List.(rev (fold_left convert_elbl [] tr))

let convert_trace arch0 outf is0 dry_run ((ty_org, tr):Trace.t) ty_new =
  match (ty_org, ty_new) with
  | (Trace.TyTrace, Trace.TyTrace)   -> (true, (ty_new, tr))
  | (Trace.TyTrace, Trace.TyScript)  -> (true, (ty_new, script_of_trace tr))

  | (Trace.TyScript, Trace.TyScript) -> (true, (ty_new, tr))
  | (Trace.TyScript, Trace.TyTrace)  ->
    if dry_run
    then (outf CO_error
            "\nFatal error: can't convert an interp-trace in dry-run mode\n";
          exit 1)
    else let interp = process_script is0 tr in
         let (error,tr) = print_interp arch0 outf interp in
         (error, (ty_new, tr))

end

(**********************************************************************)
(* instantiation of the Check functor *)

let spec_initial_state arch no_root =
  Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_init_state arch no_root

module Spec_os_ops : Os_ops with type os_state = dh_os_state = struct

  type os_state = dh_os_state

(*  let arch = M.arch;; *)
(*    let env = spec_os_ops_env M.arch M.no_root;; *)
(*  let initial_state = os_init_state env;; *)

  let os_trans s0 lbl =
    finset_image os_state_or_special_2_gen_os_state_or_special
      (Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_trans s0 lbl)

  let allowed_results_for_pid pid s0 =
    Fs_interface.Dir_heap_intf.Dir_heap_ops.dh_allowed_results_for_pid pid s0

  let dump_of_path (s0:dh_os_state) p =
    Fs_interface.Fs_dump_intf.spec_dump_of_path s0 p

end

module Check_spec = Check(Spec_os_ops)
