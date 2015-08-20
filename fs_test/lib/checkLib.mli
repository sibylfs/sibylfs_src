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

open Lem_support
open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Dir_heap_intf.Dir_heap_types
open Checklib_shared_types

type special_state = monad_special_state * string

type d_value =
| D_bytes of string Diff.diff
| D_stats of Stat.d_t
| D_names of (string list * (string * string) list * string list)
| D_num of int Diff.diff
| D_file_perm of file_perm Diff.diff
| D_type_error of ret_value Diff.diff
with sexp

type d_eov =
| D_error of error Diff.diff
| D_value of d_value
| D_value_not_error of (ret_value * error)
| D_error_not_value of (error * ret_value)
with sexp

type error_or_ret_value = ret_value error_or_value

type w_return =
| W_return_none
| W_return_choice of (ret_value list * ret_value)
| W_return_mixed_choice of (error_or_ret_value list * ret_value)

type bad_state =
| Special of (string * special_state finset)
| Empty of string

type 'os_state checked_os_finset =
| Bad_states    of bad_state
| Mixed_states  of (string * 'os_state finset * special_state finset)
| Normal_states of 'os_state finset

type 'os_state warning =
| W_return of w_return
| W_return_states
| W_dump_failed of string
| W_mixed_states of (string * special_state finset)

type 'os_state interp_line = {
  line_no : int option;
  lines   : Trace.line list;
  warns   : 'os_state warning list;
}

type 'os_state interp_state_fail = {
  bad_state : bad_state;
  interp    : 'os_state interp_line list;
}

type 'os_state interp_success = 'os_state finset * 'os_state interp_line list

type 'os_state interp =
| Interp of 'os_state interp_success
| Interp_state_fail of 'os_state interp_state_fail
| Interp_label_fail of (Trace.numbered_line * 'os_state interp_success)

type d_ret_lbl_error =
| D_ret_lbl_pid of ty_pid Diff.diff
| D_ret_lbl_errors_prohibited of error list
| D_ret_lbl_unexpected of error_or_ret_value list * error_or_ret_value list
| D_ret_lbl_diff of error_or_ret_value * d_eov * error_or_ret_value

type d_ret_lbl = {
  error : d_ret_lbl_error option;
  rvs   : error_or_ret_value list;
  reset : bool;
}

type d_lbl =
| D_check_dump_failed of (string * string)
| D_interp_dump_failed
| D_dump of (Dump.d_t * Dump.t)
| D_ret_lbl of d_ret_lbl
| D_ret_lbl_empty of d_ret_lbl_error option
| D_bad_state of bad_state
| D_mixed_states of (string * special_state finset)
| D_checklib_false_negative of (bad_state * error_or_ret_value list)
| D_checklib_false_positive of bad_state

type d_trace_line = {
  trace_ctxt : Trace.numbered_line list;
  trace_line : Trace.numbered_line;
  d_lbls     : d_lbl list;
} with sexp

type d_trace = d_trace_line list with sexp

exception Exec_error of string

val is_d_eov_zero : d_eov -> bool

(** {2 Output} *)

(** CheckLib produces varies outputs while processing traces. It is
    important to be able to store certain outputs in different files,
    print them or just discart them. This functionality is abstracted
    using [check_output_fun]s.  These functions get an
    [check_output_type] argument as well as a line of output as a
    string. Depending on the type and the application needs, various
    actions are taken. Some examples of such actions are:

    - print string to std_out (e.g. CO_normal in most situations)
    - print string to str_err (e.g. CO_error in most situations)
    - discard string (e.g. CO_verbose, if not in verbose output mode)
    - append string to some file (e.g. CO_verbose iff loggin is wanted)
    ...
*)

type check_output_type = 
| CO_error   (** Error message about a serious error, e.g. Parsing errors *)
| CO_warning (** Warning message, e.g. violation of file naming conventions *)
| CO_status  (** Status messages, like which file is currently processed *)
| CO_normal  (** Standard output *)
| CO_verbose (** Verbose output *)
| CO_dump    (** Output of dumps *)

type check_output_fun = check_output_type -> string -> unit 

(** [default_check_output_fun] is the default implementation of output
    fun.  It prints [CO_error] and [CO_warning] to [stderr] and the
    rest to [stdout]. *)
val default_check_output_fun : check_output_fun

(** [empty_check_output_fun] drops all outputs. *)
val empty_check_output_fun : check_output_fun

(** [drop_check_output_fun oty outf] drop output of type [oty] and
    otherwise behaves like [outf]. *)
val drop_check_output_fun :
  check_output_type -> check_output_fun -> check_output_fun

(** [combine_check_output_fun outf1 outf2] outputs via both [outf1]
    and [outf2]. *)
val combine_check_output_fun :
  check_output_fun -> check_output_fun -> check_output_fun

(** [out_channel_check_output_fun oty o outf] prints output of [oty]
    to out-channel [o] and uses [outf] otherwise. *)
val out_channel_check_output_fun :
  check_output_type -> out_channel -> check_output_fun -> check_output_fun

(** [buffer_check_output_fun b otys outf] appends all output of a type
    in [ofs] to the buffer [b].  If the type is not present in [otys],
    the function [outf] is used instead. This buffer is intended for
    writing to a file. Because no extra file-descriptors should be
    open during checking of traces, it is buffered before writing. *)
val buffer_check_output_fun :
  Buffer.t -> (check_output_type -> bool) -> check_output_fun -> check_output_fun

(** [buffer_to_file b outfile] write the contents of [b] to a file
    with name [outfile]. *)
val buffer_to_file : Buffer.t -> string -> unit

val string_of_special_states : special_state finset -> string

(** {2 Traces} *)

(** [trace_to_file arch ?change_type tr filename] stores a string
    representation of the trace [tr] for architecture [arch] in file
    [filename]. If [change_type] is set, the @type metadata field is
    changed from script to trace or vice versa. *)
val trace_to_file : ty_arch -> ?change_type:Trace.ty -> Trace.t -> string -> unit


(** The result signature when applying the Check functor *)
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
  val parse_print_trace_file : ty_arch -> check_output_fun -> string -> bool

  (** [print_interp arch outf script] prints the results of [script]
      with [outf] for platform [arch]. If any warnings or errors
      occurred during the interpretation, true will be returned along
      with the interpreted trace. *)
  val print_interp :
    ty_arch -> check_output_fun
    -> m_os_state interp -> bool * Trace.numbered_line list

  (** [print_check arch outf trace] prints the results of [trace] with
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

(** Produce the initial state for the spec; the bool argument is "no_root" *)
val spec_initial_state : ty_arch -> bool -> dh_os_state

module Check (M : Os_ops) :
  Check_result with type m_os_state = M.os_state

(** instance of Check for specification types *)
module Check_spec: Check_result with type m_os_state = dh_os_state 

