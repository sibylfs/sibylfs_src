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

(* Some thoughts on a new interface to checklib *)

(* the type of labels - think of as spec type os_label, but actually includes comments etc  *)
type trace_line 

type trace = trace_line list

type arch = Trace.arch

(* provide functionality to load a trace from a file, or a string; return None in case of failure *)
val file_to_string: string -> string option
(* FIXME the dependence of trace on arch is implicit; a trace should probably include the arch *)
val string_to_trace : arch -> string -> trace option
(* write a string to file; first arg is string; return whether the operation was successful or not *)
val write_string_to_file: string -> string -> bool



val trace_line_to_string: arch -> trace_line -> string



(* the basic states we are interested in *)
type spec_state_or_special (* = dh_os_state + special; may want to expose this type to allow easier manipulation *)
val mk_spec_state_or_special: Fs_interface.Dir_heap_intf.Dir_heap_types.dh_os_state_or_special -> spec_state_or_special

(* we track a list of states so we can record the evolution of a
   particular state; the head of the list is the most recent state; the
   rest are predecessors, starting with the state that lead to the head
   state *)
type history = spec_state_or_special list

(* this is a set of histories *)
type state_set 

(* we can convert the set of histories to a list *)
val state_set_to_history_list: state_set -> history list
val history_list_to_state_set: history list -> state_set

(* construct an initial state set, with histories consisting of single states *)
val mk_state_set: spec_state_or_special -> state_set




(* a checked trace is what results from checking a trace, starting from some initial state *)
type 'a full_trace = { sequence: ('a * trace_line) list; initial: 'a }

type checked_trace = state_set full_trace

type error = 
  | E_special_states
  | E_no_normal_states

type state_set_maybe_error = SE_state_set of state_set | SE_error of error list * state_set

type debugged_trace = state_set_maybe_error full_trace

val sexp_of_debugged_trace: debugged_trace -> Sexplib.Sexp.t

val sexp_to_string: Sexplib.Sexp.t -> string



(* make a transition with a given line *)
val check_line: arch -> state_set -> trace_line -> state_set

(* apply check_line repeatedly; ends when no more lines *)
val check_trace: arch -> trace -> state_set -> checked_trace

(* identify any errors in a state set *)
val debug_state_set: state_set -> state_set_maybe_error

(* apply debug_state_set over a checked trace *)
val debug_trace: checked_trace -> debugged_trace


