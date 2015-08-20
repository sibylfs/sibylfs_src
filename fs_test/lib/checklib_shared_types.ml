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

(* types shared between checlib and posix_ops *)
open Fs_interface.Fs_spec_intf.Fs_types
open Lem_support

(** The type [gen_os_state_or_special] is a generalisation of
    [os_state_or_special]. Since for testing, we might want to use
    different concrete state-types, but still have the special states
    in the same form, this is necessary. For uses, see [os_ops]. *)
type 'os_state gen_os_state_or_special = 
| GOS_normal of 'os_state
| GOS_special of (monad_special_state * string)

module type Os_ops = sig
  type os_state
(*  val arch : ty_arch *)
(*  val initial_state : os_state*)
  val os_trans :
    os_state -> os_label -> (os_state gen_os_state_or_special) finset
  val allowed_results_for_pid :
    ty_pid -> os_state -> (ret_value error_or_value) finset
  val dump_of_path : os_state -> string -> Dump.t
end;;


(* FIXME move this into the mli
(** {2 Running traces} *)


(** A module of type [os_ops] encodes everything necessary to run
    os-traces in a certain environment. *)
module type os_ops = sig
  (** The concrete implementation type of states. It represents
      concrete states; special ones are encoded using [os_state
      gen_os_state_or_special] *)
  type os_state 

  (** [arch] stores the architecture of the os-ops. It is used for
      parsing traces.  It is important that the architecture argument
      matches the behaviour of [os_trans]. *)
(*  val arch : ty_arch  *)

  (** [initial_state] is the initial state of the system. It is used
      as the starting state for running traces. *)
(*  val initial_state : os_state *)

  (** [os_trans] is the main function. It encodes how the system
      really behaves. Given a state [s0] and a label [lbl], the call
      [os_trans s0 lbl] returns the set of all possible results of
      evaluating the label in state [s0]. These results may include
      special states. *)
  val os_trans :
    os_state -> os_label -> (os_state gen_os_state_or_special) finset

  (** For debugging purposes, it is important to be able the extract
      all allowed results. The function [allowed_results_for_pid] is
      used for that. If a check fails, it is used for generating error
      messages as well as possible recovery and continued
      execution. *)
  val allowed_results_for_pid :
    ty_pid -> (os_state gen_os_state_or_special)
    -> (ret_value error_or_value) finset

  (** [dump_of_path s0 p] dumps the file-system state in state [s0]
      and path [p] *)
  val dump_of_path : os_state -> string -> Dump.t option

end
*)
