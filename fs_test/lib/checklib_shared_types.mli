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

type 'os_state gen_os_state_or_special =
    GOS_normal of 'os_state
  | GOS_special of
      (Fs_interface.Fs_spec_intf.Fs_types.monad_special_state * string)
module type Os_ops =
  sig
    type os_state
    val os_trans :
      os_state ->
      Fs_interface.Fs_spec_intf.Fs_types.os_label ->
      os_state gen_os_state_or_special Lem_support.finset
    val allowed_results_for_pid :
      Fs_interface.Fs_spec_intf.Fs_types.ty_pid ->
      os_state ->
      Fs_interface.Fs_spec_intf.Fs_types.ret_value
      Fs_interface.Fs_spec_intf.Fs_types.error_or_value Lem_support.finset
    val dump_of_path : os_state -> string -> Dump.t
  end
