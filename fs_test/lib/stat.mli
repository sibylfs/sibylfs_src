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

type t = Fs_interface.Fs_spec_intf.Fs_types.ty_os_stats
type d_t = {
  d_st_kind : Fs_interface.Fs_spec_intf.Fs_types.file_kind Diff.diff;
  d_st_perm : Fs_interface.Fs_spec_intf.Fs_types.file_perm Diff.diff;
  d_st_size : int64 Diff.diff;
  d_st_nlink : int Diff.diff;
  d_st_uid : Fs_interface.Fs_spec_intf.Fs_types.uid Diff.diff;
  d_st_gid : Fs_interface.Fs_spec_intf.Fs_types.gid Diff.diff;
}
val __d_t_of_sexp__ : Sexplib.Sexp.t -> d_t
val d_t_of_sexp : Sexplib.Sexp.t -> d_t
val sexp_of_d_t : d_t -> Sexplib.Sexp.t
val is_d_zero : d_t -> bool
val diff :
  Fs_interface.Fs_spec_intf.Fs_types.ty_os_stats ->
  Fs_interface.Fs_spec_intf.Fs_types.ty_logical_stats -> d_t
