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

exception Parse_error of string

type arch = Fs_interface.Fs_spec_intf.Fs_types.ty_arch

type ty = TyTrace | TyScript

type pid = Fs_interface.Fs_spec_intf.Fs_types.ty_pid

type os_extended_label =
| OS_simple_label of Fs_interface.Fs_spec_intf.Fs_types.os_label
| OS_no_error_return of pid
| OS_multiple_return of
    (pid * (Fs_interface.Fs_spec_intf.Fs_types.ret_value Fs_interface.Fs_spec_intf.Fs_types.error_or_value) list)

type line =
| Comment of string
| Newline
| Label of (bool * os_extended_label)
| Dump of string
| Dump_result of (string * Dump.t)
with sexp

type numbered_line = int option * line with sexp

type untyped = numbered_line list

type t = ty * untyped

type indent = numbered_line -> string

(** [type_of tr] returns the type of trace *)
val type_of : t -> ty

(** [of_string arch tr_s] parses a string [tr_s] as a trace for
    architecture [arch]. It may raise [Parse_error] exception. *)
val of_string : arch -> string -> t

(** [of_file arch tr_f] parses a file [tr_f] as a trace for
    architecture [arch]. It may raise [Parse_error] exception. *)
val of_file : arch -> string -> t

val make_indent : ?number:bool -> int -> indent

val indent_trace : indent

val input_string_of_extended_label : arch -> bool * os_extended_label -> string

val dump_to_string : ?ws:int -> ?indent:indent -> Dump.t -> string

val numbered_line_to_string :
  arch -> ?change_type:ty -> ?indent:indent -> numbered_line -> string

(** [to_string arch ?change_type ?indent tr] returns a string
    representation of the trace [tr] for architecture [arch] indented
    with function [indent] and with the @type metadata set according
    to [change_type]. *)
val to_string : arch -> ?change_type:ty -> ?indent:indent -> t -> string

(* FIXME what is the point of this? it seems to be used only in testgen *)
module Label : sig
  val create : pid:int -> uid:int -> gid:int -> line
  val add_user_to_group : uid:int -> gid:int -> line
  val umask : ?pid:int -> int -> line
  val mkdir : ?pid:int -> string -> int -> line
  val chown : ?pid:int -> uid:int -> gid:int -> string -> line
  val chmod : ?pid:int -> string -> int -> line
  val chdir : ?pid:int -> string -> line
  val open_close :
    ?pid:int -> arch -> string -> ?perm:int -> Fs_interface.Fs_spec_intf.Fs_types.open_flag list -> line
  val open_ :
    arch -> string -> ?perm:int -> Fs_interface.Fs_spec_intf.Fs_types.open_flag list -> line
  val det_write : fd:int -> string -> int -> line
  val read : fd:int -> int -> line
  val close : fd:int -> line
  val symlink : string -> string -> line
  val link : string -> string -> line
  val dump : string -> line
  val rename : string -> string -> line
  val rmdir : string -> line
  val unlink : string -> line
  val stat : string -> line
  val lstat : string -> line
  val truncate : string -> int -> line
end
