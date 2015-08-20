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

(* Dump                                                                       *)
(*                                                                            *)
(* This library is concerned with creating dumps of the current fs-state      *)

open Diff

(** {2 Basic types} *)

type file = Fs_interface.Fs_dump_intf.file = {
    file_path : string;
    file_node : int;
    file_size : int;
    file_sha : string;
  } with sexp

type dir = Fs_interface.Fs_dump_intf.dir = {
  dir_path : string;
  dir_node : int;
} with sexp

type symlink = Fs_interface.Fs_dump_intf.symlink = {
  link_path : string;
  link_val : string;
} with sexp

type error = Fs_interface.Fs_dump_intf.error with sexp

type dump_error = Unix_error of error | Unknown of string

exception Dump_error of dump_error

type entry = Fs_interface.Fs_dump_intf.entry =
  | DE_file of file
  | DE_dir of dir
  | DE_symlink of symlink
  | DE_error   of error
with sexp

type t = entry list with sexp

(* Difference types *)

type d_file = {
  d_files     : file * file;
  d_file_path : string diff;
  d_file_size : int diff;
  d_file_sha  : string diff;
} with sexp

type d_dir = {
  d_dirs     : dir * dir;
  d_dir_path : string diff;
} with sexp

type d_symlink = {
  d_links     : symlink * symlink;
  d_link_path : string diff;
  d_link_val  : string diff;
} with sexp

type d_error = {
  d_errors     : error * error;
  d_error      : Fs_interface.Fs_spec_intf.Fs_types.error diff;
  d_error_call : string diff;
  d_error_path : string diff;
} with sexp

type deviant =
| DDE_file of d_file
| DDE_dir of d_dir
| DDE_symlink of d_symlink
| DDE_error of d_error
with sexp

type d_entry =
| DDE_missing of entry
| DDE_unexpected of entry
| DDE_deviant of deviant
| DDE_type_error of entry diff
with sexp

type d_t =
| D_path of string diff
| D_t of d_entry list
with sexp

val path : entry -> string
val to_csv_strings : t -> string list

(** [diff da1 da2] checks whether the label [Dump da1]
    and [Dump da2] agree with each other. It returns an optional
    error message. If [None] is returned, everything is OK. *)
val diff : string * t -> string * t -> d_t option

val string_of_d_t : d_t -> string

(** {2 Creating dumps} *)

(** The module type [Dump_fs_operations] abstacts the operations
    needed to greate a dump of a fs *)
module type Dump_fs_operations = sig  
  type state 
  type dir_status

  (** [ls_path s p] returns a list of all the files and directories
      under path [p] excluding [p] itself. It does not list the
      content of subdirectories. [p] is assumed to be a directory (not
      a symlink to a directory). *)
  val ls_path : state -> Fs_path.t -> string list

  (** [kind_of_path s p] returns the file-kind of the file
      identified by path [p], If such a file does not exist, or
      is not a dir, regular file or symbolic link, [kind_of_path]
      fails. *)
  val kind_of_path : state -> Fs_path.t -> Unix.file_kind

  (** [sha1_of_path s p] returns the file-size and the sha hash of
      path [p] in state [s]. If [path] is not a regular file,
      the function fails. *)
  val sha1_of_path : state -> Fs_path.t -> (int * string)

  (** [readlink s p] reads the symbolic link indentified by path [p]
      in state [s]. If [path] is not a symbolic link,
      the function fails. *)
  val readlink : state -> Fs_path.t -> string

  (** [inode_of_file_path s p] gets the inode of the file indentified
      by path [p] in state [s]. If [path] is not a regular file, the
      function fails. *)
  val inode_of_file_path : state -> Fs_path.t -> int

  (** [inode_of_dir_path s p] gets the inode of the file indentified
      by path [p] in state [s]. If [path] is not a directory, the
      function fails. *)
  val inode_of_dir_path : state -> Fs_path.t -> int

  (** [enter_dir p] indicates that a directory is entered. This
      might be used to set some permissions so further commands
      won't fail. It returns some state, which is used to
      undo changes via [leave_dir] *)
  val enter_dir : state -> Fs_path.t -> dir_status

  (** see [enter_dir] *)
  val leave_dir : state -> Fs_path.t -> dir_status -> unit

end


(** [Dump_fs_operations] are used to instantiate [Make] to create dumps. *)
module Make(DO : Dump_fs_operations) : sig
  (** [of_path s0 p] creates dump of path [p] in
      state [s0]. Dump might fail for various reasons. In
      this case [Dump_error] is raised. Common reasons for it to
      fail are [p] not describing and existing directory in
      state [s0] or insufficient permissions to dump this
      directory. *)
  val of_path : DO.state -> string -> t
end


