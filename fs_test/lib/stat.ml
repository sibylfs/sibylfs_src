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

open Sexplib.Std

open Diff
open Fs_interface.Fs_spec_intf.Fs_types

type t = ty_os_stats

type d_t = {
  d_st_kind  : file_kind diff;
  d_st_perm  : file_perm diff;
  d_st_size  : int64 diff;
  d_st_nlink : int diff;
  d_st_uid   : uid diff;
  d_st_gid   : gid diff;
  (* timestamps have different types in the spec and in real world:
  cannot be compared like this *)
} with sexp

let is_d_zero = function
  | { d_st_kind  = None; d_st_perm  = None; d_st_size  = None;
      d_st_nlink = None; d_st_uid   = None; d_st_gid   = None;
    } -> true
  | _ -> false

(** [diff st st_spec] checks whether [st] of type [ty_stats]
    is subsumed by [st_spec]. For stat results, this is a bit
    tricky. For example, inode numbers or device numbers might be
    different and we will still accept the result. Also, comparing
    time stamps will probably become tricky in the future. *)
let diff st st_spec = {
  (* lets start with the simple record fields that should really be equal *)
  d_st_kind  = diff st.os_st_kind st_spec.l_st_kind; (** Kind of the file *)
  d_st_perm  = diff st.os_st_perm st_spec.l_st_perm; (** Access rights *)
  (* dir size isn't well defined *)
  d_st_size  = (match st_spec.l_st_kind with
  | S_IFDIR -> None
  | _       -> diff st.os_st_size st_spec.l_st_size); (** Size in bytes *)
  d_st_nlink = diff st.os_st_nlink st_spec.l_st_nlink; (** Number of links *)

  (* uid and gid should be the same, because posix takes care of the
     mapping for us *)
  d_st_uid   = diff st.os_st_uid st_spec.l_st_uid;
  d_st_gid   = diff st.os_st_gid st_spec.l_st_gid;

  (* do not consider inode and device numbers for now. 
     (st.st_dev   = st_spec.st_dev)   (** Device number *)  &&
     (st.st_ino   = st_spec.st_ino)   (** Device minor number *)  &&
     (st.st_rdev  = st_spec.st_rdev)  (** Inode number *)  &&
  *)
}
