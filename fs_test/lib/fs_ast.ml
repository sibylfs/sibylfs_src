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

module Fs_types = Fs_interface.Fs_spec_intf.Fs_types

exception Parse_error of string

type pid = int
type uid = int
type gid = int
type fd = int
type perm = Fs_types.file_perm
type size = int
type offset = int
type timestamp = Fs_types.ty_os_timestamp
type dh = int

type cstring = Fs_types.cstring = CS_Null | CS_Some of string
type open_flags = Fs_types.open_flag list

type timestamp_field =
| T_sec of int
| T_nsec of int64

type stat_field =
| St_dev of int
| St_ino of Fs_types.inode
| St_kind of Fs_types.file_kind
| St_perm of perm
| St_nlink of int
| St_uid of Fs_types.uid
| St_gid of Fs_types.gid
| St_rdev of int
| St_size of int64
| St_atim of timestamp
| St_mtim of timestamp
| St_ctim of timestamp

type seek_command =
| Seek_set
| Seek_cur
| Seek_end
| Seek_data
| Seek_hole
| Seek_whence of int

type op =
| Close of fd
| Link of (cstring * cstring)
| Mkdir of (cstring * perm)
| Open of (cstring * open_flags * perm option)
| Open_close of (cstring * open_flags * perm option)
| Pread of (fd * size * offset)
| Pread_det of (fd * size * offset)
| Pwrite of (fd * cstring * size * offset)
| Pwrite_det of (fd * cstring * size * offset)
| Read of (fd * size)
| Read_det of (fd * size)
| Readdir of dh
| Rewinddir of dh
| Opendir of cstring
| Closedir of dh
| Readlink of cstring
| Rename of (cstring * cstring)
| Rmdir of cstring
| Stat of cstring
| Lstat of cstring
| Symlink of (cstring * cstring)
| Truncate of (cstring * size)
| Unlink of cstring
| Write of (fd * cstring * size)
| Write_det of (fd * cstring * size)
| Add_user_to_group of (uid * gid)
| Chown of (cstring * uid * gid)
| Chmod of (cstring * perm)
| Chdir of cstring
| Lseek of (fd * offset * seek_command)
| Umask of perm

type action =
| Create
| Create_uid of (uid * gid)
| Destroy
| Call of op

type script_line =
| Comment_script of string
| Nl_script
| Type_script
| Dump of string
| Action of (pid option * action)

type return =
| Simple_return of Fs_types.ret_value Fs_types.error_or_value
| No_error_return
| Multiple_return of Fs_types.ret_value Fs_types.error_or_value list

type trace_line =
| Comment_trace of string
| Nl_trace
| Type_trace
| Dump_internal
| Dump_result of (string * Dump.t)
| Action_trace of (pid option * action)
| Tau
| Return of (pid option * return)

type script_or_trace =
| Script of script_line list
| Trace of trace_line list

let error msg = raise (Parse_error msg)

let string_of_loc (pos1, pos2) =
  let open Lexing in
  let line1 = pos1.pos_lnum in
  let start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %d, characters %d-%d:"
    pos1.pos_fname line1 (pos1.pos_cnum - start1) (pos2.pos_cnum - start1)

let add_comment c = function
  | Script s -> Script ((Comment_script c) :: s)
  | Trace t  -> Trace  ((Comment_trace c)  :: t)

let add_newline = function
  | Script s -> Script (Nl_script :: s)
  | Trace t  -> Trace  (Nl_trace :: t)
