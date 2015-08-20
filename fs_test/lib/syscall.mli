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

type file_descr
type file_kind = Unix.file_kind =
                 | S_REG
                 | S_DIR
                 | S_CHR
                 | S_BLK
                 | S_LNK
                 | S_FIFO
                 | S_SOCK
type file_perm  = Unix.file_perm

type dir_handle

type stats =
  Unix.LargeFile.stats = {
  st_dev : int;
  st_ino : int;
  st_kind : file_kind;
  st_perm : file_perm;
  st_nlink : int;
  st_uid : int;
  st_gid : int;
  st_rdev : int;
  st_size : int64;
  st_atime : float;
  st_mtime : float;
  st_ctime : float;
}

type process_status =
  Unix.process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag

type passwd_entry = Unix.passwd_entry = {
  pw_name : string;
  pw_passwd : string;
  pw_uid : int;
  pw_gid : int;
  pw_gecos : string;
  pw_dir : string;
  pw_shell : string;
}

type group_entry = Unix.group_entry = {
  gr_name : string;
  gr_passwd : string;
  gr_gid : int;
  gr_mem : string array;
}

val file_descr_of_int : int -> file_descr
val int_of_file_descr : file_descr -> int

val readlink : string -> string
val symlink : string -> string -> unit
val open_ : string -> ?perms:file_perm -> Fcntl.Oflags.t list -> file_descr
val rename : string -> string -> unit
val link : string -> string -> unit
val chmod : string -> file_perm -> unit
val stat : string -> stats
val lstat : string -> stats
val lseek : file_descr -> int -> int -> int

val read : file_descr -> int -> string
val read_full : file_descr -> int -> string
val pread : file_descr -> int -> int -> string
val pread_full : file_descr -> int -> int -> string

val write : file_descr -> string -> int -> int
val write_full : file_descr -> string -> int -> int
val pwrite : file_descr -> string -> int -> int -> int
val pwrite_full : file_descr -> string -> int -> int -> int

val close : file_descr -> unit
val unlink : string -> unit
val truncate : string -> int -> unit

val mkdir : string -> file_perm -> unit
val chdir : string -> unit
val opendir : string -> dir_handle
val readdir : dir_handle -> string
val rewinddir : dir_handle -> unit
val closedir : dir_handle -> unit
val rmdir : string -> unit

val getcwd : unit -> string
val chown : string -> int -> int -> unit
val umask : int -> int
val getuid : unit -> int
val geteuid : unit -> int
val getgid : unit -> int
val getegid : unit -> int
val initgroups : string -> int -> unit

val setegid : int -> unit
val seteuid : int -> unit

val getpwnam : string -> passwd_entry
val getgrnam : string -> group_entry

val fork : unit -> int
val waitpid : wait_flag list -> int -> int * process_status
