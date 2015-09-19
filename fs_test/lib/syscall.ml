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

type file_descr = Unix.file_descr
type file_kind = Unix.file_kind =
                 | S_REG
                 | S_DIR
                 | S_CHR
                 | S_BLK
                 | S_LNK
                 | S_FIFO
                 | S_SOCK
type file_perm = Unix.file_perm

type dir_handle = Unix.dir_handle

type stats = Unix.LargeFile.stats = {
  st_dev : int; 	(*	Device number	*)
  st_ino : int; 	(*	Inode number	*)
  st_kind : file_kind; 	(*	Kind of the file	*)
  st_perm : file_perm; 	(*	Access rights	*)
  st_nlink : int; 	(*	Number of links	*)
  st_uid : int; 	(*	User id of the owner	*)
  st_gid : int; 	(*	Group ID of the file's group	*)
  st_rdev : int; 	(*	Device minor number	*)
  st_size : int64; 	(*	Size in bytes	*)
  st_atime : float; 	(*	Last access time	*)
  st_mtime : float; 	(*	Last modification time	*)
  st_ctime : float; 	(*	Last status change time	*)
}

type process_status = Unix.process_status =
                      | WEXITED of int
                      | WSIGNALED of int
                      | WSTOPPED of int
type wait_flag = Unix.wait_flag

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

let file_descr_of_int = Fd_send_recv.fd_of_int
let int_of_file_descr = Fd_send_recv.int_of_fd

let readlink = Unix.readlink
let symlink  = Unix.symlink

let open_ path ?perms oflags = Errno_unix.with_unix_exn (fun () ->
  match perms with
  | Some perms -> Fcntl_unix.open_ path ~perms oflags
  | None       -> Fcntl_unix.open_ path oflags
)

let rename   = Unix.rename
let link     = Unix.link
let chmod    = Unix.chmod
external stat  : string -> stats = "core_unix_stat_64"
external lstat : string -> stats = "core_unix_lstat_64"

(* TODO: potentially non-portable
   should define valid and invalid symbolic commands *)
external lseek : file_descr -> int -> int -> int = "unix_lseek_int_command"

let read fd sz =
  let buf = Bytes.create sz in
  let len = Unix.read fd buf 0 sz in
  String.sub buf 0 len

let read_full fd sz =
  let buf = Bytes.create sz in
  let rec read_full boff =
    if boff = sz then boff else
      let len = Unix.read fd buf boff (sz - boff) in
      if len = 0 then boff
      else read_full (boff + len)
  in
  let len = read_full 0 in
  String.sub buf 0 len

external unix_pread : file_descr -> string -> int -> int -> int -> int
                    = "unix_pread"
let pread fd sz off =
  let buf = Bytes.create sz in
  let len = unix_pread fd buf 0 sz off in
  String.sub buf 0 len

let pread_full fd sz off =
  let buf = Bytes.create sz in
  let rec pread_full boff =
    if boff = sz then boff else
      let len = unix_pread fd buf boff (sz - boff) (off + boff) in
      if len = 0 then boff
      else pread_full (boff + len)
  in
  let len = pread_full 0 in
  String.sub buf 0 len

let write fd buf sz = Unix.single_write fd buf 0 sz

let write_full fd buf sz =
  let rec write_full boff =
    if boff = sz then boff else
      let len = Unix.single_write fd buf boff (sz - boff) in
      write_full (boff + len)
  in
  write_full 0

external unix_pwrite : file_descr -> string -> int -> int -> int -> int
                     = "unix_pwrite"
let pwrite fd buf sz off = unix_pwrite fd buf 0 sz off

let pwrite_full fd buf sz off =
  let rec pwrite_full boff =
    if boff = sz then boff else
      let len = unix_pwrite fd buf boff (sz - boff) (off + boff) in
      pwrite_full (boff + len)
  in
  pwrite_full 0

let close    = Unix.close
let unlink   = Unix.unlink
let truncate = Unix.truncate

let mkdir    = Unix.mkdir
let chdir    = Unix.chdir
let opendir  = Unix.opendir
let readdir  = Unix.readdir
let rewinddir= Unix.rewinddir
let closedir = Unix.closedir
let rmdir    = Unix.rmdir

let getcwd   = Unix.getcwd
let chown    = Unix.chown
let umask    = Unix.umask
let getuid   = Unix.getuid
let geteuid  = Unix.geteuid
let getgid   = Unix.getgid
let getegid  = Unix.getegid
let initgroups = Unix.initgroups

external setegid : int -> unit = "unix_setegid"
external seteuid : int -> unit = "unix_seteuid"

external getpwnam : string -> passwd_entry = "unix_getpwnam_uerror"
external getgrnam : string -> group_entry = "unix_getgrnam_uerror"

let fork     = Unix.fork
let waitpid  = Unix.waitpid
