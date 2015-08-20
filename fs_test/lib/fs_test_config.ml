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

let string_of_time t =
  (* TODO: this shouldn't have to depend on Unix... *)
  let open Unix in
  let tm = gmtime t in
  Printf.sprintf "%d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let name_of_string s = s

module Fs = struct
  type fuse_param = [
  | `Allow_other
  | `Default_permissions
  | `Umask0
  ] with sexp

  type t =
  | Tmpfs
  | Ext2_loop
  | Ext3_loop
  | Ext4_loop
  | Btrfs_loop
  | F2fs_loop
  | Minix_loop
  | Bind of t
  | Fusexmp of (fuse_param list * t)
  | Sshfs of (fuse_param list * t)
  | Davfs of t
  | Nfsv3 of t
  | Nfsv4 of t
  | Posixovl of (fuse_param list * t)
  | Aufs of t list (* bottom to top *)
  | Overlay of t * t (* bottom to top *)
  | Gluster of t
  | Ntfs3g_loop
  | Fuse_ext2_loop
  | Fuse_ext3_loop
  | Hfsplus_loop
  | Ufs2_loop
  | Nilfs2_loop
  | Vfat_loop
  | Xfs_loop
  | Zfs_loop
  with sexp

  type version =
  | Function of version * version
  | Fuse of string * string
  | Version of string
  | System
  with sexp

  let rec string_of_version = function
    | System -> "system"
    | Function (dom, cod) ->
      (string_of_version cod)^" over "^(string_of_version dom)
    | Fuse (fusev, fsv) -> fsv^" fuse "^fusev
    | Version v -> v

  let rec name_of_version = function
    | System -> "system"
    | Function (dom,cod) ->
      "["^(name_of_version dom)^"]["^(name_of_version cod)^"]"
    | Fuse (fusev, fsv) ->
      "["^(name_of_string fsv)^"][fuse="^(name_of_string fusev)^"]"
    | Version v -> name_of_string v

  let string_of_fuse_param = function
    | `Allow_other -> "allow_other"
    | `Default_permissions -> "default_permissions"
    | `Umask0 -> "umask=0"

  let name_of_fuse_param = function
    | `Allow_other -> "all"
    | `Default_permissions -> "defperms"
    | `Umask0 -> "umask0"

  let name_of_fuse_params ps =
    String.concat "" (List.map (fun p -> "_"^(name_of_fuse_param p)) ps)

  let rec name = function
    | Tmpfs        -> "tmpfs"
    | Ext2_loop    -> "ext2_loop"
    | Ext3_loop    -> "ext3_loop"
    | Ext4_loop    -> "ext4_loop"
    | Btrfs_loop   -> "btrfs_loop"
    | F2fs_loop    -> "f2fs_loop"
    | Minix_loop   -> "minix_loop"
    | Bind fs      -> "bind_"^(name fs)
    | Fusexmp (ps,fs) -> "fusexmp"^(name_of_fuse_params ps)^"_"^(name fs)
    | Sshfs (ps,fs)   -> "sshfs"^(name_of_fuse_params ps)^"_"^(name fs)
    | Davfs fs     -> "davfs_"^(name fs)
    | Nfsv3 fs      -> "nfsv3_"^(name fs)
    | Nfsv4 fs      -> "nfsv4_"^(name fs)
    | Posixovl (ps,fs)-> "posixovl"^(name_of_fuse_params ps)^"_"^(name fs)
    | Aufs btt     ->
      List.fold_left (fun s fs -> s^"_("^(name fs)^")") "aufs" btt
    | Overlay (b,t)-> "overlay_("^(name b)^")_("^(name t)^")"
    | Gluster fs   -> "gluster_"^(name fs)
    | Ntfs3g_loop  -> "ntfs3g_loop"
    | Fuse_ext2_loop -> "fuse_ext2_loop"
    | Fuse_ext3_loop -> "fuse_ext3_loop"
    | Hfsplus_loop -> "hfsplus_loop"
    | Ufs2_loop    -> "ufs2_loop"
    | Nilfs2_loop  -> "nilfs2_loop"
    | Vfat_loop    -> "vfat_loop"
    | Xfs_loop     -> "xfs_loop"
    | Zfs_loop     -> "zfs_loop"

  let versioned_name (fs,v) = (name fs)^"["^(name_of_version v)^"]"
end

module Os = struct
  type t =
  | Linux
  | Darwin
  | FreeBSD
  with sexp

  type version = {
    node    : string;
    release : string;
    version : string;
    machine : string;
    all     : string;
  } with sexp

  let of_string = function
    | "Darwin"  -> Some Darwin
    | "FreeBSD" -> Some FreeBSD
    | "Linux"   -> Some Linux
    | _         -> None

  let to_string = function
    | Darwin  -> "Darwin"
    | FreeBSD -> "FreeBSD"
    | Linux   -> "Linux"

  let basic_fs = function
    | Darwin -> Fs.Hfsplus_loop
    | Linux | FreeBSD -> Fs.Tmpfs

  let name = function
    | Darwin  -> "darwin"
    | FreeBSD -> "freebsd"
    | Linux   -> "linux"

  let name_of_version ({ release }) = name_of_string release
  let versioned_name (os,v) = (name os)^"["^(name_of_version v)^"]"
end

module Libc = struct
  type t = System | Musl
  with sexp

  type version = string with sexp

  let name = function System -> "system" | Musl -> "musl"

  let name_of_version = name_of_string
  let versioned_name (libc,v) = (name libc)^"["^(name_of_version v)^"]"
end

module Spec = struct
  type flavor =
  | Linux
  | Posix
  | Mac_os_x
  | FreeBSD
  with sexp

  let name_of_flavor = function
    | Linux    -> "linux_spec"
    | Posix    -> "posix_spec"
    | Mac_os_x -> "mac_os_x_spec"
    | FreeBSD  -> "freebsd"

  module Descr = struct
    type t = {
      flavor : flavor;
    } with sexp

    let name ({ flavor }) = name_of_flavor flavor
  end

  type t = {
    flavor : flavor;
    fs_spec_version : string;
  } with sexp

  let name ({ flavor; fs_spec_version }) =
    Printf.sprintf "%s@[%s]" (name_of_flavor flavor) fs_spec_version

  let to_descr ({ flavor; }) = Descr.({ flavor; })
end

module Model = struct
  type ('mnt,'spec) t =
  | Mount of 'mnt
  | Spec of 'spec
  with sexp

  let combine mount spec = function
    | Mount fs -> mount fs
    | Spec s   -> spec s

  let map mount spec =
    combine (fun fs -> Mount (mount fs)) (fun s -> Spec (spec s))
end

module Stack = struct
  module Descr = struct
    type t = {
      os   : Os.t;
      fs   : Fs.t;
      libc : Libc.t;
    } with sexp
  end

  type t = {
    time : float;
    os   : Os.t * Os.version;
    fs   : Fs.t * Fs.version;
    libc : Libc.t * Libc.version;
    fs_test_version : string;
  } with sexp

  let to_descr ({ os = (os,_); fs = (fs,_); libc = (libc,_); }) =
    Descr.({ os; fs; libc; })

  let name s =
    (Os.versioned_name s.os)
    ^"["^(Libc.versioned_name s.libc)^"]"
    ^"["^(Fs.versioned_name s.fs)^"]"
    ^"@["^(string_of_time s.time)^"."^s.fs_test_version^"]"
end

module Descr = struct
  type t = (Stack.Descr.t, Spec.Descr.t) Model.t with sexp
end

type t = (Stack.t, Spec.t) Model.t with sexp

let to_descr = Model.map Stack.to_descr Spec.to_descr

let name = Model.combine Stack.name Spec.name

module Check = struct
  type config = t
  type t = {
    config : config;
    spec   : Spec.t;
  }

end
