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

open Printf
open Fs_test_system

module Fs = Fs_test_config.Fs
module Os = Fs_test_config.Os

type params = {
  mnt_path : string;
  size     : int64;
  system   : Fs_test_config.Os.t;
}

type t = {
  fs            : Fs_test_config.Fs.t;
  params        : params;
  mnt           : string;
  subdir        : string option;
  mutable users : int;
}

let exec_path = Unix.getcwd ()
let cmd_path = Filename.dirname Sys.argv.(0)
let cmd_path = Filename.(
  if is_relative cmd_path then concat exec_path cmd_path else cmd_path
)

let sector_size = 512_L
let sector_count sz = Int64.(div sz sector_size)
let blk_size = 4096_L
let blk_count sz = Int64.(div sz blk_size)
let blk_overhead = 512_L
let subfs_blk_count sz = Int64.(sub (blk_count sz) blk_overhead)

let memo_query fn =
  let q = Lazy.from_fun fn in
  fun () -> Lazy.force q

let dpkg_version pkg =
  let command = "dpkg -s "^pkg^" | grep Version" in
  let version_line = List.hd (read_command command) in
  let line_length = String.length version_line in
  let version_lbl = "Version: " in
  let lbl_length = String.length version_lbl in
  if String.sub version_line 0 lbl_length = version_lbl
  then String.sub version_line lbl_length (line_length - lbl_length)
  else begin
    printf "Can't read \"%s\": got \"%s\". Aborting.\n" command version_line;
    raise Not_found
  end

let plist_version plist =
  let key = "CFBundleShortVersionString" in
  List.hd (read_command ("defaults read "^plist^" "^key))

let tmpfs_if_available () = match get_system () with
  | Os.Darwin ->
    printf "tmpfs not available on Darwin/OS X; skipping tmpfs\n%!";
    []
  | Os.Linux | Os.FreeBSD -> Fs.([Tmpfs, System])

let btrfs_version = memo_query (fun () ->
  Fs.Version (List.hd (read_command_err "mkfs.btrfs --version"))
)

let btrfs_if_available () =
  try
    let btrfs_version = btrfs_version () in
    Fs.([ Btrfs_loop, btrfs_version ])
  with Command_failure (_,_,_) ->
    printf "mkfs.btrfs not found; skipping btrfs\n%!";
    []

let f2fs_version = memo_query (fun () ->
  let lines = read_command ~exit_code:1 "mkfs.f2fs" in
  Fs.Version (String.trim List.(hd (tl lines)))
)

let f2fs_if_available () =
  try
    let f2fs_version = f2fs_version () in
    Fs.([ F2fs_loop, f2fs_version ])
  with Command_failure (_,_,_) ->
    printf "mkfs.f2fs not found; skipping f2fs\n%!";
    []

let minix_version = memo_query (fun () ->
  Fs.Version (List.hd (read_command "fsck.minix --version"))
)

let minix_if_available () =
  try
    let minix_version = minix_version () in
    Fs.([ Minix_loop, minix_version ])
  with Command_failure (_,_,_) ->
    printf "fsck.minix not found; skipping minix\n%!";
    []

let fuse_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux  -> List.hd (read_command "fusermount --version")
  | Os.Darwin ->
    List.hd (read_command_err ~exit_code:64
               "/Library/Filesystems/osxfusefs.fs/Support/mount_osxfusefs")
  | Os.FreeBSD ->
    List.hd (read_command ~exit_code:64 "mount_fusefs --version")
)

let ext_version = memo_query (fun () ->
  match get_system () with
  | Os.FreeBSD (* really? probably there is a better version of the port... *)
  | Os.Linux -> Fs.Version (List.hd (read_command_err "mke2fs -V"))
  | Os.Darwin ->
    let plist = "/System/Library/Filesystems/ufsd_ExtFS.fs/Contents/Info.plist" in
    Fs.Version ("Paragon ExtFS "^(plist_version plist))
)

let extfs_if_available ?(only=[2;3;4]) () =
  try
    let ext_version = ext_version () in
    List.map Fs.(function
    | 2 -> Ext2_loop, ext_version
    | 3 -> Ext3_loop, ext_version
    | 4 -> Ext4_loop, ext_version
    | k -> failwith
      ("ext filesystem "^(string_of_int k)^" is unknown to this program")
    ) only
  with Command_failure (_,_,_) ->
    match get_system () with
    | Os.Linux | Os.FreeBSD ->
      printf "mke2fs not found; skipping ext2, ext3, ext4\n%!";
      []
    | Os.Darwin ->
      printf "Paragon ExtFS not found; skipping ext2, ext3, ext4\n%!";
      []

let fuse_ext2_version = memo_query (fun () ->
  match get_system () with
  | Os.Darwin ->
    let plist = "/System/Library/Filesystems/fuse-ext2.fs/Contents/Info.plist" in
    "fuse-ext2 "^(plist_version plist)
  | Os.Linux ->
    List.hd (read_command ~exit_code:255 "fuseext2")
  | Os.FreeBSD ->
    raise Not_found (* really? probably has an impl *)
)

let fuse_ext2_if_available ?(only=[2;3]) () =
  try
    let ext_version = fuse_ext2_version () in
    List.map Fs.(function
    | 2 -> Fuse_ext2_loop, Fuse (fuse_version (), ext_version)
    | 3 -> Fuse_ext3_loop, Fuse (fuse_version (), ext_version)
    | k -> failwith
      ("fuse-ext2 filesystem "^(string_of_int k)^" is unknown to this program")
    ) only
  with Command_failure (_,_,_) ->
    begin match get_system () with
    | Os.Darwin ->
      printf "fuse-ext2 plist not found; skipping ext2, ext3\n%!";
    | Os.Linux ->
      printf "fuseext2 not found; skipping ext2, ext3\n%!";
    | Os.FreeBSD ->
      printf "fuse_ext2 not bound on FreeBSD; skipping\n%!";
    end;
    []

let fusexmp_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux | Os.Darwin -> fuse_version ()
  | Os.FreeBSD -> String.concat "\n"
    (read_command_err ~exit_code:1 "fusexmp_fh --version")
)

let sshfs_version = memo_query (fun () ->
  String.concat "\n" (read_command "sshfs --version")
)

let davfs_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux | Os.FreeBSD ->
    let version = List.hd (read_command "mount.davfs --version") in
    Fs.Fuse (fuse_version (), version)
  | Os.Darwin -> Fs.System
)

let apache2_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux | Os.FreeBSD ->
    Fs.Version (String.concat "\n" (read_command "apache2 -v"))
  | Os.Darwin ->
    Fs.Version (List.hd (read_command "httpd -v"))
)

let nfs_server_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux -> Fs.Version (dpkg_version "nfs-kernel-server")
  | Os.Darwin -> Fs.System
  | Os.FreeBSD -> raise Not_found
)

let nfs_client_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux -> Fs.Version (dpkg_version "nfs-common")
  | Os.Darwin -> Fs.System
  | Os.FreeBSD -> raise Not_found
)

let ntfs3g_version = memo_query (fun () ->
  List.hd (read_command_err "ntfs-3g --version")
)

let hfsplus_version = memo_query (fun () ->
  match get_system () with Os.Darwin -> Fs.System
  | Os.Linux -> Fs.Version (dpkg_version "hfsprogs")
  | Os.FreeBSD -> raise Not_found
)

let hfsplus_if_available () =
  try
    let hfsplus_version = hfsplus_version () in
    Fs.([ Hfsplus_loop, hfsplus_version ])
  with Not_found | Command_failure (_,_,_) ->
    printf "HFS+ support not found; skipping hfsplus\n%!";
    []

let ufs2_if_available () = match get_system () with
  | Os.FreeBSD -> Fs.([ Ufs2_loop, System ])
  | (Os.Linux | Os.Darwin) as s ->
    printf "ufs2 not available on %s; skipping ufs2\n%!" (Os.to_string s);
    []

let nilfs2_version = memo_query (fun () ->
  Fs.Version (List.hd (read_command_err "mkfs.nilfs2 -V"))
)

let nilfs2_if_available () = match get_system () with
  | Os.Linux ->
    Fs.([ Nilfs2_loop, nilfs2_version () ])
  | (Os.FreeBSD | Os.Darwin) as s ->
    printf "nilfs2 not available on %s; skipping nilfs2\n%!" (Os.to_string s);
    []

let posixovl_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux -> dpkg_version "fuse-posixovl"
  | Os.Darwin | Os.FreeBSD -> raise Not_found
)

let aufs_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux -> dpkg_version "aufs-tools"
  | Os.Darwin | Os.FreeBSD -> raise Not_found
)

let gluster_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux -> dpkg_version "glusterfs-client"
  | Os.Darwin | Os.FreeBSD -> raise Not_found
)

let vfat_version = memo_query (fun () ->
  Fs.Version (List.hd (read_command ~exit_code:1 "mkfs.vfat"))
)

let vfat_if_available () = match get_system () with
  | Os.Linux -> Fs.([ Vfat_loop, vfat_version () ])
  | (Os.FreeBSD | Os.Darwin) as s ->
    printf "vfat not available on %s; skipping vfat\n%!" (Os.to_string s);
    []

let xfs_version = memo_query (fun () ->
  Fs.Version (List.hd (read_command "mkfs.xfs -V"))
)

let xfs_if_available () = match get_system () with
  | Os.FreeBSD | Os.Linux | Os.Darwin ->
    Fs.([ Xfs_loop, xfs_version () ])

let zfs_version = memo_query (fun () ->
  match get_system () with
  | Os.Linux  -> Fs.Version ("ZFS on Linux "^(dpkg_version "zfsutils"))
  | Os.Darwin ->
    let plist = "/Library/Extensions/zfs.kext/Contents/Info.plist" in
    Fs.Version ("OpenZFS on OS X "^(plist_version plist))
  | Os.FreeBSD -> Fs.System
)

let zfs_if_available () = match get_system () with
  | Os.FreeBSD | Os.Linux | Os.Darwin ->
    Fs.([ Zfs_loop, zfs_version () ])

let rec version_of_fs = Fs.(function
  | Tmpfs -> System
  | Ext2_loop | Ext3_loop | Ext4_loop -> ext_version ()
  | Btrfs_loop -> btrfs_version ()
  | F2fs_loop -> f2fs_version ()
  | Minix_loop -> minix_version ()
  | Bind subfs -> Function (version_of_fs subfs, System)
  | Fusexmp (_params,subfs) ->
    Function (version_of_fs subfs,
              Fuse (fuse_version (), fusexmp_version ()))
  | Sshfs (_params,subfs) ->
    Function (version_of_fs subfs,
              Fuse (fuse_version (), sshfs_version ()))
  | Davfs subfs ->
    Function (version_of_fs subfs,
              Function (apache2_version (), davfs_version ()))
  | Nfsv3 subfs | Nfsv4 subfs ->
    Function (version_of_fs subfs,
              Function (nfs_server_version (), nfs_client_version ()))
  | Posixovl (_params,subfs) ->
    Function (version_of_fs subfs,
              Fuse (fuse_version (), posixovl_version ()))
  | Aufs btt ->
    List.fold_right (fun fs v ->
      Function (version_of_fs fs, v)
    ) btt (Version (aufs_version ()))
  | Overlay (b, t) ->
    Function (version_of_fs b, (Function (version_of_fs t, System)))
  | Gluster subfs ->
    Function (version_of_fs subfs,
              Fuse (fuse_version (), gluster_version ()))
  | Ntfs3g_loop -> Fuse (fuse_version (), ntfs3g_version ())
  | Fuse_ext2_loop
  | Fuse_ext3_loop -> Fuse (fuse_version (), fuse_ext2_version ())
  | Hfsplus_loop -> hfsplus_version ()
  | Ufs2_loop -> System
  | Nilfs2_loop -> nilfs2_version ()
  | Vfat_loop -> vfat_version ()
  | Xfs_loop -> xfs_version ()
  | Zfs_loop -> zfs_version ()
  | Path _   -> Version "unknown"
)

let local_target fs = fs, version_of_fs fs

(* END OF SIMPLE FUNCTIONS AND QUERIES *)

(* BEGIN STATEFUL FUNCTIONS *)

let destroy_funs = Hashtbl.create 16
let dtor_stack = ref []

let destroy ?(force=false) env () =
  env.users <- env.users - 1;
  if not force && env.users > 0 then ()
  else begin
    let users = env.users in
    env.users <- 0;
    List.iter (fun destroy ->
      Hashtbl.remove destroy_funs env;
      destroy ()
    ) (Hashtbl.find_all destroy_funs env);
    env.users <- users
  end

let add_destructor env dtor =
  let users = env.users in
  env.users <- 0;
  Hashtbl.add destroy_funs env dtor;
  dtor_stack := env::(!dtor_stack);
  env.users <- users

let destroy_world () =
  kill_children ();
  let errors = List.fold_left (fun errors env ->
    try (destroy ~force:true env (); errors)
    with Command_failure (_,_,_) | Unix.Unix_error (_,_,_) -> errors + 1
  ) 0 !dtor_stack
  in
  eprintf "%d errors encountered unloading filesystems.\n%!" errors

let destroy_mount dir () =
  let mnt_path = Filename.dirname dir in
  Unix.chdir mnt_path; (* don't want to be in dir when unmounting *)
  (* continue "umount" [|dir|]; *)
  let rec try_umount tries = Unix.(
    try
      continue "umount" [|dir|];
    with e ->
      if tries < 1 then raise e
      else (
        eprintf "umount failed; exception was %s\n" (Printexc.to_string e);
        eprintf "umount failed; retrying in 1s (%d left)\n%!" tries;
        Unix.sleep 1;
        try_umount (tries-1)))
  in
  try_umount 10;
  let rec try_rmdir tries = Unix.(
    try rmdir dir
    with Unix_error(EBUSY,_,_) ->
      if tries < 1 then failwith ("EBUSY couldn't rmdir mount dir "^dir)
      else
        let tries = tries - 1 in
        eprintf "rmdir failed with EBUSY; retrying in 1s (%d left)\n%!" tries;
        sleep 1;
        try_rmdir tries
  ) in
  try_rmdir 25

let init_fuse = Os.(function Linux | Darwin -> ()
  | FreeBSD -> continue "kldload" [|"-n"; "fuse"|]
)

let load_fs ?subdir ?(destroy=true) params fs mount_fun =
  let mnt = Filename.concat params.mnt_path (Fs.name fs) in
  continue "mkdir" [|"-p"; mnt|];
  let env = { fs; params; users = 1; mnt; subdir } in
  (try mount_fun env
   with e -> Unix.rmdir mnt; raise e);
  if destroy then add_destructor env (destroy_mount mnt);
  env

let mount_tmpfs size env =
  continue "mount"
    [|"-t"; "tmpfs"; "-o"; "size="^(Int64.to_string size); "tmpfs"; env.mnt|]

let load_tmpfs params fs = load_fs params fs (mount_tmpfs params.size)

let dev_of_file file =
  let status = List.hd (read_command ("losetup -j "^file)) in
  String.sub status 0 (String.index status ':')
let loop file =
  continue "losetup" [|"-f"; file|];
  dev_of_file file
let rec unloop dev = exit_command "losetup" [|"-d"; dev|] () (function
  | 0 -> Some ()
  | 1 as x ->
    (* TODO: investigate the root cause and report it *)
    eprintf "Loop race error: losetup -d %s failed with %d; retrying.\n%!"
      dev x;
    Some (unloop dev)
  | _ -> None
)

let make_tmploopdisk env format_fun =
  let name = Fs.name env.fs in
  let mnt = Filename.concat env.params.mnt_path (name^"_tmp") in
  continue "mkdir" [|"-p"; mnt|]; (* TODO: clean? *)
  let img = Filename.concat mnt (name^".img") in
  mount_tmpfs env.params.size { env with mnt };
  add_destructor env (destroy_mount mnt);
  continue "dd"
    [|"if=/dev/zero"; "of="^img; "bs=4k";
      "count="^(Int64.to_string (subfs_blk_count env.params.size))|];
  let loopdev = loop img in
  (try format_fun loopdev
   with e -> unloop loopdev; raise e);
  unloop loopdev;
  img

let make_md env =
  let sectors = sector_count env.params.size in
  let devno = List.hd (
    read_command ("mdconfig -n -s "^(Int64.to_string sectors))
  ) in
  add_destructor env (fun () -> continue "mdconfig" [|"-d"; "-u"; devno|]);
  let dev = "/dev/md"^devno in
  dev

let make_ramdisk env =
  let sectors = Int64.to_string (sector_count env.params.size) in
  let dev = List.hd (
    read_command ("hdiutil attach -owners on -nomount ram://"^sectors)
  ) in
  let dev = String.(sub dev 0 (index dev ' ')) in
  add_destructor env (fun () -> continue "hdiutil" [|"detach"; dev|]);
  dev

let load_ext_loop params typ fs = load_fs params fs Os.(fun env ->
  match params.system with
  | FreeBSD (* certainly wrong *)
  | Linux ->
    let img = make_tmploopdisk env (fun dev ->
      continue "mke2fs"
        [|"-t"; typ; "-T"; "small";
          "-b"; Int64.to_string blk_size;
          dev; Int64.to_string (subfs_blk_count params.size);
        |]
    ) in
    continue "mount" [|"-t"; typ; "-o"; "loop"; img; env.mnt|]
  | Darwin ->
    let dev = make_ramdisk env in
    (* hmmm.... not ideal *)
    continue "fuse-ext2.mke2fs"
      [|"-t"; typ; "-T"; "small";
        "-b"; Int64.to_string blk_size;
        dev; Int64.to_string (subfs_blk_count params.size);
      |];
    continue "mount_ufsd_ExtFS" [|dev; env.mnt|]
)

let load_fuse_ext2_loop params typ fs = load_fs params fs Os.(fun env ->
  match params.system with
  | FreeBSD -> (* certainly wrong *)
    raise Not_found
  | Linux ->
    let img = make_tmploopdisk env (fun dev ->
      continue "mke2fs"
        [|"-t"; typ; "-T"; "small";
          "-b"; Int64.to_string blk_size;
          dev; Int64.to_string (subfs_blk_count params.size);
        |]
    ) in
    init_fuse params.system;
    continue "fuseext2" [|img; env.mnt; "-orw,force"|]
  | Darwin ->
    init_fuse params.system;
    let dev = make_ramdisk env in
    continue "fuse-ext2.mke2fs"
      [|"-t"; typ; "-T"; "small";
        "-b"; Int64.to_string blk_size;
        dev; Int64.to_string (subfs_blk_count params.size);
      |];
    continue "mount" [|"-t"; "fuse-ext2"; dev; env.mnt|]
)

let load_btrfs_loop params fs = load_fs params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.btrfs" [|dev|]
  ) in
  continue "mount" [|"-t"; "btrfs"; "-o"; "loop"; img; env.mnt|]
)

let load_f2fs_loop params fs = load_fs params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.f2fs" [|dev|]
  ) in
  continue "mount" [|"-t"; "f2fs"; "-o"; "loop"; img; env.mnt|]
)

let load_minix_loop params fs = load_fs params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.minix" [|dev|]
  ) in
  continue "mount" [|"-t"; "minix"; "-o"; "loop"; img; env.mnt|]
)

let load_ntfs3g_loop params fs = load_fs params fs Os.(fun env ->
  init_fuse params.system;
  match params.system with
  | Linux ->
    let img = make_tmploopdisk env (fun dev ->
      continue "mkntfs" [|"-Q"; dev|]
    ) in
    continue "ntfs-3g" [|img; env.mnt|]
  | FreeBSD ->
    let dev = make_md env in
    (* FreeBSD doesn't have block devices so force onto a char device *)
    continue "mkntfs" [|"-FQ"; dev|];
    continue "ntfs-3g" [|dev; env.mnt|]
  | Darwin ->
    let dev = make_ramdisk env in
    continue "/usr/local/sbin/mkntfs" [|"-Q"; dev|];
    continue "ntfs-3g" [|dev; env.mnt|]
)

let load_hfsplus_loop params fs = load_fs params fs Os.(fun env ->
  match params.system with
  | Linux | FreeBSD ->
    let img = make_tmploopdisk env (fun dev ->
      continue "mkfs.hfsplus" [|"-s"; dev|]
    ) in
    continue "modprobe" [|"hfsplus"|];
    add_destructor env (fun () -> continue "modprobe" [|"-r"; "hfsplus"|]);
    continue "mount" [|"-t"; "hfsplus"; "-o"; "loop"; img; env.mnt|]
  | Darwin ->
    let name = Fs.name fs in
    let dev = make_ramdisk env in
    continue "diskutil" [|"erasevolume"; "hfsx"; name; dev|];
    (* diskutil automatically mounted our new fs *)
    continue "diskutil" [|"unmountdisk"; dev|];
    continue "hdiutil"
      [|"attach"; "-owners"; "on"; "-mountpoint"; env.mnt; dev|]
)

let load_ufs2_loop params fs = load_fs params fs (fun env ->
  let dev = make_md env in
  continue "newfs" [|"-O"; "2"; dev|];
  continue "mount" [|"-t"; "ufs"; dev; env.mnt|]
)

let load_nilfs2_loop params fs = load_fs ~destroy:false params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.nilfs2" [|dev|]
  ) in
  continue "mount" [|"-t"; "nilfs2"; "-o"; "loop,nogc"; img; env.mnt|];
  let dev = dev_of_file img in
  let config = Filename.concat params.mnt_path "nilfs_cleanerd.conf" in
  let config_oc = open_out config in
  output_string config_oc "protection_period\t1\n";
  output_string config_oc "min_clean_segments\t0%\n";
  output_string config_oc "clean_check_interval\t1\n";
  output_string config_oc "nsegments_per_clean\t10\n";
  output_string config_oc "mc_nsegments_per_clean\t20\n";
  output_string config_oc "retry_interval\t5\n";
  close_out config_oc;
  let nilfs_gc_pid = Scanf.sscanf
    (List.hd (read_command ("nilfs_cleanerd -c "^config^" "^dev^" "^env.mnt)))
    "NILFS_CLEANERD_PID=%d"
    (fun x -> x)
  in
  add_destructor env (fun () ->
    continue "kill" [|string_of_int nilfs_gc_pid|];
    continue "rm" [|config|];
    destroy_mount env.mnt ();
  )
)

let load_vfat_loop params fs = load_fs params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.vfat" [|"-v"; dev|]
  ) in
  continue "mount" [|"-t"; "vfat"; "-o"; "check=s,loop"; img; env.mnt|]
)

let load_xfs_loop params fs = load_fs params fs (fun env ->
  let img = make_tmploopdisk env (fun dev ->
    continue "mkfs.xfs" [|dev|]
  ) in
  continue "mount" [|"-t"; "xfs"; "-o"; "loop"; img; env.mnt|]
)

let load_zfs_loop params fs = load_fs ~destroy:false params fs (fun env ->
  let pool = Fs.(name Zfs_loop) in
  Os.(match params.system with
  | Linux ->
    let img = make_tmploopdisk env (fun _dev -> ()) in
    continue "zpool" [|"create"; "-m"; env.mnt; pool; img|]
  | FreeBSD ->
    let dev = make_md env in
    continue "zpool" [|"create"; "-m"; env.mnt; pool; dev|]
  | Darwin ->
    let dev = make_ramdisk env in
    continue "zpool" [|"create"; "-m"; env.mnt; pool; dev|]
  );
  add_destructor env (fun () ->
    continue "zpool" [|"destroy"; pool|];
    Unix.chdir params.mnt_path;
    Unix.rmdir env.mnt
  )
)

let load_path params path fs =
  { fs; users = 1; mnt = path; params; subdir=None }

let fuse_args arg_array ?(also=[]) = function
  | [] -> arg_array
  | params -> Array.append arg_array
    [|"-o"; String.concat "," ((List.map Fs.string_of_fuse_param params)@also) |]

let envs = Hashtbl.create 16

let rec load params fs =
  try let env = Hashtbl.find envs fs in
      env.users <- env.users + 1;
      env
  with Not_found ->
    let env = Fs.(match fs with
      | Tmpfs      -> load_tmpfs params fs
      | Ext2_loop  -> load_ext_loop params "ext2" fs
      | Ext3_loop  -> load_ext_loop params "ext3" fs
      | Ext4_loop  -> load_ext_loop params "ext4" fs
      | Btrfs_loop -> load_btrfs_loop params fs
      | F2fs_loop  -> load_f2fs_loop params fs
      | Minix_loop -> load_minix_loop params fs
      | Bind subfs -> load_bind params subfs fs
      | Fusexmp subfs -> load_fusexmp params subfs fs
      | Sshfs subfs   -> load_sshfs params subfs fs
      | Davfs subfs   -> load_davfs params subfs fs
      | Nfsv3 subfs   -> load_nfsv3 params subfs fs
      | Nfsv4 subfs   -> load_nfsv4 params subfs fs
      | Posixovl subfs-> load_posixovl params subfs fs
      | Aufs btt      -> load_aufs params btt fs
      | Overlay (b,t) -> load_overlay params b t fs
      | Gluster subfs -> load_gluster params subfs fs
      | Ntfs3g_loop   -> load_ntfs3g_loop params fs
      | Fuse_ext2_loop-> load_fuse_ext2_loop params "ext2" fs
      | Fuse_ext3_loop-> load_fuse_ext2_loop params "ext3" fs
      | Hfsplus_loop  -> load_hfsplus_loop params fs
      | Ufs2_loop     -> load_ufs2_loop params fs
      | Nilfs2_loop   -> load_nilfs2_loop params fs
      | Vfat_loop     -> load_vfat_loop params fs
      | Xfs_loop      -> load_xfs_loop params fs
      | Zfs_loop      -> load_zfs_loop params fs
      | Path path     -> load_path params path fs
    ) in
    Hashtbl.replace envs fs env;
    add_destructor env (fun () -> Hashtbl.remove envs fs);
    env
and load_bind params subfs fs = load_fs params fs (fun env ->
  let subenv = load params subfs in
  add_destructor env (destroy subenv);
  continue "mount" [|"--bind"; subenv.mnt; env.mnt|]
)
and load_fusexmp params (fuse_params,subfs) fs =
  let subname = Fs.name subfs in
  let subdir = Filename.concat params.mnt_path subname in
  load_fs ~subdir params fs Os.(fun env ->
    let subenv = load params subfs in
    add_destructor env (destroy subenv);
    init_fuse params.system;
    match params.system with
    | Linux ->
      continue (Filename.concat cmd_path "fusexmp")
               (fuse_args [|env.mnt|] fuse_params)
    | FreeBSD ->
      continue "fusexmp_fh" (fuse_args [|env.mnt|] fuse_params)
    | Darwin ->
      continue "./loopback"
        (fuse_args [|env.mnt; "-omodules=threadid"|]
                   ~also:["native_xattr"] fuse_params)
  )
and load_sshfs params (fuse_params,subfs) fs = load_fs params fs (fun env ->
  let subenv = load params subfs in
  add_destructor env (destroy subenv);
  init_fuse params.system;
  continue "sshfs"
    (fuse_args [|"root@localhost:"^subenv.mnt; env.mnt|] fuse_params)
)
and load_davfs params subfs fs = load_fs params fs (fun env ->
  match params.system with
  | Os.Linux | Os.FreeBSD ->
    let subenv = load { params with mnt_path = "/var/www/html" } subfs in
    add_destructor env (destroy subenv);
    let subname = Fs.name subfs in
    init_fuse params.system;
    continue "mount.davfs" [|"http://localhost/"^subname; env.mnt|]
  | Os.Darwin ->
    let mnt_path = "/Library/WebServer/Documents" in
    let subenv = load { params with mnt_path } subfs in
    add_destructor env (destroy subenv);
    let subname = Fs.name subfs in
    continue "./mount_webdav_osx.expect"
      [|"http://localhost/"^subname; env.mnt|]
)
and load_nfsv3 params subfs fs = load_fs params fs (fun env ->
  match params.system with
  | Os.FreeBSD
  | Os.Linux ->
    let name = Fs.name fs in
    let subenv = load { params with mnt_path = "/tmp/"^name } subfs in
    add_destructor env (destroy subenv);
    continue "exportfs" [|"-v"; "-r"|];
    continue "mount"
      [|"-o"; "vers=3"; "-t"; "nfs"; "localhost:"^subenv.mnt; env.mnt|];
    add_destructor env (fun () ->
      continue "exportfs" [|"-u"; "localhost:"^subenv.mnt|]
    )
  | Os.Darwin ->
    let name = Fs.name fs in
    let subenv = load { params with mnt_path = "/Volumes/"^name } subfs in
    add_destructor env (destroy subenv);
    continue "nfsd" [|"restart"|];
    Unix.sleep 10;
    continue "mount"
      [|"-o"; "vers=3"; "-t"; "nfs";
        "localhost:"^subenv.mnt; env.mnt|];
      add_destructor env (fun () ->
      continue "nfsd" [|"stop"|]
    )
)
and load_nfsv4 params subfs fs = load_fs params fs (fun env ->
  let name = Fs.name fs in
  let subenv = load { params with mnt_path = "/tmp/"^name } subfs in
  add_destructor env (destroy subenv);
  continue "exportfs" [|"-v"; "-r"|];
  continue "mount"
    [|"-t"; "nfs"; "localhost:/"; env.mnt; "-o"; "vers=4"|];
  add_destructor env (fun () ->
    continue "exportfs" [|"-u"; "localhost:"^subenv.mnt|]
  )
)
and load_posixovl params (fuse_params,subfs) fs = load_fs params fs (fun env ->
  let subenv = load params subfs in
  add_destructor env (destroy subenv);
  init_fuse params.system;
  continue "mount.posixovl"
    (fuse_args [|"-S"; subenv.mnt; env.mnt; "--"|] fuse_params)
)
and load_aufs params btt fs = load_fs params fs (fun env ->
  let subenvs = List.map (load params) btt in
  List.iter (fun subenv -> add_destructor env (destroy subenv)) subenvs;
  let branches =
    "br:"^(String.concat ":" (List.rev_map (fun { mnt } -> mnt) subenvs))
  in
  continue "mount" [|"-v"; "-t"; "aufs"; "-o"; branches; "none"; env.mnt|]
)
and load_overlay params b t fs = load_fs params fs (fun env ->
  let benv = load params b in
  add_destructor env (destroy benv);
  let tenv = load params t in
  add_destructor env (destroy tenv);
  let work  = Filename.concat tenv.mnt "work" in
  let upper = Filename.concat tenv.mnt "upper" in
  add_destructor env (fun () ->
    continue "rm" [| "-fr"; work |];
    continue "rm" [| "-fr"; upper |];
  );
  continue "mkdir" [| work |];
  continue "mkdir" [| upper |];
  let dirs =
    Printf.sprintf "lowerdir=%s,upperdir=%s,workdir=%s" benv.mnt upper work
  in
  continue "mount" [|"-t"; "overlay"; "-o"; dirs; "none"; env.mnt|]
)
and load_gluster params subfs fs = load_fs params fs (fun env ->
  let subenv = load params subfs in
  add_destructor env (destroy subenv);
  let name = Fs.name fs in
  let self = List.hd (read_command "hostname") in
  let brick = Filename.concat subenv.mnt "brick" in
  add_destructor env (fun () ->
    continue "rm" [| "-fr"; brick |];
  );
  continue "mkdir" [| brick |];
  continue "gluster" [|"volume"; "create"; name; self^":"^brick|];
  add_destructor env (fun () ->
    ignore (read_command ("echo \"y\" | gluster volume delete "^name))
  );
  continue "gluster" [|"volume"; "start"; name|];
  add_destructor env (fun () ->
    ignore (read_command ("echo \"y\" | gluster volume stop "^name))
  );
  init_fuse params.system;
  continue "mount" [|"-t"; "glusterfs"; self^":/"^name; env.mnt|]
)

(* END STATEFUL FUNCTIONS *)

let set_signals_destroy_world signals =
  let sigh = Sys.Signal_handle (fun signal ->
    ignore Unix.(sigprocmask SIG_BLOCK signals);
    eprintf "%d caught %s; unloading filesystems.\n%!"
      (Unix.getpid ()) (string_of_signal signal);
    destroy_world ();
    exit 1
  ) in
  List.iter (fun signal -> Sys.set_signal signal sigh) signals
