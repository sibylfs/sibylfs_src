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

(* 

posix: similar to interp; read labels and execute them against the real filesystem state; additional label "dump" to dump the state; can be restricted to a subdirectory

Interactive additional commands:

    #directory "../fs_test/posix";;
    #load "syscall.cma";;

*)

open Fs_prelude
open Fs_interface.Fs_spec_intf
open Fs_interface.Fs_spec_intf.Fs_types
(* open Fs_spec_extras *)

open Fs_dict_wrappers
open Fmap_default
open Lem_support
(* open CheckLib *)
open Fs_test_system

module Arch = Fs_interface.Fs_spec_intf.Fs_arch
module Agent = Posix_agent

module IntMap = Map.Make(struct
  type t = int
  let compare (x:int) (y:int) = compare x y
end)

(** user and group management *)

let my_uid = Syscall.getuid ()
let my_username = List.hd (read_command "id -u -n")

let my_gid = Syscall.getgid ()
let my_groupname = List.hd (read_command "id -g -n")

let is_root_user () = Syscall.getuid () = 0

(* map to real-world uids *)
let user_map_sing = IntMap.singleton 0 (my_uid, my_username)
(* map from real-world uids *)
let user_map_rev_sing = IntMap.singleton my_uid 0

(* map to real-world group ids *)
let group_map_sing = IntMap.singleton 0 (my_gid, my_groupname)
(* map from real-world group ids *)
let group_map_rev_sing = IntMap.singleton my_gid 0

let user_map = ref user_map_sing;;
let user_map_rev = ref user_map_rev_sing;;
let group_map = ref group_map_sing;;
let group_map_rev = ref group_map_rev_sing;;

(* A map from our internal dir-handles to the dir-handles of Syscall. *)
let dir_handle_map = ref (IntMap.empty : (Syscall.dir_handle IntMap.t))

let lookup_dh (DH dh) =
  try IntMap.find dh !dir_handle_map
  with Not_found -> raise Unix.(Unix_error (EBADF, "lookup_dh", ""))

let reset_global_state () =
  (* groups before users to evade FreeBSD 10 pw userdel divergence bug *)
  (* See <https://svnweb.freebsd.org/base/head/usr.sbin/pw/pw_user.c?r1=263114&r2=267970&pathrev=267970> *)
  IntMap.iter
    (fun _gid (_real_gid, groupname) ->
      if groupname <> my_groupname then delete_group groupname
    ) !group_map;
  group_map := group_map_sing;
  group_map_rev := group_map_rev_sing;
  IntMap.iter
    (fun _uid (_real_uid, username) ->
      if username <> my_username then delete_user username
    ) !user_map;
  user_map := user_map_sing;
  user_map_rev := user_map_rev_sing;
  dir_handle_map := IntMap.empty

let signals = Sys.([sigterm; sigint])
let block_signals ()   = ignore Unix.(sigprocmask SIG_BLOCK   signals)
let unblock_signals () = ignore Unix.(sigprocmask SIG_UNBLOCK signals)

let lookup_uid uid =
  try
    IntMap.find uid !user_map
  with Not_found ->
    block_signals ();
    let (new_real_uid, new_username) = create_new_user() in
    user_map     := IntMap.add uid (new_real_uid, new_username) !user_map;
    unblock_signals ();
    user_map_rev := IntMap.add new_real_uid uid !user_map_rev;
    (new_real_uid, new_username)

let lookup_gid gid =
  try
    IntMap.find gid !group_map
  with Not_found ->
    block_signals ();
    let (new_real_gid, new_group_name) = create_new_group() in
    group_map     := IntMap.add gid (new_real_gid, new_group_name) !group_map;
    unblock_signals ();
    group_map_rev := IntMap.add new_real_gid gid !group_map_rev;
    (new_real_gid, new_group_name)

let lookup_uid_rev uid = try
  IntMap.find uid !user_map_rev 
with Not_found -> (Printf.eprintf "posix_ops.ml: unknown uid: %d" uid; -99)

let lookup_gid_rev gid = try
  IntMap.find gid !group_map_rev
with Not_found -> (Printf.eprintf "posix_ops.ml: unknown gid: %d" gid; -99)

let get_file_kind filename = 
  let stat = Syscall.stat filename in
  stat.Syscall.st_kind

let rec rm_recursive filename = 
  (try Syscall.chmod filename 0o777 with _ -> ());
  try (
    Syscall.unlink filename
  ) with _ -> (* ok, so perhaps it is a non-empty dir *) try
    let d = Syscall.opendir filename in 
    try while true do begin
      let ent = (Syscall.readdir d) in
      if (String.compare ent "." == 0) then () else
      if (String.compare ent ".." == 0) then () else
      rm_recursive (filename ^ "/" ^ ent)
    end
    done with End_of_file -> begin
       Syscall.closedir d;
       Syscall.rmdir filename
    end
  with Unix.Unix_error (err,syscall,path) ->
    Printf.eprintf "Error: %s on %s failed with \"%s\"\n"
      syscall path (Unix.error_message err)
  | _ ->
    Printf.eprintf "Unknown error removing %s\n" filename

(* [run_in_child_process signal_handler clean f] runs [f ()] in a child process
   and waits for it to terminate. Afterwards, the cleanup function [clean] is
   called. *)
let run_in_child_process
    (signal_handler : ?pids:int list -> int list -> int -> unit)
    (clean : ?status:Unix.process_status -> unit -> unit)
    f =
  block_signals ();
  let pid = Syscall.fork () in
  if pid <> 0 then begin
    List.iter (fun s ->
      Sys.(set_signal s  (Signal_handle (signal_handler ~pids:[pid] signals)))
    ) signals;
    unblock_signals ();
    (* parent process, wait for child to terminate *)
    let (_, ts) = Syscall.waitpid [] pid in
    clean ~status:ts ();
    (* if the child did not terminate normally, exit as well *)
    match ts with
    | Syscall.WEXITED 0 -> ()
    | Syscall.WEXITED n -> exit n
    | _ -> exit 1
  end else begin
    Sys.(set_signal sigint  Signal_ignore);
    Sys.(set_signal sigterm (Signal_handle (signal_handler [sigterm])));
    unblock_signals ();
    (* child process, execute f and return normally *)
    let ret = (try f () with e -> print_endline (Printexc.to_string e); clean (); raise e) in
    clean ();
    exit ret
  end

(* copied from fs_spec; don't want to expose this type in general; its use
   here is local *)
type ty_pps_pid_run_state =
| RUNNING
| BLOCKED_CALL of ty_os_command
| PENDING_RETURN of ret_value error_or_value

type posix_per_process_state = {
  pps_pid_run_state : ty_pps_pid_run_state;
  pps_gid : int;
  pps_uid : int;
  username : string;
  pps_channel :
    (Fs_types.ty_arch * ty_os_command, ret_value error_or_value)
    Agent.channel Lazy.t
}

type posix_os_state = {
  agents : (Fs_types.ty_arch * ty_os_command, ret_value error_or_value) Agent.t;
  pid_table: (ty_pid, posix_per_process_state) fmap;
  pos_root_dir: string;
  (* pos_arch is used to munge open flags and other OS-dependent values *)
  pos_arch: Fs_types.ty_arch;
}

let agents_list = ref []
let stop_all () = List.iter Agent.stop_all !agents_list

let create_pps s0 root (User_id uid) (Group_id gid) =
  let (pps_gid, _)        = lookup_gid gid in
  let (pps_uid, username) = lookup_uid uid in
  let pps_channel = Lazy.from_fun (fun () ->
    Posix_agent.deploy s0.agents ~username ~uid:pps_uid ~gid:pps_gid ~root
  ) in
  {
    pps_pid_run_state=RUNNING;
    pps_gid; pps_uid; username;
    pps_channel;
  }

let update_per_process_state s0 pid f = match fmap_lookup s0.pid_table pid with
  | None -> s0 (* pid not present, so don't update *)
  | Some ppstate ->
    let ppstate' = f ppstate in
    let pid_table = fmap_update s0.pid_table (pid,ppstate') in
    { s0 with pid_table }

let update_run_state s0 pid new_rst =
  update_per_process_state s0 pid (fun ppstate -> {
    ppstate with pps_pid_run_state = new_rst
  })

let error_of_unix_error e = Unix.(match e with
  | E2BIG           -> Fs_types.E2BIG
  | EACCES          -> Fs_types.EACCES
  | EAGAIN          -> Fs_types.EAGAIN
  | EBADF           -> Fs_types.EBADF
  | EBUSY           -> Fs_types.EBUSY
  | ECHILD          -> Fs_types.ECHILD
  | EDEADLK         -> Fs_types.EDEADLK
  | EDOM            -> Fs_types.EDOM
  | EEXIST          -> Fs_types.EEXIST
  | EFAULT          -> Fs_types.EFAULT
  | EFBIG           -> Fs_types.EFBIG
  | EINTR           -> Fs_types.EINTR
  | EINVAL          -> Fs_types.EINVAL
  | EIO             -> Fs_types.EIO
  | EISDIR          -> Fs_types.EISDIR
  | EMFILE          -> Fs_types.EMFILE
  | EMLINK          -> Fs_types.EMLINK
  | ENAMETOOLONG    -> Fs_types.ENAMETOOLONG
  | ENFILE          -> Fs_types.ENFILE
  | ENODEV          -> Fs_types.ENODEV
  | ENOENT          -> Fs_types.ENOENT
  | ENOEXEC         -> Fs_types.ENOEXEC
  | ENOLCK          -> Fs_types.ENOLCK
  | ENOMEM          -> Fs_types.ENOMEM
  | ENOSPC          -> Fs_types.ENOSPC
  | ENOSYS          -> Fs_types.ENOSYS
  | ENOTDIR         -> Fs_types.ENOTDIR
  | ENOTEMPTY       -> Fs_types.ENOTEMPTY
  | ENOTTY          -> Fs_types.ENOTTY
  | ENXIO           -> Fs_types.ENXIO
  | EPERM           -> Fs_types.EPERM
  | EPIPE           -> Fs_types.EPIPE
  | ERANGE          -> Fs_types.ERANGE
  | EROFS           -> Fs_types.EROFS
  | ESPIPE          -> Fs_types.ESPIPE
  | ESRCH           -> Fs_types.ESRCH
  | EXDEV           -> Fs_types.EXDEV
  | EWOULDBLOCK     -> Fs_types.EWOULDBLOCK
  | EINPROGRESS     -> Fs_types.EINPROGRESS
  | EALREADY        -> Fs_types.EALREADY
  | ENOTSOCK        -> Fs_types.ENOTSOCK
  | EDESTADDRREQ    -> Fs_types.EDESTADDRREQ
  | EMSGSIZE        -> Fs_types.EMSGSIZE
  | EPROTOTYPE      -> Fs_types.EPROTOTYPE
  | ENOPROTOOPT     -> Fs_types.ENOPROTOOPT
  | EPROTONOSUPPORT -> Fs_types.EPROTONOSUPPORT
  | ESOCKTNOSUPPORT -> Fs_types.ESOCKTNOSUPPORT
  | EOPNOTSUPP      -> Fs_types.EOPNOTSUPP
  | EPFNOSUPPORT    -> Fs_types.EPFNOSUPPORT
  | EAFNOSUPPORT    -> Fs_types.EAFNOSUPPORT
  | EADDRINUSE      -> Fs_types.EADDRINUSE
  | EADDRNOTAVAIL   -> Fs_types.EADDRNOTAVAIL
  | ENETDOWN        -> Fs_types.ENETDOWN
  | ENETUNREACH     -> Fs_types.ENETUNREACH
  | ENETRESET       -> Fs_types.ENETRESET
  | ECONNABORTED    -> Fs_types.ECONNABORTED
  | ECONNRESET      -> Fs_types.ECONNRESET
  | ENOBUFS         -> Fs_types.ENOBUFS
  | EISCONN         -> Fs_types.EISCONN
  | ENOTCONN        -> Fs_types.ENOTCONN
  | ESHUTDOWN       -> Fs_types.ESHUTDOWN
  | ETOOMANYREFS    -> Fs_types.ETOOMANYREFS
  | ETIMEDOUT       -> Fs_types.ETIMEDOUT
  | ECONNREFUSED    -> Fs_types.ECONNREFUSED
  | EHOSTDOWN       -> Fs_types.EHOSTDOWN
  | EHOSTUNREACH    -> Fs_types.EHOSTUNREACH
  | ELOOP           -> Fs_types.ELOOP
  | EOVERFLOW       -> Fs_types.EOVERFLOW
  | EUNKNOWNERR(i)  -> Fs_types.EUNKNOWNERR(i)
)

let run_syscall f x =
  try Value (f x)
  with Unix.Unix_error(e,_,_)  -> Error (error_of_unix_error e)

let close i = run_syscall (fun () ->
  let fd = Syscall.file_descr_of_int i in
  Syscall.close fd;
  RV_none
) ()

let link s d = run_syscall (fun () ->
  Syscall.link s d;
  RV_none
) ()

let unlink s = run_syscall (fun () ->
  Syscall.unlink s;
  RV_none
) ()

let mkdir s (File_perm p) = run_syscall (fun () ->
  Syscall.mkdir s (Int32.to_int p);
  RV_none
) ()

let chmod s (File_perm p) = run_syscall (fun () ->
  Syscall.chmod s (Int32.to_int p);
  RV_none
) ()

let chown s (User_id u) (Group_id g) = run_syscall (fun () ->
  Syscall.chown s u g;
  RV_none
) ()

let raw_open path oflags perms =
  let perms = match perms with
    | None -> None
    | Some (File_perm i) -> Some (Int32.to_int i)
  in
  Syscall.open_ path ?perms oflags

let open_ path oflags perms = run_syscall (fun () ->
  let fd = raw_open path oflags perms in
  let i = Syscall.int_of_file_descr fd in
  RV_num(i)
) ()

let open_close path oflags perms = run_syscall (fun () ->
  let fd = raw_open path oflags perms in
  Syscall.close fd;
  RV_none
) ()

let pread fd sz ofs = run_syscall (fun () ->
  let bs = Abstract_string.of_string (Syscall.pread fd sz ofs) in
  RV_bytes (bs)
) ()

let pread_full fd sz ofs = run_syscall (fun () ->
  let bs = Abstract_string.of_string (Syscall.pread_full fd sz ofs) in
  RV_bytes (bs)
) ()

let read fd sz = run_syscall (fun () ->
  let bs = Abstract_string.of_string (Syscall.read fd sz) in
  RV_bytes (bs)
) ()

let read_full fd sz = run_syscall (fun () ->
  let bs = Abstract_string.of_string (Syscall.read_full fd sz) in
  RV_bytes (bs)
) ()

let lseek fd ofs seek_command = run_syscall (fun () ->
  let res = Syscall.lseek fd ofs seek_command in
  RV_num res
) ()

let rec get_smallest_free_dh n =
  if IntMap.mem n !dir_handle_map
  then get_smallest_free_dh (n+1)
  else n

let opendir p = run_syscall (fun () ->
  let d = Syscall.opendir p in
  let dh = get_smallest_free_dh 1 in
  dir_handle_map := IntMap.add dh d !dir_handle_map;
  RV_num dh
) ()

let readdir dh = run_syscall (fun () ->
  let d = lookup_dh dh in
  try RV_bytes (Abstract_string.of_string (Syscall.readdir d))
  with End_of_file -> RV_none
) ()

let rewinddir dh = run_syscall (fun () ->
  let d = lookup_dh dh in
  Syscall.rewinddir d;
  RV_none
) ()

let closedir ((DH dh) as dhdh) = run_syscall (fun () ->
  let d = lookup_dh dhdh in
  Syscall.closedir d;
  dir_handle_map := IntMap.remove dh !dir_handle_map;
  RV_none
) ()

let rename s d = run_syscall (fun () ->
  Syscall.rename s d;
  RV_none
) ()

let rmdir p = run_syscall (fun () ->
  Syscall.rmdir p;
  RV_none
) ()
  
let kind_of_syscall_kind k =
  let open Syscall in
  match k with
  | S_REG  -> S_IFREG
  | S_DIR  -> S_IFDIR
  | S_CHR  -> S_IFCHR
  | S_BLK  -> S_IFBLK
  | S_LNK  -> S_IFLNK
  | S_FIFO -> S_IFIFO
  | S_SOCK -> S_IFSOCK

let file_perm_of_syscall_file_perm p = File_perm (Int32.of_int p)
let uid_of_syscall_uid u = User_id u
let gid_of_syscall_gid g = Group_id g

let stats_of_syscall_stats ({
  Syscall.st_dev;
  st_ino;
  st_kind;
  st_perm;
  st_nlink;
  st_uid;
  st_gid;
  st_rdev;
  st_size;
  st_atime;
  st_mtime;
  st_ctime;
}) = (
  let open Fs_interface.Fs_spec_intf.Fs_types in
  let get_os_timestamp seconds =
    let tv_sec' = floor seconds in
    let tv_nsec'= (int_of_float ((seconds -. tv_sec') *. 1000000000.0)) in
    {tv_sec=(int_of_float tv_sec');tv_nsec=(Int64.of_int tv_nsec')}
  in
 {
  os_st_dev=st_dev;
  os_st_ino = Inode st_ino;
  os_st_kind = kind_of_syscall_kind st_kind;
  os_st_perm = file_perm_of_syscall_file_perm st_perm;
  os_st_nlink = st_nlink;
  os_st_uid = uid_of_syscall_uid st_uid;
  os_st_gid = gid_of_syscall_gid st_gid;
  os_st_rdev = st_rdev;
  os_st_size = st_size;
  os_st_atime = (Os_timestamp (get_os_timestamp st_atime));
  os_st_mtime = (Os_timestamp (get_os_timestamp st_mtime));
  os_st_ctime = (Os_timestamp (get_os_timestamp st_ctime));
})

(* FIXME may want stat64 *)
let stat p = run_syscall (fun () ->
  RV_os_stats (stats_of_syscall_stats (Syscall.stat p))
) ()

(* FIXME may want stat64 *)
let lstat p = run_syscall (fun () ->
  RV_os_stats (stats_of_syscall_stats (Syscall.lstat p))
) ()

let readlink p = run_syscall (fun () ->
  RV_bytes (Abstract_string.of_string (Syscall.readlink p))
) ()

let symlink s d = run_syscall (fun () ->
  Syscall.symlink s d;
  RV_none
) ()

let truncate p l = run_syscall (fun () ->
  Syscall.truncate p l;
  RV_none
) ()

let pwrite fd bs len ofs = run_syscall (fun () ->
  RV_num (Syscall.pwrite fd bs len ofs)
) ()

let pwrite_full fd bs len ofs = run_syscall (fun () ->
  RV_num (Syscall.pwrite_full fd bs len ofs)
) ()

let write fd bs len = run_syscall (fun () ->
  RV_num (Syscall.write fd bs len)
) ()

let write_full fd bs len = run_syscall (fun () ->
  RV_num (Syscall.write_full fd bs len)
) ()

let add_user_to_group (User_id u) (Group_id g) =
  let (_, gn) = lookup_gid g in
  let (_, un) = lookup_uid u in
  put_user_in_group un gn;
  Value RV_none

let umask (File_perm new_mask) = run_syscall (fun () ->
  let mask = Int32.to_int new_mask in
  let old_mask = Syscall.umask mask in
  RV_file_perm (File_perm (Int32.of_int old_mask))
) ()

let chdir s = run_syscall (fun () ->
  Syscall.chdir s;
  RV_none
) ()

(* FIXME O_SHARE_DELETE? *)
let syscall_open_flag_of_o_f f = Fcntl.Oflags.(
  match f with
  | Fs_types.O_RDONLY    ->  (Some(O_RDONLY   )) (** Open for reading *)
  | Fs_types.O_WRONLY    ->  (Some(O_WRONLY   )) (** Open for writing *)
  | Fs_types.O_RDWR      ->  (Some(O_RDWR     )) (** Open for reading and writing *)
  | Fs_types.O_NONBLOCK  ->  (Some(O_NONBLOCK )) (** Open in non-blocking mode *)
  | Fs_types.O_APPEND    ->  (Some(O_APPEND   )) (** Open for append *)
  | Fs_types.O_CREAT     ->  (Some(O_CREAT    )) (** Create if nonexistent *)
  | Fs_types.O_TRUNC     ->  (Some(O_TRUNC    )) (** Truncate to 0 length if existing *)
  | Fs_types.O_EXCL      ->  (Some(O_EXCL     )) (** Fail if existing *)
  | Fs_types.O_NOCTTY    ->  (Some(O_NOCTTY   )) (** Don't make this dev a controlling tty *)
  | Fs_types.O_DSYNC     ->  (Some(O_DSYNC    )) (** Writes complete as `Synchronised I/O data integrity completion' *)
  | Fs_types.O_SYNC      ->  (Some(O_SYNC     )) (** Writes complete as `Synchronised I/O file integrity completion' *)
  | Fs_types.O_RSYNC     ->  (Some(O_RSYNC    )) (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)
  | Fs_types.O_CLOEXEC   ->  (Some(O_CLOEXEC  ))
  | Fs_types.O_DIRECTORY ->  (Some(O_DIRECTORY))
  | Fs_types.O_NOFOLLOW  ->  (Some(O_NOFOLLOW ))
  | Fs_types.O_SEARCH
  | Fs_types.O_TTY_INIT
  | Fs_types.O_EXEC      ->  None (* some flags are not available in Linux *)
)

let syscall_oflags_of_oflags oflags =
  oflags
  |> List.map syscall_open_flag_of_o_f
  |> List.filter (function None -> false | _ -> true)
  |> List.map (function None -> assert false | Some x -> x)

let posix_syscall_ext_cmd arch = function
  (* run an underlying syscall and get either an error or a value back *)
  | OS_OPEN_CLOSE (CS_Some p,fs,perms) ->
    let arch = Arch.architecture_of_ty_arch arch in
    let fs = list_from_finset (arch.arch_open_flags_of_int fs) in
    open_close p (syscall_oflags_of_oflags fs) perms
  | OS_OPEN_CLOSE (CS_Null, fs, perms) ->
    failwith "posix_syscall_ext_cmd: OS_OPEN_CLOSE(CS_Null,..) unimplemented"
  | OS_ADD_USER_TO_GROUP (u, g) ->
    failwith "posix_syscall_ext_cmd: can't add user to group in subprocess"
  | OS_DET_PWRITE (FD fd,bs,len,ofs) ->
    pwrite_full
      (Syscall.file_descr_of_int fd) (Abstract_string.to_string bs) len ofs
  | OS_DET_WRITE (FD fd,bs,len) ->
    write_full (Syscall.file_descr_of_int fd) (Abstract_string.to_string bs) len
  | OS_DET_PREAD (FD fd,i,j) ->
    pread_full (Syscall.file_descr_of_int fd) i j
  | OS_DET_READ (FD fd,i) ->
    read_full (Syscall.file_descr_of_int fd) i

let fail_posix_syscall_unimpl s =
  failwith ("posix_syscall_cmd: " ^ s ^ " unimplemented")

(* run an underlying syscall and get either an error or a value back *)
let posix_syscall_cmd (arch,lbl) = match lbl with
  | OS_CLOSE (FD fd) -> close fd
  | OS_LINK (CS_Some s, CS_Some d) -> link s d
  | OS_LINK (_,_) -> fail_posix_syscall_unimpl "OS_LINK(..,CS_Null,..)"
  | OS_MKDIR (CS_Some s,p) -> mkdir s p
  | OS_MKDIR (CS_Null, _) -> fail_posix_syscall_unimpl "OS_MKDIR(CS_Null,..)"
  | OS_OPEN (CS_Some p,oflags,perms) ->
    let oflags =
      let arch = Arch.architecture_of_ty_arch arch in
      list_from_finset (arch.arch_open_flags_of_int oflags)
    in
    open_ p (syscall_oflags_of_oflags oflags) perms
  | OS_OPEN (CS_Null, _, _) ->
    fail_posix_syscall_unimpl "OS_OPEN(CS_Null,..)"
  | OS_READDIR dh -> readdir dh
  | OS_CLOSEDIR dh -> closedir dh
  | OS_REWINDDIR dh -> rewinddir dh
  | OS_OPENDIR (CS_Some p) -> opendir p
  | OS_OPENDIR CS_Null -> fail_posix_syscall_unimpl "OS_OPENDIR(CS_Null)"
  | OS_RENAME (CS_Some s, CS_Some d) -> rename s d
  | OS_RENAME (_, _) -> fail_posix_syscall_unimpl "OS_RENAME(..,CS_Null,..)"
  | OS_RMDIR (CS_Some p) -> rmdir p
  | OS_RMDIR CS_Null -> fail_posix_syscall_unimpl "OS_RMDIR(CS_Null)"
  | OS_STAT (CS_Some p) -> stat p
  | OS_STAT CS_Null -> fail_posix_syscall_unimpl "OS_STAT(CS_Null)"
  | OS_LSTAT (CS_Some p) -> lstat p
  | OS_LSTAT CS_Null -> fail_posix_syscall_unimpl "OS_STAT(CS_Null)"
  | OS_SYMLINK (CS_Some s, CS_Some d) -> symlink s d
  | OS_SYMLINK (_, _) -> fail_posix_syscall_unimpl "OS_SYMLINK(..,CS_Null,..)"
  | OS_TRUNCATE (CS_Some p,l) -> truncate p l
  | OS_TRUNCATE (CS_Null, _) ->
    fail_posix_syscall_unimpl "OS_TRUNCATE(CS_Null,..)"
  | OS_UNLINK (CS_Some p) -> unlink p
  | OS_UNLINK CS_Null -> fail_posix_syscall_unimpl "OS_UNLINK(CS_Null)"
  | OS_PWRITE (FD fd,bs,len,ofs) ->
    pwrite (Syscall.file_descr_of_int fd) (Abstract_string.to_string bs) len ofs
  | OS_WRITE (FD fd,bs,len) ->
    write (Syscall.file_descr_of_int fd) (Abstract_string.to_string bs) len
  | OS_PREAD (FD fd,i,j) -> pread (Syscall.file_descr_of_int fd) i j
  | OS_READ (FD fd,i) -> read (Syscall.file_descr_of_int fd) i
  | OS_LSEEK (FD fd,ofs,seek_command) ->
    lseek (Syscall.file_descr_of_int fd) ofs seek_command
  | OS_READLINK (CS_Some p) -> readlink p
  | OS_READLINK CS_Null -> fail_posix_syscall_unimpl "OS_READLINK(CS_Null)"
  | OS_UMASK p -> umask p
  | OS_CHMOD (CS_Some s, p) -> chmod s p
  | OS_CHMOD (CS_Null, _) -> fail_posix_syscall_unimpl "OS_CHMOD(CS_Null,..)"
  | OS_CHOWN (CS_Some s, u, g) -> chown s u g
  | OS_CHOWN (CS_Null, _, _) -> fail_posix_syscall_unimpl "OS_CHOWN(CS_Null,..)"
  | OS_CHDIR (CS_Some s) -> chdir s
  | OS_CHDIR CS_Null -> fail_posix_syscall_unimpl "OS_CHDIR(CS_Null)"
  | OS_EXTENDED_CMD cmd' -> posix_syscall_ext_cmd arch cmd'

open Checklib_shared_types

let posix_pre_os_trans arch ppstate lbl = Agent.(match lbl with
  | OS_CHOWN (p, User_id u, Group_id g) ->
    let (g', _) = lookup_gid g in
    let (u', _) = lookup_uid u in
    let lazy channel = ppstate.pps_channel in
    channel.command (arch,OS_CHOWN (p, User_id u', Group_id g'))
  | OS_EXTENDED_CMD (OS_ADD_USER_TO_GROUP (u, g)) -> begin
    try `Result (add_user_to_group u g)
    with e -> `Error e
  end
  | _ ->
    let lazy channel = ppstate.pps_channel in
    channel.command (arch,lbl)
)

let posix_post_os_trans = function
  | Value (RV_os_stats ({ os_st_uid = User_id uid; os_st_gid = Group_id gid } as st)) ->
    Value (RV_os_stats { st with
                      os_st_uid = User_id (lookup_uid_rev uid);
                      os_st_gid = Group_id (lookup_gid_rev gid);
                    })
  | eov -> eov

let posix_os_trans_pid (s0 : posix_os_state) (pid:ty_pid) lbl =
  match fmap_lookup s0.pid_table pid with
  | None -> failwith "os_trans: impossible"
  | Some ppstate -> match (ppstate.pps_pid_run_state,lbl) with
    | (RUNNING,(OS_CALL(pid',lbl))) ->
      if pid' <> pid then finset_empty ()
      else
        let s1 = update_run_state s0 pid (BLOCKED_CALL lbl) in
        finset_singleton (GOS_normal s1)
    | (BLOCKED_CALL(lbl),OS_TAU) ->
      Agent.(match posix_pre_os_trans s0.pos_arch ppstate lbl with
      | `Error exn -> raise exn
      | `Result eov ->
        let eov = posix_post_os_trans eov in
        (* store the result *)
        let s1 = update_run_state s0 pid (PENDING_RETURN eov) in
        (* return singleton set *)
        finset_singleton (GOS_normal s1)
      )
    | (PENDING_RETURN(ev),(OS_RETURN(pid',ev'))) ->
      if pid' <> pid then finset_empty ()
      else if ev' <> ev then finset_empty ()
      else
        let s1 = update_run_state s0 pid RUNNING in
        finset_singleton (GOS_normal s1)
    | (_,_) -> finset_empty ()

let posix_os_trans_pcreate_destroy s0 lbl =
  match lbl with
  | OS_CREATE(pid, uid, gid) ->
    if finset_mem compare pid (fmap_dom s0.pid_table)
    then finset_empty ()
    else
      let ppstate = create_pps s0 s0.pos_root_dir uid gid in
      let pid_table = fmap_update s0.pid_table (pid,ppstate) in
      let s0 = { s0 with pid_table } in
      finset_singleton (GOS_normal s0)
  | OS_DESTROY(pid) -> begin
    if not (finset_mem compare pid (fmap_dom s0.pid_table))
    then finset_empty ()
    else match fmap_lookup s0.pid_table pid with
    | None -> finset_empty ()
    | Some ppstate ->
      match ppstate.pps_pid_run_state with
      | BLOCKED_CALL _ | PENDING_RETURN _ ->
        (* we don't allow processes to be destroyed if they are in kernel *)
        finset_empty ()
      | RUNNING -> (* TODO: should unregister the channel for stop_all later *)
        let lazy channel = ppstate.pps_channel in
        Agent.(match channel.stop () with
        | `Ok _ -> (* TODO: care about return code? *)
          let pid_table = fmap_remove s0.pid_table pid in
          let s0 = { s0 with pid_table } in
          finset_singleton (GOS_normal s0)
        | `Error exn -> raise exn
        )
  end
  | _ -> finset_empty ()

let posix_os_trans s0 lbl =
  let pids = fmap_dom s0.pid_table in
  let ss = finset_bigunion_image (fun pid ->
    posix_os_trans_pid s0 pid lbl
  ) pids in
  finset_union ss (posix_os_trans_pcreate_destroy s0 lbl)

let posix_initial_state pos_arch pos_root_dir =
  let agents = Agent.create ~run:posix_syscall_cmd in
  agents_list := agents :: !agents_list;
  let empty = { agents; pid_table = fmap_empty (); pos_root_dir; pos_arch; } in
  let ppstate = create_pps empty pos_root_dir root_uid root_gid in
  let pid_table = fmap_update empty.pid_table (Pid 1,ppstate) in
  { empty with pid_table }

module Posix_ops = struct

  (* Now the real interface *)
  type os_state = posix_os_state;;

  let os_trans = posix_os_trans;;

  module Unix_dump_fs_ops :
    Dump.Dump_fs_operations with type state = posix_os_state =
  struct

    open Unix.LargeFile
    type state = posix_os_state
    type dir_status = int (* a file permission *)

    let catch_unix_error f =
      try f ()
      with Unix.Unix_error (err, call, p) ->
        let err = error_of_unix_error err in
        raise Dump.(Dump_error (Unix_error (err, call, p)))

    let path_to_fname s0 p =
      let root = s0.pos_root_dir in
      let path = Fs_path.to_string p in
      Filename.concat root
        (if path.[0] = '/'
         then String.(sub path 1 (length path - 1))
         else path)

    let readlink s0 (p : Fs_path.t) = catch_unix_error (fun () ->
      Unix.readlink (path_to_fname s0 p)
    )

    let get_stat s0 (p : Fs_path.t) = catch_unix_error (fun () ->
      Unix.LargeFile.lstat (path_to_fname s0 p)
    )

    let enter_dir s0 (p : Fs_path.t) = catch_unix_error (fun () ->
      let perm = (get_stat s0 p).st_perm in
      let _ = Unix.chmod (path_to_fname s0 p) 0o777 in
      perm
    )

    let leave_dir s0 p ds = catch_unix_error (fun () ->
      Unix.chmod (path_to_fname s0 p) ds
    )

    (* sha1 of contents of file *)
    let sha1_of_path s0 p = catch_unix_error (fun () ->
      let st = get_stat s0 p in
      Unix.chmod (path_to_fname s0 p) 0o777;
      let sha = Sha1.to_hex (Sha1.file_fast (path_to_fname s0 p)) in
      Unix.chmod (path_to_fname s0 p) st.st_perm;
      (Int64.to_int st.st_size, sha)
    )

    let atime_of_path s0 p =
      let st = get_stat s0 p in
      string_of_float (st.st_atime)

    let mtime_of_path s0 p =
      let st = get_stat s0 p in
      string_of_float (st.st_mtime)

    let ctime_of_path s0 p =
      let st = get_stat s0 p in
            string_of_float (st.st_ctime)

    let inode_of_file_path s0 p =
      let st = get_stat s0 p in
      st.st_ino

    let inode_of_dir_path s0 p =
      let st = get_stat s0 p in
      st.st_ino

    let kind_of_path s p =
      let st = get_stat s p in
      st.st_kind

    let ls_path (s0 : state) (p : Fs_path.t) : string list =
      catch_unix_error (fun () ->
        let dents = ref [] in
        let fn = path_to_fname s0 p in
        Syscall.chmod fn 0o777;
        let h = Unix.opendir fn in
        try while true do
            let dent = Unix.readdir h in
            if dent <> "." && dent <> ".."
            then dents := dent :: !dents
          done; []
        with End_of_file ->
          Unix.closedir h;
          List.sort String.compare !dents
      )
  end

  module Unix_dump = Dump.Make(Unix_dump_fs_ops)
  let dump_of_path = Unix_dump.of_path

  open Checklib_shared_types

  let allowed_results_for_pid pid s0 =
    match (Fmap_default.fmap_lookup s0.pid_table pid) with 
    | None -> finset_empty ()
    | Some ppstate -> (match ppstate.pps_pid_run_state with 
      | PENDING_RETURN(ev) -> finset_singleton ev
      | _ -> finset_empty ()
    )

end
