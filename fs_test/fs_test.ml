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
open Sexplib.Std

open Fs_test_config

module System = Fs_test_system
module Mount = Fs_test_mount
module Cli = Fs_test_cli
module Index = Fs_test_index

(*
check already mounted

kernel:
openbsd ?
sunos ?

libc:
eglibc ?
dietlibc ?
bionic ?

*)

type suite = {
  path : string;
  nick : string;
}

type run = {
  index    : Index.stage Index.test_entry Index.test;
  env      : (Mount.t, Spec.Descr.t) Model.t;
  suite    : suite;
  script   : string;
  trace    : string;
  process  : System.process;
  run_dir  : string;
  out_buf  : Buffer.t;
  err_buf  : Buffer.t;
  out_file : string;
  err_file : string;
}

type error =
| Invalid_suite of suite
| Missing_suite of string
| Missing_trace of suite list * string
| Unknown_model of string
| Unknown_param of string * string (* name, class *)
exception Invocation_error of error

let ( / ) = Filename.concat

let signals = Sys.([sigterm; sigint])
let () = Mount.set_signals_destroy_world signals

let fs_size = System.megs 512

let print_success = false

let system = System.get_system ()

let exec_path = Unix.getcwd ()
let cmd_path = Filename.dirname Sys.argv.(0)
let cmd_path = Filename.(
  if is_relative cmd_path then concat exec_path cmd_path else cmd_path
)
let mnt_path = exec_path / "mnt"

let posix_command = cmd_path / "posix.native"
let check_command = cmd_path / "check.native"

let params = { Mount.size = fs_size; system; mnt_path }

let pid_idx = Hashtbl.create 16
let fd_idx  = Hashtbl.create 32

let name_of_model = Model.combine Fs.name Spec.Descr.name

let model_of_env = Model.(function
  | Mount env -> Mount env.Mount.fs
  | Spec spec -> Spec spec
)

let success_of_status =
  Index.(function Unix.WEXITED 0 -> Ok | _ -> Fail)

let output_of_run ({ out_file = stdout; err_file = stderr }) =
  Index.({ stdout; stderr })

let test_result run result = {
  Index.suite  = run.suite.nick;
  script = run.script;
  result;
}

let trace_result run status = function
  | None -> test_result run
    Index.(Trace {
      prev_phase = run.trace;
      success = success_of_status status;
      output = output_of_run run;
    })
  | Some _ ->
    Printf.eprintf "too many traces for %s/%s\n%!" run.suite.nick run.trace;
    exit 1

let diff_result diff_spec run status = Index.(function
  | Some ({ result = Trace diff_trace }) -> test_result run
    (let output = output_of_run run in
     let diff_checks = create_check_of {
       diff_spec;
       diff_result = {
         prev_phase = ();
         success = success_of_status status;
         output = { output with stdout = output.stdout ^ ".sexp" };
       };
       diff_human = {
         prev_phase = ();
         success = success_of_status status;
         output;
       };
     } in
     Diff { diff_checks; diff_trace })
  | Some ({ result = Script _ }) ->
    Printf.eprintf "index: expected trace but found script for %s/%s\n%!"
      run.suite.nick run.script;
    exit 1
  | Some ({ result = Diff _ }) ->
    Printf.eprintf "index: expected trace but found diff for %s/%s\n%!"
      run.suite.nick run.script;
    exit 1
  | None ->
    Printf.eprintf "index: no trace found for diff of %s/%s\n%!"
      run.suite.nick run.script;
    exit 1
)

let destroy_env = Model.(function
  | Mount env -> Mount.destroy env ()
  | Spec _ -> ()
)

let string_of_run ({ env; suite; trace }) =
  sprintf "%s:%s:%s" (name_of_model (model_of_env env)) suite.nick trace

let output_file prefix trace typ =
  sprintf "%s_%s%s" prefix (Filename.basename trace) typ

let test_rel_dir k dir =
  let rec n l =
    if List.length l < k
    then String.concat "/" l
    else n (List.tl l)
  in n (Str.(split (regexp "/") dir))

let wait_read ?test_result wait_flags pid =
  match System.waitpid wait_flags pid with
  | 0, _ -> ()
  | pid, status ->
    let run = Hashtbl.find pid_idx pid in
    let env = run.env in
    destroy_env env;
    let run_str = string_of_run run in
    let results = run.run_dir / "results" in
    let label = if status = Unix.WEXITED 0 then "SUCCESS" else "FAILURE" in
    if status <> Unix.WEXITED 0 || print_success
    then printf "\n%s %s: %s\n" run_str label (System.string_of_status status);
    let out_drain = System.(drain_into_buf run.process.stdout run.out_buf) in
    Hashtbl.remove fd_idx run.process.System.stdout;
    let out_sz = Buffer.length run.out_buf in
    let out_file = results / run.out_file in
    let oc = open_out out_file in
    Buffer.output_buffer oc run.out_buf;
    close_out oc;
    if status <> Unix.WEXITED 0 || print_success
    then printf "%s STDOUT: %d bytes (%d drained) in [%s](%s)\n"
      run_str out_sz out_drain out_file (test_rel_dir 5 out_file);
    Buffer.clear run.out_buf;
    let err_drain = System.(drain_into_buf run.process.stderr run.err_buf) in
    Hashtbl.remove fd_idx run.process.System.stderr;
    let err_sz = Buffer.length run.err_buf in
    let err_file = results / run.err_file in
    let oc = open_out err_file in
    Buffer.output_buffer oc run.err_buf;
    close_out oc;
    if status <> Unix.WEXITED 0 || print_success
    then printf "%s STDERR: %d bytes (%d drained) in [%s](%s)\n%!"
      run_str err_sz err_drain err_file (test_rel_dir 5 err_file);
    Buffer.clear run.err_buf;
    Hashtbl.remove pid_idx pid;
    match test_result with None -> ()
    | Some test_result ->
      Index.put_test_map run.index run.script (test_result run status)

let drain_children () =
  eprintf "%d live children receiving SIGTERM\n%!" (Hashtbl.length pid_idx);
  let pids = Hashtbl.fold (fun pid _ pids -> pid::pids) pid_idx [] in
  let pids = List.fold_left Unix.(fun remaining pid ->
    try kill pid Sys.sigterm; pid::remaining
    with Unix_error(ESRCH,_,_) ->
      eprintf "Can't SIGTERM pid %d: ESRCH!\n%!" pid;
      remaining
  ) [] pids in
  List.iter (fun pid ->
    eprintf "Trying to drain pid %d...\n%!" pid;
    wait_read Unix.([WUNTRACED]) pid;
    eprintf "Child pid %d drained\n%!" pid;
  ) pids

let () =
  let sigh = Hashtbl.create 4 in
  List.iter (fun signal_ ->
    Hashtbl.replace sigh signal_ Sys.(
      signal signal_ (Signal_handle (fun signal ->
        ignore Unix.(sigprocmask SIG_BLOCK signals);
        eprintf "%d caught %s; draining children.\n%!"
          (Unix.getpid ()) (System.string_of_signal signal);
        drain_children ();
        try (match Hashtbl.find sigh signal_ with
        | Signal_default -> exit 1
        | Signal_ignore -> ()
        | Signal_handle f -> f signal_
        ) with Not_found -> exit 1
      ))
    )
  ) signals

let run_trace env file extra_args = match env with
  | Model.Mount env ->
    let dir = Mount.(match env.subdir with
      | None -> env.mnt
      | Some sub -> env.mnt / sub
    ) in
    let arch = Os.(match system with
      | Linux   -> "linux"
      | FreeBSD -> "freebsd"
      | Darwin  -> "mac_os_x"
    ) in
    let args =
      Array.append [|"-v"; file; "-arch"; arch; "-tmp"; dir|] extra_args
    in
    let process =
      System.create_process posix_command args (Unix.environment ())
    in
    Unix.close process.System.stdin;
    process
  | Model.Spec { Spec.Descr.flavor } ->
    let arch = Spec.(match flavor with
      | Linux    -> "linux"
      | Posix    -> "posix"
      | Mac_os_x -> "mac_os_x"
      | FreeBSD  -> "freebsd"
    ) in
    let args =
      Array.append [|"-v"; file; "-arch"; arch; "--root"|] extra_args
    in
    let process =
      System.create_process check_command args (Unix.environment ())
    in
    Unix.close process.System.stdin;
    process

let os_version = Lazy.from_fun (fun () -> Fs_test_config.Os.({
  node    = List.hd (System.read_command "uname -n");
  release = List.hd (System.read_command "uname -r");
  version = List.hd (System.read_command "uname -v");
  machine = List.hd (System.read_command "uname -m");
  all     = List.hd (System.read_command "uname -a");
}))
let libc_version bin = System.(match system with
  | Os.Darwin -> Libc.System, List.hd (
    read_command
      ("otool -L "^bin
       ^" | grep libSystem"
      )
  )
  | Os.FreeBSD -> Libc.System, "system"
  | Os.Linux ->
    let exec = "ldd" in
    let args = [|bin|] in
    let sed_script = "/libc.so/s/.*=> \\([^ ]*\\).*/\\1/p" in
    if exit_command exec args true (function 1 -> Some false | _ -> None)
    then (* ldd can understand the ELF binary *)
      Libc.System, List.hd (
        read_command
          ("`ldd "^bin
           ^" | sed -ne "^(sprintf "%S" sed_script)
           ^"`"
           ^"| head -1"
          )
      )
    else (* ldd can't understand the ELF binary;
            we probably have a different libc... *)
      let musl_ldd = "/lib/x86_64-linux-musl/libc.so" in
      if Sys.file_exists musl_ldd
      then begin
        let musl_args = Array.append [|exec|] args in
        let env = [||] in
        let p = create_process_exec_args musl_ldd musl_args env in
        match waitpid [] p.pid with
        | (_, Unix.WEXITED 0) ->
          let b = Buffer.create 1024 in
          ignore (drain_into_buf p.stdout b);
          eprintf "%s\n%!" (Buffer.contents b);
          let sed = create_process "sed" [|"-ne"; sed_script|] env in
          let sed_oc = Unix.out_channel_of_descr sed.stdin in
          Buffer.output_buffer sed_oc b;
          flush sed_oc;
          Unix.close sed.stdin;
          (match waitpid [] sed.pid with
          | (_, Unix.WEXITED 0) ->
            let cmd = input_line (Unix.in_channel_of_descr sed.stdout) in
            (* TODO: should this check that we did, in fact, extract a
               musl so? *)
            Libc.Musl, List.hd (read_command (cmd^" 2>&1 | head -2 | tail -1"))
          | (_, Unix.WEXITED _) -> failwith "Error: sed exec failed."
          | (_,_) -> failwith "Error: sed exec failed unexpectedly."
          )
        | (_, Unix.WEXITED _) -> failwith "Error: musl libc exec failed."
        | (_, _) -> failwith "Error: musl libc exec failed unexpectedly."
      end
      else failwith "Error: libc detection failure. Submit a report."
)

let version =
  Fs_test_version.git_rev
  ^ (if Fs_test_version.git_dirty then " (dirty)" else "")

let traces_of_suite suite =
  let { path } = suite in
  let dh = Unix.opendir path in
  let rec list_traces lst =
    let trace = try Some (Unix.readdir dh) with End_of_file -> None in
    match trace with
    | Some t when Filename.check_suffix t ".trace" -> list_traces (t::lst)
    | Some _ -> list_traces lst
    | None -> lst
  in
  let traces = list_traces [] in
  Unix.closedir dh;
  eprintf "Found %d traces for %s\n%!" (List.length traces) suite.nick;
  traces

let exec_run_of_trace index model suite run_dir trace =
  let env = Model.(match model with
    | Mount fs    -> Mount (Mount.load params fs)
    | Spec spec   -> Spec spec
  ) in
  Unix.chdir run_dir;
  let run_dir = Unix.getcwd () in
  let process = run_trace env trace [||] in
  let index = Index.tests_of_suite index suite.nick in
  let run = {
    index; env; suite; script = trace; trace; process; run_dir;
    out_buf  = Buffer.create 4096;
    err_buf  = Buffer.create 4096;
    out_file = output_file "exec" trace "";
    err_file = output_file "exec" trace ".err";
  } in
  Unix.chdir exec_path;
  Hashtbl.replace pid_idx process.System.pid    run;
  Hashtbl.replace fd_idx  process.System.stdout (false, run);
  Hashtbl.replace fd_idx  process.System.stderr (true,  run);
  run

let suite_of_trace trace =
  let path = Filename.dirname trace in
  let nick = Filename.basename path in
  { nick; path }

let check_run_of_trace index spec run_dir script trace =
  let env = Model.Spec spec in
  let suite = suite_of_trace (run_dir / "a_trace") in
  let pwd = Unix.getcwd () in
  Unix.chdir (run_dir / "results");
  let out_file = output_file "check" trace "" in
  let process = run_trace env trace [|"--sexp"; out_file ^ ".sexp" |] in
  let run = {
    index; env; suite; script; trace; process; run_dir;
    out_buf  = Buffer.create 4096;
    err_buf  = Buffer.create 4096;
    out_file;
    err_file = output_file "check" trace ".err";
  } in
  Unix.chdir pwd;
  Hashtbl.replace pid_idx process.System.pid    run;
  Hashtbl.replace fd_idx  process.System.stdout (false, run);
  Hashtbl.replace fd_idx  process.System.stderr (true,  run);
  run

let fuse_params_of_params : string list -> Fs.fuse_param list =
  List.map (function
  | "allow_other" -> `Allow_other
  | "default_permissions" -> `Default_permissions
  | "umask0" -> `Umask0
  | p -> raise (Invocation_error (Unknown_param (p,"FUSE")))
)

(* TODO: work out errors/unavailability *)
let parse_fs params model_s =
  let basic = Os.basic_fs system in
  let fuse_params = fuse_params_of_params params in
  match model_s with
  | "bind"    -> Mount.([local_target (Fs.Bind basic)])
  | "fusexmp" -> Mount.([local_target (Fs.Fusexmp (fuse_params, basic))])
  | "sshfs"   -> Mount.([local_target (Fs.Sshfs (fuse_params, basic))])
  | "davfs"   -> Mount.([local_target (Fs.Davfs basic)])
  | "nfsv3"   -> Mount.([local_target (Fs.Nfsv3 (Os.basic_fs system))])
  | "nfsv4"   -> Mount.([local_target (Fs.Nfsv4 (Os.basic_fs system))])
  | "posixovl_vfat"->
     Mount.([local_target (Fs.Posixovl (fuse_params, Fs.Vfat_loop))])
  | "posixovl_ntfs"->
     Mount.([local_target (Fs.Posixovl (fuse_params, Fs.Ntfs3g_loop))])
  | "aufs"    -> Mount.([local_target Fs.(Aufs [Ext4_loop; Tmpfs])])
  | "overlay" -> Mount.([local_target Fs.(Overlay (Ext4_loop, Tmpfs))])
  | "gluster" -> Mount.([local_target Fs.(Gluster Xfs_loop)])
  | "ntfs"    -> Mount.([local_target Fs.Ntfs3g_loop])
  | "tmpfs"   -> Mount.tmpfs_if_available ()
  | "ext"     -> Mount.extfs_if_available ()
  | "ext2"    -> Mount.extfs_if_available ~only:[2] ()
  | "ext3"    -> Mount.extfs_if_available ~only:[3] ()
  | "ext4"    -> Mount.extfs_if_available ~only:[4] ()
  | "fuse_ext"-> Mount.fuse_ext2_if_available ()
  | "fuse_ext2"-> Mount.fuse_ext2_if_available ~only:[2] ()
  | "fuse_ext3"-> Mount.fuse_ext2_if_available ~only:[3] ()
  | "btrfs"   -> Mount.btrfs_if_available ()
  | "f2fs"    -> Mount.f2fs_if_available ()
  | "minix"   -> Mount.minix_if_available ()
  | "hfsplus" -> Mount.hfsplus_if_available ()
  | "ufs2"    -> Mount.ufs2_if_available ()
  | "nilfs2"  -> Mount.nilfs2_if_available ()
  | "vfat"    -> Mount.vfat_if_available ()
  | "xfs"     -> Mount.xfs_if_available ()
  | "zfs"     -> Mount.zfs_if_available ()
  | fs        -> match String.sub fs 0 5 with
    | "path=" ->
      let path = String.sub fs 5 (String.length fs - 5) in
      let path = if path.[0] = '/'
        then path
        else exec_path / path
      in
      Mount.([local_target (Fs.Path path)])
    | _ -> raise (Invocation_error (Unknown_model fs))

let spec_params_of_params = function
  | []    -> []
  | p::ps -> raise (Invocation_error (Unknown_param (p,"spec")))

let parse_spec params spec_s =
  let _parsed_params = spec_params_of_params params in
  match spec_s with
  | "linux_spec"    -> Spec.Linux
  | "posix_spec"    -> Spec.Posix
  | "mac_os_x_spec" -> Spec.Mac_os_x
  | "freebsd_spec"  -> Spec.FreeBSD
  | spec         -> raise (Invocation_error (Unknown_model spec))

let parse_model params str =
  try
    let spec = Model.Spec { Spec.Descr.flavor = parse_spec params str } in
    [spec, Fs.Version version]
  with Invocation_error _ ->
    List.map (fun (fs, v) -> (Model.Mount fs, v)) (parse_fs params str)

let parse_fs_to_predicate = Fs.(function
  | "bind"    -> (function Bind _      -> true | _ -> false)
  | "fusexmp" -> (function Fusexmp _   -> true | _ -> false)
  | "sshfs"   -> (function Sshfs _     -> true | _ -> false)
  | "davfs"   -> (function Davfs _     -> true | _ -> false)
  | "nfsv3"   -> (function Nfsv3 _     -> true | _ -> false)
  | "nfsv4"   -> (function Nfsv4 _     -> true | _ -> false)
  | "posixovl_vfat"-> (function Posixovl (_,Vfat_loop)   -> true | _ -> false)
  | "posixovl_ntfs"-> (function Posixovl (_,Ntfs3g_loop) -> true | _ -> false)
  | "aufs"    -> (function Aufs _      -> true | _ -> false)
  | "overlay" -> (function Overlay _   -> true | _ -> false)
  | "gluster" -> (function Gluster _   -> true | _ -> false)
  | "ntfs"    -> (function Ntfs3g_loop -> true | _ -> false)
  | "tmpfs"   -> (function Tmpfs       -> true | _ -> false)
  | "btrfs"   -> (function Btrfs_loop  -> true | _ -> false)
  | "f2fs"    -> (function F2fs_loop   -> true | _ -> false)
  | "minix"   -> (function Minix_loop  -> true | _ -> false)
  | "hfsplus" -> (function Hfsplus_loop-> true | _ -> false)
  | "ufs2"    -> (function Ufs2_loop   -> true | _ -> false)
  | "nilfs2"  -> (function Nilfs2_loop -> true | _ -> false)
  | "vfat"    -> (function Vfat_loop   -> true | _ -> false)
  | "xfs"     -> (function Xfs_loop    -> true | _ -> false)
  | "zfs"     -> (function Zfs_loop    -> true | _ -> false)
  | "ext"     -> (function
    | Ext2_loop | Ext3_loop | Ext4_loop -> true
    | _ -> false)
  | "ext2"    -> (function Ext2_loop   -> true | _ -> false)
  | "ext3"    -> (function Ext3_loop   -> true | _ -> false)
  | "ext4"    -> (function Ext4_loop   -> true | _ -> false)
  | "fuse_ext"-> (function
    | Fuse_ext2_loop | Fuse_ext3_loop -> true
    | _ -> false)
  | "fuse_ext2" -> (function Fuse_ext2_loop -> true | _ -> false)
  | "fuse_ext3" -> (function Fuse_ext3_loop -> true | _ -> false)
  | fs        -> match String.sub fs 0 5 with
    | "path=" ->
      let path = String.sub fs 5 (String.length fs - 5) in
      (function Path p when p = path -> true | _ -> false)
    | _ -> raise (Invocation_error (Unknown_model fs))
)

let parse_model_to_predicate str =
  try let spec = Model.Spec { Spec.Descr.flavor = parse_spec [] str } in
      (=) spec
  with Invocation_error _ -> function
  | Model.Mount m -> parse_fs_to_predicate str m
  | _ -> false

let all_specs = [
  "linux_spec";
  "posix_spec";
  "mac_os_x_spec";
  "freebsd_spec";
]

let all_fs = [
  "tmpfs";
  "ext";
  "ext2";
  "ext3";
  "ext4";
  "fuse_ext2";
  "btrfs";
  "f2fs";
  "minix";
  "hfsplus";
  "ufs2";
  "nilfs2";
  "vfat";
  "xfs";
  "zfs";
  "bind";
  "fusexmp";
  "sshfs";
  "davfs";
  "nfsv3";
  "nfsv4";
  "posixovl_vfat";
  "posixovl_ntfs";
  "aufs";
  "overlay";
  "gluster";
  "ntfs";
]

let all_models = all_specs @ all_fs

let fuse_params = List.map Fs.string_of_fuse_param [
  `Allow_other;
  `Default_permissions;
  `Umask0;
]

let fs_params = fuse_params

let all_params = fs_params

let spec_of_spec_descr ({ Spec.Descr.flavor }) = {
  Spec.flavor;
  fs_spec_version = version;
}

let libc_v = Lazy.from_fun (fun () -> libc_version posix_command)

let config_of_model model model_version = Fs_test_config.(
  Model.map (fun fs -> {
    Stack.time = Unix.time ();
    os = system, Lazy.force os_version;
    fs = fs, model_version;
    libc = Lazy.force libc_v;
    fs_test_version = version;
  }) spec_of_spec_descr model
)

let model_of_config =
  Fs_test_config.(Model.map (fun { Stack.fs = (fs,_) } -> fs) Spec.to_descr)

let check_commands () =
  let open Unix in
  let check command =
    try access command [X_OK]
    with Unix_error ((EACCES | ENOENT),_,_) ->
      eprintf "%s not found.\n" command;
      eprintf "Please build %s.\n%!" command;
      exit 1
  in
  check posix_command;
  check check_command

let print_spec_version () =
  printf "Spec version: %s\n" version

let print_versions model_list =
  printf "OS version: %s\n" (Lazy.force os_version).Fs_test_config.Os.all;
  printf "libc version: %s\n" (Libc.versioned_name (Lazy.force libc_v));
  print_spec_version ();
  List.iter (fun (model, model_version) ->
    printf "%s version: %s\n"
      (name_of_model model) (Fs.string_of_version model_version)
  ) model_list;
  flush stdout

let prepare_mnt () =
  at_exit (System.ignore_failure (fun () -> Unix.rmdir mnt_path));
  System.continue "mkdir" [|"-p"; mnt_path|]

let all_suites = [
  "file_descriptors";
  "link";
  "chdir";
  "mkdir";
  "readdir";
  "rmdir";
  "rename";
  "stat";
  "lstat";
  "truncate";
  "permissions";
  "open";
  "unlink";
  "symlink";
]

let make_temp_dir out_base_path =
  let date = List.hd (System.read_command "date +'%F'") in
  let cwd = Unix.getcwd () in
  Unix.chdir out_base_path;
  let temp_dir = out_base_path /
    (List.hd (System.read_command (
      sprintf "mktemp -d %s_XXX" date
     ))) in
  Unix.chdir cwd;
  temp_dir

let cleanup e =
  eprintf "Current working directory: %s\n%!" (Unix.getcwd ());
  eprintf "\nFatal error: %s\n%!" (Printexc.to_string e);
  Printexc.print_backtrace stderr;
  eprintf "\nFatal error encountered; cleaning up.\n%!";
  drain_children ();
  Mount.destroy_world ();
  begin match e with
  | Unix.Unix_error (e, syscall, "") ->
    eprintf "%s error: %s\n%!" syscall (Unix.error_message e);
    exit 1
  | Unix.Unix_error (e, syscall, path) ->
    eprintf "%s error on %s: %s\n%!" syscall path (Unix.error_message e);
    exit 1
  | _ -> ()
  end;
  raise e

let gather_some ~test_result =
  let fds = Hashtbl.fold (fun k _ l -> k::l) fd_idx [] in
  begin match fds with
  | [] -> ()
  | fds ->
    let read_fds, _, _ = Unix.select fds [] [] (-0.1) in
    List.iter (fun fd ->
      let is_err, { out_buf; err_buf } = Hashtbl.find fd_idx fd in
      let buf = if is_err then err_buf else out_buf in
      ignore (System.read_into_buf fd buf)
    ) read_fds
  end;
  wait_read ~test_result Unix.([WNOHANG; WUNTRACED]) 0

let exec_width index width suite_traces out_path model model_version =
  let test_result = trace_result in
  let model_name = name_of_model model in
  let fs_dir = out_path / model_name in
  let config = config_of_model model model_version in
  let config_result = Index.config_result_of_config model_name config in
  Index.add_config index config_result;
  let index = config_result.Index.test in
  List.iter (fun (suite, trace) ->
    if Hashtbl.length pid_idx >= width
    then while Hashtbl.length pid_idx >= width do
        gather_some ~test_result
      done;
    printf ".%!";
    let run_dir = fs_dir / suite.nick in
    let name = Filename.basename trace in
    System.continue "cp" [|suite.path / trace; run_dir|];
    eprintf "Starting trace %s against %s\n%!" name model_name;
    ignore (exec_run_of_trace index model suite run_dir trace)
  ) suite_traces;
  while Hashtbl.length pid_idx > 0 do
    gather_some ~test_result
  done

let check_width width trace_index spec_descr =
  let spec = spec_of_spec_descr spec_descr in
  let test_result = diff_result spec in
  Index.(iter_config_suite_tests (fun config path index test_entry ->
    if Hashtbl.length pid_idx >= width
    then while Hashtbl.length pid_idx >= width do
        gather_some ~test_result
      done;
    match test_entry with
    | { suite; script;
        result = Trace { success = Ok; output = { stdout = trace } } } ->
      let run_dir = path / suite in
      printf ".%!";
      ignore (check_run_of_trace index spec_descr run_dir script trace)
    | { result = Trace { success = Fail; output = { stdout = trace } } } ->
      eprintf "Skipping failed script execution '%s'.\n%!" trace
    | { suite; script } ->
      eprintf "Skipping non-trace result for '%s/%s'.\n%!" suite script
  ) trace_index);
  while Hashtbl.length pid_idx > 0 do
    gather_some ~test_result
  done

let run_width index out_base_path width model_list suite_traces =
  let out_path = make_temp_dir out_base_path in
  let suite_nicks = List.fold_left (fun uniq_suites (suite,_) ->
    if List.mem suite.nick uniq_suites
    then uniq_suites
    else suite.nick::uniq_suites
  ) [] suite_traces in
  try
    List.(iter (fun (model, model_version) ->
      let fs_name = name_of_model model in
      let fs_dir = out_path / fs_name in
      Unix.chdir exec_path;
      List.iter (fun nick ->
        let run_dir = fs_dir / nick in
        System.continue "mkdir" [|"-p"; run_dir / "results"|]
      ) suite_nicks;
      exec_width index width suite_traces out_path model model_version
    ) model_list);
    out_path
  with e -> cleanup e

let rec resolve_suite search_path suite_path = match search_path with
  | dir::rest ->
    let path = Filename.(concat (concat dir suite_path) "") in
    let nick = Filename.basename suite_path in
    let suite = { nick; path; } in
    let open Unix in
    (try access path []; suite
     with
     | Unix_error (ENOENT, _, _) -> resolve_suite rest suite_path
     | Unix_error (ENOTDIR, _, _) ->
       raise (Invocation_error (Invalid_suite suite))
    )
  | [] -> raise (Invocation_error (Missing_suite suite_path))

let resolve_suite_list search_path = List.map (fun suite ->
  if Filename.is_relative suite
  then resolve_suite search_path suite
  else { nick = Filename.basename suite; path = suite; }
)

let rec resolve_trace suites trace = match suites with
  | suite::rest ->
    let path = suite.path / trace in
    let open Unix in
    (try access path []; (suite, trace)
     with Unix_error (ENOENT, _, _) -> resolve_trace rest trace
    )
  | [] -> raise Not_found

let resolve_trace_list suites = List.map (fun trace ->
  if Filename.is_relative trace
  then try resolve_trace suites trace
    with Not_found -> raise (Invocation_error (Missing_trace (suites, trace)))
  else failwith "Un-suited traces not implemented. Use a suite."
)

let exec out_base_path models params search_path suites traces skips width =
  let search_path = List.map (fun path ->
    if Filename.is_relative path then exec_path / path else path
  ) search_path in
  try
    let index = Index.empty_config_table () in
    let fs_list = List.(flatten (map (parse_model params) models)) in
    let suites = resolve_suite_list search_path suites in
    check_commands ();
    print_versions fs_list;
    prepare_mnt ();
    let out_path = match traces with
      | [] -> run_width index out_base_path 4 fs_list
        List.(flatten (map (fun suite ->
          let traces = traces_of_suite suite in
          let traces = List.filter (fun x -> not (List.mem x skips)) traces in
          map (fun trace -> (suite, trace)) traces
        ) suites))
      | traces ->
         let traces = List.filter (fun x -> not (List.mem x skips)) traces in
         let traces = resolve_trace_list suites traces in
         run_width index out_base_path width fs_list traces
    in

    let args = Array.of_list (List.tl (Array.to_list Sys.argv)) in
    let command_line = System.string_of_exec_args Sys.argv.(0) args in
    Unix.chdir exec_path;
    Index.write index (out_path / "index.traces");
    printf "\nFINISHED: %s\n%!" command_line
  with
  | Invocation_error error -> begin match error with
    | Missing_trace (suites, trace) ->
      eprintf "Trace '%s' not found in %s\n%!"
        trace (String.concat ":" (List.map (fun suite -> suite.path) suites))
    | Missing_suite nick ->
      eprintf "Suite '%s' not found in %s\n%!"
        nick (String.concat ":" search_path)
    | Invalid_suite suite ->
      eprintf "Suite '%s' found at %s but it is not a directory.\n%!"
        suite.nick suite.path
    | Unknown_model fs ->
      eprintf "Model name '%s' is not known to this program.\n%!" fs
    | Unknown_param (p,c) ->
      eprintf "Parameter '%s' is not recognized by model class '%s'.\n%!" p c
  end; exit 1

let replace_config_prefix rename_prefix src dest (config,model,d) =
  let config_result = Hashtbl.find src config in
  let path = rename_prefix ^ d in
  Hashtbl.replace dest config { config_result with Index.path }

let any_predicate ps v = List.exists ((|>) v) ps

let merge src dest model_ss rename_prefix =
  let model_p = any_predicate (List.map parse_model_to_predicate model_ss) in
  let src_index = Index.read (src / "index.traces") in
  let srcs = Index.(fold_config (fun { config; path; } a ->
    let model = model_of_config config in
    if model_p model
    then (config,model,path)::a
    else a
  ) [] src_index) in
  match List.fold_left (fun l (_,_,d) ->
    let new_name = rename_prefix ^ d in
    if Sys.file_exists (dest / new_name) then new_name::l else l
  ) [] srcs with
  | [] ->
    List.iter (fun (_,_,d) ->
      let new_name = rename_prefix ^ d in
      System.continue "cp" [|"-r"; src / d; dest / new_name|]
    ) srcs;
    let dest_index = Index.read (dest / "index.traces") in
    List.iter (replace_config_prefix rename_prefix src_index dest_index) srcs;
    Index.write dest_index (dest / "index.traces");
    let src_index  = Index.read (src  / "index.diffs") in
    let dest_index = Index.read (dest / "index.diffs") in
    List.iter (replace_config_prefix rename_prefix src_index dest_index) srcs;
    Index.write dest_index (dest / "index.diffs");
  | collisions ->
    eprintf "Can't merge %s into %s due to colliding directories:\n%s\n"
      src dest (String.concat "\n" collisions);
    exit 1

let check spec dir suite_list width =
  try
    let spec = parse_spec [] spec in
    check_commands ();
    print_spec_version ();

    let traces = Index.read (dir / "index.traces") in
    let filtered_traces = match suite_list with
      | [] -> traces
      | suites ->
        (* TODO: make this not horrible *)
        let filtered = Hashtbl.copy traces in
        Hashtbl.iter (fun config_descr { Index.config; path; test } ->
          let filtered_suites = Hashtbl.copy test in
          Hashtbl.iter (fun suite _ ->
            if not (List.mem suite suites)
            then Hashtbl.remove filtered_suites suite
          ) test;
          Hashtbl.replace filtered config_descr
            { Index.config; path; test = filtered_suites }
        ) traces;
        filtered
    in
    Unix.chdir dir;
    check_width width filtered_traces {
      Fs_test_config.Spec.Descr.flavor = spec;
    };

    let args = Array.of_list (List.tl (Array.to_list Sys.argv)) in
    let command_line = System.string_of_exec_args Sys.argv.(0) args in
    Unix.chdir exec_path;
    Index.write filtered_traces (dir / "index.diffs");
    printf "\nFINISHED: %s\n%!" command_line
  with
  | Invocation_error error -> begin match error with
    | Unknown_model model ->
      eprintf "Model name '%s' is not known to this program.\n%!" model
    | _ -> (* TODO: FIXME *)
      eprintf "Unknown reason for inapplicable exception during check.\n%!"
  end; exit 1

let get_leftover_users () =
  let prefix = "fs_user_" in
  let spec = 32 in
  let rec try_up_to acc k = function
    | 0 -> acc
    | limit ->
      let acc, limit =
        try
          let username = prefix ^ (string_of_int k) in
          ignore (Unix.getpwnam username);
          (username :: acc), spec
        with Not_found -> acc, (limit - 1)
      in
      try_up_to acc (k + 1) limit
  in
  try_up_to [] 0 spec

let get_leftover_groups () =
  let prefix = "fs_group_" in
  let spec = 32 in
  let rec try_up_to acc k = function
    | 0 -> acc
    | limit ->
      let acc, limit =
        try
          let groupname = prefix ^ (string_of_int k) in
          ignore (Unix.getgrnam groupname);
          (groupname :: acc), spec
        with Not_found -> acc, (limit - 1)
      in
      try_up_to acc (k + 1) limit
  in
  try_up_to [] 0 spec

let clean_users = List.iter System.delete_user
let clean_groups = List.iter System.delete_group

let rec yes_no q yes no =
  Printf.printf "%s (y/N) " q;
  match read_line () with
  | "Y" | "y" -> yes ()
  | ""  | "N" | "n" -> no ()
  | _ ->
    Printf.printf "Please answer 'y' or 'n'.\n";
    yes_no q yes no

let clean () =
  let users  = get_leftover_users  () in
  let n_users = List.length users in
  let groups = get_leftover_groups () in
  let n_groups = List.length groups in
(*  let mounts = get_leftover_mounts () in
  let n_mounts = List.length mounts in*)
  if n_users > 0
  then Printf.printf "Found %d leftover users:\n  %s\n"
    n_users (String.concat ", " users);
  if n_groups > 0
  then Printf.printf "Found %d leftover groups:\n  %s\n"
    n_groups (String.concat ", " groups);
(*  if n_mounts > 0
  then Printf.printf "Found %d leftover mounts:\n  %s\n"
    n_mounts (String.concat ", " mounts);*)
  if n_groups > 0 || n_users > 0
  then yes_no "Would you like to remove these resources?"
    (fun () -> clean_users users; clean_groups groups)
    (fun () -> ())
  else Printf.printf "Found no leaked users or groups. Exiting.\n"

let run fs fs_params cmd =
  try
    match parse_fs fs_params fs with
    | [] -> failwith "missing fs"
    | _::_::_ -> failwith "multiple file systems not supported"
    | [fs, v] ->
      let model = (Model.Mount fs, v) in
      let name = name_of_model (Model.Mount fs) in
      print_versions [model];
      prepare_mnt ();
      try
        let mount = Mount.load params fs in
        let env = Model.Mount mount in
        Unix.chdir mount.Mount.mnt;

        Printf.eprintf "\n\n**** entering %s ****\n" name;

        let chdir = exec_path in
        System.subproc ~chdir (List.hd cmd) (Array.of_list (List.tl cmd));

        Printf.eprintf "\n\n**** leaving %s ****\n" name;

        destroy_env env
      with e -> cleanup e
  with
  | Invocation_error error -> begin match error with
    | Missing_trace (_,_) | Missing_suite _ | Invalid_suite _ ->
      eprintf "impossible error?\n%!"
    | Unknown_model fs ->
      eprintf "File system '%s' is not known to this program.\n%!" fs
    | Unknown_param (p,c) ->
      eprintf "Parameter '%s' is not recognized by file system class '%s'.\n%!"
        p c
  end; exit 1

open Cmdliner

let width = Arg.(
  value & opt int 4 & info ["j";"jobs"]
    ~docv:"JOB_COUNT"
    ~doc:"The maximum number of parallel processes to use."
)

let exec_cmd =
  let doc = "Execute a set of scripts in a model" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,exec) interprets a set of scripts with the "
        ^"specification or the local machine.");
  ]@Cli.man_trailer in
  Term.(pure exec
          $ Cli.out_dir exec_path
          $ Cli.models all_models
          $ Cli.params all_params
          $ Cli.suite_path ["."]
          $ Cli.suite all_suites
          $ Cli.trace
          $ Cli.skip_trace
          $ width
  ), Term.info "exec" ~version ~doc ~man

let merge_cmd =
  let doc = "Merge a set of traces into another" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,merge) combines a set of traces with another "
        ^"set of traces.");
  ]@Cli.man_trailer in
  let rename = Arg.(
    value & opt string "" & info ["rename"]
      ~docv:"NAME_PREFIX"
      ~doc:"The new relative path prefix to use in the target trace set."
  ) in
  Term.(pure merge
          $ Cli.dir ~docv:"SOURCE_DIR" ~doc:"The directory of traces to merge" 0
          $ Cli.dir ~docv:"TARGET_DIR" ~doc:"The target directory of traces" 1
          $ Cli.models all_models
          $ rename
  ), Term.info "merge" ~version ~doc ~man

let check_cmd =
  let doc = "Check a set of traces against a model" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,check) checks a set of traces against a "
        ^"specification configuration (model).");
  ]@Cli.man_trailer in
  Term.(pure check
          $ Cli.spec all_specs 0
          $ Cli.dir ~docv:"EXEC_DIR" ~doc:"The directory of traces to check" 1
          $ Cli.suite []
          $ width
  ), Term.info "check" ~version ~doc ~man

let html_cmd =
  let doc = "Generate HTML from checked traces" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,html) generates HTML from checked traces.");
  ]@Cli.man_trailer in
  Term.(pure Fs_test_html.generate
          $ Cli.dir ~docv:"CHECK_DIR"
          ~doc:"The directory of checked traces to render" 0
  ), Term.info "html" ~version ~doc ~man

let clean_cmd =
  let doc = "Clean up the system from previous failed test executions" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,clean) checks the system for remnants of aborted "
        ^"test script executions (e.g. mounted file systems, temporary "
        ^"users, and temporary groups) and then cleans them.");
  ]@Cli.man_trailer in
  Term.(pure clean $ pure ()), Term.info "clean" ~version ~doc ~man

let run_cmd =
  let doc = "Run an arbitrary command in a file system" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b,fs_test) $(b,run) runs a command in the supplied file system.");
  ]@Cli.man_trailer in
  let cmd = Arg.(
    non_empty & pos_right 0 string [] & info []
      ~docv:"COMMAND"
      ~doc:"The command to run in the file system"
  ) in
  let fs = Arg.(
    required & pos 0 (some string) None & info []
      ~docv:"FS"
      ~doc:"The file system to use"
  ) in
  Term.(pure run $ fs $ Cli.params fs_params $ cmd),
  Term.info "run" ~version ~doc ~man

let default_cmd =
  let doc = "execute the fs_test model" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,fs_test) offers POSIX file system specification tools";
  ]@Cli.man_trailer in
  Term.(ret (pure (`Help (`Plain, None)))),
  Term.info "fs_test" ~version ~doc ~man

;;

match Term.eval_choice default_cmd [
  exec_cmd;
  clean_cmd;
  run_cmd;
  merge_cmd;
  check_cmd;
  html_cmd;
] with
| `Error _ -> exit 1
| `Help | `Ok _ | `Version -> exit 0
