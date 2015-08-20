#!/usr/bin/env ocaml

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;

#use "topfind"
#require "unix"

let pkg_dir =
  "posix-fs-spec-opam-repo/packages/posix-fs-spec/posix-fs-spec.0.0.0" 

let add_dev_repo = [
  "mkdir -p " ^ pkg_dir;
  "cp opam " ^ (Filename.concat pkg_dir "opam");
  "opam remote add posix-fs-spec posix-fs-spec-opam-repo";
]

let deps = [
  "posix-fs-spec", add_dev_repo;
]

let exec cmdline =
  Printf.printf "+ %s\n%!" cmdline;
  Unix.system cmdline

let is_success = function
  | Unix.WEXITED 0 -> true
  | _ -> false

let rec run_commands = function
  | [] -> ()
  | cmd::rest ->
    begin match exec cmd with
    | Unix.WEXITED 0 -> run_commands rest
    | Unix.WEXITED x ->
      Printf.printf "Subcommand exited with %d, aborting.\n%!" x
    | _ -> Printf.printf "Subcommand exited abnormally, aborting.\n%!"
    end

let install_flags =
  let argc = Array.length Sys.argv in
  let rec find_install i =
    if i < argc
    then
      let n = i + 1 in
      if Sys.argv.(i) = "--install"
      then if n < argc then Sys.argv.(n) else ""
      else find_install n
    else ""
  in find_install 1
;;

List.iter (fun (pkgname, cmds) ->
  if not (is_success (exec ("opam info " ^ pkgname)))
  then run_commands cmds
  else ()
) deps;

run_commands [
  "opam update";
  "opam pin add posix-fs-spec .";
  "opam update";
  "opam install " ^ install_flags ^ " --deps-only posix-fs-spec";
]
