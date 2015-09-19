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

open Sexplib.Std

open Printf
open Diff

module Types = Fs_interface.Fs_spec_intf.Fs_types
module Printer = Fs_interface.Fs_printer_intf

(* basic types                       *)

include Fs_interface.Fs_dump_intf

type dump_error = Unix_error of error | Unknown of string

exception Dump_error of dump_error

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
  d_error      : Types.error diff;
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

(* --------------------------------- *)
(* operations on basic types         *)
(* --------------------------------- *)

let entry_to_csv_record = function
  | DE_file { file_path; file_node; file_size; file_sha } ->
    sprintf "%S|F|%d|%d|%S\n" file_path file_node file_size file_sha
  | DE_dir { dir_path; dir_node } ->
    sprintf "%S|D|%d\n" dir_path dir_node
  | DE_symlink { link_path; link_val } ->
    sprintf "%S|L|%S\n" link_path link_val
  | DE_error (err, call, path) ->
    sprintf "%S|!|%S|%s\n" path call (Printer.string_of_error err)

(* convert to csv data *)
let to_csv_strings des = List.(rev (rev_map entry_to_csv_record des))

let is_d_entry_zero = function
  | DDE_deviant (
      DDE_file { d_file_path = None; d_file_size = None; d_file_sha = None }
    | DDE_dir { d_dir_path = None }
    | DDE_symlink { d_link_path = None; d_link_val = None }
    | DDE_error { d_error = None; d_error_call = None; d_error_path = None }
  )
  | DDE_type_error None -> true
  | DDE_type_error (Some _)
  | DDE_deviant _
  | DDE_missing _
  | DDE_unexpected _ -> false

let entries_of_deviant = function
  | DDE_file { d_files = (f1,f2) } -> DE_file f1, DE_file f2
  | DDE_dir { d_dirs = (d1,d2) } -> DE_dir d1, DE_dir d2
  | DDE_symlink { d_links = (l1,l2) } -> DE_symlink l1, DE_symlink l2
  | DDE_error { d_errors = (e1,e2) } -> DE_error e1, DE_error e2

let deviance e1 e2 = match e1, e2 with
  | (DE_file ({ file_path = p1; file_size = fs1; file_sha = sha1} as f1),
     DE_file ({ file_path = p2; file_size = fs2; file_sha = sha2} as f2)) ->
    DDE_deviant (DDE_file {
      d_files     = (f1,f2);
      d_file_path = diff p1 p2;
      d_file_size = diff fs1 fs2;
      d_file_sha  = diff sha1 sha2;
    })
  | (DE_dir ({ dir_path = p1 } as d1), DE_dir ({ dir_path = p2 } as d2)) ->
    DDE_deviant (DDE_dir { d_dirs = (d1,d2); d_dir_path = diff p1 p2 })
  | (DE_symlink ({ link_path = p1; link_val = c1 } as l1),
     DE_symlink ({ link_path = p2; link_val = c2 } as l2)) ->
    DDE_deviant (DDE_symlink {
      d_links     = (l1,l2);
      d_link_path = diff p1 p2;
      d_link_val  = diff c1 c2;
    })
  | (DE_error ((error, call, path) as e1)),
    (DE_error ((error',call',path') as e2)) ->
    DDE_deviant (DDE_error {
      d_errors     = (e1,e2);
      d_error      = diff error error';
      d_error_call = diff call call';
      d_error_path = diff path path';
    })
  | de1, de2 -> DDE_type_error (Some (de1, de2))

let diff_dump_entries des des_spec =
  (* get the paths / entries that are present in both, or
     in only one *)
  let cmp de1 de2 = String.compare (path de1) (path de2) in
  let only_des, both, only_des_spec = inter_diff cmp des des_spec in

  let errors1 = List.map (fun e -> DDE_unexpected e) only_des in
  let errors2 = List.map (fun e -> DDE_missing e) only_des_spec in
  let errors3 = List.fold_right (fun (e1, e2) lst ->
    let dev = deviance e1 e2 in
    if is_d_entry_zero dev then lst else dev::lst
  ) both [] in

  let errors = errors1 @ errors2 @ errors3 in
  if (List.length errors > 0) then Some (D_t errors) else None

let diff (p1, dump1) (p2, dump2) =
  if p1 <> p2
  then Some (D_path (diff p1 p2))
  else diff_dump_entries dump1 dump2

let string_of_d_entry = function
  | DDE_missing entry    -> sprintf "missing entry: %s\n" (path entry)
  | DDE_unexpected entry -> sprintf "unexpected entry: %s\n" (path entry)
  | DDE_deviant dev ->
    let e1, e2 = entries_of_deviant dev in
    sprintf "differing entry: %s\n" (path e1)
  | DDE_type_error None -> "\n"
  | DDE_type_error (Some (e1, _)) -> sprintf "differing entry: %s\n" (path e1)

let string_of_d_t = function
  | D_path _ -> "comparing dumps of different directories"
  | D_t d_entries -> "\n   comparison of dump-results failed:\n      "
    ^(String.concat "      " (List.map string_of_d_entry d_entries))

(* --------------------------------- *)
(* creating dumps                    *)
(* --------------------------------- *)

(* The module type [dump_fs_operations] abstacts the operations
   needed to greate a dump of a fs *)
module type Dump_fs_operations = sig  
  type state 
  type dir_status
  val ls_path : state -> Fs_path.t -> string list
  val kind_of_path : state -> Fs_path.t -> Unix.file_kind
  val sha1_of_path : state -> Fs_path.t -> (int * string)
  val readlink : state -> Fs_path.t -> string
  val atime_of_path : state -> Fs_path.t -> string
  val mtime_of_path : state -> Fs_path.t -> string
  val ctime_of_path : state -> Fs_path.t -> string
  val inode_of_file_path : state -> Fs_path.t -> int
  val inode_of_dir_path : state -> Fs_path.t -> int
  val enter_dir : state -> Fs_path.t -> dir_status
  val leave_dir : state -> Fs_path.t -> dir_status -> unit
end

module Make(DO : Dump_fs_operations) = struct
  (* [find_of_path state path] returns the files and directories in a dir *)
  let find_of_path s0 p =
    let xs = List.map (fun n -> Fs_path.concat p [n]) (DO.ls_path s0 p) in
    List.partition (fun p -> DO.kind_of_path s0 p <> Unix.S_DIR) xs

  let entry_of_path (s : DO.state) path =
    let path_s = Fs_path.to_string path in
    try
      let k = DO.kind_of_path s path in
      match k with
      | Unix.S_REG ->
        let file_size, file_sha = DO.sha1_of_path s path in
        DE_file {
          file_path = path_s;
          file_node = DO.inode_of_file_path s path;
          file_size;
          file_sha;
          file_atim = DO.atime_of_path s path;
          file_mtim = DO.mtime_of_path s path;
          file_ctim = DO.ctime_of_path s path;
        }
      | Unix.S_DIR -> DE_dir {
          dir_path = path_s;
          dir_node = DO.inode_of_dir_path s path;
          dir_atim = DO.atime_of_path s path;
          dir_mtim = DO.mtime_of_path s path;
          dir_ctim = DO.ctime_of_path s path;
      }
      | Unix.S_LNK -> DE_symlink {
          link_path = path_s;
          link_val = DO.readlink s path;
          link_atim = DO.atime_of_path s path;
          link_mtim = DO.mtime_of_path s path;
          link_ctim = DO.ctime_of_path s path;
      }
      | _ -> failwith ("Dump_fs(DO).entry_of_path unhandled kind: " ^ path_s)
    with
    | (Dump_error (Unknown msg)) as e -> raise e
    | Dump_error (Unix_error e) -> DE_error e
    | e ->
      let exc = Printexc.to_string e in
      let bt  = Printexc.get_backtrace () in
      let msg = Printf.sprintf "unknown error for %s:\n%s\nBacktrace:\n%s"
        path_s exc bt
      in
      raise (Dump_error (Unknown msg))

  let rec entries_of_path (s : DO.state) (path : Fs_path.t) : entry list =
    try
      let path_status = DO.enter_dir s path in
      let fs, sub_dirs = find_of_path s path in

      let des_direct = List.rev_map (entry_of_path s) (path :: fs) in
      let des_sub = List.flatten (List.map (entries_of_path s) sub_dirs) in
      DO.leave_dir s path path_status;

      List.rev_append des_direct des_sub
    with Dump_error (Unix_error e) -> [DE_error e]

  let of_path (s : DO.state) (path_s : string) : t =
    try entries_of_path s (Fs_path.of_string path_s)
    with
    | e ->
      let exc = Printexc.to_string e in
      raise (Dump_error (Unknown ("Error: error while dumping fs-state: "^exc)))
end
