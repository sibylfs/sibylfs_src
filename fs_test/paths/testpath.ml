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

let trace_s ="@type script
mkdir /tmp/ 0
mkdir /tmp/d1 0
mkdir /tmp/d1/empty_dir1 0
mkdir /tmp/d1/empty_dir2 0
mkdir /tmp/d1/nonempty_dir1 0
open_close /tmp/d1/nonempty_dir1/f1.txt [O_CREAT;O_RDWR] 0
open_close /tmp/d1/nonempty_dir1/f2.txt [O_CREAT;O_RDWR] 0
mkdir /tmp/d1/nonempty_dir1/d2 0
open_close /tmp/d1/nonempty_dir1/d2/f3.txt [O_CREAT;O_RDWR] 0
open_close /tmp/d1/nonempty_dir1/d2/f4.txt [O_CREAT;O_RDWR] 0
mkdir /tmp/d1/nonempty_dir1/d2/d3 0
mkdir /tmp/d1/nonempty_dir1/d2/d4 0
mkdir /tmp/d1/nonempty_dir2 0
open_close /tmp/d1/nonempty_dir2/f1.txt [O_CREAT;O_RDWR] 0
open_close /tmp/d1/nonempty_dir2/f2.txt [O_CREAT;O_RDWR] 0
mkdir /tmp/d1/nonempty_dir2/d2 0
open_close /tmp/d1/nonempty_dir2/d2/f3.txt [O_CREAT;O_RDWR] 0
open_close /tmp/d1/nonempty_dir2/d2/f4.txt [O_CREAT;O_RDWR] 0
mkdir /tmp/d1/nonempty_dir2/d2/d3 0
mkdir /tmp/d1/nonempty_dir2/d2/d4 0
mkdir /tmp/d2 0
open_close /tmp/d2/f5.txt [O_CREAT;O_RDWR] 0
symlink /tmp /link_to_tmp
symlink \"../d2\" /tmp/d1/rel_link_to_d2
symlink \"f5.txt\" /tmp/d2/link_to_f5.txt
dump
"

open Fs_prelude
open Fs_interface.Fs_spec_intf
open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
open Fs_interface.Fs_spec_intf.Resolve
open Fs_dict_wrappers
open Checklib_shared_types
open CheckLib
open Fs_interface.Dir_heap_intf.Dir_heap_types

(* some auxiliary definitions *)

type ('a, 'b) expected_result =
  | ER_dir of 'a
  | ER_file of 'b
  | ER_error of error
  | ER_none
 
let dir_ref_to_string (Dir_ref (Inode n)) = string_of_int n
let file_ref_to_string (File_ref (Inode n)) = string_of_int n

let rn_to_string = function
    (RN_dir(ref,_)) -> "ER_dir " ^ (dir_ref_to_string ref)
  | (RN_file(_, _, ref,_)) -> "ER_file " ^ (file_ref_to_string ref)
  | (RN_error(e,_)) -> "ER_error " ^ (Fs_interface.Fs_printer_intf.string_of_error e)
  | (RN_none _) -> "ER_none"

let check_result rn expected_result = match (rn, expected_result) with
    (RN_dir(ref,_), ER_dir ref') -> (ref = ref')
  | (RN_file(_, _, ref,_), ER_file ref') -> (ref = ref')
  | (RN_error(e,_), ER_error e') -> (e = e')
  | (RN_none _, ER_none) -> true
  | _ -> (print_endline (rn_to_string rn);false)


(* simulate default architecture as root. *)
(* FIXME the following is needed because Trace requires an arch
   to construct labels (which are based on os_labels). Ideally the test
   infrastructure should not use os_labels. Using POSIX_ARCH should
   ensure reasonable behaviour. *)
let arch = ARCH_POSIX
let no_root = false;;

(* initialise environment *)
(*
let env = CheckLib.spec_os_ops_env arch no_root;;
module S = CheckLib.Spec_os_ops (* (struct let arch = arch;; let no_root = no_root end) *);;
*)
module C = CheckLib.Check_spec


(* parse trace and interpret it *)
let (_, tr) = Trace.(of_string arch trace_s)
let interp = C.process_script (spec_initial_state arch no_root) tr
let (ok, _) = C.print_interp arch default_check_output_fun interp

(* get a single result state to run tests against *)
let s0 = (match interp with
  | Interp (ss, _) -> finset_choose ss
  | Interp_state_fail _ | Interp_label_fail _ -> failwith "interp error"
)

(* now we are in a position to check path resolution *)

let dummy_command = (OS_READLINK (CS_Null))
let pp_from_root p = process_path_from_root (s0 |> os_state_to_env) (s0 |> os_state_to_fs_state) dummy_command (CS_Some p)
let pp dr p = process_path (s0 |> os_state_to_env) (s0 |> os_state_to_fs_state) dr dummy_command (CS_Some p)

let rn_dir_only = function
  | RN_dir (rf,_) -> rf
  | _ -> failwith "expected only RN_dir"
let root_ref   = rn_dir_only (pp_from_root "/")
let tmp_ref    = rn_dir_only (pp_from_root "/tmp")
let tmp_d1_ref = rn_dir_only (pp_from_root "/tmp/d1")
let tmp_d2_ref = rn_dir_only (pp_from_root "/tmp/d2")


let f5_ref = File_ref (Inode 22);;
let _ = pp root_ref "/link_to_tmp/";;
let ns2 x = ty_name_list_to_list x


let _ = [
  assert((split_path_string "") |> ns2 = [""]);
  assert((split_path_string "tom") |> ns2 = ["tom"]);
  assert((split_path_string "tom/jen") |> ns2 = ["tom";"jen"]);
  assert((split_path_string "/tom/jen") |> ns2 = ["";"tom";"jen"]);
  assert((split_path_string "/tom/jen/") |> ns2 = ["";"tom";"jen";""]);
  assert((split_path_string "/") |> ns2 = ["";""]);
  assert((split_path_string "//") |> ns2 = ["";"";""]);
  assert((split_path_string "//tom") |> ns2 = ["";"";"tom"]);
  assert((split_path_string "///") |> ns2 = ["";"";"";""]);
  assert((split_path_string "///tom") |> ns2 = ["";"";"";"tom"]);
  assert((path_starts_with_exactly_two_slashes (CS_Some "/")) = false);
  assert((path_starts_with_exactly_two_slashes (CS_Some "/tom")) = false);
  assert((path_starts_with_exactly_two_slashes (CS_Some "//")) = true);
  assert((path_starts_with_exactly_two_slashes (CS_Some "//tom")) = true); 
  assert((path_starts_with_exactly_two_slashes (CS_Some "///")) = false);
  assert((path_starts_with_exactly_two_slashes (CS_Some "///tom")) = false); 
  assert((path_starts_with_exactly_two_slashes (CS_Some "////")) = false);
  assert((path_starts_with_exactly_two_slashes (CS_Some "////tom")) = false);
  assert(root_ref = Dir_ref (Inode 0));
  assert(tmp_ref = Dir_ref (Inode 1));
  assert(tmp_d1_ref = Dir_ref (Inode 2));
  assert(check_result (pp tmp_d1_ref "..") (ER_dir tmp_ref));
  assert(check_result (pp tmp_d1_ref "..") (ER_dir tmp_ref));
  assert(check_result (pp tmp_d1_ref "../..") (ER_dir root_ref));
  assert(check_result (pp tmp_d1_ref "../../..") (ER_dir root_ref));
  assert(check_result (pp tmp_d1_ref "../../../tmp") (ER_dir tmp_ref));
  assert(check_result (pp tmp_d1_ref "..////") (ER_dir tmp_ref));
  assert(check_result (pp tmp_d1_ref "///") (ER_dir root_ref));
  assert(check_result (pp tmp_d1_ref "nonexistdir/..") (ER_error ENOENT));
(*
  assert(check_result (pp tmp_d1_ref "nonexistdir///") (ER_error ENOENT));
*)
  assert(check_result (pp tmp_d1_ref "nonexistdir///") ER_none);
  assert(check_result (pp tmp_d1_ref "") (ER_error ENOENT));
  assert(check_result (pp root_ref "/link_to_tmp/") (ER_dir tmp_ref));
  assert(check_result (pp root_ref "/tmp/d1/rel_link_to_d2/") (ER_dir tmp_d2_ref));
  assert(check_result (pp root_ref "/tmp/d1/rel_link_to_d2/f5.txt") (ER_file f5_ref));
  (* FIXME for following we need inode_ref of link_to_f5.txt, and we also need to be clear when we follow a link 
  assert(dest_fname2(pp root_ref "/tmp/d2/link_to_f5.txt") = f5_ref);
  assert(dest_fname2(pp root_ref "/tmp/d2/link_to_f5.txt/") = f5_ref);
  *)
  (* FIXME etc *)
]

let _ = print_endline "All tests pass."

(*
Local Variables:
mode: tuareg
End:
*)


