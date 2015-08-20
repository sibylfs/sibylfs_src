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

#use "topfind";;
#require "unix";;
#require "bigarray";;
#require "str";;
#require "sha";;
#require "num";;

#directory "../../src_ext/lem/ocaml-lib/_build";;
#directory "../../build_spec";;
#directory "../../src_ext/p3";;
#load "p3.cma";;
#load "extract.cma";;
#load "fs_spec_lib.cma";;

#mod_use "../lib/diff.ml";;
#mod_use "../lib/fs_path.ml";;
#mod_use "../lib/dump.ml";;
#mod_use "../lib/trace.ml";;

*)

(* see documentation in ../../doc/md/testing.md *)


open Lem_support
open Fs_interface.Fs_spec_intf
open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
(* open Fs_spec_extras *)

open Fs_interface.Fs_printer_intf


(* FIXME the following is needed because Trace.Label requires an arch
   to construct labels (which are based on os_labels). Ideally the test
   infrastructure should not use os_labels. Using ARCH_POSIX should
   ensure reasonable behaviour. *)
let arch = ARCH_POSIX


(******************************************************************************)
(* Auxiliary functions                                                        *)
(******************************************************************************)

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  List.fold_right (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []

let list_product l1 l2 = allpairs (fun x -> fun y -> (x,y)) l1 l2

(* upto i j = [i..j-1] *)
let rec upto i j = (if i<j then i::(upto (i+1) j) else [])

(* from http://www.cl.cam.ac.uk/~jrh13/atp/OCaml/lib.ml - quick and
   dirty adaptation here *)
let union l1 l2 = l1 @ l2

let rec allsets m l =
  let image = List.map in
  if m = 0 then [[]] else
  match l with
    [] -> []
  | h::t -> union (image (fun g -> h::g) (allsets (m - 1) t)) (allsets m t);;

let rec allsubsets s =
  let image = List.map in
  match s with
    [] -> [[]]
  | (a::t) -> let res = allsubsets t in
              union (image (fun b -> a::b) res) res;;

let duplicates_of_list lst =
  let table = Hashtbl.create (List.length lst) in
  List.iter (fun x ->
    try
      let c = Hashtbl.find table x in
      Hashtbl.replace table x (c+1)
    with Not_found -> Hashtbl.replace table x 1
  ) lst;
  Hashtbl.fold (fun k c l -> if c > 1 then k::l else l) table []

(* 
let _ = allsubsets [1;2;3]
*)


(******************************************************************************)
(* Initialise filesystems                                                     *)
(******************************************************************************)

(* We need a simple way to initialise a filesystem. The datatype
   fs_entry describes a directory, link, hardlink or file. All paths
   should be given as relative paths. All the described entities can
   then be created relative to some given root-directory. *)

type fs_entry =
  | FSE_dir  of (string * fs_entry) list (* a directory *)
  | FSE_file of int
  (* [FSE_file size] represents a file with dummy content of length
     [size] *)
  | FSE_symlink of string
  (* [FSE_symlink content] represents a symbolic link with content
     [content] *)
  | FSE_link of string
  (* [FSE_link target_name] represents a hard link to target [link_target] *)
  | FSE_missing
  (* [FSE_missing] represents a non-existent dir or file which should be
     treated as existing *)
  | FSE_ignore
  (* [FSE_ignore] represents a non-existent dir or file which should be
     ignored by iteration (but is required as a target) *)

type fs_entry_zipper =
| FSEZ_root
(* Base case *)
| FSEZ_dir of (string * (string * fs_entry) list * fs_entry_zipper)
(* Regular directory path descent *)
| FSEZ_parent of (fs_entry_zipper * fs_entry_zipper)
(* Directory ascent *)
| FSEZ_link of (string * fs_entry_zipper * fs_entry_zipper)
(* Path descent through a hard link *)
| FSEZ_symlink of (string * fs_entry_zipper * fs_entry_zipper)
(* Path descent through a symlink *)

type fs_entry_error =
| Bad_file_size
| Duplicate_children of string
| Bad_name of string
| Dangling_link of string

exception Structural_errors of (Fs_path.t * fs_entry_error) list

type test_case = {
  name  : string;
  trace : Trace.line list;
}

let ( / ) = Filename.concat

(* it is often handy to be able to extract the paths of the fs_entries *)
let rec subpaths_of_fs_entry root = function
  | FSE_dir children ->
    root :: List.(flatten (map (fun (name,node) ->
      subpaths_of_fs_entry (root / name) node
    ) children))
  | FSE_file _ | FSE_symlink _ | FSE_link _
  | FSE_missing -> [root]
  | FSE_ignore -> []

let paths_of_fs_entries entries =
  (* Use only the tail to exclude the base directory itself *)
  List.tl (subpaths_of_fs_entry "" (FSE_dir entries))

let fs_parent_zipper zipper =
  let rec traverse = function
    | FSEZ_root -> raise Not_found
    | FSEZ_dir (_, dir, z) -> FSE_dir dir, z
    | FSEZ_parent (z, _)
    | FSEZ_link (_, _, z) -> (* equality *)
       traverse z
    | FSEZ_symlink (_, target_z, _) -> (* reference *)
       traverse target_z
  in
  let dir, z = traverse zipper in
  dir, FSEZ_parent (z, zipper)

(* Find a path in an fs_entry tree and return the entry at that path
   as well as a zipper to that entry to look them up. Identity
   traversals are consumed, inverse/parent traversals are zipped. *)
let rec fs_zipper_of_path root path =
  let rec unzip z fs path = match path, fs with
    | ([], _) -> (fs, z)
    | (_, FSE_symlink lpath) ->
       let path = List.append (Fs_path.of_string lpath) path in
       let target, target_z = fs_zipper_of_path root path in
       let z = FSEZ_symlink (lpath, target_z, z) in
       unzip z target path
    | (_, FSE_link lpath) ->
       let path = List.append (Fs_path.of_string lpath) path in
       let target, target_z = fs_zipper_of_path root path in
       let z = FSEZ_link (lpath, target_z, z) in
       unzip z target path
    | (_ :: _, (FSE_missing | FSE_ignore | FSE_file _)) -> raise Not_found
    | ("" :: [], FSE_dir _) -> (fs, z)
    | ("" :: ((_ :: _) as tl), FSE_dir _) ->
       if z = FSEZ_root
       then raise Not_found (* absolute path *)
       else unzip z fs tl   (* sequence of /'s *)
    | ("."  :: ((_ :: _) as tl), FSE_dir _) -> unzip z fs tl
    | (".." :: path, FSE_dir _) ->
       let node, z = fs_parent_zipper z in
       unzip z node path
    | (path_el :: [], FSE_dir dir) ->
       let node = List.assoc path_el dir in
       (node, FSEZ_dir (path_el, dir, z))
    | (path_el :: path_tl, FSE_dir dir) ->
       let node = try
           List.assoc path_el dir
         with Not_found -> (* FSE_missing | FSE_ignore *)
           List.assoc (Fs_path.to_string path) dir
       in
       unzip (FSEZ_dir (path_el, dir, z)) node path_tl
  in
  unzip FSEZ_root root path

let fs_entry_of_path entry path = fst (fs_zipper_of_path entry path)

let maybe_entry entry path =
  try Some (fs_entry_of_path entry path)
  with Not_found -> None

let exists_entry entry path =
  match maybe_entry entry path with Some _ -> true | None -> false

let is_bad_dir_entry = function
  | ("" | "." | ".."), _ -> true
  | (path_el, (FSE_missing | FSE_ignore)) ->
     (try ignore Fs_path.(resolve (of_string path_el));
          path_el.[0] = '/'
      with Not_found -> true)
  | (path_el, _) -> String.contains path_el '/'

let errors_of_fs entry =
  let rec check_fs_entry errors path_stack = function
    | FSE_dir dir ->
      let names = List.rev_map fst dir in
      let root_path = List.rev path_stack in
      let dupe_list = List.rev_map (fun n ->
        (root_path, Duplicate_children n)
      ) (duplicates_of_list names) in
      List.fold_left (fun errors ((path_el, entry) as dirent) ->
        check_fs_entry
          (if is_bad_dir_entry dirent
           then (root_path, Bad_name path_el)::errors
           else errors
          ) (path_el :: path_stack) entry
      ) (List.rev_append dupe_list errors) dir
    | FSE_file sz ->
       if sz < 0 then (List.rev path_stack, Bad_file_size) :: errors
       else errors
    | FSE_symlink link_path | FSE_link link_path ->
      let path = Fs_path.of_string link_path in
      if exists_entry entry List.(rev_append (tl path_stack) path)
      then errors
      else (List.rev path_stack, Dangling_link link_path) :: errors
    | FSE_missing | FSE_ignore -> errors
  in
  check_fs_entry [] [] entry

let string_of_fs_error = function
  | Bad_name path_el -> "bad name '"^path_el^"'"
  | Bad_file_size -> "bad file size"
  | Duplicate_children n -> "duplicate child '"^n^"'"
  | Dangling_link link_path -> "dangling link '"^link_path^"'"

let lorem_ipsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
let lorem_ipsum_size = String.length lorem_ipsum;;

let rec generate_file_content_of_size size = 
  if size <= 0
  then ""
  else if lorem_ipsum_size < size
  then
    let size = size - lorem_ipsum_size - 1 in
    lorem_ipsum ^ " " ^ (generate_file_content_of_size size)
  else
    String.sub lorem_ipsum 0 size

let create_fs_entries dir =
  let rec generate path_s = Trace.Label.(function
    | dn, FSE_dir []  -> [mkdir (path_s / dn) 0o777]
    | dn, FSE_dir dir -> [
      mkdir (path_s / dn) 0o777;
    ]@(List.fold_right
         (fun dirent cmds -> (generate (path_s / dn) dirent)@cmds))
        dir []
    | fn, FSE_file size ->
       if size = 0 then
         [open_close arch (path_s / fn) ~perm:0o666 [O_WRONLY;O_CREAT]]
       else
         (* We assume that open returns FD 3, this means that no other
         FDs are opened at this point in the trace *)
         let content = generate_file_content_of_size size in
         [open_ arch (path_s / fn) ~perm:0o666 [O_WRONLY;O_CREAT];
          det_write ~fd:3 content size;
          close ~fd:3;
         ]
    | fn, FSE_symlink content -> [symlink content (path_s / fn)]
    | fn, FSE_link targ       -> [link    targ    (path_s / fn)]
    | _, (FSE_ignore | FSE_missing) -> []
  ) in
  List.fold_right (fun dirent cmds -> (generate "" dirent)@cmds) dir []

(******************************************************************************)
(* Testing against a file system                                              *)
(******************************************************************************)

let dir_name_of_test name = "/test_dir_" ^ name

(* TODO: name_of_path should make *any* string into a path segment *)
let name_of_path = function
  | CS_Null -> "NULL"
  | CS_Some p -> String.concat "__" (Fs_path.of_string p)

let name_of_perm (File_perm perm) = Printf.sprintf "%04lo" perm

let name_of_perm_opt = function None -> "none" | Some perm -> name_of_perm perm

let name_of_oflag oflag =
  let open_flags_of_int = arch |> Fs_arch.architecture_of_ty_arch |> (fun x -> x.arch_open_flags_of_int) in
  let oflags = list_from_finset (open_flags_of_int oflag) in
  String.concat "__" List.(sort String.compare (rev_map string_of_flag oflags))

let name_of_lseek_cmd cmd =
  let seek_command_of_int = arch |> Fs_arch.architecture_of_ty_arch |> (fun x -> x.arch_seek_command_of_int) in
  match seek_command_of_int cmd with
  | None -> "SEEK_"^(string_of_int cmd)
  | Some c -> string_of_seek_command c

let hash_of_buffer b = match cstring_of_bytes b with
  | CS_Null -> "NULL"
  | CS_Some s -> Sha1.to_hex (Sha1.string s)

let name_of_cmd = function
  | OS_MKDIR (p,perm) -> "mkdir", [name_of_path p; name_of_perm perm]
  | OS_RMDIR p -> "rmdir", [name_of_path p]
  | OS_CLOSE (FD fd) -> "close", [string_of_int fd]
  | OS_LINK (p, p') -> "link", [name_of_path p; name_of_path p']
  | OS_OPEN (p, oflag, perm) -> "open", [
    name_of_path p; name_of_oflag oflag; name_of_perm_opt perm;
  ]
  | OS_EXTENDED_CMD (OS_OPEN_CLOSE (p, oflag, perm)) -> "open_close", [
    name_of_path p; name_of_oflag oflag; name_of_perm_opt perm;
  ]
  | OS_PREAD (FD fd, sz, off) -> "pread", [
    string_of_int fd; string_of_int sz; string_of_int off;
  ]
  | OS_EXTENDED_CMD (OS_DET_PREAD (FD fd, sz, off)) -> "det_pread", [
    string_of_int fd; string_of_int sz; string_of_int off;
  ]
  | OS_READ (FD fd, sz) -> "read", [string_of_int fd; string_of_int sz]
  | OS_EXTENDED_CMD (OS_DET_READ (FD fd, sz)) -> "read", [
    string_of_int fd; string_of_int sz;
  ]
  | OS_READDIR (DH dh) -> "readdir", [string_of_int dh]
  | OS_OPENDIR p -> "opendir", [name_of_path p]
  | OS_REWINDDIR (DH dh) -> "rewinddir", [string_of_int dh]
  | OS_CLOSEDIR (DH dh) -> "closedir", [string_of_int dh]
  | OS_READLINK p -> "readlink", [name_of_path p]
  | OS_RENAME (p, p') -> "rename", [name_of_path p; name_of_path p']
  | OS_STAT p ->  "stat" , [name_of_path p]
  | OS_LSTAT p -> "lstat", [name_of_path p]
  | OS_SYMLINK (p, p') -> "symlink", [name_of_path p; name_of_path p']
  | OS_TRUNCATE (p, sz) -> "truncate", [name_of_path p; string_of_int sz]
  | OS_UNLINK p -> "unlink", [name_of_path p]
  | OS_PWRITE (FD fd, s, sz, off) -> "pwrite", [
    string_of_int fd; hash_of_buffer s; string_of_int sz; string_of_int off;
  ]
  | OS_EXTENDED_CMD (OS_DET_PWRITE (FD fd, s, sz, off)) -> "det_pwrite", [
    string_of_int fd; hash_of_buffer s; string_of_int sz; string_of_int off;
  ]
  | OS_WRITE (FD fd, s, sz) -> "write", [
    string_of_int fd; hash_of_buffer s; string_of_int sz;
  ]
  | OS_EXTENDED_CMD (OS_DET_WRITE (FD fd, s, sz)) -> "det_write", [
    string_of_int fd; hash_of_buffer s; string_of_int sz;
  ]
  | OS_UMASK perm -> "umask", [name_of_perm perm]
  | OS_CHMOD (p, perm) -> "chmod", [name_of_path p; name_of_perm perm]
  | OS_CHOWN (p, User_id uid, Group_id gid) -> "chown", [
    name_of_path p; string_of_int uid; string_of_int gid;
  ]
  | OS_CHDIR p -> "chdir", [name_of_path p]
  | OS_LSEEK (FD fd, off, cmd) -> "lseek", [
    string_of_int fd; string_of_int off; name_of_lseek_cmd cmd;
  ]
  | OS_EXTENDED_CMD (OS_ADD_USER_TO_GROUP (User_id uid, Group_id gid)) ->
    "add_user_to_group", [string_of_int uid; string_of_int gid]

let test_name_of_trace_lines suite lines =
  let rec concat_name prefix = Trace.(function
    | [] -> prefix
    | (Newline | Comment _) :: more -> concat_name prefix more
    | (Dump_result _) :: _ ->
      failwith "test_name_of_trace_lines: literal dumps not supported"
    | (Dump p) :: more ->
      concat_name (prefix ^ "___" ^ p) more
    | (Label (true,_)) :: _ ->
      failwith "test_name_of_trace_lines: labels with pids not supported"
    | (Label (false, (OS_no_error_return _ | OS_multiple_return _))) :: _ ->
      failwith "test_name_of_trace_lines: non-simple labels not supported"
    | (Label (false, OS_simple_label (
      OS_RETURN _ | OS_CREATE _ | OS_DESTROY _ | OS_TAU))
    ) :: _ -> failwith "test_name_of_trace_lines: non-calls not supported"
    | (Label (false, OS_simple_label (OS_CALL (_, cmd)))) :: more ->
      append prefix (name_of_cmd cmd) more
  ) and append prefix (call, segs) =
    concat_name
      (Printf.sprintf "%s___%s_%s" prefix call (String.concat "___" segs))
  in
  concat_name suite lines

(* When a new test is started, we want to create a subdir for it and
   initialise it.  Also add some header *)
let create_test_header name =
  Trace.(Label.([
    Comment  "@type script";
    Comment  "####################################";
    Comment (" Test " ^ name);
    Comment  "####################################";
  ]))

(* [create_fs_tests fs tests] runs with initial
   filesystem [fs] all the tests in [tests] *)
let create_fs_tests suite fs tests =
  let init_fs = create_fs_entries fs in
  let create_single_test t =
    let name = test_name_of_trace_lines suite t in
    { name;
      trace = create_test_header name @
      init_fs @
      t @ Trace.([
        Label.dump "/";
        Newline;
        Newline;
      ]);
  } in
  List.map create_single_test tests

(* often we want to run tests against all paths
   of a fs. This is done by the following function.
   [gen_test] thereby generates a list of tests from
   a list of paths. *)
let create_fs_exhaustive_tests_aux suite fs gen_tests =
  (* Check some invariants of the filesystem structure *)
  match errors_of_fs (FSE_dir fs) with
  | [] ->
     (* build the list of our test-paths *)
     let test_paths =
       let paths = paths_of_fs_entries fs in
       let paths_sl = List.map (fun p -> (p ^ "/")) paths in
       List.sort String.compare (paths @ paths_sl)
     in

     (* use these paths to generate tests against *)
     let tests = gen_tests test_paths in

     (* create the tests *)
     create_fs_tests suite fs tests
  | errors -> raise (Structural_errors errors)

(* for commands that take one argument use this function. *)
let create_fs_exhaustive_tests_one suite fs gen_test =
  create_fs_exhaustive_tests_aux suite fs (List.map gen_test)
    
(* for commands that take two argument use this function. *)
let create_fs_exhaustive_tests_two suite fs gen_test = (
  let tests test_paths = (
    (* all combinations of our test_paths *)
    let path_combinations = list_product test_paths test_paths in
    let tests = List.map gen_test path_combinations in
    tests
  ) in
  create_fs_exhaustive_tests_aux suite fs tests
)
    

(******************************************************************************)
(* Testing simple commands exhaustively                                       *)
(******************************************************************************)

(* a simple filesystem containing all kinds of entities at least twice *)
let default_test_fs = [
  "empty_dir1", FSE_dir [];
  "empty_dir2", FSE_dir [];

  "nonexist_1",              FSE_missing;
  "nonexist_2",              FSE_missing;
  "nonexist_dir/nonexist_3", FSE_missing;

  "nonempty_dir1", FSE_dir [
    "d2",                FSE_dir [
      "f3.txt",                              FSE_file 83;
      "d3",                                  FSE_dir [];
      "sl_dotdot_f1.txt",                    FSE_symlink "../f1.txt";
      "no_such_target",                      FSE_ignore;
      "sl_no_such_target",                   FSE_symlink "no_such_target";
      "sl_no_such_target/nonexist_6",        FSE_missing;
      "sl_dotdot_no_such_target",            FSE_symlink "../no_such_target";
      "sl_dotdot_no_such_target/nonexist_6", FSE_missing;
      "sl_dotdot_d2",                        FSE_symlink "../d2";
    ];
    "no_such_target",    FSE_ignore;
    "nonexist_4",        FSE_missing;
    "f1.txt",            FSE_file 0;
    "f1.txt/nonexist_5", FSE_missing;
    "sl_f1.txt",         FSE_symlink "f1.txt";
  ];

  "nonempty_dir2", FSE_dir [
    "f1.txt", FSE_file 0;
    "f2.txt", FSE_file 167;
    "d2",     FSE_dir [
      "d3",        FSE_dir [];
      "sl_f3.txt", FSE_symlink "../../nonempty_dir1/d2/f3.txt";
    ];
  ];
]

(* we use the default_test_fs, get all paths out of it and execute link between
   all of them. Not very smart, but exhaustive and simple to write *)

let do_link_tests () = create_fs_exhaustive_tests_two "link"
  default_test_fs
  (fun (p1, p2) -> Trace.([Newline; Label.link p1 p2; Newline]))

let do_rename_tests () = create_fs_exhaustive_tests_two "rename"
  default_test_fs
  (fun (p1, p2) -> Trace.([Newline; Label.rename p1 p2; Newline]))

let do_mkdir_tests () = create_fs_exhaustive_tests_one "mkdir"
  default_test_fs
  (fun p -> Trace.([Newline; Label.mkdir p 0o777; Newline]))

let do_rmdir_tests () = create_fs_exhaustive_tests_one "rmdir"
  default_test_fs
  (fun p -> Trace.([Newline; Label.rmdir p; Newline]))

let do_unlink_tests () = create_fs_exhaustive_tests_one "unlink"
  default_test_fs
  (fun p -> Trace.([Newline; Label.unlink p; Newline]))

let do_stat_tests () = create_fs_exhaustive_tests_one "stat"
  default_test_fs
  (fun p -> Trace.([Newline; Label.stat p; Newline]))

let do_lstat_tests () = create_fs_exhaustive_tests_one "lstat"
  default_test_fs
  (fun p -> Trace.([Newline; Label.lstat p; Newline]))

let do_truncate_test_size n = create_fs_exhaustive_tests_one "truncate"
  default_test_fs
  (fun p -> Trace.([Newline; Label.truncate p n; Newline]))

let do_truncate_tests () = List.flatten
  (List.map do_truncate_test_size [0; 10; 100; 1000; -100])

let trace_initialize_non_root ~pid ~uid ~gid = Trace.(Label.([
  create ~pid ~uid ~gid;
  add_user_to_group ~uid ~gid;
  umask ~pid 0o000;
  Newline;
]))

let trace_initialize_group_user = Trace.(Label.([
  create ~pid:3 ~uid:2 ~gid:2;
  add_user_to_group ~uid:2 ~gid:2;
  add_user_to_group ~uid:2 ~gid:1;
  umask ~pid:3 0o000;
  Newline;
]))

let do_permissions_open_tests
    limit perm_fn role trace_init_users ~pid ~pid' ~uid ~gid =
  let rec next_test tests = function
    | k when k >= limit -> tests
    | k ->
      let perm = perm_fn k in
      let name = Printf.sprintf "perm_%s_open_%03o" role perm in
      let trace = create_test_header name @ [Trace.Newline] @
        trace_init_users @ Trace.(Label.([
          mkdir ~pid "/d" 0o755;
          open_close ~pid ~perm arch "/d/f" [O_CREAT; O_RDWR];
          chown ~pid ~uid ~gid "/d/f";
          Newline;
          open_close ~pid:pid' arch "/d/f" [O_RDONLY];
          open_close ~pid:pid' arch "/d/f" [O_WRONLY];
          open_close ~pid:pid' arch "/d/f" [O_RDWR];
          open_close ~pid:pid' arch "/d/f" [O_RDONLY; O_TRUNC];
          open_close ~pid:pid' arch "/d/f" [O_TRUNC; O_WRONLY];
          open_close ~pid:pid' arch "/d/f" [O_RDONLY; O_APPEND];
          open_close ~pid:pid' arch "/d/f" [O_APPEND; O_WRONLY];
          Newline;
          dump "/";
        ])) in
      next_test ({ name; trace; } :: tests) (k + 1)
  in
  next_test [] 0

let do_permissions_create_tests limit perm_fn role trace_init_user2 =
  let rec next_test tests = function
    | k when k >= limit -> tests
    | k ->
      let perm = perm_fn k in
      let name = Printf.sprintf "perm_%s_create_%03o" role perm in
      let trace = create_test_header name @ [Trace.Newline] @
        trace_initialize_non_root ~pid:2 ~uid:1 ~gid:1 @
        trace_init_user2 @ Trace.(Label.([
          Comment "Initialize environment";
          Newline;
          mkdir ~pid:2 "/d" 0o777;
          chdir ~pid:2 "/d";
          chdir ~pid:3 "/d";
          open_close ~pid:2 ~perm:0o666 arch "f2" [O_RDWR; O_CREAT; O_EXCL];
          open_close ~pid:2 ~perm:0o666 arch "f4" [O_RDWR; O_CREAT; O_EXCL];
          mkdir ~pid:2 "d2" 0o777;
          mkdir ~pid:2 "d4" 0o777;
          chown ~pid:2 ~uid:1 ~gid:1 "/d";
          chmod ~pid:2 "/d" perm;
          Newline;
          Comment "Test";
          Newline;
          mkdir ~pid:3 "d1" 0o777;
          mkdir ~pid:3 "d2" 0o777;
          open_close ~pid:3 ~perm:0o666 arch "f1" [O_RDWR; O_CREAT; O_EXCL];
          open_close ~pid:3 ~perm:0o666 arch "f2" [O_RDWR; O_CREAT; O_EXCL];
          mkdir ~pid:3 "/d/d3" 0o777;
          mkdir ~pid:3 "/d/d4" 0o777;
          open_close ~pid:3 ~perm:0o666 arch "/d/f3" [O_RDWR; O_CREAT; O_EXCL];
          open_close ~pid:3 ~perm:0o666 arch "/d/f4" [O_RDWR; O_CREAT; O_EXCL];
          open_close ~pid:3 arch "/d/f-nonexist" [O_RDONLY];
          Newline;
          dump "/";
        ])) in
      next_test ({ name; trace; } :: tests) (k + 1)
  in
  next_test [] 0

let do_umask_tests ~umasks ~perms =
  List.rev_map (fun uperm ->
    let name = Printf.sprintf "perm_umask_%03o" uperm in
    let trace = create_test_header name @ Trace.(Label.([
      Newline;
      Comment "Initialize environment";
      Newline;
      umask uperm;
      umask uperm;
      Newline;
      Comment "Test";
      Newline;
      symlink "nonexistent" "sl";
      lstat "sl";
    ])) @
      List.fold_left Trace.(Label.(fun lines perm ->
        let suffix = Printf.sprintf "_%03o" perm in
        Newline::
          (open_close ~perm arch ("f"^suffix) [O_RDWR; O_CREAT])::
          (stat ("f"^suffix))::
          (Newline)::
          (mkdir ("d"^suffix) perm)::
          (stat ("d"^suffix))::
          lines
      )) [] perms
    in
    { name; trace; }
  ) umasks

let do_permissions_tests () =
  let uid = 1 in
  let gid = 1 in
   (do_permissions_open_tests
      8 (fun k -> k lsl 6) "user"
      (trace_initialize_non_root ~pid:2 ~uid ~gid)
      ~pid:2 ~pid':2 ~uid ~gid)
  @(do_permissions_open_tests
      64 (fun k -> k lsl 3) "group"
      (trace_initialize_non_root ~pid:2 ~uid ~gid
       @trace_initialize_group_user)
      ~pid:2 ~pid':3 ~uid ~gid)
  @(do_permissions_open_tests
      8 (fun k -> k) "other"
      (trace_initialize_non_root ~pid:2 ~uid ~gid
       @trace_initialize_non_root ~pid:3 ~uid:2 ~gid:2)
      ~pid:2 ~pid':3 ~uid ~gid)
  @(do_permissions_create_tests
      64 (fun k -> k lsl 3) "group" trace_initialize_group_user)
  @(do_permissions_create_tests (* other *)
      8 (fun k -> k) "other" (trace_initialize_non_root ~pid:3 ~uid:2 ~gid:2))
  @(do_umask_tests
      ~umasks:[0o000; 0o011; 0o022; 0o444; 0o770; 0o777]
      ~perms:[0o000; 0o322; 0o444; 0o777])

(******************************************************************************)
(* Testing open                                                               *)
(******************************************************************************)

let open_test_fs = [
  "empty_dir",     FSE_dir [];

  "nonempty_dir",  FSE_dir [
    "f1.txt",           FSE_file 0;
    "f1.txt/nonexist3", FSE_missing;
    "f2.txt",           FSE_file 30;
    "f4.txt",           FSE_ignore;
  ];

  "f3_sl.txt",              FSE_symlink "nonempty_dir/f2.txt";
  "broken",                 FSE_ignore;
  "broken_sl",              FSE_symlink "broken";
  "f4_link.txt",            FSE_link "nonempty_dir/f4.txt";
  "dir_link",               FSE_link "nonempty_dir";

  "nonexist1",              FSE_missing;
  "nonexist_dir/nonexist2", FSE_missing;
  "broken_sl/nonexist4",    FSE_missing;
]
  
let non_access_flags_we_care_about = [
  O_APPEND;
  O_CLOEXEC;
  O_CREAT;
  O_DIRECTORY;
  O_EXCL;
  O_NOFOLLOW;
  O_TRUNC;
]


let open_flags_lists = (
  let access_mode_l = (list_from_finset access_mode_flags) in
  let mll = allsubsets non_access_flags_we_care_about in
  allpairs (fun am ml -> am::ml) access_mode_l mll
)


let create_open_tests_for_path_flags p oflags = (
  let perm = if (List.mem O_CREAT oflags) then Some 0o666 else None in
  let cmd = Trace.Label.open_ arch p ?perm oflags in
  let write = Trace.Label.det_write ~fd:3 "@" 1 in
  let read = Trace.Label.read ~fd:3 1 in
  let clse = Trace.Label.close ~fd:3 in
  Trace.([Newline; cmd; write; read; clse; Newline])
)

let create_open_tests_for_paths pl = 
  List.flatten (List.map (fun p -> List.map (create_open_tests_for_path_flags p) open_flags_lists) pl);;

let do_open_tests () =
  create_fs_exhaustive_tests_aux "open"
    open_test_fs create_open_tests_for_paths

(******************************************************************************)
(* rest *)


let main () =
  let gen_lbl = ref "" in
  let print_version = ref false in
  Arg.(parse
    [("-l", Set_string gen_lbl, " SYSCALL_OR_TOPIC");
     ("--version",Unit (fun _ -> print_version:=true), " print version information and exit")
    ]
    (fun _ -> ())
    "Generate combinatorial tests for a syscall or topic");
  let _ = 
    if !print_version then (
      Printf.printf "testgen version: %s (dirty=%b)\n" Fs_test_version.git_rev Fs_test_version.git_dirty;
      exit 0)
    else
      ()
  in  
  let suite = !gen_lbl in
  let gen_tests = match suite with
    | "link"     -> do_link_tests
    | "mkdir"    -> do_mkdir_tests
    | "open"     -> do_open_tests
    | "rmdir"    -> do_rmdir_tests
    | "rename"   -> do_rename_tests
    | "stat"     -> do_stat_tests
    | "lstat"    -> do_lstat_tests
    | "truncate" -> do_truncate_tests
    | "unlink"   -> do_unlink_tests
    | "permissions" -> do_permissions_tests
    | cmd -> (failwith ("unrecognized syscall or topic: "^cmd))
  in
  try
    let indent = Trace.make_indent 0 in
    let test_cases = gen_tests () in
    try
      Unix.access suite [];
      prerr_endline ("Output directory '"^suite^"' already exists.");
      prerr_endline "Terminating with exit code 1.";
      exit 1
    with Unix.Unix_error (Unix.ENOENT, "access", _) ->
      Unix.mkdir suite 0o700;
      Unix.chdir suite;
      List.iter (fun { name; trace } ->
        let trace_path = name^"-int.trace" in
        let oc = open_out trace_path in
        List.iter (fun line ->
          (* TODO: shouldn't have to use POSIX only *)
          let line_s = Trace.numbered_line_to_string
            ARCH_POSIX ~indent (None,line)
          in
          output_string oc (line_s^"\n")
        ) trace;
        close_out oc
      ) test_cases
  with (Structural_errors errors) ->
    List.iter (fun (path, e) ->
      let path_s = Fs_path.to_string path in
      Printf.eprintf "Structure error: %s (%s)\n" (string_of_fs_error e) path_s
    ) errors;
    prerr_endline "Fatal test filesystem structural errors encountered.";
    prerr_endline "Terminating with exit code 2.";
    exit 2

;;
main ()
