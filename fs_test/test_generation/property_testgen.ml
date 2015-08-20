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

(* a version of exhaustive testing based on properties *)

(*
todo:

- X add hard links
- X replace sample fs with fs generated from spec via posix arch
- X check semantics of single paths with posix name resolution
- X test current code
  - generate all paths
  - find first path that satisfies each property
  - determine which properties are mutually exclusive
- X semantics for all properties
- X semantics for properties of pairs of paths
- X create spec fs from s0
- X resolve a path using spec resolve functions
- X add paths that don't resolve to anything
- X check that all combs of single path properties have some path that satisfies them
- X check that all combinations of properties have some path that satisfies them
- check that testgen actually tests all paths, with the same state (or start using this to generate tests)
- need to isolate each test in a subdir, whilst still ensuring that all the properties are satsified; this might be difficult since to isolate tests we typically make a new subdirectory "test_id" and the test commands execute within this directory; however, now we require that we test paths such as "/", and this references something outside the "test_id" directory, and is therefore likely to interfere with the results of other tests; the alternative is to run each test in a clean filesystem
- FIXMEs in this file
*)



let rec find_first_rest p xs = (match xs with 
    | [] -> failwith "find_first_rest: []"
    | x::xs -> if p x then (x,xs) else find_first_rest p xs)


let find_first_n p xs = 
  let rec f1 (xs,n) = (match xs with 
      | [] -> failwith "find_first_n: []"
      | x::xs -> if p x then (x,n) else f1 (xs,n+1))
  in
  f1 (xs,0)


let rec drop n h = (match (n,h) with
    | (0,_) -> h
    | (_,[]) -> []
    | (_,x::xs) -> drop (n-1) xs)

let list_subset ps qs = (
  List.for_all (fun p -> List.mem p qs) ps)


(**********************************************************************)
(* file system initialization; link to spec *)

module DH = Fs_interface.Dir_heap_intf

type path = string list  (* pth = [..] then the corresponding string is String.concat "/" pth; the pth cannot be empty (the empty string "" corresponds to the path [""]); components cannot contain '/' *)

let string_of_path (p:path) = String.concat "/" p

let arch0 = Fs_interface.Fs_spec_intf.Fs_types.ARCH_POSIX

let script0 = "@type script
# initial state
mkdir \"empty_dir1\" 0o777
mkdir \"empty_dir2\" 0o777
mkdir \"nonempty_dir1\" 0o777

# nonempty_dir1
mkdir \"nonempty_dir1/d2\" 0o777
open \"nonempty_dir1/d2/f3.txt\" [O_CREAT;O_WRONLY] 0o666
write! (FD 3) \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor inc\" 83
close (FD 3)
mkdir \"nonempty_dir1/d2/d3\" 0o777
symlink \"../f1.txt\" \"nonempty_dir1/d2/sl_dotdot_f1.txt\"
symlink \"no_such_target\" \"nonempty_dir1/d2/sl_no_such_target\"
symlink \"../no_such_target\" \"nonempty_dir1/d2/sl_dotdot_no_such_target\"
symlink \"../d2\" \"nonempty_dir1/d2/sl_dotdot_d2\"
open_close \"nonempty_dir1/f1.txt\" [O_CREAT;O_WRONLY] 0o666
symlink \"f1.txt\" \"nonempty_dir1/sl_f1.txt\"
symlink \"../empty_dir1\" \"nonempty_dir1/sl_dotdot_empty_dir1\"
symlink \"../empty_dir2\" \"nonempty_dir1/sl_dotdot_empty_dir2\"
symlink \"../nonempty_dir1\" \"nonempty_dir1/sl_dotdot_nonempty_dir1\"
symlink \"../nonempty_dir2\" \"nonempty_dir1/sl_dotdot_nonempty_dir2\"
symlink \"/nonempty_dir1\" \"/sl_nonempty_dir1\"
link \"nonempty_dir1/f1.txt\" \"nonempty_dir1/link_f1.txt\"
symlink \"link_f1.txt\" \"nonempty_dir1/sl_link_f1.txt\"
link \"nonempty_dir1/sl_link_f1.txt\" \"nonempty_dir1/link_to_symlink\"

# nonempty_dir2
mkdir \"nonempty_dir2\" 0o777
open_close \"nonempty_dir2/f1.txt\" [O_CREAT;O_WRONLY] 0o666
open \"nonempty_dir2/f2.txt\" [O_CREAT;O_WRONLY] 0o666
write! (FD 3) \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exer\" 167
close (FD 3)
mkdir \"nonempty_dir2/d2\" 0o777
mkdir \"nonempty_dir2/d2/d3\" 0o777
symlink \"../../nonempty_dir1/d2/f3.txt\" \"nonempty_dir2/d2/sl_f3.txt\"

open \"/f4.txt\" [O_CREAT;O_WRONLY] 0o666
write! (FD 3) \"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor inc\" 83
close (FD 3)
"



let t0 = Trace.of_string arch0 script0

(* let outfun = CheckLib.default_check_output_fun (* FIXME may want this silent *) *)

let d0 = CheckLib.spec_initial_state arch0 true (* no_root *)

let r0 = CheckLib.Check_spec.process_script d0 (snd t0)

let s0 = match r0 with | CheckLib.Interp (s0,_) -> s0 | _ -> failwith "impossible"

let s0 = match Fs_prelude.distinct_list_from_finset_by (fun x y -> false) s0  with
  | [(s0:CheckLib.Check_spec.m_os_state)]  -> s0
  | _ -> failwith "impossible"

(* potentially dangerous *)
let s0 : Dir_heap.Dir_heap_types.dh_os_state = Obj.magic (s0)

let dhops = Dir_heap.Dir_heap_ops.dir_heap_ops
let dh0 : Dir_heap.Dir_heap_types.dir_heap_state = s0.Fs_spec.Fs_types.oss_fs_state
let env0 = Dir_heap.Dir_heap_ops.dir_heap_env

open Fs_spec.Fs_types

(* take a state and return the set of paths, as a list of lists 

   - maintain a path to current directory /tmp/a/b
   - in directory b 
   - return current path
   - for dirs, recurse on all directory entries (with updated cwd)
   - for files, just add /tmp/a/b/f to the list of paths to be returned

*)
let get_paths : ('a, 'b, 'c) Fs_spec.Fs_types.fs_ops -> 'c -> Fs_spec.Fs_types.name list list 
  = (fun fops s0 ->
      let root = match fops.fops_get_root s0 with | Some root -> root | _ -> failwith "impossible" in
      (* sofar is a list of strings indicating the directories travelled so far; d0 is the current directory *)
      let rec f1 sofar d0 = (
        let (s1_ignore,es) = fops.fops_readdir s0 d0 in
        let f2 name = (
          match fops.fops_resolve s0 d0 name with
          | None -> (failwith "get_paths: impossible")
          | Some x -> (
              match x with
              | Dir_ref_entry d1 -> (f1 (sofar@[name]) d1)
              | File_ref_entry f1 -> [sofar@[name]]))
        in
        sofar::(List.concat (List.map f2 es)))
      in
      f1 [""] root)  (* "" ensures we get a slash at front of path; this also means we get an "empty" path "" to check; do we want this? yes, it seems reasonable and is allowed by the model *)

let paths0 = (
  let ps = get_paths dhops dh0 in
  (* get relative paths - we add leading / later *)
  let ps =   
    let f1 = fun x -> match x with (""::x) -> x | _ -> failwith "impossible" in
    ps |> List.map f1 |> List.filter (fun x -> x <> [])
  in
  let ps = [[""];["";""]]@ps in (* don't forget root and empty string *)
  let other_paths = [ 
    (* must have nonexist paths, and some must end in / *)
    ["nonexist_1"]; ["";"nonexist_1";"nonexist_11"]; ["";"nonexist_2"]; ["";"nonexist_2";"nonexist_22"];
    (* we also need nonexistent path in an empty dir *)
    ["empty_dir1";"nonexist_3"];
    (* and error through an empty dir *)
    ["empty_dir1";"nonexist_3";"nonexist_4"];
    ["empty_dir1";"nonexist_3";"nonexist_5"]; (* ux7 *)
    (* also want paths via a symlink *)
    ["nonempty_dir1";"d2";"sl_dotdot_d2";"..";".."];
    ["";"nonempty_dir1";"d2";"sl_dotdot_d2";"..";".."];
    ["";"";"nonempty_dir1";"d2";"sl_dotdot_d2";"..";".."];
    ["";"";"";"nonempty_dir1";"d2";"sl_dotdot_d2";"..";".."];
    (* and relative paths to nonexist, not via a symlink *)
    ["nonempty_dir1";"nonexist_1"];
    ["nonempty_dir1";"nonexist_1";""];
    (* and .. paths *)
    ["nonempty_dir1";".."];
    (* nonexist via symlink *)
    ["nonempty_dir1";"sl_dotdot_empty_dir1";"nonexist_1"];
    ["nonempty_dir1";"sl_dotdot_nonempty_dir2";"nonexist_1"]
  ]
  in  
  (* add trailing slash to all paths; use in List.concat (List.map) *)
  let add_trailing_slash p = [p;p@[""]] in
  (* add initial slashes *)
  let add_initial_slashes p = [
    p;
    ""::p;
    ""::""::p;
    ""::""::""::p
  ]
  in
  let add_via_symlink p = [
    p;
    ["nonempty_dir1";"sl_dotdot_nonempty_dir1";".."]@p;
    ["";"nonempty_dir1";"sl_dotdot_nonempty_dir1";".."]@p;
    ["nonempty_dir1";"sl_dotdot_nonempty_dir2";".."]@p;
    ["";"nonempty_dir1";"sl_dotdot_nonempty_dir2";".."]@p
  ]
  in
  (ps@other_paths) 
  |> List.map add_via_symlink |> List.concat
  |> List.map add_trailing_slash |> List.concat 
  |> List.map add_initial_slashes |> List.concat
)


type dh_res_name = (Dir_heap.Dir_heap_types.dh_dir_ref, Dir_heap.Dir_heap_types.dh_file_ref)
    Fs_spec.Fs_types.res_name

(* resolve a path; we don't want to follow symlinks ever *)
let resolve : string -> dh_res_name = fun path ->
  let path = CS_Some path in 
  let dummy_cmd = OS_READLINK path in
  Fs_spec.Resolve.process_path_from_root env0 dh0 dummy_cmd path 

let memo f = (
  let tbl = Hashtbl.create 100 in
  let key_of_input i = Some i in
  fun i -> 
    let k = key_of_input i in
    match k with 
    | None -> (f i)
    | Some k -> (
        if (Hashtbl.mem tbl k) then 
          (Hashtbl.find tbl k) 
        else
          let v = f i in
          let _ = Hashtbl.add tbl k v in
          v))

let resolve = memo resolve


(**********************************************************************)
(* properties *)

type single_path_props =
  | Ends_in_slash
  | Starts_with_no_slash | Starts_with_one_slash | Starts_with_slash_x2 | Starts_with_slash_x3
  | Is_empty_string
  | Is_slash
  | Is_file | Is_dir | Is_symlink | Is_nonexist | Is_error
  | If_dir_then_is_empty | If_dir_then_is_nonempty  
  (* FIXME satisifed if not dir - but this is not really what we want;
     except that we also have combination with Is_dir *)
  | Has_symlink_component
  | Not of single_path_props

(* a list of (representatives of) equivalence classes; an equivalence
   class is represented as a list of properties which are mutually
   disjoint and cover the space *)
let single_path_props = [
  [Ends_in_slash;Not Ends_in_slash];
  [Starts_with_no_slash; Starts_with_one_slash; Starts_with_slash_x2; Starts_with_slash_x3];
  [Is_empty_string; Not Is_empty_string];
  [Is_slash;Not Is_slash];
  [Is_file;Is_dir;Is_symlink;Is_nonexist;Is_error];
  [Not Is_dir; If_dir_then_is_empty;If_dir_then_is_nonempty];
  [Has_symlink_component;Not Has_symlink_component]
]

let drop_last xs = xs |> List.rev |> List.tl |> List.rev

(* all prefixes, including empty and full *)
let prefixes_of_path : path -> path list = (fun p -> 
    let rec f1 acc xs = (
      match xs with
      | [] -> acc (* we don't add [] since it isn't a valid path *)
      | _ -> (f1 (xs::acc) (drop_last xs)))
    in
    f1 [] p)

(*
let _ = prefixes_of_path ["";"a";"b"]
*)

let rec allpairs f l1 l2 =
  match l1 with
    h1::t1 ->  List.fold_right (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
  | [] -> []

let list_product l1 l2 = allpairs (fun x -> fun y -> (x,y)) l1 l2

let list_product_as_list : 'a list -> 'a list -> 'a list list = (fun l1 l2 -> allpairs (fun x -> fun y -> [x;y]) l1 l2)

let _ = list_product_as_list [1;2;3] [4;5;6]

let list_product_as_list : 'a list list -> 'a list -> 'a list list = (fun l1 l2 -> allpairs (fun x -> fun y -> x@[y]) l1 l2)

let _ = list_product_as_list [[1;2;3];[7]] [4;5;6]

let property_combs0 = 
  let f1 acc x = list_product_as_list acc x in
  List.fold_left f1 (List.map (fun x -> [x]) (List.hd single_path_props)) (List.tl single_path_props)

let _ = List.length property_combs0

(* semantics of properties *)

let is_symlink i0_ref = (
  (dhops.fops_stat_file dh0 i0_ref).st_kind = S_IFLNK)

let rec sem1 prop p0 = (
  match p0 with 
  | None -> false (* check for null path *)
  | Some p -> (
      match prop with
      | Ends_in_slash -> (p |> List.rev |> (fun x -> match x with [""] -> false | ""::_ -> true | _ -> false))
      | Starts_with_no_slash -> (
          match p with
          | [""] -> true
          | c::_ -> c <> ""
          | _ -> failwith "impossible")
      | Starts_with_one_slash -> (
          match p with
          | ["";""] -> true
          | ""::x::_ -> (x <> "")  (* exactly one slash *)
          | _ -> false)
      | Starts_with_slash_x2 -> (
          match p with
          | ""::""::x::_ -> (x <> "")  
          | _ -> false)
      | Starts_with_slash_x3 -> (
          match p with
          | ""::""::""::x::_ -> (x <> "") 
          | _ -> false)
      | Is_empty_string -> (p = [""]) (* path was "" *)
      | Is_slash -> (p = ["";""])
      | Is_file -> (p |> string_of_path |> resolve |> is_RN_file)
      | Is_dir -> (p |> string_of_path |> resolve |> is_RN_dir)
      | Is_symlink -> (
          p |> string_of_path |> resolve |> 
          (fun x -> match x with
             | RN_file (_,_,i0_ref,_) -> (is_symlink i0_ref)
             | _ -> false))
      | Is_nonexist -> (p |> string_of_path |> resolve |> is_RN_none)
      | Is_error -> (p |> string_of_path |> resolve |> is_RN_error)
      | If_dir_then_is_empty -> (
          p |> string_of_path |> resolve |> 
          (fun x -> match x with 
             | RN_dir(d0_ref,_) -> (
                 dhops.fops_readdir dh0 d0_ref |> (fun (_,ns) -> ns=[]))
             | _ -> true))
      | If_dir_then_is_nonempty -> (
          p |> string_of_path |> resolve |> 
          (fun x -> match x with 
             | RN_dir(d0_ref,_) -> (
                 dhops.fops_readdir dh0 d0_ref |> (fun (_,ns) -> ns<>[]))
             | _ -> true))
      | Has_symlink_component -> (
          (* take a path and form prefixes *)
          let is_symlink p = (
            p |> string_of_path |> resolve |> 
            (fun x -> match x with
               | RN_file (_,_,i0_ref,_) -> (is_symlink i0_ref)
               | _ -> false))
          in
          p |> prefixes_of_path |> List.exists is_symlink)
      | Not x -> not(sem1 x p0)))

let sem1_list props p = List.for_all (fun prop -> sem1 prop p) props

let implies p q = (not p) || q

(* properties that logically can occur together; we remove impossible
   properties from the combinations of all properties, justifying the
   impossibility of each *)
let logically_possible : single_path_props list -> bool = fun props -> 
  let sub_props ps = List.for_all (fun p -> List.mem p props) ps in
  (not (List.mem Ends_in_slash props && List.mem Is_empty_string props))
  && (not (List.mem Is_slash props && List.mem Starts_with_no_slash props))
  && (not (sub_props [Ends_in_slash;Is_file]))
  && (not (sub_props [Ends_in_slash;Is_symlink]))
  && (not (sub_props [Is_slash;Has_symlink_component]))
  && (not (sub_props [Is_slash;If_dir_then_is_empty])) (* FIXME interesting case - we need to check a path which is / where there are no entries in the root directory *)
  && (not (sub_props [Is_empty_string;Is_file]))
  && (not (sub_props [Is_empty_string;Is_dir]))
  && (not (sub_props [Is_empty_string;Is_symlink]))
  && (not (List.mem Is_slash props && List.exists (fun x -> List.mem x [Is_file;Is_symlink;Is_nonexist;Is_error]) props))
  && (not (List.mem Is_slash props && not (List.mem Starts_with_one_slash props)))
  && (implies (List.mem Is_empty_string props) (List.mem Is_error props))
  && (implies (List.mem Is_empty_string props) (not (List.mem Has_symlink_component props)))
  && (implies (List.mem Is_symlink props) (not (List.mem (Not Has_symlink_component) props)))
  && (not (sub_props [Is_empty_string; Starts_with_one_slash]))
  && (not (sub_props [Is_slash;Not Ends_in_slash]))
  && (implies (List.mem Is_empty_string props) (List.mem Starts_with_no_slash props))
  && (not (sub_props [Is_dir; Not Is_dir])) (* FIXME possible? *)

let property_combs1 = property_combs0 |> List.filter logically_possible

let _ = List.length property_combs1

let list_find_opt p xs = try Some(List.find p xs) with _ -> None

let path_with_property : single_path_props -> path option = (fun prop -> 
    paths0 |> (List.map (fun p -> Some p)) |> list_find_opt (sem1 prop)
    |> (fun x -> match x with | None -> None | Some(Some p) -> Some p | _ -> failwith "impossible"))

let _ = path_with_property (Starts_with_one_slash)

(* FIXME shouldn't this be path opt list? - a path is either none or some *)
let paths_with_properties : single_path_props list -> path list = (fun props -> 
    paths0 |> List.filter (fun p -> List.for_all (fun prop -> sem1 prop (Some p)) props))



(**********************************************************************)
(* checking single path properties on selection of paths *)


(*

let _ = paths_with_properties [Starts_with_one_slash;Is_file;Not Is_symlink]

(* first interesting missing case - no symlink to emptydir *)
let _ = paths_with_properties [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash; Is_dir;
    If_dir_then_is_empty; Has_symlink_component] 

(* another interesting case - we need nonexistent paths that are not just via symlink *)
let _ = paths_with_properties [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
    Is_nonexist; If_dir_then_is_empty; Not Has_symlink_component]

(* another interesting case - we need to check single paths / where the root directory is empty; FIXME marking this as impossible for now so we can continue to explore *)
let _ = paths_with_properties [Is_slash; Is_dir; If_dir_then_is_empty]

(*  *)
let _ = paths_with_properties [Is_empty_string; Is_file]

let _ = paths_with_properties [Is_slash; Is_nonexist]

let _ = paths_with_properties [Is_empty_string;Is_nonexist]

let _ = paths_with_properties [Is_empty_string]

let _ = paths_with_properties [Not Ends_in_slash; Is_empty_string]

let _ = paths_with_properties [Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; ]

(* interesting case we are missing - need a path that resolves to a dir via a symlink *)
let _ = paths_with_properties [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string;
    Not Is_slash; Is_dir; If_dir_then_is_empty; Has_symlink_component]

let _ = paths_with_properties [Is_dir; If_dir_then_is_empty; Has_symlink_component ]

let _ = paths_with_properties [Is_symlink; Not Has_symlink_component ]

(* really interesting - we were missing an error that is not due to a slash on end of file (so we should include eg an attempt to resolve a file through a directory that doesn't exist) *)
let _ = paths_with_properties [Not Ends_in_slash; Not Is_empty_string; Is_error]

let _ = List.find (list_subset [Not Ends_in_slash; Not Is_empty_string; Is_error]) property_combs1

(* we were not testing paths starting with two slashes, with a symlink component *)
let _ = paths_with_properties [Not Ends_in_slash; Starts_with_slash_x2; Not Is_empty_string;
    Not Is_slash; Is_dir; If_dir_then_is_empty; Has_symlink_component]

let _ = paths_with_properties [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
   Is_dir; Not Is_dir; Has_symlink_component]


*)

(* properties with no satisfying paths *)
let _ = assert([] = (
    property_combs1 |> List.map (fun x -> (x,paths_with_properties x)) |> 
    List.filter (fun (x,y) -> y=[])
  ))



(**********************************************************************)
(* checking path-pair properties on selection of path-pairs *)

(* properties of pairs of paths *)
type path_pair_props = 
  | Paths_equal  (* FIXME do we want this on the realpath, or on the syntactic representation of the path? really, the real path is what matters *)
  | Essentially_different_paths_same_inode  (* two paths to same file; paths not minor variations e.g. differ by trailing slash; currently uses realpath *)
  | First_path_proper_prefix_of_second  (* prefix component-wise; currently uses realpath *)
  | Not2 of path_pair_props
  | Swap of path_pair_props  (* apply a property "the other way round" *)

let rp_of_res_name rn = (
  match rn with
  | RN_dir (_,rp) -> (Some rp)
  | RN_file (_,_,_,rp) -> (Some rp)
  | RN_none (_,_,rp) -> (Some rp)
  | RN_error _ -> None)

(* FIXME what about null paths? *)
let rec sem2 prop (p0,p1) = (
  match prop with
  | Paths_equal -> (p1=p0)
  | Essentially_different_paths_same_inode -> (
      let r0 = p0 |> string_of_path |> resolve in
      let r1 = p1 |> string_of_path |> resolve in
      match (r0,r1) with
      | (RN_file(_,_,i0_ref,rp0),RN_file(_,_,i1_ref,rp1)) -> ((i0_ref = i1_ref) && rp0.rp_ns <> rp1.rp_ns)
      | _ -> false)
  | First_path_proper_prefix_of_second -> (  (* note that this is on realpaths - i.e. it is checking whether first contains second *)
      let rec f1 p q = (match p,q with
          | [],[] -> false
          | [],_ -> true
          | _,[] -> false
          | x::xs,y::ys -> (x=y) && f1 xs ys)
      in
      let r0 = p0 |> string_of_path |> resolve in
      let r1 = p1 |> string_of_path |> resolve in
      let rp0 = (rp_of_res_name r0) in
      let rp1 = (rp_of_res_name r1) in
      match (rp0,rp1) with
      | Some(rp0),Some(rp1) -> (
          match (rp0.rp_ns, rp1.rp_ns) with (* / - root is special cased *)
          | (["";""],[""]) -> false
          | (["";""],["";""]) -> false
          | (["";""],_) -> true
          | _ -> f1 rp0.rp_ns rp1.rp_ns)
      | _ -> ( (* fall back to textual comparison FIXME semantically not clear this is the right thing to do *)
          match (p0,p1) with (* / - root is special cased *)
          | (["";""],[""]) -> false
          | (["";""],["";""]) -> false
          | (["";""],_) -> true
          | _ -> 
            f1 p0 p1)
    )
  | Not2 prop -> (not (sem2 prop (p0,p1)))
  | Swap prop -> (sem2 prop (p1,p0)))

let sem2_list: path_pair_props list -> path * path -> bool = (
  fun props p -> List.for_all (fun prop -> sem2 prop p) props)

let path_pair_props = [
  [Paths_equal; Not2 Paths_equal];
  [Essentially_different_paths_same_inode; Not2 Essentially_different_paths_same_inode];
  (* following should also have swap variants *)
  [First_path_proper_prefix_of_second; Not2 First_path_proper_prefix_of_second];
  [Swap First_path_proper_prefix_of_second; Not2 (Swap First_path_proper_prefix_of_second)];
]


let ppairs1 = allpairs (fun x y -> (x,y)) paths0 paths0
let _ = List.length ppairs1

(* get a prefix of pairs, to make checking more feasible *)
let ppairs0 = 
  let f1 (acc,n) x = if n=0 then (acc,n) else (x::acc,n-1) in
  (List.fold_left f1 ([],3000000) ppairs1) |> (fun (acc,n) -> acc)

let props_hold_for_path_pair: path_pair_props list -> (path * path) -> bool = (fun props (p1,p2) ->
    List.for_all (fun prop -> sem2 prop (p1,p2)) props)

let path_pairs_with_properties : path_pair_props list -> (path * path) list = (fun props -> 
    ppairs0 |> List.filter (props_hold_for_path_pair props))

let pair_prop_combs0 = 
  let f1 acc x = list_product_as_list acc x in
  List.fold_left f1 (List.map (fun x -> [x]) (List.hd path_pair_props)) (List.tl path_pair_props)

let pair_props_logically_possible : path_pair_props list -> bool = (fun props ->
    let sub_props ps = List.for_all (fun p -> List.mem p props) ps in
    (* if the paths are equal, then we can't expect essentially different paths etc *)
    (implies (List.mem Paths_equal props) (not (List.exists (fun x -> List.mem x props) [
         Essentially_different_paths_same_inode;
         First_path_proper_prefix_of_second;
         Swap First_path_proper_prefix_of_second;
       ])))
    (*    && (not (List.mem (Not2 Essentially_different_paths_same_inode) props))  *)
    (* can't expect a proper_prefix b, and b proper_prefix of a *)
    && (not (sub_props [First_path_proper_prefix_of_second; Swap First_path_proper_prefix_of_second]))
    (* first_path_proper_prefix_of_second implies first_path is_dir, so can't expect first path dir to reference a file *)
    && (not (sub_props [Essentially_different_paths_same_inode; First_path_proper_prefix_of_second]))  (* link to file, so can't be a prefix *)
    && (not (sub_props [Essentially_different_paths_same_inode; Swap First_path_proper_prefix_of_second]))
  )

let pair_prop_combs0 = pair_prop_combs0 |> List.filter pair_props_logically_possible



(**********************************************************************)
(* checking pair properties *)

(*

let _ = path_pairs_with_properties [Paths_equal]
let _ = path_pairs_with_properties [Not2 Paths_equal]

(* interesting error in comparison of realpaths - we were comparing the realpath component whereas we should compare rp.rp_ns *)
let _ = resolve (string_of_path [""; ""; ""; "nonempty_dir1"; "d2"; "sl_dotdot_d2"; ".."; ".."; "";
   "nonempty_dir1"; "sl_f1.txt"])

(* should not be empty *)
let _ = path_pairs_with_properties [Not2 Paths_equal; Essentially_different_paths_same_inode]

(* first path must be a dir, so can't reference a file (and thus have same inode) *)
let _ = path_pairs_with_properties
[Essentially_different_paths_same_inode;
   First_path_proper_prefix_of_second;
]

let _ = path_pairs_with_properties
  [Essentially_different_paths_same_inode;
   Swap First_path_proper_prefix_of_second]

(* expect not nil *)
let _ = path_pairs_with_properties
  [Paths_equal; Not2 Essentially_different_paths_same_inode;
   Not2 First_path_proper_prefix_of_second;
]

let _ = path_pairs_with_properties [First_path_proper_prefix_of_second]
*)

(* which combinations are not possible ? can skip this if working interactively - the assertion should hold *)

(* FIXME uncomment

   let _ = assert(
   None = 
   try
    let f1 x = ([] = path_pairs_with_properties x) in
    Some (List.find f1 pair_prop_combs0)
   with _ -> None)

*)

(**********************************************************************)
(* checking all combinations, of both single path properties, and path pair properties *)

let classes_for_single_path : single_path_props list list = property_combs1

let classes_for_path_pairs : path_pair_props list list = pair_prop_combs0

(* S is the set of path pairs. We have E1 which is a set of equiv
   classes for the first path. We also have E2, which is a set of
   equiv classes for the second path. Finally, we have E3, which is a
   set of equiv classes on the set of pairs. We combine these
   equivalences to get a very fine equivalence.

   To combine two equivalence classes E1 and E2:

   Suppose E1 = {e11,e12}; E2 = {e21,e22}

   The combined set of classes is: E = { e11 /\ e21, e11 /\ e22, e12 /\ e21, e12 /\ e22 }

   There is a slight wrinkle: if e1i /\ e2j = {} then E is not a
   proper set of equivalence classes: one of the partitions is
   empty. In our setting, this means that there is a combination such
   that we cannot find a satisfying path pair. Obviously such empty
   entries should be discarded. (See picture in tr notes 2014-11-10)

   Now, E1 is a property on the first path, and E2 is a property on
   the second path. So as properties on pairs of paths, they are such
   that any e1i /\ e2j is nonempty.

   However, when combining with E3, which is an equivalence of pairs
   of paths, it may be that we have a e_ijk that is empty. So we must
   check for this when forming the combination. We hope it doesn't
   take too long to eliminate the problematic cases.

   Previously we manually removed those combinations that were
   empty. Can't we just automatically remove these? Manually removing
   them forced us to uncover bugs, and also identifies the smallest
   "unsatisfiable core" of the empty partitions. This also provides a
   "human-readable" explanation of why some combination e_ijk is not
   satisfiable. However, if we believe the code is correct, we might
   be tempted just to remove all those combinations that are
   empty. NO! the problem is that we were uncovering the fact that the
   partition was empty, which meant (in some cases) that the
   underlying set of paths was not big enough, rather than that the
   combination was unsatisfiable. So this is a technique that properly
   helps to increase coverage and completeness. So we do have to
   manually check the unsatisfiable combinations.
*)

(*
type combined_pair_props = P1 of single_path_props list | P2 of single_path_props list | P12 of path_pair_props list
*)

let (_E1,_E2,_E3) = 
  (classes_for_single_path)
,(classes_for_single_path)
,(classes_for_path_pairs)

let es = allpairs (fun x y -> (x,y)) _E1 _E2

let es2 = allpairs (fun (x1,x2) y -> (x1,x2,y)) es _E3

let _ = List.length es2

let cp0 = es2

type combined_property = single_path_props list * single_path_props list * path_pair_props list

let logically_possible_cp qi =
  let sub_props (q1,q2,q12) (xs,ys,zs) = 
    List.for_all (fun p -> List.mem p q1) xs
    && List.for_all (fun p -> List.mem p q2) ys
    && List.for_all (fun p -> List.mem p q12) zs
  in
  let symmetric_prop p (q1,q2,q12) = p (q2,q1,q12) in
  let b1 = 
    let (q1,q2,q12) = qi in
    let sub_props = sub_props qi in
    not (sub_props ([Is_dir],[],[Essentially_different_paths_same_inode]))
    && not (sub_props ([],[Is_dir],[Essentially_different_paths_same_inode]))
    && not (sub_props ([Is_dir;If_dir_then_is_empty],[],[First_path_proper_prefix_of_second]) && List.exists (fun x -> List.mem x q2) [Is_file;Is_dir;Is_symlink])  (* FIXME note how this could easily have an error; must be revisited if single path equivs change *)
    && not (sub_props ([],[Is_dir;If_dir_then_is_empty],[Swap First_path_proper_prefix_of_second]) && List.exists (fun x -> List.mem x q1) [Is_file;Is_dir;Is_symlink])
    && (implies (List.mem Paths_equal q12) (q1=q2))  (* assuming paths_equal means syntactic equality, it suffices to check p=p for each possible single path equiv class *)
    && not (sub_props ([Is_dir],[Not Is_dir],[Swap First_path_proper_prefix_of_second]))
    && not (sub_props ([Not Is_dir],[Is_dir],[First_path_proper_prefix_of_second]))
    && implies (List.mem First_path_proper_prefix_of_second q12) (not (List.exists (fun x -> List.mem x q1) [Is_file; Is_nonexist; Is_error; Is_symlink]))
    && implies (List.mem (Swap First_path_proper_prefix_of_second) q12) (not (List.exists (fun x -> List.mem x q2) [Is_file; Is_nonexist; Is_error; Is_symlink]))
    && not (sub_props ([],[Is_error],[First_path_proper_prefix_of_second]))  (* if we have error, realpath doesn't exist *)
    && not (sub_props ([Is_error],[],[Swap First_path_proper_prefix_of_second]))  (* if we have error, realpath doesn't exist *)
    && not (sub_props ([Is_dir;If_dir_then_is_empty],[Is_slash],[Not2 (Swap First_path_proper_prefix_of_second)]))  (* we don't test an empty root directory cf cg6 *)
    && not (sub_props ([],[Is_slash],[Not2 (Swap First_path_proper_prefix_of_second)]))  (* we don't test an empty root directory cf cg6 *)
    && not (sub_props ([],[Ends_in_slash],[Essentially_different_paths_same_inode]))  (* version of resolve doesn't allow / to refer to file 36k *)
    && not (sub_props ([Ends_in_slash],[],[Essentially_different_paths_same_inode])) 
    && not (sub_props ([Is_slash],[If_dir_then_is_empty],[Not2 First_path_proper_prefix_of_second; Not2(Swap First_path_proper_prefix_of_second)])) (* "" is only prefix of /, but then "" is a proper prefix of / hdh *)
    && not (sub_props ([If_dir_then_is_empty],[Is_slash],[Not2 First_path_proper_prefix_of_second; Not2(Swap First_path_proper_prefix_of_second)])) 
    && not (sub_props ([Is_slash],[Ends_in_slash],[Swap First_path_proper_prefix_of_second])) 
    && not (sub_props ([Ends_in_slash],[Is_slash],[First_path_proper_prefix_of_second])) 
    && not (sub_props ([Is_slash],[Not Is_empty_string],[Swap First_path_proper_prefix_of_second])) 
    && not (sub_props ([Not Is_empty_string],[Is_slash],[First_path_proper_prefix_of_second])) 
    && implies 
      (sub_props ([Is_empty_string], [], [Not2 (First_path_proper_prefix_of_second)]))
      (not (List.exists (fun x -> List.mem x q2) [Starts_with_one_slash;Starts_with_slash_x2;Starts_with_slash_x3]))
    && implies 
      (sub_props ([], [Is_empty_string], [Not2 (Swap (First_path_proper_prefix_of_second))]))
      (not (List.exists (fun x -> List.mem x q1) [Starts_with_one_slash;Starts_with_slash_x2;Starts_with_slash_x3]))
    && not (sub_props ([Is_empty_string],[Is_empty_string],[Not2 Paths_equal])) 
    && not (sub_props ([Is_slash],[Is_slash],[Not2 Paths_equal])) 
    && not (sub_props ([Not Has_symlink_component],[Is_symlink],[Essentially_different_paths_same_inode])) 
    && not (sub_props ([Is_symlink],[Not Has_symlink_component],[Essentially_different_paths_same_inode])) 
  in
  (* for symmetric, programmatically defined classes; q12 must be symmetric *)
  let b2 = 
    (
      let p (q1,q2,q12) = 
        implies 
          (sub_props (q1,q2,q12) ([Is_slash],[],[Not2 First_path_proper_prefix_of_second;Not2 (Swap First_path_proper_prefix_of_second)])) 
          (not (List.exists (fun x -> List.mem x [Is_file;Is_nonexist;Is_symlink;Is_error]) q2))
      in
      p qi && symmetric_prop p qi)
    && (
      let p (q1,q2,q12) = 
        implies
          (sub_props (q1,q2,q12) ([],[],[Essentially_different_paths_same_inode]))
          (List.exists (fun x -> List.mem x [Is_file;Is_symlink]) q1)
      in
      p qi && symmetric_prop p qi)
  in
  b1 && b2


(* want the "simplest" first *)
(*
let compare_cp (p1,p2,p12) (q1,q2,q12) = 
  let x = (List.length p1, List.length p2, List.length p12) in
  let x'= (List.length q1, List.length q2, List.length q12) in
  Pervasives.compare x x'  (* not antireflexive *)
*)

let cp0 = cp0 |> List.filter logically_possible_cp

let _ = List.length cp0  (* 138927->97887->59265->54675->54659->52115->51945->41289->41228->41163-> ... -> 34101 -> 33813 *)

(* pairs of paths *)
let ppairs0 = ppairs0

let combined_properties_hold_of_path_path: combined_property -> (path * path) -> bool = (fun (q1,q2,q12) (p1,p2) -> 
    sem1_list q1 (Some p1) && sem1_list q2 (Some p2) && sem2_list q12 (p1,p2))

let paths_with_combined_properties : combined_property -> (path * path) list = (fun (q1,q2,q12) ->
    let ppairs = allpairs (fun x y -> (x,y)) (paths_with_properties q1) (paths_with_properties q2) in
    List.filter (combined_properties_hold_of_path_path (q1,q2,q12)) ppairs)

(* we want a minimizer that attempts to determine the minimal
   properties that are unsatisfiable; if a list of props is of length n,
   we try to find a subset of length (n-1) such that there are still no
   paths which satisfy *)

(* http://www.cl.cam.ac.uk/~jrh13/atp/OCaml/lib.ml *)
let rec allsets m l =
  if m = 0 then [[]] else
    match l with
      [] -> []
    | h::t -> List.map (fun g -> h::g) (allsets (m - 1) t) @ allsets m t;;


(* find a minimal subset of an initial set, satisfying property P *)
let rec minimize_set xs p = (
  if xs=[] then xs else
    let l = List.length xs in
    let ss = allsets (l - 1) xs in
    let xs' = 
      try 
        Some(List.find p ss) 
      with _ -> 
        None
    in
    match xs' with 
    | None -> xs
    | Some xs' -> minimize_set xs' p)

let rec minimize_cp' stage cp = (
  match stage with
  | 1 -> (
      let (p1,p2,p12) = cp in
      let p p1' = ([] = paths_with_combined_properties (p1',p2,p12)) in
      let p1' = minimize_set p1 p in
      let _ = print_endline "1" in
      minimize_cp' 2 (p1',p2,p12))
  | 2 -> (
      let (p1,p2,p12) = cp in
      let p p2' = ([] = paths_with_combined_properties (p1,p2',p12)) in
      let p2' = minimize_set p2 p in
      let _ = print_endline "2" in
      minimize_cp' 3 (p1,p2',p12))
  | 3 -> (
      let (p1,p2,p12) = cp in
      let p p12' = ([] = paths_with_combined_properties (p1,p2,p12')) in
      let p12' = minimize_set p12 p in
      (p1,p2,p12'))
  | _ -> (failwith "impossible")
)

let minimize_cp cp = minimize_cp' 1 cp


let compare_paths ((p1:path),(p2:path)) (p3,p4) = Pervasives.compare (List.length p1,List.length p2) (List.length p3, List.length p4)
let sort_paths_list = List.sort compare_paths



module type IGNORE = sig end


(*


(* we can't require that directories have different paths but the same inode *)
let _ = paths_with_combined_properties
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Not2 Paths_equal; Essentially_different_paths_same_inode]
    )

(* we can't expect to have an empty dir which contains another dir *)
let _ = paths_with_combined_properties
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [First_path_proper_prefix_of_second;
     ])

(* same problem as previous, different way round *)
let _ = paths_with_combined_properties 
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [
       Swap First_path_proper_prefix_of_second
     ])

let _ = paths_with_combined_properties 
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Not Has_symlink_component],
     [Paths_equal; Not2 Essentially_different_paths_same_inode;
      Not2 First_path_proper_prefix_of_second;
      Not2 (Swap First_path_proper_prefix_of_second)])

(* if we demand paths are equal, then we shouldn't expect fundamentally different properties of each of the paths *)
let _ = paths_with_combined_properties 
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Not Has_symlink_component],
     [Paths_equal
     ])

(* if either of the paths is a dir, we can't expect Essentially_different_paths_same_inode *)
let _ = paths_with_combined_properties
    ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_dir; If_dir_then_is_empty; Has_symlink_component],
     [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
      Is_nonexist; Not Is_dir; Has_symlink_component],
     [Essentially_different_paths_same_inode;
     ])

(* interesting case: we need to check nonexist entries in an empty dir *)
let _ = paths_with_combined_properties
    ([Is_dir; If_dir_then_is_empty],
     [
       Is_nonexist; ],
     [
       First_path_proper_prefix_of_second;
     ])

(* can't have first is_dir, second not is_dir, and second prefix of first *)
let _ = paths_with_combined_properties
    ([
      Is_dir],
      [Not Is_dir],
      [
        Swap First_path_proper_prefix_of_second])

(* again, can't have the second a prefix of first, if second is not a directory *)
let _ = paths_with_combined_properties
    ([
      Is_dir],
      [
        Is_nonexist],
      [
        Swap First_path_proper_prefix_of_second])

(* we don't check for errors through empty directories; also, if we are using rp, then we can't check errors and prefix FIXME? 

   should be 

   /empty_dir1 , /empty_dir1/nonexist_3/nonexist_4

   let p1 = ["";"empty_dir1"]
   let p2 = ["";"empty_dir1";"nonexist_3";"nonexist_4"]
   let _ = assert( false = combined_properties_hold_of_path_path prop (p1,p2))  (* expect to be true! *)

   let _ = sem1_list [Is_dir; If_dir_then_is_empty] (Some p1)
   let _ = sem1_list [Is_error] (Some p2)
   let _ = sem2_list [First_path_proper_prefix_of_second] (p1,p2)  (* apparently false; ah, we are using realpath, and this doesn't exist for an error case; fixed by changing semantics of First_path_proper_prefix_of_second to fallback on textual comparison when no realpath  *)

*)
let prop = ([
    Is_dir; If_dir_then_is_empty],
    [
      Is_error],
    [
      First_path_proper_prefix_of_second;
    ])


let _ = paths_with_combined_properties prop  (* now we have tests for this *)






(*
this should not have any satisfying paths; rp for relative path seems wrong

NO! this is fine - first resolves to /, second is /, and . is not a proper prefix of slash; so there are many paths with this property, providing first path is /

*)
let _ = paths_with_combined_properties
    ([
      Is_dir],
      [Is_slash],
      [
        Not2 (Swap First_path_proper_prefix_of_second)])


let _ = ["nonempty_dir1"; "d2"; "sl_dotdot_d2"; ".."; ".."; ""] |> string_of_path |> resolve



(* 

cg6

cf above, we don't test on empty root directory, hence this condition is not satisfiable (the first path is forced to be root)

so this is unsatisfiable but FIXME probably should be checked (if we get round to checking with an empty root)

*)
let _ = paths_with_combined_properties
    ([
      Is_dir ; If_dir_then_is_empty],
      [Is_slash],
      [
        Not2 (Swap First_path_proper_prefix_of_second)])





let _ = (
  module struct

    let _ = paths_with_combined_properties
        ([(*Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;*)
          Is_dir; (* If_dir_then_is_empty; Has_symlink_component *)],
          [(*Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash; *)
            Is_symlink; (* If_dir_then_is_empty; Has_symlink_component *)],
          [(* Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
              Not2 First_path_proper_prefix_of_second; *)
            Swap First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties
        ([(*Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;*)
          Is_symlink; (* If_dir_then_is_empty; Has_symlink_component *)],
          [(*Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash; *)
            Is_dir; (* If_dir_then_is_empty; Has_symlink_component *)],
          [(* Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
              Not2 First_path_proper_prefix_of_second; *)
            First_path_proper_prefix_of_second])

    (* need a symlink to a directory which contains a subdirectory? *)

    let prop = ([(*Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;*)
        Is_symlink; (* If_dir_then_is_empty; Has_symlink_component *)],
        [(*Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash; *)
          Is_dir; (* If_dir_then_is_empty; Has_symlink_component *)],
        [(* Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
            Not2 First_path_proper_prefix_of_second; *)
          First_path_proper_prefix_of_second])

    (* in fact this isn't satisfiable - symlink can't be prefix of anything *)

    let p1 = ["";"sl_nonempty_dir1"]
    let p2 = ["";"nonempty_dir1";"d2"]
    let _ = assert( false = combined_properties_hold_of_path_path prop (p1,p2))  (* expect to be true! *)

    let _ = sem1_list [Is_symlink] (Some p1)
    let _ = sem1_list [Is_dir] (Some p2)
    let _ = sem2_list [First_path_proper_prefix_of_second] (p1,p2) 

    let _ = p1 |> string_of_path |> resolve  (* this gives rp_ns = [""; ""; "sl_nonempty_dir1"] which is wrong *)
    let _ = p2 |> string_of_path |> resolve
    let _ = ["";"nonempty_dir1"] |> string_of_path |> resolve  (* OK *)
    let _ = ["";"nonempty_dir1";"f1.txt"] |> string_of_path |> resolve  (* OK *)
    let _ = ["";"f4.txt"] |> string_of_path |> resolve  (* not OK; rp_ns is incorrect for files in the root directory *)


  end : IGNORE)





let _ = (
  module struct

    let prop = 
      ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
        Is_dir; If_dir_then_is_nonempty; Has_symlink_component],
       [Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
        Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
       [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
        First_path_proper_prefix_of_second;
        Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = minimize_cp prop

    (* if p2 is /, and p1 is a proper prefix (ie "") then we can't require that p2 is not a proper prefix of p1 (because root is a proper prefix of ""?) *)
    let prop = ([], [Is_slash],
                [First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    (* is / a proper prefix of "" ? apparently yes! fixed definition of proper prefix to take empty string into account *)
    let prop = ([],[],[Swap (First_path_proper_prefix_of_second)])
    let p1 = [""]
    let p2 = ["";""]
    let _ = combined_properties_hold_of_path_path prop (p1,p2)

(*
(* this shows that p1="" and p2=/ satisfy that first path is a proper prefix of / *)
let prop = ([], [Is_slash],
 [First_path_proper_prefix_of_second;
  ])

let _ = paths_with_combined_properties prop

(* this combination is easy: eg / is not a proper prefix of any relative path *)
let prop = ([], [Is_slash],
 [
  Not2 (Swap First_path_proper_prefix_of_second)])

let _ = paths_with_combined_properties prop
*)

  end : IGNORE)





let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let prop = minimize_cp prop

    (* expect nonexmpty_dir1, nonexmpty_dir1/nonexist_1 should satisfy *)
    let prop = ([Starts_with_no_slash; If_dir_then_is_nonempty; Not Has_symlink_component], 
                [Starts_with_no_slash; Is_nonexist; Not Has_symlink_component],
                [First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    let p1 = ["nonempty_dir1"]
    let p2 = ["nonempty_dir1";"nonexist_1"]
    let _ = combined_properties_hold_of_path_path prop (p1,p2)

    (* fixed by adding nonempty_dir1,nonempty_dir1/nonexist_1 *)

    (* satisfiable? *)
    let prop = ([Starts_with_no_slash; If_dir_then_is_nonempty; Not Has_symlink_component], (* eg nonempty_dir1 *)
                [Starts_with_no_slash; Is_nonexist],   (* eg ["nonempty_dir1"; "d2"; "sl_dotdot_no_such_target"; ""] *)
                [First_path_proper_prefix_of_second])

    (* so we need to have something that doesn't contain a symlink, and resolves to nonexist eg nonempty_dir1/nonexist*)


    let compare_paths (p1,p2) (p3,p4) = Pervasives.compare (List.length p1,List.length p2) (List.length p3, List.length p4)
    let sort_paths_list = List.sort compare_paths
    let _ = paths_with_combined_properties prop |> sort_paths_list


  end : IGNORE)









let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let prop = minimize_cp prop

    (* expect nonexmpty_dir1, nonexmpty_dir1/nonexist_1 should satisfy *)
    let prop = ([Starts_with_no_slash; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Is_nonexist;
                 Not Has_symlink_component],
                [First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    (* as with previous case, but in addition need to have a path ending in / *)


  end : IGNORE)










let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])


    let prop = minimize_cp prop

    let prop = ([Ends_in_slash; Starts_with_no_slash; If_dir_then_is_nonempty;
                 Not Has_symlink_component],
                [Starts_with_one_slash; Is_nonexist; Not Has_symlink_component],
                [First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    (* FIXED - made sure paths that should have been there, are there *)


  end : IGNORE)







let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let prop = minimize_cp prop

    let prop = ([Starts_with_no_slash; Is_dir; If_dir_then_is_empty;
                 Has_symlink_component],
                [Starts_with_no_slash; Is_dir; If_dir_then_is_empty;
                 Has_symlink_component],
                [Not2 Paths_equal])

    let _ = paths_with_combined_properties prop

    (* FIXED by adding further sl to empty dirs *)



  end : IGNORE)


let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let prop = minimize_cp prop

    let prop = ([If_dir_then_is_empty], [Is_nonexist; Has_symlink_component],
                [First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    (* we don't check a nonexist in an empty dir, with the nonexist via a symlink component 

       eg nonempty_dir1/sl_dotdot_empty_dir1/nonexist_1

       FIXED

    *)


  end : IGNORE)







let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Swap First_path_proper_prefix_of_second])


    let prop = minimize_cp prop

    let prop = ([], [Is_slash], [])

    let _ = paths_with_combined_properties prop

    let _ = paths_with_properties [Is_slash]

    (* should be satisfied by _,/ ; mistake after updating defn of paths; fixed *)


  end : IGNORE)


let _ = (
  module struct


    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([],
                [Not Ends_in_slash; Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [])

    let _ = paths_with_combined_properties prop

    (* need a path to an empty dir via symlink; missing paths after refactoring paths0; fixed *)


  end : IGNORE)




let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([], [Ends_in_slash], [Essentially_different_paths_same_inode])

    let _ = paths_with_combined_properties prop

    (* can't expect p2 to end in slash, and resolve to something that is a file? or need a symlink to something that is a link 

       nonempty_dir1/sl_f1.txt/ should resolve to .../f1.txt which has a hard link in nonempty_dir1/link_f1.txt

    *)
    let p1 = ["nonempty_dir1";"f1.txt"]
    let p2 = ["nonempty_dir1";"sl_link_f1.txt";""]
    let _ = combined_properties_hold_of_path_path prop (p1,p2)

    let _ = sem1_list [Ends_in_slash] (Some p2)
    let _ = p1 |> string_of_path |> resolve
    let _ = p2 |> string_of_path |> resolve

    let _ = sem2 Essentially_different_paths_same_inode (p1,p2)

    (* the version of resolve we use means that a (symlink that) ends in a slash cannot refer to a file 36k *)


  end : IGNORE)








let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [If_dir_then_is_empty],
                [Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    (* this forces the second path to be ""; then we can't stop "" being a proper prefix of "";"" hdh *)
    let prop = ([Is_slash], [If_dir_then_is_empty],
                [Not2 First_path_proper_prefix_of_second;
                ])

    let _ = paths_with_combined_properties prop

  end : IGNORE)





let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Swap First_path_proper_prefix_of_second])


    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [Ends_in_slash], [Swap First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    (* can't have something that is not "" being a prefix of / *)


  end : IGNORE)


let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_nonexist; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [Is_nonexist], [Not2 First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    (* slash is always a prefix of non-exist *)


  end : IGNORE)





let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])


    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [Is_error],
                [Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    (* if first path is /, and it is not a prefix, then second path must be "" to get error; then we can't require that "" is not a proper prefix of / *)
    let prop = ([Is_slash], [Is_error],
                [Not2 First_path_proper_prefix_of_second;
                ])

    let _ = paths_with_combined_properties prop


    (* fixed by adjusting lp *)


  end : IGNORE)






let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])


    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [Is_error],
                [Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    (* if first path is /, and it is not a prefix, then second path must be "" to get error; then we can't require that "" is not a proper prefix of / *)
    let prop = ([Is_slash], [Is_error],
                [Not2 First_path_proper_prefix_of_second;
                ])

    let _ = paths_with_combined_properties prop


    (* fixed by adjusting lp *)


  end : IGNORE)






let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_nonempty; Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Swap First_path_proper_prefix_of_second])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_slash], [Not Is_empty_string],
                [Swap First_path_proper_prefix_of_second])

    (* can't have a prefix of slash that isn't empty; fixed by adjusting lp *)


  end : IGNORE)




let _ = (
  module struct

    let prop = ([Ends_in_slash; Starts_with_one_slash; Not Is_empty_string; Not Is_slash;
                 Is_dir; If_dir_then_is_empty; Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Starts_with_one_slash], [Is_empty_string],
                [Not2 (Swap First_path_proper_prefix_of_second)])

    let prop = ([Is_empty_string], [Starts_with_one_slash], 
                [Not2 (First_path_proper_prefix_of_second)])


    let _ = paths_with_combined_properties prop


    (* the empty string is a prefix of anything that starts with a slash; but empty string should resolve to error, in which case *)

    let _ = ""|>resolve  (* ENOENT *)


  end : IGNORE)





let _ = (
  module struct

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([], [Is_empty_string], [Essentially_different_paths_same_inode])

    let _ = paths_with_combined_properties prop

    let _ = logically_possible_cp prop


    (* Essentially_different_paths_same_inode must be for files (or symlinks?) FIXME *)


  end : IGNORE)



let _ = (
  module struct

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Is_empty_string; Not Is_slash;
                 Is_error; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Is_empty_string], [Is_empty_string], [Not2 Paths_equal])

    (* easy case: obviously impossible *)

  end : IGNORE)





let _ = (
  module struct

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_file; Not Is_dir; Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Not Is_slash;
                 Is_symlink; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([], [Is_symlink], [Essentially_different_paths_same_inode])

    (* need to have a hard link to a symlink! interesting case; but doesn't link dereference the symlink? *)

  end : IGNORE)




let _ = (
  module struct

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string;
                 Not Is_slash; Is_file; Not Is_dir; Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string;
                 Not Is_slash; Is_symlink; Not Is_dir; Has_symlink_component],
                [Not2 Paths_equal; Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Not Has_symlink_component], [Is_symlink],
                [Essentially_different_paths_same_inode])

    (* is 2nd is a symlink, then to have the same inode the first must point to the symlink; but then the first must have a symlink component *)

    let prop = ([], [Is_symlink],
                [Essentially_different_paths_same_inode])

    let _ = paths_with_combined_properties prop |> sort_paths_list

    (* (["nonempty_dir1"; "link_to_symlink"], ["nonempty_dir1"; "sl_link_f1.txt"]) *)

  end : IGNORE)


let _ = (
  module struct

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string;
                 Not Is_slash; Is_error; Not Is_dir; Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string;
                 Not Is_slash; Is_error; Not Is_dir; Not Has_symlink_component],
                [Not2 Paths_equal; Not2 Essentially_different_paths_same_inode;
                 Not2 First_path_proper_prefix_of_second;
                 Not2 (Swap First_path_proper_prefix_of_second)])

    let _ = paths_with_combined_properties prop

    let prop = minimize_cp prop

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Is_error;
                 Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Is_error;
                 Not Has_symlink_component],
                [Not2 Paths_equal])

    (* we only have one relative path to an error ? fixed by ux7 *)

    let prop = ([Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Is_error;
                 Not Has_symlink_component],
                [Not Ends_in_slash; Starts_with_no_slash; Not Is_empty_string; Is_error;
                 Not Has_symlink_component],
                [])

    let _ = paths_with_combined_properties prop |> sort_paths_list

    (* (["empty_dir1"; "nonexist_3"; "nonexist_4"],
       ["empty_dir1"; "nonexist_3"; "nonexist_4"]) *)

    let _ = paths_with_properties [ Starts_with_no_slash; Not Is_empty_string; Is_error;
                                    Not Has_symlink_component]

  end : IGNORE)


*)

let _ = 
  let _ = assert (not (List.exists (fun x -> [] = paths_with_combined_properties x) cp0)) in
  ()


let _ = Printf.printf "property_testgen.ml: all tests pass\n"

(*

(* could make this quicker by rebinding cp0 *)

let n0 = ref 0

let prop =   
  let f1 x = ([] = paths_with_combined_properties x) in
  let (prop,n') = find_first_n f1 (drop (!n0) cp0) in
  let _ = n0 := !n0 + n' in
  let _ = Printf.printf "Index now %d \n" (!n0) in
  prop



let _ = 
  let f1 x = ([] = paths_with_combined_properties x) in
  List.find f1 cp0

let _ = 
  let f1 x = ([] = paths_with_combined_properties x) in
  cp0 |> List.filter f1 |> List.length

*)
