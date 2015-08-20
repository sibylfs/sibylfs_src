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

open Fs_interface.Fs_spec_intf
open Fs_interface.Fs_spec_intf.Fs_types
open Fs_interface.Fs_spec_intf.Fs_arch
open Lem_support

open Fs_ast
open Fs_lr_parser
open Fs_interface.Fs_printer_intf

type arch = Fs_interface.Fs_spec_intf.Fs_types.ty_arch

exception Parse_error of string

type ty = TyTrace | TyScript

type pid = Fs_interface.Fs_spec_intf.Fs_types.ty_pid with sexp

(* FIXME OS_multiple_return doesn't make sense really; also
   OS_no_error_return doesn't make much sense; *)
type os_extended_label =
| OS_simple_label of os_label
| OS_no_error_return of pid
| OS_multiple_return of (pid * (ret_value error_or_value) list)
with sexp

type line =
| Comment of string
| Newline
| Label of (bool * os_extended_label)
(* FIXME what is the bool representing? whether a pid was included?
   but the pid is recorded in os_label; either we have a separate type
   for labels that appear in traces (some of which may have pids, and
   some may not), or we should avoid using this bool *)
| Dump of string
| Dump_result of (string * Dump.t)
with sexp

type numbered_line = int option * line with sexp

type untyped = numbered_line list
type t = ty * untyped

type indent = numbered_line -> string

let type_of ((ty, _):t) = ty

(* AST -> Label transition *)

let int_of_seek_command arch = function
  | Seek_cur      -> arch.arch_int_of_seek_command Fs_types.SEEK_CUR
  | Seek_set      -> arch.arch_int_of_seek_command Fs_types.SEEK_SET
  | Seek_end      -> arch.arch_int_of_seek_command Fs_types.SEEK_END
  | Seek_data     -> arch.arch_int_of_seek_command Fs_types.SEEK_DATA
  | Seek_hole     -> arch.arch_int_of_seek_command Fs_types.SEEK_HOLE
  | Seek_whence w -> w

let os_command_of_call arch = function
  | Close fd -> OS_CLOSE (FD fd)
  | Link (p, p') -> OS_LINK (p, p')
  | Mkdir (path, perm) -> OS_MKDIR (path, perm)
  | Open (path, flags, perm_opt) ->
    OS_OPEN (path,
             arch.arch_int_of_open_flags (finset_from_list flags),
             perm_opt)
  | Open_close (path, flags, perm_opt) ->
    OS_EXTENDED_CMD
      (OS_OPEN_CLOSE (path,
                      arch.arch_int_of_open_flags (finset_from_list flags),
                      perm_opt))
  | Pread (fd, size, offset) -> OS_PREAD (FD fd, size, offset)
  | Pread_det (fd, size, offset) ->
    OS_EXTENDED_CMD (OS_DET_PREAD (FD fd, size, offset))
  | Pwrite (fd, str, size, offset) ->
    OS_PWRITE (FD fd, bytes_of_cstring str, size, offset)
  | Pwrite_det (fd, str, size, offset) ->
    OS_EXTENDED_CMD (OS_DET_PWRITE (FD fd, bytes_of_cstring str, size, offset))
  | Read (fd, size) -> OS_READ (FD fd, size)
  | Read_det (fd, size) -> OS_EXTENDED_CMD (OS_DET_READ (FD fd, size))
  | Readdir dh -> OS_READDIR (DH dh)
  | Rewinddir dh -> OS_REWINDDIR (DH dh)
  | Opendir str -> OS_OPENDIR str
  | Closedir dh -> OS_CLOSEDIR (DH dh)
  | Readlink str -> OS_READLINK str
  | Rename (p, p') -> OS_RENAME (p, p')
  | Rmdir p -> OS_RMDIR p
  | Stat p -> OS_STAT p
  | Lstat p -> OS_LSTAT p
  | Symlink (p, p') -> OS_SYMLINK (p, p')
  | Truncate (p, size) -> OS_TRUNCATE (p, size)
  | Unlink p -> OS_UNLINK p
  | Write (fd, str, size) -> OS_WRITE (FD fd, bytes_of_cstring str, size)
  | Write_det (fd, str, size) ->
    OS_EXTENDED_CMD (OS_DET_WRITE (FD fd, bytes_of_cstring str, size))
  | Add_user_to_group (uid, gid) ->
    OS_EXTENDED_CMD (OS_ADD_USER_TO_GROUP (User_id uid, Group_id gid))
  | Chown (p, uid, gid) -> OS_CHOWN (p, User_id uid, Group_id gid)
  | Chmod (p, perm) -> OS_CHMOD (p, perm)
  | Chdir p -> OS_CHDIR p
  | Lseek (fd, offset, cmd) ->
    OS_LSEEK (FD fd, offset, int_of_seek_command arch cmd)
  | Umask perm -> OS_UMASK perm

let label_of_action arch pid = function
  | Create -> OS_CREATE (pid, root_uid, root_gid)
  | Create_uid (uid, gid) -> OS_CREATE (pid, User_id uid, Group_id gid)
  | Destroy -> OS_DESTROY pid
  | Call call -> OS_CALL (pid, os_command_of_call arch call)

let label_of_pid_opt_action arch = function
  | (None, action)        ->
    Label (false, OS_simple_label (label_of_action arch (Pid 1) action))
  | (Some pid, action)    ->
    Label (true,  OS_simple_label (label_of_action arch (Pid pid) action))

let translate_script_line arch lineno line = Some lineno, match line with
  | Comment_script c -> Comment c
  | Nl_script -> Newline
  | Type_script -> Comment "@type script"
  | Dump path -> Dump path
  | Action pid_opt_action -> label_of_pid_opt_action arch pid_opt_action

let label_of_pid_opt_return arch = function
  | (None, Simple_return eov) ->
    Label (false, OS_simple_label (OS_RETURN (Pid 1, eov)))
  | (Some pid, Simple_return eov) ->
    Label (true, OS_simple_label (OS_RETURN (Pid pid, eov)))
  | (None, No_error_return) ->
    Label (false, OS_no_error_return (Pid 1))
  | (Some pid, No_error_return) ->
    Label (true, OS_no_error_return (Pid pid))
  | (None, Multiple_return eovs) ->
    Label (false, OS_multiple_return (Pid 1, eovs))
  | (Some pid, Multiple_return eovs) ->
    Label (true, OS_multiple_return (Pid pid, eovs))

let translate_action arch lineno (pid, action) =
  [Some lineno, label_of_pid_opt_action arch (pid, action)]

let translate_return arch lineno (pid, return) =
  [Some lineno, label_of_pid_opt_return arch (pid, return)]

let translate_trace_line arch lineno = function
  | Comment_trace c -> [Some lineno, Comment c]
  | Nl_trace -> [Some lineno, Newline]
  | Type_trace -> [Some lineno, Comment "@type trace"]
  | Dump_result (path, dump) -> [Some lineno, Dump_result (path, dump)]
  | Action_trace action -> translate_action arch lineno action
  | Return return -> translate_return arch lineno return
  | Tau -> [Some lineno, Label (false, OS_simple_label OS_TAU)]

let translate_trace arch = function
  | Script s ->
    let arch = architecture_of_ty_arch arch in
    let lbls = List.(rev (fst (fold_left (fun (acc,lineno) line ->
      ((translate_script_line arch lineno line) :: acc, lineno + 1)
    ) ([],1) s))) in
    (TyScript,lbls)
  | Trace t ->
    let arch = architecture_of_ty_arch arch in
    let lbls = List.(rev (fst (fold_left (fun (acc,lineno) trans ->
      let lines = translate_trace_line arch lineno trans in
      (List.rev_append lines acc, lineno + List.length lines)
    ) ([],1) t))) in
    (TyTrace,lbls)

(* parsing *)
  
let parse_trace_lexbuf lexbuf =
  try
    Fs_lr_parser.script_or_trace Fs_lexer.(token (fresh_lex_state ())) lexbuf
  with Fs_lr_parser.Error ->
    let loc = Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
    Fs_ast.(error (string_of_loc loc ^ "\nSyntax error."))

let of_string arch trace_s =
  translate_trace arch (parse_trace_lexbuf (Lexing.from_string trace_s))

let of_file arch filename =
  let ic = open_in filename in
  let trace =
    try
      let lexbuf = Lexing.from_channel ic in
      lexbuf.Lexing.lex_curr_p <- {
        lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename;
      };
      let trace = parse_trace_lexbuf lexbuf in
      close_in ic;
      trace
    with
    | Fs_ast.Parse_error msg -> close_in ic; raise (Parse_error msg)
    | e ->
      close_in ic;
      let msg = "parse_trace_file: unknown error reading and parsing file '" in
      raise (Parse_error
               (msg^filename^"': "^(Printexc.to_string e)^"\n"
                ^(Printexc.get_backtrace ())))
  in
  translate_trace arch trace

(* printing *)

let input_string_of_extended_label arch (had_pid, lbl) = match lbl with
  | OS_simple_label lbl ->
    Fs_interface.Fs_printer_intf.input_string_of_os_label (not had_pid)
      (architecture_of_ty_arch arch) lbl
  | OS_no_error_return pid ->
    if had_pid then ((string_of_pid pid)^" <- -") else "-"
  | OS_multiple_return (pid, el) ->
    let el_sl = List.(rev (rev_map input_string_of_rv_error_or_value el)) in
    let el_s = String.concat ";" el_sl in
    (if had_pid then (string_of_pid pid)^" <- " else "") ^ ("{" ^ el_s ^ "}")

let make_indent ?(number=false) ws =
  let lineno_tl = ": " in
  let lineno_tl_len = String.length lineno_tl in
  let lineno_fmt = Scanf.format_from_string
    ("%"^(string_of_int (ws - lineno_tl_len))^"d"^lineno_tl) "%d"
  in
  let ws = String.make ws ' ' in
  if number then function
  | _, Comment _ | None, _ -> ws
  | Some n, _ -> sprintf lineno_fmt n
  else fun (n_opt, _lbl) -> ws

let dump_to_string ?(ws=3) ?indent dump =
  let ws = String.make ws ' ' in
  let indent = match indent with
    | None -> ws
    | Some indent -> ws ^ (indent (None, Comment ""))
  in
  String.concat "" (List.map (fun l -> indent ^ l) (Dump.to_csv_strings dump))

let decl_of_maybe_type type_decl = function
  | Some TyScript -> "@type script"
  | Some TyTrace -> "@type trace"
  | None -> type_decl

let string_of_ext_lbl indent ?change_type string_of_lbl
    ((n_opt, lbl) as n_lbl) =
  match lbl with
  | Newline   -> ""
  | Comment "@type script" ->
    (indent n_lbl)^(decl_of_maybe_type "@type script" change_type)
  | Comment "@type trace" ->
    (indent n_lbl)^(decl_of_maybe_type "@type trace" change_type)
  | Comment x -> (indent n_lbl)^"#"^x
  | Label lbl -> (indent n_lbl)^(string_of_lbl lbl)
  | Dump p    -> sprintf "%sdump %S" (indent n_lbl) p
  | Dump_result (p, dump) -> sprintf
    "%sdump-result %S\n%s%send dump-result"
    (indent n_lbl) p
    (dump_to_string ~indent dump) (indent (None, Comment ""))

let indent_trace_line = make_indent ~number:true 8

let numbered_line_to_string arch ?change_type ?(indent=indent_trace_line) n_lbl =
  let string_of_lbl = input_string_of_extended_label arch in
  string_of_ext_lbl indent ?change_type string_of_lbl n_lbl

let indent_trace = make_indent 8

let to_string arch ?change_type ?(indent=indent_trace) (trace_ty, lbls) =
  let string_of_lbl = input_string_of_extended_label arch in
  let format = string_of_ext_lbl indent ?change_type string_of_lbl in
  String.concat "\n" List.(rev (rev_map format lbls))

(* label constructors *)

module Label = struct

  let simple_call ?(pid=1) cmd =
    Label (pid <> 1, OS_simple_label (OS_CALL (Pid pid, cmd)))
  let maybe f = function None -> None | Some x -> Some (f x)
  let perm_of_int perm = File_perm (Int32.of_int perm)

  let create ~pid ~uid ~gid =
    let uid = User_id uid in
    let gid = Group_id gid in
    Label (pid <> 1, OS_simple_label (OS_CREATE (Pid pid, uid, gid)))

  let add_user_to_group ~uid ~gid =
    let uid = User_id uid in
    let gid = Group_id gid in
    simple_call (OS_EXTENDED_CMD (OS_ADD_USER_TO_GROUP (uid, gid)))

  let umask ?pid mask =
    simple_call ?pid (OS_UMASK (perm_of_int mask))

  let mkdir ?pid name perm =
    simple_call ?pid (OS_MKDIR (CS_Some name, perm_of_int perm))

  let chown ?pid ~uid ~gid path =
    simple_call ?pid (OS_CHOWN (CS_Some path, User_id uid, Group_id gid))

  let chmod ?pid name perm =
    simple_call ?pid (OS_CHMOD (CS_Some name, perm_of_int perm))

  let chdir ?pid name = simple_call ?pid (OS_CHDIR (CS_Some name))

  let open_close ?pid arch name ?perm flags =
    let int_of_open_flags = arch |> Fs_arch.architecture_of_ty_arch |> (fun x -> x.arch_int_of_open_flags) in
    let flags = int_of_open_flags (finset_from_list flags) in
    let args = (CS_Some name, flags, maybe perm_of_int perm) in
    simple_call ?pid (OS_EXTENDED_CMD (OS_OPEN_CLOSE args))

  let open_ arch name ?perm flags =
    let int_of_open_flags = arch |> Fs_arch.architecture_of_ty_arch |> (fun x -> x.arch_int_of_open_flags) in
    let flags = int_of_open_flags (finset_from_list flags) in
    simple_call (OS_OPEN (CS_Some name, flags, maybe perm_of_int perm))

  let det_write ~fd content size =
    let content = bytes_of_cstring (CS_Some content) in
    simple_call (OS_EXTENDED_CMD (OS_DET_WRITE (FD fd, content, size)))

  let read ~fd size =
    simple_call (OS_READ (FD fd, size))

  let close ~fd =
    simple_call (OS_CLOSE (FD fd))

  let symlink content name =
    simple_call (OS_SYMLINK (CS_Some content, CS_Some name))

  let link target name =
    simple_call (OS_LINK    (CS_Some target,  CS_Some name))

  let dump name = Dump name

  let rename src dest = simple_call (OS_RENAME (CS_Some src, CS_Some dest))

  let rmdir dir = simple_call (OS_RMDIR (CS_Some dir))

  let unlink name = simple_call (OS_UNLINK (CS_Some name))

  let stat name = simple_call (OS_STAT (CS_Some name))

  let lstat name = simple_call (OS_LSTAT (CS_Some name))

  let truncate name size = simple_call (OS_TRUNCATE (CS_Some name, size))
end
