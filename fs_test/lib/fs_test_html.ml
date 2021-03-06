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

module Types = Fs_interface.Fs_spec_intf.Fs_types
module Arch = Fs_interface.Fs_spec_intf.Fs_arch
module Printer = Fs_interface.Fs_printer_intf
module Config = Fs_test_config
module Index = Fs_test_index

type location = {
  suite : string;
  script : string;
}

type cat = (location * ((int option * CheckLib.d_lbl) list)) list

type error_cats = {
  dir_nlink_high  : cat;
  dir_nlink_low   : cat;
  file_nlink_high : cat;
  file_nlink_low  : cat;
  other           : cat;
}

type check_result =
| Ok
| Assertion_failure
| Failure

let and_check a b = match a, b with
  | Failure, _ | _, Failure -> Failure
  | Assertion_failure, _ | _, Assertion_failure -> Assertion_failure
  | Ok, Ok -> Ok

let (/) = Filename.concat

let pwd = Unix.getcwd ()

module Dir = struct
  type t = { root: string; path : string }

  let perm = 0o777 (* rely on umask *)

  let rec make_exist ~perm path =
    try Unix.access path []
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let dir = Filename.dirname path in
      make_exist ~perm dir;
      Unix.(mkdir path perm)
    | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Printf.eprintf "fatal: %s is not a directory\n%!" path;
      exit 1

  let dir ?parent path =
    let d = match parent with
      | None -> let root = pwd / path in { root; path = root }
      | Some p -> { root = p.root; path = p.path / path }
    in
    make_exist ~perm d.path;
    d

  let path dir name = dir.path / name

  let root dir = dir.root
end

let empty_cats = {
  dir_nlink_high = [];
  dir_nlink_low = [];
  file_nlink_high = [];
  file_nlink_low = [];
  other = [];
}

let sort_gen_alist a = List.sort (fun (x,_) (y,_) -> compare x y) a
let sort_alist a = List.sort (fun (x,_) (y,_) -> String.compare x y) a

let page ?head ~title html =
  let open Fs_test_version in
  let dirty = if git_dirty then " (dirty)" else "" in
  let href = "https://github.com/sibylfs/sibylfs_src/commit/"^git_rev in
  let rev = <:html<<a href=$str:href$>$str:git_rev$$str:dirty$</a>&>> in
<:html<
<html>
<head>
$opt:head$
<title>$str:title$</title>
<style type="text/css">
tr.summary { margin-top: 0.3em; margin-bottom: 0.3em }
th.row { text-align: left }
th.wide { border-left: 1px solid black; border-right: 1px solid black }
td.num { text-align: right }
td.good, th.good { color: green }
td.bad, th.bad { color: red; font-weight: bold }
td.warn, th.warn { color: orange; font-weight: bold }
td.fail, th.fail { color: violet; font-weight: bold }
tr.check td.good, tr.check td.bad, tr.check td.warn, tr.check td.fail {
  text-align: center;
}
td.error { border: 2px solid red }
td.line { vertical-align: top; text-align: right; padding-right: 1em }
td.line:target { background-color: yellow }
td.comment { color: gray }
td.meta { color: green }
td.dump table { margin: 1em; color: brown }
td.return.label, td.process.label { padding-bottom: 1em }
span.checked { font-size: 80% }
td.terminated, td.diverged { border: 2px solid red }
td.terminated { font-weight: bold; font-size: 150% }
td.envelope { border: 2px solid green }
div.stat table { margin-left: 1em }
strong.fail { font-size: 120%; color: violet; }
div.footer { font-size: 70%; color: gray; padding-top: 3em; text-align: right; }
</style>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
</head>
<body>
<a href="../../index.html">Up</a><br />
$html$
<div class="footer">
  Generated by <em><a href="http://sibylfs.io/">SibylFS</a></em>
  <strong>$rev$</strong>
</div>
</body>
</html>
>>

let write_html dir file html =
  let oc = open_out (Dir.path dir file) in
  Cow.Html.output_doc (`Channel oc) html;
  close_out oc

let rel_path_of_script config_path suite file = config_path / suite / file

let rel_path_of_test_file config_path suite file =
  rel_path_of_script config_path suite ("results" / file)

let path_of_test_file dir config_path suite file =
  (Dir.root dir) / (rel_path_of_test_file config_path suite file)

let load_sexp =
  let open CheckLib in
  let cache = Hashtbl.create 8192 in
  fun sexp_file ->
    try Hashtbl.find cache sexp_file
    with Not_found ->
      let sexp = d_trace_of_sexp (Sexplib.Sexp.load_sexp sexp_file) in
      Hashtbl.replace cache sexp_file sexp;
      sexp

let is_assertion = CheckLib.(function
  | D_checklib_false_positive _ | D_checklib_false_negative _ -> true
  | D_check_dump_failed _ | D_interp_dump_failed | D_dump _ | D_ret_lbl _
  | D_ret_lbl_empty _ | D_bad_state _ | D_mixed_states _ -> false
)

let check_result_of_d_lbls d_lbls =
  if List.for_all is_assertion d_lbls
  then Assertion_failure else Failure

let is_check_ok sexp_file =
  let open CheckLib in
  let lines = load_sexp sexp_file in
  List.fold_left (fun error -> function
  | { d_lbls=[] }   -> error
  | { d_lbls } ->
    match error with
    | Failure -> Failure
    | Ok | Assertion_failure -> check_result_of_d_lbls d_lbls
  ) Ok lines

let suite_fail_by_config dir by_config =
  let fails = Index.(map_config (fun { config; path; test } ->
    ((config, path), (fun suite ->
      let tests = Index.tests_of_suite test suite in
      let spec_bools, exec_fails = Index.fold_tests (function
        | { suite; result = Diff { diff_checks } } -> fun (lst,exec_fails) ->
          (Index.fold_checks (fun { diff_spec; diff_result } checks ->
            let sexp_file =
              path_of_test_file dir path suite diff_result.output.stdout
            in
            (diff_spec, is_check_ok sexp_file)::checks
           ) [] diff_checks)::lst, exec_fails
        | { suite; result = Trace { success = Fail } } ->
          fun (lst,exec_fails) -> lst, succ exec_fails
        | { suite; result = Trace { success = Ok } } ->
          failwith "fs_test_html.ml: successful unchecked traces found: check!"
        | _ ->
          failwith
            "fs_test_html.ml: don't know what to do with non-check non-trace"
      ) ([],0) tests
      in
      let spec_bools = List.flatten spec_bools in
      let spec_tbl = Hashtbl.create 8 in
      List.iter (function
      | (spec,Failure) ->
        let (f,af) = try Hashtbl.find spec_tbl spec with Not_found -> (0,0) in
        Hashtbl.replace spec_tbl spec (f+1,af)
      | (spec,Assertion_failure) ->
        let (f,af) = try Hashtbl.find spec_tbl spec with Not_found -> (0,0) in
        Hashtbl.replace spec_tbl spec (f,af+1)
      | (spec,Ok) ->
        let (f,af) = try Hashtbl.find spec_tbl spec with Not_found -> (0,0) in
        Hashtbl.replace spec_tbl spec (f,af)
      ) spec_bools;
      let fail_counts = Hashtbl.fold (fun spec c lst ->
        (spec,c)::lst
      ) spec_tbl [] in
      List.sort
        Config.(fun (x,_) (y,_) -> String.compare (Spec.name x) (Spec.name y))
        fail_counts,
      exec_fails
     ))
  ) by_config) in
  let spec_fails, mount_fails =
    List.fold_left Config.(fun (specs,mounts) -> Model.(function
    | (Spec spec, path), f ->
      (Spec.name spec, ((spec, path), f))::specs,mounts
    | (Mount mnt, path), f ->
      specs,(Stack.name mnt, ((mnt, path), f))::mounts
    )) ([],[]) fails in
  List.map snd (sort_alist spec_fails),
  List.map snd (sort_alist mount_fails)

let arch_of_config = Fs_interface.Fs_spec_intf.(Config.(function
  | Model.Spec  { Spec.flavor = Spec.Linux }
  | Model.Mount { Stack.os = (Os.Linux,_) } -> Fs_types.ARCH_LINUX
  | Model.Spec  { Spec.flavor = Spec.Posix } -> Fs_types.ARCH_POSIX
  | Model.Spec  { Spec.flavor = Spec.Mac_os_x }
  | Model.Mount { Stack.os = (Os.Darwin,_) } -> Fs_types.ARCH_MAC_OS_X
  | Model.Spec  { Spec.flavor = Spec.FreeBSD }
  | Model.Mount { Stack.os = (Os.FreeBSD,_) } -> Fs_types.ARCH_FREEBSD
))

let string_of_eov =
  Fs_interface.Fs_printer_intf.input_string_of_rv_error_or_value

let strings_of_eovs = List.map string_of_eov

let html_continue_of_d_ret_lbl ({ CheckLib.rvs; reset }) =
  if reset
  then let rv_s = String.concat ", " (strings_of_eovs rvs) in
       <:html<<tr><td>continuing check with $str:rv_s$</td></tr>&>>
  else <:html<&>>

let html_error_row html = <:html<
  <tr><td class="bad">$html$</td></tr>
>>

let html_of_kind k = Printer.(<:html<$str:string_of_kind k$>>)

let html_of_d_kind r s =
  <:html<expected st_kind $html_of_kind s$ but got st_kind $html_of_kind r$>>

let html_of_perm p = Printer.(<:html<$str:string_of_perm p$>>)

let html_of_d_perm r s =
  <:html<expected st_perm $html_of_perm s$ but got st_perm $html_of_perm r$>>

let html_of_d_size r s =
  let soi = Int64.to_string in
  <:html<expected st_size $str:soi s$ but got st_size $str:soi r$>>

let html_of_d_nlink r s =
  <:html<expected st_nlink $int:s$ but got st_nlink $int:r$>>

let html_of_d_uid r s =
  let str = Printer.simple_string_of_uid in
  <:html<expected st_uid $str:str s$ but got st_uid $str:str r$>>

let html_of_d_gid r s =
  let str = Printer.simple_string_of_gid in
  <:html<expected st_gid $str:str s$ but got st_gid $str:str r$>>

let html_of_d_atime (Types.Float r) (Types.Float s) =
  <:html<expected st_atime $int:s$ but got st_atime $int:r$>>

let html_of_d_mtime (Types.Float r) (Types.Float s) =
  <:html<expected st_mtime $int:s$ but got st_mtime $int:r$>>

let html_of_d_ctime (Types.Float r) (Types.Float s) =
  <:html<expected st_ctime $int:s$ but got st_ctime $int:r$>>

let html_of_d_stat d = Stat.(
     (match d.d_st_kind  with None -> [] | Some (r,s) -> html_of_d_kind  r s)
    @(match d.d_st_perm  with None -> [] | Some (r,s) -> html_of_d_perm  r s)
    @(match d.d_st_size  with None -> [] | Some (r,s) -> html_of_d_size  r s)
    @(match d.d_st_nlink with None -> [] | Some (r,s) -> html_of_d_nlink r s)
    @(match d.d_st_uid   with None -> [] | Some (r,s) -> html_of_d_uid   r s)
    @(match d.d_st_gid   with None -> [] | Some (r,s) -> html_of_d_gid   r s)
    @(match d.d_st_atime with None -> [] | Some (r,s) -> html_of_d_atime r s)
    @(match d.d_st_mtime with None -> [] | Some (r,s) -> html_of_d_mtime r s)
    @(match d.d_st_ctime with None -> [] | Some (r,s) -> html_of_d_ctime r s)
)

let html_of_error err = <:html<$str:Printer.string_of_error err$>>

let html_of_value rv = <:html<$str:Printer.string_of_ret_value rv$>>

let html_of_d_names res _both spec =
  let res = match res with [] -> <:html<&>> | res ->
    let res = List.map (fun s -> <:html<$str:s$>>) res in
    html_error_row <:html<
      result contained unexpected names $list:res$
    >>
  in
  let spec = match spec with [] -> <:html<&>> | spec ->
    let spec = List.map (fun s -> <:html<$str:s$>>) spec in
    html_error_row <:html<
      spec expected names $list:spec$
    >>
  in
  <:html<
    $res$
    $spec$
  >>

let html_of_d_file_perm res spec = html_error_row <:html<
  expected permissions $str:Printer.string_of_perm spec$
  but got permissions $str:Printer.string_of_perm res$
>>

let html_of_type rv = Types.(
  let rv_s = match rv with
    | RV_none        -> "None"
    | RV_num _       -> "Number"
    | RV_bytes _     -> "Bytes"
    | RV_names _     -> "Names"
    | RV_stats _     -> "Stat"
    | RV_file_perm _ -> "File permissions"
  in
  <:html<$str:rv_s$>>
)

let html_of_d_type_error res spec = html_error_row <:html<
  expected return type $html_of_type res$
  but got return type $html_of_type spec$
>>

let html_of_d_ret_lbl_diff eov spec = CheckLib.(function
  | D_value (D_stats s) when Stat.is_d_zero s -> <:html<&>>
  | D_value (D_bytes None
                | D_names ([],_,[])
                | D_num None
                | D_file_perm None
                | D_type_error None
  )
  | D_error None -> <:html<&>>
  | D_value_not_error (res_val,spec_err) -> html_error_row <:html<
    expected error $html_of_error spec_err$ but got value $html_of_value res_val$
  >>
  | D_error_not_value (res_err,spec_val) -> html_error_row <:html<
    expected value $html_of_value spec_val$ but got error $html_of_error res_err$
  >>
  | D_error (Some (res,spec)) -> html_error_row <:html<
    expected error $html_of_error spec$ but got error $html_of_error res$
  >>
  | D_value (D_bytes (Some (res,spec))) ->
    html_error_row <:html<expected "$str:spec$" but got "$str:res$">>
  | D_value (D_stats s) -> html_error_row (html_of_d_stat s)
  | D_value (D_names (res, both, spec)) -> html_of_d_names res both spec
  | D_value (D_num (Some (res,spec))) -> html_error_row
    <:html<expected int $int:spec$ but got int $int:res$>>
  | D_value (D_file_perm (Some (res,spec))) ->
    html_error_row (html_of_d_file_perm res spec)
  | D_value (D_type_error (Some (res,spec))) ->
    html_error_row (html_of_d_type_error res spec)
)

let html_of_d_ret_lbl = CheckLib.(fun ({ error } as d_ret_lbl) ->
  let continue = html_continue_of_d_ret_lbl d_ret_lbl in
  let open Fs_interface.Fs_printer_intf in
  let open Fs_interface.Fs_spec_intf.Fs_types in
  match error with
  | None | Some ( D_ret_lbl_pid None
                | D_ret_lbl_errors_prohibited []
                | D_ret_lbl_unexpected ([],_)) -> <:html<<tr>
    <td class="bad">html_of_d_ret_lbl invariant violated</td>
    $continue$
  </tr>&>>
  | Some (D_ret_lbl_diff (_, diff, _)) when CheckLib.is_d_eov_zero diff ->
    <:html<<tr>
      <td class="bad">html_of_d_ret_lbl invariant violated: diff zero</td>
      $continue$
    </tr>&>>
  | Some (D_ret_lbl_pid (Some (Pid pid,Pid pid'))) -> <:html<<tr>
    <td class="bad">
      process ids do not match (pid $int:pid$ &lt;> pid $int:pid'$)
    </td>
    $continue$
  </tr>&>>
  | Some (D_ret_lbl_errors_prohibited errs) ->
    let spec_result_s = String.concat ", " (List.map string_of_error errs) in
    <:html<
      <tr><td class="bad">errors possible: $str:spec_result_s$</td></tr>
      $continue$
    >>
  | Some (D_ret_lbl_unexpected (missing,spec)) ->
    let result_s = String.concat ", " (strings_of_eovs missing) in
    let spec_result_s = String.concat ", " (strings_of_eovs spec) in
    <:html<
      <tr><td class="bad">unexpected results: $str:result_s$</td></tr>
      <tr><td class="bad">allowed are only: $str:spec_result_s$</td></tr>
      $continue$
    >>
  | Some (D_ret_lbl_diff (eov,diff,spec)) ->
    html_of_d_ret_lbl_diff eov spec diff
)

let html_of_eov prefix = Types.(function
  | Value (RV_stats stat) ->
    let stat_table = <:html<<table>
      <tr><td>st_dev</td><td>=</td><td>$int:stat.st_dev$;</td></tr>
      <tr><td>st_ino</td><td>=</td>
        <td>$str:Printer.string_of_inode stat.st_ino$;</td></tr>
      <tr><td>st_kind</td><td>=</td>
        <td>$str:Printer.string_of_kind stat.st_kind$;</td></tr>
      <tr><td>st_perm</td><td>=</td>
        <td>$str:Printer.string_of_perm stat.st_perm$;</td></tr>
      <tr><td>st_nlink</td><td>=</td><td>$int:stat.st_nlink$;</td></tr>
      <tr><td>st_uid</td><td>=</td>
        <td>$str:Printer.simple_string_of_uid stat.st_uid$;</td></tr>
      <tr><td>st_gid</td><td>=</td>
        <td>$str:Printer.simple_string_of_gid stat.st_gid$;</td></tr>
      <tr><td>st_rdev</td><td>=</td><td>$int:stat.st_rdev$;</td></tr>
      <tr><td>st_size</td><td>=</td>
        <td>$str:Int64.to_string stat.st_size$;</td></tr>
    </table>&>> in
    <:html<<div class="stat">$prefix$RV_stat {$stat_table$}</div>&>>
  | eov -> <:html<$str:string_of_eov eov$>>
)

let html_of_d_lbl = CheckLib.(function
  | D_dump (dump_dt, expected) -> <:html<
    <tr><td>$str:Trace.dump_to_string expected$</td></tr>
    <tr><td>$str:Dump.string_of_d_t dump_dt$</td></tr>
  >>
  | D_interp_dump_failed -> <:html<
    <tr><td>Interpretation of dump failed</td></tr>
  >>
  | D_check_dump_failed (err, _path) -> <:html<
    <tr><td>Execution of dump failed: $str:err$</td></tr>
  >>
  | D_ret_lbl d_ret_lbl -> html_of_d_ret_lbl d_ret_lbl
  | D_ret_lbl_empty _ -> <:html<<tr><td>ret_lbl_empty</td></tr>&>>
  | D_bad_state (Special (_,ss)) -> <:html<
    <tr><td>$str:string_of_special_states ss$</td></tr>
    <tr><td>no normal result states</td></tr>
  >>
  | D_bad_state (Empty _) -> <:html<<tr><td>no result states</td></tr>&>>
  | D_mixed_states (location, ss) -> <:html<
    <tr><td>MIXED STATES via $str:location$</td></tr>
    <tr><td>$str:string_of_special_states ss$</td></tr>
  >>
  | D_checklib_false_positive (Empty _) ->
    <:html<<tr><td>CHECKLIB ASSERTS THERE ARE NO STATES</td></tr>&>>
  | D_checklib_false_positive (Special (_,ss)) -> <:html<
    <tr><td>$str:string_of_special_states ss$</td></tr>
    <tr><td>CHECKLIB ASSERTS THERE ARE SPECIAL STATES</td></tr>
    <tr><td>THE SPEC DOES NOT AGREE</td></tr>
  >>
  | D_checklib_false_negative (Empty _, eovs) ->
    let eovs_html = List.rev_map (fun eov ->
      let prefix = <:html<&>> in
      <:html<<li>$html_of_eov prefix eov$</li>&>>
    ) eovs in
    <:html<
      <tr><td>THE SPEC ASSERTS THE STATE SET IS EMPTY</td></tr>
      <tr><td>The spec permitted:
        <ul>$list:eovs_html$</ul>
      </td></tr>
    >>
  | D_checklib_false_negative (Special (_,ss), _eovs) -> <:html<
    <tr><td>$str:string_of_special_states ss$</td></tr>
    <tr><td>CHECKLIB ASSERTS THERE ARE NORMAL STATES</td></tr>
    <tr><td>THE SPEC ASSERTS THERE ARE SPECIAL STATES</td></tr>
  >>
)

let html_of_dump_entry = Dump.(function
  | DE_file { file_path; file_node; file_size; file_sha } ->
    <:html<<tr>
      <td>$str:file_path$</td>
      <td>F</td>
      <td>$int:file_node$</td>
      <td>$int:file_size$</td>
      <td>$str:file_sha$</td>
    </tr>&>>
  | DE_dir { dir_path; dir_node } ->
    <:html<<tr>
      <td>$str:dir_path$</td>
      <td>D</td>
      <td>$int:dir_node$</td>
      <td/>
      <td/>
    </tr>&>>
  | DE_symlink { link_path; link_val } ->
    <:html<<tr>
      <td>$str:link_path$</td>
      <td>L</td>
      <td/>
      <td/>
      <td>$str:link_val$</td>
    </tr>&>>
  | DE_error (err, call, path) ->
    <:html<<tr>
      <td>$str:path$</td>
      <td>!</td>
      <td/>
      <td/>
      <td>$str:call$ -> $str:Printer.string_of_error err$</td>
    </tr>&>>
)

let html_of_dump dump = <:html<$list:List.map html_of_dump_entry dump$>>

let html_of_lbl string_of_lbl = Types.(Trace.(function
  | (has_pid, OS_simple_label (OS_RETURN (pid, eov))) ->
    let prefix =
      if has_pid
      then <:html<$str:Printer.string_of_pid pid$ &lt;- >>
      else <:html<&>>
    in
    html_of_eov prefix eov
  | lbl -> <:html<$str:string_of_lbl lbl$>>
))

let html_of_line arch = Trace.(function
  | Comment "@type script" -> <:html<<td class="meta">@type script</td>&>>
  | Comment "@type trace"  -> <:html<<td class="meta">@type trace</td>&>>
  | Comment s -> <:html<<td class="comment"># $str:s$</td>&>>
  | Newline -> <:html<<td><br/></td>&>>
  | Dump s -> <:html<<td class="dump">dump $str:s$</td>&>>
  | Dump_result (p,result) -> <:html<
    <td class="dump">dump-result $str:p$
    <table>
    $html_of_dump result$
    </table></td>&>>
  | Label (has_pid, lbl) ->
    let open Types in
    let string_of_lbl = input_string_of_extended_label arch in
    let klass = match lbl with
      | OS_simple_label (OS_CALL _) -> "call label"
      | OS_simple_label (OS_CREATE _ | OS_DESTROY _) -> "process label"
      | OS_simple_label OS_TAU -> "tau label"
      | OS_simple_label (OS_RETURN _)
      | OS_no_error_return _
      | OS_multiple_return _ -> "return label"
    in
    <:html<<td class=$str:klass$>$html_of_lbl string_of_lbl (has_pid,lbl)$</td>&>>
)

let html_of_numbered_line arch = function
  | (None, line)   -> <:html<<td/>$html_of_line arch line$&>>
  | (Some k, line) ->
    let k = string_of_int k in <:html<
    <td class="line" id=$str:k$><a href=$str:"#"^k$>$str:k$</a></td>
    $html_of_line arch line$
    >>

let html_of_d_trace_line arch ({ CheckLib.trace_ctxt; trace_line; d_lbls }) =
  let line = <:html<
    <tr>$html_of_numbered_line arch trace_line$</tr>
  >> in
  match d_lbls with
  | [] -> line
  | lbls -> <:html<
    $line$
    <tr><td/><td class="error">
      <table>$list:List.map html_of_d_lbl lbls$</table>
    </td></tr>
  >>

let html_of_config_result config html = <:html<
  <h4>$str:Config.name config$</h4>
  <div style="margin-bottom: 0.5em; margin-left: 1em">$html$</div>
>>

let rel_test_page_prefix = "../../../../../"

let html_of_diff_result path_fn rel_path_fn diff_result = Index.(
  let sexp_file = path_fn diff_result.diff_result.output.stdout in
  let klass = match is_check_ok sexp_file with
    | Ok -> "good" | Assertion_failure -> "warn" | Failure -> "bad"
  in
  let prefix = rel_test_page_prefix in
  let sexp_uri =
    prefix^(rel_path_fn diff_result.diff_result.output.stdout)
  in
  let stdout_uri = prefix^(rel_path_fn diff_result.diff_human.output.stdout) in
  let stderr_uri = prefix^(rel_path_fn diff_result.diff_human.output.stderr) in
  <:html<<tr>
    <th class=$str:klass$>$str:Config.Spec.name diff_result.diff_spec$</th>
    <td><a href=$str:sexp_uri$>sexp</a></td>
    <td><a href=$str:stdout_uri$>stdout</a></td>
    <td><a href=$str:stderr_uri$>stderr</a></td>
  </tr>&>>
  )

let just_html_of_script rel_path_fn script = Index.(
  let prefix = rel_test_page_prefix in
  let uri = prefix^(rel_path_fn script) in
  <:html<
    <table><tr><th>Script</th>
      <td><a href=$str:uri$>script</a></td>
    </tr></table>
  >>
)

let just_html_of_trace rel_script_fn rel_path_fn trace = Index.(
  let prefix = rel_test_page_prefix in
  let stdout_uri = prefix^(rel_path_fn trace.output.stdout) in
  let stderr_uri = prefix^(rel_path_fn trace.output.stderr) in
  <:html<
    <table><tr><th>Traced Execution</th>
      <td><a href=$str:stdout_uri$>stdout</a></td>
      <td><a href=$str:stderr_uri$>stderr</a></td>
    </tr></table>
    $just_html_of_script rel_script_fn trace.prev_phase$
  >>
)

let html_of_script suite config path script =
  let rel_path_fn = rel_path_of_script path suite in
  html_of_config_result config (just_html_of_script rel_path_fn script)

let html_of_trace suite config path trace =
  let rel_script_fn = rel_path_of_script path suite in
  let rel_path_fn = rel_path_of_test_file path suite in
  let prefix = Index.(match trace.success with
    | Ok -> <:html<&>>
    | Fail -> <:html<<strong class="fail">TRACE EXECUTION FAILED</strong>&>>
  ) in
  let html = just_html_of_trace rel_script_fn rel_path_fn trace in
  html_of_config_result config <:html<$prefix$$html$>>

let html_of_diff dir suite config path diff_trace diff_checks =
  let rel_script_fn = rel_path_of_script path suite in
  let path_fn = path_of_test_file dir path suite in
  let rel_path_fn = rel_path_of_test_file path suite in
  html_of_config_result config <:html<
    <table>
    <tr><th style="text-align: left">Checked Trace</th><td/><td/><td/></tr>
    $list:Index.fold_checks (fun diff lst ->
            (html_of_diff_result path_fn rel_path_fn diff)::lst
          ) [] diff_checks$
    </table>
    $just_html_of_trace rel_script_fn rel_path_fn diff_trace$
  >>

let call_compatible config call config' call' =
  (* this is awful but it's not my fault *)
  let open Fs_interface.Fs_printer_intf in
  let arch   = Arch.architecture_of_ty_arch (arch_of_config config) in
  let cmd_s  = input_string_of_os_label false arch  (Types.OS_CALL call) in
  let arch'  = Arch.architecture_of_ty_arch (arch_of_config config') in
  let cmd'_s = input_string_of_os_label false arch' (Types.OS_CALL call')
  in
  cmd_s = cmd'_s

let eov_compatible d_lbls eneov d_lbls' next = match d_lbls, d_lbls' with
  | [], [] -> true
  | _, _ -> d_lbls = d_lbls' && eneov = next
let eovs_compatible d_lbls _eneov d_lbls' _next = match d_lbls, d_lbls' with
  | [], [] -> true
  | _, _ -> false
let dump_compatible d_lbls _dump d_lbls' _dump' = match d_lbls, d_lbls' with
  | [], [] -> true
  | _, _ -> false

let label_compatible d_lbls (_,config) enlbl d_lbls' (_,config') next =
  Types.(match enlbl, next with
  | OS_RETURN (pid,eov), OS_RETURN (pid',eov') ->
    pid = pid' && eov_compatible d_lbls eov d_lbls' eov'
  | OS_CALL call, OS_CALL call' -> call_compatible config call config' call'
  | OS_TAU, OS_TAU -> true
  | OS_CREATE p, OS_CREATE p' -> p = p'
  | OS_DESTROY p, OS_DESTROY p' -> p = p'
  | _, _ -> false
  )

let ext_label_compatible d_lbls checked enlbl d_lbls' checked' next =
  Trace.(match enlbl, next with
  | OS_simple_label enos, OS_simple_label nextos ->
    label_compatible d_lbls checked enos d_lbls' checked' nextos
  | OS_no_error_return pid, OS_no_error_return pid' -> pid = pid'
  | OS_multiple_return (pid, eovs), OS_multiple_return (pid', eovs') ->
    pid = pid' && eovs_compatible d_lbls eovs d_lbls' eovs'
  | _, _ -> false (* TODO: there are relations between these not captured, yet *)
  )

let line_compatible (d_enline, checked, _) d_next checked' =
  let { CheckLib.trace_line = (k, enline); d_lbls } = d_enline in
  let { CheckLib.trace_line = (j, next); d_lbls = d_lbls' } = d_next in
  k = j &&
  Trace.(match enline, next with
  | Label (has_pid, extlbl), Label (has_pid', extlbl') ->
    has_pid = has_pid'
    && ext_label_compatible d_lbls checked extlbl d_lbls' checked' extlbl'
  | Dump_result (path, dump), Dump_result (path',dump') ->
    path = path' && dump_compatible d_lbls dump d_lbls' dump'
  | _, _ -> enline = next && d_lbls = d_lbls'
  )

let call_eq config call config' call' =
  (* this is awful but it's not my fault *)
  let open Fs_interface.Fs_printer_intf in
  let arch   = Arch.architecture_of_ty_arch (arch_of_config config) in
  let cmd_s  = input_string_of_os_label false arch  (Types.OS_CALL call) in
  let arch'  = Arch.architecture_of_ty_arch (arch_of_config config') in
  let cmd'_s = input_string_of_os_label false arch' (Types.OS_CALL call')
  in
  cmd_s = cmd'_s

let eov_eq eneov next = eneov = next
let eovs_eq eneov next = eneov = next
let dump_eq dump dump' = dump = dump'

let label_eq (_,config) enlbl (_,config') next =
  Types.(match enlbl, next with
  | OS_RETURN (pid,eov), OS_RETURN (pid',eov') ->
    pid = pid' && eov_eq eov eov'
  | OS_CALL call, OS_CALL call' -> call_eq config call config' call'
  | OS_TAU, OS_TAU -> true
  | OS_CREATE p, OS_CREATE p' -> p = p'
  | OS_DESTROY p, OS_DESTROY p' -> p = p'
  | _, _ -> false
  )

let ext_label_eq checked enlbl checked' next =
  Trace.(match enlbl, next with
  | OS_simple_label enos, OS_simple_label nextos ->
    label_eq checked enos checked' nextos
  | OS_no_error_return pid, OS_no_error_return pid' -> pid = pid'
  | OS_multiple_return (pid, eovs), OS_multiple_return (pid', eovs') ->
    pid = pid' && eovs_eq eovs eovs'
  | _, _ -> false
  )

let line_eq (d_enline, checked, _) d_next checked' =
  let { CheckLib.trace_line = (k, enline); d_lbls } = d_enline in
  let { CheckLib.trace_line = (j, next); d_lbls = d_lbls' } = d_next in
  k = j &&
  Trace.(match enline, next with
  | Label (has_pid, extlbl), Label (has_pid', extlbl') ->
    has_pid = has_pid'
    && d_lbls = d_lbls'
    && ext_label_eq checked extlbl checked' extlbl'
  | Dump_result (path, dump), Dump_result (path',dump') ->
    path = path' && d_lbls = d_lbls' && dump_eq dump dump'
  | _, _ -> enline = next && d_lbls = d_lbls'
  )

let rec put_in_envelope env next checked = match env with
  | [] -> [next, checked, []]
  | ((line, c, cl) as ln)::rest ->
    if line_eq ln next checked
    then (line, c, checked::cl)::rest
    else (line, c, cl)::(put_in_envelope rest next checked)

let merge_of_next_d_trace_line lines =
  List.fold_left (fun (merged,lines,terminated) line ->
    match merged, line with
    | [], (next::rest, spec, config) ->
      let checked = (spec, config) in
      [put_in_envelope [] next checked],
      (rest, spec, config)::lines,
      terminated
    | [(enline::_) as envelope], (next::rest, spec, config) ->
      let checked = (spec, config) in
      (if line_compatible enline next checked
       then [put_in_envelope envelope next checked]
       else [put_in_envelope []      next checked; envelope]),
      (rest, spec, config)::lines,
      terminated
    | envelopes, (next::rest, spec, config) ->
      let checked = (spec, config) in
      (match List.partition (function
      | hd::_ -> line_compatible hd next checked
      | [] -> assert false
       ) envelopes with
      | [], _ -> (put_in_envelope [] next checked)::envelopes
      | e::rest, incompat ->
        ((put_in_envelope e next checked)::rest@incompat)
      ),
      (rest, spec, config)::lines,
      terminated
    | _, ([], spec, config) ->
      merged, lines, (spec, config)::terminated
  ) ([], [], []) lines

let html_of_checked (_spec, config) = match config with
  | Config.Model.Mount stack ->
    let os = Config.Os.versioned_name stack.Config.Stack.os in
    let libc = Config.Libc.name (fst stack.Config.Stack.libc) in
    let fs = Config.Fs.name (fst stack.Config.Stack.fs) in
    <:html<$str:os$/$str:libc$/$str:fs$ >>
  | Config.Model.Spec spec ->
    <:html<$str:Config.Spec.(Descr.name (to_descr spec))$>>

let html_of_group = function
  | [] -> <:html<&>>
  | group -> <:html<
    <span class="checked">$list:List.map html_of_checked group$</span>
  >>

let html_of_envelope lines =
  let html_lines = List.map (fun (line, (spec, config), others) ->
    let html = html_of_d_trace_line (arch_of_config config) line in
    let group = (spec, config)::others in
    <:html<<tr><td/><td class="group">
      $html_of_group group$
      $html$
    </td></tr>&>>
  ) lines in
  <:html<<tr><td/><td class="envelope"><table>
    $list:html_lines$
  </table></td></tr>&>>

let html_of_diverged envelopes = <:html<<tr><td/>
  <td class="diverged"><table>
    $list:List.map html_of_envelope envelopes$
  </table></td></tr>
>>

let html_of_merged_d_trace_line = function
  | [] -> <:html<&>>
  | [[line, (_,config), _]] -> html_of_d_trace_line (arch_of_config config) line
  | [envelope] -> html_of_envelope envelope
  | diverged -> html_of_diverged diverged

let html_of_terminated = function
  | [] -> <:html<&>>
  | checked -> <:html<<tr><td/>
    <td class="terminated">TERMINATED: $html_of_group checked$</td>
  </tr>&>>

let html_of_d_traces d_traces =
  let lines = List.(flatten (map (fun (config, spec_alist) ->
    map (fun (spec, diff) -> (diff, spec, config)) spec_alist
  ) d_traces)) in
  let rec html_lines html_list lines =
    let merged, remaining, terminated = merge_of_next_d_trace_line lines in
    let html = html_of_merged_d_trace_line merged in
    match remaining with
    | [] -> html::html_list
    | _ ->
      html_lines (html::(html_of_terminated terminated)::html_list) remaining
  in
  List.rev (html_lines [] lines)

let html_of_test_results results suite dir =
  let html_files = Index.(fold_config (fun config_result lst ->
    match config_result with
    | { config; path; test=Diff { diff_trace; diff_checks } } ->
      (html_of_diff dir suite config path diff_trace diff_checks)::lst
    | { config; path; test=Trace trace } ->
      (html_of_trace suite config path trace)::lst
    | { config; path; test=Script script } ->
      (html_of_script suite config path script)::lst
  ) [] results) in
  let d_traces = Index.(fold_config (function
    | { config; path; test=Diff { diff_checks } } -> fun lst ->
      let checked_traces = fold_checks (fun { diff_spec; diff_result } list ->
        let sexp_file =
          path_of_test_file dir path suite diff_result.output.stdout
        in
        let lines = load_sexp sexp_file in
        (diff_spec, lines)::list
      ) [] diff_checks in
      (config, checked_traces) :: lst
    | _ -> fun lst -> lst
  ) [] results) in
  let html_d_traces = html_of_d_traces d_traces in
  <:html<
    <h3>Traces</h3>
    $list:html_files$
    <h3>Combined Trace</h3>
    <table>$list:html_d_traces$</table>
  >>

let test_link name =
  <:html<<a href=$str:"test/"^name^"/index.html"$>$str:name$</a>&>>

let generate_test_page ({ Index.suite; script; result }) test_dir =
  let title = suite ^ " / " ^ script in
  let dir = Dir.dir ~parent:test_dir script in
  let index_page = page ~title <:html<
  $html_of_test_results result suite dir$
  >> in
  write_html dir "index.html" index_page

let row_of_checks ((_score,script),config_checks) =
  let cells = List.map (fun (_config_name,(_config, checks)) ->
    match checks with
    | [] -> <:html<<td class="fail">&#x2620;</td>&>>
    | checks ->
      let ok = List.fold_left (fun check_ok (_spec,ok) ->
        and_check check_ok ok
      ) Ok checks in
      match ok with
      | Ok -> <:html<<td class="good">&#x2713;</td>&>>
      | Failure -> <:html<<td class="bad">&#x2717;</td>&>>
      | Assertion_failure -> <:html<<td class="warn">?</td>&>>
  ) config_checks in
  <:html<
    <tr class="check"><th class="row">$test_link script$</th>$list:cells$</tr>
  >>

let html_of_wide_branch
    ?(proj=(fun (name,_) -> <:html<$str:name$>>)) ((_,(m,_)) as branch) =
  <:html<<th colspan=$int:m$ class="wide">$proj branch$</th>&>>

let default_mount_html (stack,_path) =
    let fs_name = Config.(Fs.versioned_name stack.Stack.fs) in
    <:html<$str:fs_name$>>

let header_of_mount_fails
    ?(mount_html=default_mount_html) first_cols mount_fails =
  let os_configs = Hashtbl.create 4 in
  List.iter (fun ((c, path), (m, suite_alist)) ->
    let os_name = Config.(Os.versioned_name c.Stack.os) in
    let cur_width, libc_configs =
      try Hashtbl.find os_configs os_name
      with Not_found -> (0,Hashtbl.create 4)
    in
    Hashtbl.replace os_configs os_name (cur_width + m, libc_configs);
    let libc_name = Config.(Libc.versioned_name c.Stack.libc) in
    let cur_width, v_configs =
      try Hashtbl.find libc_configs libc_name
      with Not_found -> (0,Hashtbl.create 4)
    in
    Hashtbl.replace libc_configs libc_name (cur_width + m, v_configs);
    let commit = c.Config.Stack.fs_test_version in
    let cur_width, fs_configs =
      try Hashtbl.find v_configs commit
      with Not_found -> (0,Hashtbl.create 4)
    in
    Hashtbl.replace v_configs commit (cur_width + m, fs_configs);
    let fs_name = Config.(Fs.versioned_name c.Stack.fs) in
    let cur_width =
      try fst (Hashtbl.find fs_configs fs_name)
      with Not_found -> 0
    in
    Hashtbl.replace fs_configs fs_name
      (cur_width + m, (mount_html (c,path), suite_alist))
  ) mount_fails;
  let sorted_list_of_ht ht =
    sort_alist (Hashtbl.fold (fun k v l -> (k,v)::l) ht [])
  in
  let flat_vmap f r = List.(flatten (map (fun (_,v) -> f v) r)) in
  let os_row = sorted_list_of_ht os_configs in
  let libc_row = flat_vmap (fun (_,h) -> sorted_list_of_ht h) os_row in
  let v_row = flat_vmap (fun (_,h) -> sorted_list_of_ht h) libc_row in
  let fs_row = flat_vmap (fun (_,h) -> sorted_list_of_ht h) v_row in
  let first_spc = List.rev_map (fun _ -> <:html<<th/>&>>) first_cols in
  let proj (_name,(_m,(link, _))) = link in
  <:html<
    <tr>$list:first_spc$$list:List.map html_of_wide_branch os_row$</tr>
    <tr>$list:first_spc$$list:List.map html_of_wide_branch libc_row$</tr>
    <tr>$list:first_spc$$list:List.map html_of_wide_branch v_row$</tr>
    <tr>$list:first_cols$$list:List.map (html_of_wide_branch ~proj) fs_row$</tr>
  >>, fs_row

let score_of_check = function
  | Ok -> 0
  | Failure -> -100
  | Assertion_failure -> -10

let index_of_tests suite_idx dir =
  let test_dir = Dir.dir ~parent:dir "test" in
  let tests = Index.(
    fold_tests (fun ({ suite; script; result } as test_entry) tests ->
      let (score,lst) = fold_config (fun config_result (score,lst) ->
        match config_result with
        | { config = (Config.Model.Mount stack); path;
            test = Diff { diff_checks } } ->
          let (check_score,checks) = Index.fold_checks
            (fun { diff_spec; diff_result } (ok,checks) ->
              let sexp_file =
                path_of_test_file dir path suite diff_result.output.stdout
              in
              let check_ok = is_check_ok sexp_file in
              (score + score_of_check check_ok, (diff_spec, check_ok)::checks)
            ) (0,[]) diff_checks
          in
          (check_score + score,
           (Config.Stack.name stack, ((stack, path), checks))::lst)
        | { config = (Config.Model.Spec _); test = Diff _ } ->
          (* TODO: don't skip spec tests *)
          (score,lst)
        | { config = (Config.Model.Mount stack); path;
            test = Trace { success = Fail } } ->
          (score + -1, (Config.Stack.name stack, ((stack, path), []))::lst)
        | { config = (Config.Model.Spec _);
            test = Trace { success = Fail } } ->
          (* TODO: don't skip spec tests *)
          (score, lst)
        | { test = Trace { success = Ok } } ->
          failwith "fs_test_html.ml: successful unchecked traces found: check!"
        | _ ->
          failwith
            "fs_test_html.ml: don't know what to do with non-check non-trace"
      ) (0,[]) result in
      let lst = sort_alist lst in
      generate_test_page test_entry test_dir;
      ((score,script),lst)::tests
    ) [] suite_idx) in
  (* assumes some things about config run counts and homogeneity... *)
  let header =
    let tbl = Hashtbl.create 8 in
    List.iter (fun (_,alist) ->
      List.iter (fun (stack_name,(config,checks)) ->
        let old_max = try snd (Hashtbl.find tbl stack_name)
          with Not_found -> 0
        in
        let max_checks = if checks = [] then max old_max 1
          else List.length checks
        in
        Hashtbl.replace tbl stack_name (config,max_checks)
      ) alist
    ) tests;
    let config_alist = Hashtbl.fold (fun k (c,m) a ->
      (k,(c,(m,())))::a
    ) tbl [] in
    let config_alist = sort_alist config_alist in
    let mount_fails  = List.map snd config_alist in
    fst (header_of_mount_fails [ <:html<<th/>&>> ] mount_fails)
  in
  <:html<<table>
    $header$
    $list:List.map row_of_checks (sort_gen_alist tests)$
  </table>
  >>

let eov_of_line = Trace.(function
  | Label (_,OS_simple_label (Types.OS_RETURN (_pid, eov))) -> Some eov
  | _ -> None
)

let other_fld =
  (fun { other } -> other),
  (fun cats other -> { cats with other })

let dir_nlink_low_fld =
  (fun { dir_nlink_low } -> dir_nlink_low),
  (fun cats dir_nlink_low -> { cats with dir_nlink_low })

let dir_nlink_high_fld =
  (fun { dir_nlink_high } -> dir_nlink_high),
  (fun cats dir_nlink_high -> { cats with dir_nlink_high })

let file_nlink_low_fld =
  (fun { file_nlink_low } -> file_nlink_low),
  (fun cats file_nlink_low -> { cats with file_nlink_low })

let file_nlink_high_fld =
  (fun { file_nlink_high } -> file_nlink_high),
  (fun cats file_nlink_high -> { cats with file_nlink_high })

let cat (proj,inj) cats location line_opt other =
  try
    let errs = List.assoc location (proj cats) in
    let others = List.remove_assoc location (proj cats) in
    inj cats ((location,(line_opt,other)::errs)::others)
  with Not_found ->
    inj cats ((location,[line_opt,other])::(proj cats))

let categorize_d_stat location line_opt line cats d_lbl = Stat.(function
  | { d_st_nlink = None } -> cat other_fld cats location line_opt d_lbl
  | { d_st_nlink = Some (x,y) } as d_stat -> Types.(match eov_of_line line with
    | Some (Value (RV_stats { st_kind = S_IFDIR })) ->
      if is_d_zero { d_stat with d_st_nlink = None }
      then begin
        if x < y
        then cat dir_nlink_low_fld cats location line_opt d_lbl
        else if x > y
        then cat dir_nlink_high_fld cats location line_opt d_lbl
        else cat other_fld cats location line_opt d_lbl
      end
      else cat other_fld cats location line_opt d_lbl
    | Some (Value (RV_stats { st_kind = S_IFREG })) ->
      if is_d_zero { d_stat with d_st_nlink = None }
      then begin
        if x < y
        then cat file_nlink_low_fld cats location line_opt d_lbl
        else if x > y
        then cat file_nlink_high_fld cats location line_opt d_lbl
        else cat other_fld cats location line_opt d_lbl
      end
      else cat other_fld cats location line_opt d_lbl
    | Some (Value _ | Error _) | None ->
      cat other_fld cats location line_opt d_lbl
  )
)

let categorize_d_lbl location line_opt line cats d_lbl =
  CheckLib.(match d_lbl with
  | D_ret_lbl {
    error = Some (D_ret_lbl_diff (_, D_value (D_stats d_stat), _))
  } ->
    categorize_d_stat location line_opt line cats d_lbl d_stat
  | D_checklib_false_negative (Empty _, _) -> cats
  | other -> cat other_fld cats location line_opt other
)

let href_of_location ({ suite; script }) line_opt =
  "../../suite/"^suite^"/test/"^script^"/index.html"^(match line_opt with
  | Some k -> "#"^(string_of_int k)
  | None -> ""
  )

let html_of_location ({ suite; script } as location) line_opt =
  let href = href_of_location location line_opt in
  match line_opt with
  | None -> <:html<<a href=$str:href$>$str:suite$/$str:script$</a>&>>
  | Some k ->
    <:html<<a href=$str:href$>$str:suite$/$str:script$#$int:k$</a>&>>

let html_loc_list_of_cat lst =
  let f (loc,d_lbls) =
    let li (line_opt,_) = <:html<<li>$html_of_location loc line_opt$</li>&>> in
    <:html<$list:List.map li d_lbls$>>
  in
  <:html<<ul>$list:List.map f lst$</ul>&>>

let html_list_of_cat lst =
  let f (loc,d_lbls) =
    let li (line_opt,d_lbl) = <:html<
      <li><div>$html_of_location loc line_opt$</div>
        <table>$html_of_d_lbl d_lbl$</table>
      </li>
    >> in
    <:html<<li><div>$html_of_location loc None$</div>
      <ul>
      $list:List.map li d_lbls$
      </ul>
    </li>&>>
  in
  <:html<<ul>$list:List.map f lst$</ul>&>>

let html_of_cats config dir cats =
  let dir_nlink_low = html_loc_list_of_cat cats.dir_nlink_low in
  let dir_nlink_high = html_loc_list_of_cat cats.dir_nlink_high in
  let file_nlink_low = html_loc_list_of_cat cats.file_nlink_low in
  let file_nlink_high = html_loc_list_of_cat cats.file_nlink_high in
  let other = html_list_of_cat cats.other in
  <:html<
  <h3>Error Index</h3>
  <h4>$str:Config.name config$</h4>
  <h5>st_nlink errors</h5>
  <ul>
  <li><h6>directory nlink too low</h6>
    $dir_nlink_low$
  </li>
  <li><h6>directory nlink too high</h6>
    $dir_nlink_high$
  </li>
  <li><h6>file nlink too low</h6>
    $file_nlink_low$
  </li>
  <li><h6>file nlink too high</h6>
    $file_nlink_high$
  </li>
  </ul>
  <h5>other errors</h5>
  $other$
  >>

let errors_of_config config suites dir path =
  let cats = ref empty_cats in
  Index.(iter_suite_tests (fun _ -> function
  | { result = (Trace _ | Script _) } -> ()
  | { suite; script; result = Diff { diff_checks } } ->
    fold_checks (fun { diff_result } () ->
      let sexp_file =
        path_of_test_file dir path suite diff_result.output.stdout
      in
      let lines = load_sexp sexp_file in
      let open CheckLib in
      List.iter (function
      | { d_lbls = [] } -> ()
      (* TODO: what about ignored d_lbls like checklib_false_negative empty? *)
      | { trace_line = (line_opt,line); d_lbls } ->
        let location = { suite; script } in
        cats :=
          List.fold_left (categorize_d_lbl location line_opt line) !cats d_lbls
      ) lines
    ) () diff_checks
  ) suites);
  html_of_cats config dir !cats

let generate_error_page ({ Index.config; path; test }) error_dir =
  let name = Config.name config in
  let title = name^" error summary" in
  let dir = Dir.dir ~parent:error_dir path in
  let head = match config with
    | Config.Model.Spec _ -> None
    | Config.Model.Mount stack ->
      let descr = Config.Stack.to_descr stack in
      let name = Config.Fs.name descr.Config.Stack.Descr.fs in
      let pieces = Fs_test_util.split [] name '/' in
      match pieces with
      | [] | [ _ ] -> None
      | _::rest ->
        let href = List.fold_left (fun s _ -> "../"^s) "" rest in
        Some <:html<<base href=$str:href$/>&>>
  in
  let index_page = page ?head ~title <:html<
  $errors_of_config config test dir path$
  >> in
  write_html dir "index.html" index_page

let error_href name = "errors/"^name^"/index.html"

let collate_errors by_config error_dir (stack,path) =
  let anchor = default_mount_html (stack,path) in
  let href = error_href path in
  let config = Hashtbl.find by_config (Config.Model.Mount stack) in
  generate_error_page config error_dir;
  <:html<<a href=$str:href$>$anchor$</a>&>>

let suite_link name =
  <:html<<a href=$str:"suite/"^name^"/index.html"$>$str:name$</a>&>>

let generate_suite_page name suite_idx suite_dir =
  let title = name ^ " test suite" in
  let dir = Dir.dir ~parent:suite_dir name in
  let index_page = page ~title <:html<
  $index_of_tests suite_idx dir$
  >> in
  write_html dir "index.html" index_page

let count_fails by_suite (config, suite_fun) =
  (config, Index.fold_suites
    (fun name _ (m,l) ->
      let fails, exec_fails = suite_fun name in
      (max (List.length fails) m, (name, (fails, exec_fails))::l)
    ) (0,[]) by_suite)

let index_of_suite by_config by_suite (spec_fails,mount_fails) html_dir =
  (* TODO: render the spec tests, too *)
  let _spec_fail_counts = List.map (count_fails by_suite) spec_fails in
  let mount_fail_counts = List.map (count_fails by_suite) mount_fails in
  let first_cols = [ <:html<<th/>&>>; <:html<<th>Total</th>&>> ] in
  let error_dir = Dir.dir ~parent:html_dir "errors" in
  let mount_html = collate_errors by_config error_dir in
  let header, fs_row =
    header_of_mount_fails ~mount_html first_cols mount_fail_counts
  in
  let suite_dir = Dir.dir ~parent:html_dir "suite" in
  let suites = sort_alist (Index.map_suite (fun name suite_idx ->
    let count = Hashtbl.length suite_idx in
    let fail_cols = List.map (fun (_,(_,(_,suite_alist))) ->
      (* assumes some things about check run counts and homogeneity... *)
      let fails,exec_fails = List.assoc name suite_alist in
      let cols = List.map (fun (_spec,(fails,ass_fails)) ->
        let klass =
          if fails > 0 then "bad"
          else if ass_fails > 0 then "warn"
          else if exec_fails > 0 then "fail"
          else "good"
        in
        let cell =
          ((if fails > 0
            then [string_of_int fails]
            else [])
           @(if ass_fails > 0
             then [string_of_int ass_fails ^"?"]
             else [])
           @(if exec_fails > 0
             then [string_of_int exec_fails ^"!"]
             else []))
        in
        let cell = match cell with [] -> "0" | s -> String.concat "/" s in
        <:html<<td class=$str:klass^" num"$>$str:cell$</td>&>>
      ) fails in
      <:html<$list:cols$>>
    ) fs_row in
    generate_suite_page name suite_idx suite_dir;
    (name,
     <:html<
     <tr class="summary"><th class="row">$suite_link name$</th>
         <td class="num">$int:count$</td>$list:fail_cols$</tr>
     >>)
  ) by_suite) in
  <:html<<table>$header$$list:List.map snd suites$</table>&>>

let generate dir =
  let index_file = dir / "index.diffs" in
  let by_config = Index.read index_file in
  let by_suite = Index.index_suite_test by_config in
  let dir = Dir.dir dir in
  let fails = suite_fail_by_config dir by_config in
  let html_dir = Dir.dir ~parent:dir "html" in
  let index_page = page ~title:"Test results" <:html<
  $index_of_suite by_config by_suite fails html_dir$
  >> in
  write_html html_dir "index.html" index_page;
  Printf.printf "Browse to file://%s\n%!" (Dir.path html_dir "index.html")
