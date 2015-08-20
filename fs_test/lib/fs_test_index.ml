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

open Sexplib.Std

module Config = Fs_test_config

type path = string with sexp

type output = {
  stdout : path;
  stderr : path;
} with sexp

type success =
| Ok
| Fail
with sexp

type 'a result = {
  prev_phase : 'a;
  success    : success;
  output     : output;
} with sexp

type script = path with sexp
type trace = script result with sexp

type 'a check = (Config.Spec.t, 'a) Hashtbl.t with sexp

type diff_result = {
  diff_spec : Config.Spec.t;
  diff_result : unit result;
  diff_human : unit result;
} with sexp

type diff = {
  diff_checks : diff_result check;
  diff_trace : trace;
} with sexp

type stage =
| Script of script
| Trace of trace
| Diff of diff
with sexp

type 'a test_entry = {
  suite   : path;
  script  : path;
  result  : 'a;
} with sexp

type 'a config_result = {
  config : Config.t;
  path   : path;
  test   : 'a ;
} with sexp

type 'a suite  = (path, 'a) Hashtbl.t with sexp
type 'a test   = (path, 'a) Hashtbl.t with sexp
type 'a config = (Config.t, 'a) Hashtbl.t with sexp

type config_suite_tests =
  stage test_entry test suite config_result config
with sexp
type suite_test_configs = stage config_result config test_entry test suite

let create_check_of diff =
  let tbl = Hashtbl.create 8 in
  Hashtbl.replace tbl diff.diff_spec diff;
  tbl

let empty_suite_table () : 'a suite = Hashtbl.create 16
let empty_test_table () : 'a test = Hashtbl.create 16
let empty_config_table () : 'a config = Hashtbl.create 8

let put_test_map test_index test result_fn =
  let entry_opt =
    try Some (Hashtbl.find test_index test)
    with Not_found -> None
  in
  Hashtbl.replace test_index test (result_fn entry_opt)

let tests_of_suite suite_tests suite =
  try Hashtbl.find suite_tests suite
  with Not_found ->
    let tbl = empty_test_table () in
    Hashtbl.replace suite_tests suite tbl;
    tbl

let config_result_of_config path config = {
  config; path;
  test = empty_suite_table ();
}

let add_config config_index config_result =
  Hashtbl.add config_index config_result.config config_result

let map_suite f index =
  Hashtbl.fold (fun name suite_idx lst -> (f name suite_idx)::lst) index []

let fold_config f acc index = Hashtbl.fold (fun _ -> f) index acc

let map_config f index =
  fold_config (fun config_result lst -> (f config_result)::lst) [] index

let fold_checks f acc index = Hashtbl.fold (fun _ -> f) index acc

let fold_tests f acc index = Hashtbl.fold (fun _ -> f) index acc

let fold_suites f acc index = Hashtbl.fold f index acc

let iter_suite_tests f index =
  Hashtbl.iter (fun _ suite ->
    Hashtbl.iter (fun _ test_entry -> f suite test_entry) suite;
  ) index

let iter_config_suite_tests f index =
  Hashtbl.iter (fun _ { config; path; test } ->
    iter_suite_tests (f config path) test
  ) index

let read file_name =
  try
    config_suite_tests_of_sexp (Sexplib.Sexp.load_sexp file_name)
  with Sexplib.Pre_sexp.Of_sexp_error (exn, t) ->
    Printf.eprintf "deserialization error: %s\n%!" (Printexc.to_string exn);
    exit 1

let write index file_name =
  let oc = open_out file_name in
  Sexplib.Sexp.output_hum oc (sexp_of_config_suite_tests index);
  close_out oc

let index_suite_test : config_suite_tests -> suite_test_configs =
  fun by_config ->
    let suite_tbl = empty_suite_table () in
    iter_config_suite_tests (fun config path _ { suite; script; result } ->
      let test_tbl =
        try Hashtbl.find suite_tbl suite
        with Not_found ->
          let tbl = empty_test_table () in
          Hashtbl.replace suite_tbl suite tbl;
          tbl
      in
      let test =
        try Hashtbl.find test_tbl script
        with Not_found -> { suite; script; result=empty_config_table () }
      in
      add_config test.result { config; path; test = result };
      Hashtbl.replace test_tbl script test
    ) by_config;
    suite_tbl
