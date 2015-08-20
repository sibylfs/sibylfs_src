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
module System = Fs_test_system

let int_suff = "-int.trace"
let chk_suff = "-check.trace"

let exec_dir = Filename.dirname Sys.argv.(0)
let check_command = Filename.concat exec_dir "check.native"
let posix_command = Filename.concat exec_dir "posix.native"

let argc = Array.length Sys.argv
let argr = ref 1

let quiet =
  if argc > !argr && Sys.argv.(!argr) = "-q"
  then (incr argr; true)
  else false

let file_raw =
  if argc > !argr
  then let i = !argr in incr argr; Sys.argv.(i)
  else ""

let arch_args = System.(match get_system () with
  | Fs_test_config.Os.Linux -> [|"-arch"; "linux"|]
  | Fs_test_config.Os.FreeBSD -> [|"-arch"; "freebsd"|]
  | Fs_test_config.Os.Darwin -> [|"-arch"; "mac_os_x"|]
)

let rest_args =
  if argc > !argr
  then Array.(
    append
      (sub Sys.argv !argr (length Sys.argv - !argr))
      arch_args
  )
  else arch_args

let result_dir = "results"

let results filename = Filename.concat result_dir filename

let check_suffix = Filename.check_suffix

let env = Array.append (Unix.environment ()) [|"OCAMLRUNPARAM=b"|]

let tee = System.tee

;;

if check_suffix file_raw int_suff
then begin
  let file = Filename.chop_suffix file_raw int_suff in
  let ident = Filename.basename file in
  if check_suffix file "check"
  then begin
    printf "Interpreting %s with check ...\n%!" ident;
    let process = System.create_process check_command
      [|"-arch"; "linux"; file_raw|]
      env
    in
    exit
      (tee ~quiet process (results ("check_results-"^ident)))
  end
  else if check_suffix file "posix"
  then begin
    printf "Interpreting %s with posix ...\n%!" ident;
    let process = System.create_process posix_command
      (Array.append
         [|file_raw;
           "-o"; results ("posix_results-"^ident^chk_suff);
         |]
         rest_args
      ) env
    in
    exit
      (tee ~quiet process (results ("posix_results-"^ident)))
  end
  else
    let args = Array.append [|"-v"; file_raw|] rest_args in
    let process = System.create_process posix_command args env in
    let trace   = results ("posix_results-"^ident^chk_suff) in
    let errfile = results ("posix_results-"^ident^".stderr") in
    let exit_code = tee ~quiet:true ~no_stderr:true ~errfile process trace in
    match exit_code with
    | 0 ->
      let args = Array.append [|"-v"; trace|] arch_args in
      let process = System.create_process check_command args env in
      let check = results ("diff_results-"^ident^chk_suff) in
      let errfile = results ("diff_results-"^ident^".stderr") in
      let exit_code = tee ~quiet:true ~no_stderr:true ~errfile process check in
      exit exit_code
    | k ->
      printf "%s failed with exit code %d\n%!"
        (System.string_of_exec_args posix_command args) k;
      exit k
end
else if check_suffix file_raw chk_suff
  then begin
    let file = Filename.chop_suffix file_raw chk_suff in
    let ident = Filename.basename file in
    if check_suffix file "posix"
    then begin
      printf "Processing %s with posix ...\n%!" ident;
      let process = System.create_process posix_command
        (Array.append [|file_raw; "-v"|] rest_args) env
      in
      exit
        (tee ~quiet process (Filename.concat result_dir ("posix_results-"^ident)))
    end
    else begin
      printf "Processing %s with check ...\n%!" ident;
      let process = System.create_process check_command
        [|"-arch"; "linux"; "-v"; file_raw|] env
      in
      exit
        (tee ~quiet process (Filename.concat result_dir ("check_results-"^ident)))
    end
  end
else (print_endline "Error: argument is not a trace file"; exit 1)
