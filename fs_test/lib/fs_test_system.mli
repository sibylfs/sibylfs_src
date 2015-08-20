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

exception Command_failure of string * string * string

type process = {
  pid : int;
  stdout : Unix.file_descr;
  stdin : Unix.file_descr;
  stderr : Unix.file_descr;
}

val all : string -> string
val megs : int -> int64

val string_of_exec_args : string -> string array -> string

val string_of_signal : int -> string
val string_of_status : Unix.process_status -> string
val ignore_failure : (unit -> unit) -> unit -> unit

val subproc : chdir:string -> string -> string array -> unit

val continue : string -> string array -> unit
val exit_command : string -> string array -> 'a -> (int -> 'a option) -> 'a

val read_command :
  ?exit_code:int -> ?env:string array -> string -> string list
val read_command_err :
  ?exit_code:int -> ?env:string array -> string -> string list

val create_process_exec_args : string -> string array -> string array -> process

val create_process : string -> string array -> string array -> process

val read_into_buf : ?block:bool -> Unix.file_descr -> Buffer.t -> int

val drain_into_buf : Unix.file_descr -> Buffer.t -> int

val waitpid : Unix.wait_flag list -> int -> int * Unix.process_status

val tee :
  ?quiet:bool -> ?no_stderr:bool -> ?errfile:string -> process -> string -> int

val kill_children : unit -> unit

val get_system : unit -> Fs_test_config.Os.t

val create_new_user : unit -> int * string

val delete_user : ?force:bool -> string -> unit

val create_new_group : unit -> int * string

val delete_group : ?force:bool -> string -> unit

val put_user_in_group : string -> string -> unit
