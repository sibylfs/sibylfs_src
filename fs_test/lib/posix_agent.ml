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

type user
type 'a system

type 'a command =
| Stop  : 'a command
| Trans : 'a -> 'a command

type error = [ `Error of exn ]

type 'a system_result = [ `Ok of 'a | error ]
type 'a user_result = [ `Result of 'a | error ]

type (-'a,+'b) channel = {
  pid  : int;
  sock : Unix.file_descr;
  stop : unit -> Unix.process_status system_result;
  command : 'a -> 'b user_result;
}

type ('a,'b) t = {
  run : 'a -> 'b;
  mutable channels : ('a,'b) channel list;
}

(* Duplicate an fd k fds into the fd-space. *)
let dup2_k k fd =
  let rec iter = function
    | (high_fd::fds,0) ->
      List.iter Unix.close fds;
      high_fd
    | ([],0) -> Unix.dup fd
    | (high_fd::fds,k) -> iter ((Unix.dup high_fd)::high_fd::fds,k - 1)
    | ([],k) -> iter ([Unix.dup fd],k - 1)
  in
  iter ([],k)

let fork_proc t ~listen_fn ~request_fn =
  let psock, csock = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let child = Unix.fork () in
  if child = 0
  then begin
    Unix.close psock;
    let high_fd = dup2_k 8 csock in
    Unix.close csock;
    List.iter (fun { sock } -> Unix.close sock) t.channels;
    let request_in = Unix.in_channel_of_descr high_fd in
    let reply_out = Unix.out_channel_of_descr high_fd in
    listen_fn request_in reply_out
  end
  else begin
    Unix.close csock;
    let request_out = Unix.out_channel_of_descr psock in
    let reply_in = Unix.in_channel_of_descr psock in
    request_fn child psock request_out reply_in
  end

let create ~run = { run; channels = []; }

let deploy t ~username ~uid ~gid ~root =
  let listen_fn request_in reply_out =
    (* Become another user *)
    begin
      try
        Unix.initgroups username gid;
        Unix.chroot root;
        Unix.chdir "/";
        Unix.setgid gid;
        Unix.setuid uid;
        ignore (Unix.umask 0o022);
        assert (Unix.umask 0o022 = 0o022)
      with e ->
        (* Report our failed transition *)
        Marshal.to_channel reply_out (`Error e) [];
        flush reply_out;
        exit 1
    end;
    (* Confirm we transitioned successfully *)
    Marshal.to_channel reply_out (`Ok ()) [];
    flush reply_out;
    while true do
      let (cmd : 'a command) = Marshal.from_channel request_in in
      match cmd with
      | Stop -> exit 0
      | Trans cmd ->
        let result : 'b user_result =
          try `Result (t.run cmd)
          with e -> `Error e
        in
        Marshal.to_channel reply_out result [];
        flush reply_out
    done;
    exit 0
  in
  let request_fn pid sock request_out reply_in =
    Printf.eprintf "%d creating agent %d\n%!" (Unix.getpid ()) pid;
    let stop () =
      Printf.eprintf "%d stopping agent %d\n%!" (Unix.getpid ()) pid;
      Marshal.to_channel request_out Stop [];
      flush request_out;
      `Ok (snd (Unix.waitpid [] pid))
    in
    let command cmd =
      Marshal.to_channel request_out (Trans cmd) [];
      flush request_out;
      let result : 'b user_result = Marshal.from_channel reply_in in
      result
    in
    match ((Marshal.from_channel reply_in) : unit system_result) with
    | `Ok () ->
      let channel = { pid; sock; stop; command; } in
      t.channels <- channel :: t.channels;
      channel
    | `Error exn ->
      (* These will be uncatchable due to extensible constructor
         registration. *)
      raise exn
  in
  fork_proc t ~listen_fn ~request_fn

let stop_all t =
  List.iter (fun { stop } -> match stop () with
  | `Ok _ -> () (* TODO: care about return code? *)
  | `Error exn -> raise exn
  ) t.channels;
  t.channels <- []
