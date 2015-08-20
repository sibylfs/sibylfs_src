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

type t = string list

let split_string delimiter name =
  let rec doit part acc =
    let open String in
    let len = length part in
    let idx = try index part delimiter with _ -> len in
    let fst = sub part 0 idx in
    let idx' = idx + 1 in
    if idx' <= len then
      let rt = sub part idx' (len - idx') in
      doit rt (fst :: acc)
    else
      fst :: acc
  in
  List.rev (doit name [])

let of_string : string -> t = split_string '/'
let to_string (p:t) : string = String.concat "/" p

let concat p1 p2 = match List.rev p1, p2 with
  | "" :: p1, "" :: p2 | "" :: p1, p2 | p1, "" :: p2 | p1, p2 ->
    List.rev_append p1 p2

let resolve path =
  let rec remove_dots parts outp = match parts, outp with
    | ".."::r, a::rt -> remove_dots r  rt
    | ".."::r, []    -> raise Not_found
    | "."::r , rt    -> remove_dots r  rt
    | r::rs  , rt    -> remove_dots rs (r :: rt)
    | []     , rt    -> List.rev rt
  in
  remove_dots path []
