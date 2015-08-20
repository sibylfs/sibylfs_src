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

type 'a diff = ('a * 'a) option with sexp

let diff a b = if a = b then None else Some (a,b)

let inter_diff_map (fn_l,fn_c,fn_r) cmp l l' =
  let l  = List.sort cmp l in
  let l' = List.sort cmp l' in

  let rec group (only, both, only') l l' =
    match (l, l') with
    | ([], _) ->
      (List.rev_map fn_l only,
       List.rev_map fn_c both,
       List.rev_map fn_r (l' @ only'))
    | (_, []) ->
      (List.rev_map fn_l (l @ only),
       List.rev_map fn_c both,
       List.rev_map fn_r only')
    | (e1 :: l, e2 :: l') ->
      if cmp e1 e2 = 0
      then group (only, (e1, e2) :: both, only') l l'
      else if cmp e1 e2 < 0
      then group (e1::only, both, only')     l       (e2::l')
      else group (only,     both, e2::only') (e1::l) l'
  in
  group ([], [], []) l l'

let inter_diff cmp = inter_diff_map ((fun x -> x),(fun x -> x),(fun x -> x)) cmp
