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

val removeDuplicatesByAux :
  ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val removeDuplicatesBy : ('a -> 'a -> bool) -> 'a list -> 'a list
val removeDuplicates : 'a Lem_basic_classes.eq_class -> 'a list -> 'a list

val set_from_finset :
  'a Lem_basic_classes.setType_class -> 'a Lem_support.finset -> 'a Pset.set
val finset_from_set : 'a -> 'b Pset.set -> 'b Lem_support.finset
val finset_equal :
  'a Lem_basic_classes.setType_class ->
  'a Lem_support.finset -> 'a Lem_support.finset -> bool
val instance_Basic_classes_Eq_Fs_prelude_Finset_finset_dict :
  'a Lem_basic_classes.setType_class ->
  'a Lem_support.finset Lem_basic_classes.eq_class
val finset_empty : unit -> 'a Lem_support.finset
val finset_is_empty : 'a Lem_support.finset -> bool
val finset_insert : 'a -> 'a Lem_support.finset -> 'a Lem_support.finset
val finset_singleton : 'a -> 'a Lem_support.finset
val finset_union :
  'a Lem_support.finset -> 'a Lem_support.finset -> 'a Lem_support.finset
val finset_bigunion :
  'a Lem_support.finset Lem_support.finset -> 'a Lem_support.finset
val finset_image :
  ('a -> 'b) -> 'a Lem_support.finset -> 'b Lem_support.finset
val finset_bigunion_image :
  ('a -> 'b Lem_support.finset) ->
  'a Lem_support.finset -> 'b Lem_support.finset
val finset_filter :
  ('a -> bool) -> 'a Lem_support.finset -> 'a Lem_support.finset
val finset_partition :
  ('a -> bool) ->
  'a Lem_support.finset -> 'a Lem_support.finset * 'a Lem_support.finset
val finset_choose : 'a Lem_support.finset -> 'a
val finset_any : ('a -> bool) -> 'a Lem_support.finset -> bool
val finset_all : ('a -> bool) -> 'a Lem_support.finset -> bool
val distinct_list_from_finset_by :
  ('a -> 'a -> bool) -> 'a Lem_support.finset -> 'a list
val distinct_list_from_finset :
  'a Lem_basic_classes.eq_class -> 'a Lem_support.finset -> 'a list
val finset_size_by : ('a -> 'a -> bool) -> 'a Lem_support.finset -> int
val finset_size :
  'a Lem_basic_classes.eq_class -> 'a Lem_support.finset -> int
val finset_memBy : ('a -> 'b -> bool) -> 'a -> 'b Lem_support.finset -> bool
val finset_mem : 'a -> 'b -> 'b Lem_support.finset -> bool
val finset_cleanup :
  ('a -> 'a -> bool) -> 'a Lem_support.finset -> 'a Lem_support.finset

type ('dom, 'zcod) fmap =
    Finmap of ('dom, 'zcod) Pmap.map
val map_from_fmap : ('a, 'b) fmap -> ('a, 'b) Pmap.map
val fmap_empty : 'a Lem_map.mapKeyType_class -> unit -> ('a, 'b) fmap
val fmap_update : 'a -> ('b, 'c) fmap -> 'b * 'c -> ('b, 'c) fmap
val fmap_remove : 'a -> ('b, 'c) fmap -> 'b -> ('b, 'c) fmap
val fmap_update_option :
  'a -> ('b, 'c) fmap -> 'b * 'c option -> ('b, 'c) fmap
val fmap_from_list :
  'a Lem_map.mapKeyType_class -> ('a * 'b) list -> ('a, 'b) fmap
val fmap_lookup : 'a -> ('b, 'c) fmap -> 'b -> 'c option
val fmap_dom : 'a -> 'b -> ('c, 'd) fmap -> 'c Lem_support.finset
val fmap_range : 'a Lem_map.mapKeyType_class ->
  'b Lem_basic_classes.setType_class -> ('a, 'b) fmap -> 'b Lem_support.finset
val fmap_image : 'a -> 'b -> ('c -> 'd) -> ('e, 'c) fmap -> ('e, 'd) fmap
val fmap_in_dom : 'a -> 'b -> 'c -> 'd -> ('d, 'e) fmap -> bool
val fmap_bindings :
  'a ->
  'b Lem_basic_classes.setType_class ->
  'c Lem_basic_classes.setType_class -> ('b, 'c) fmap -> ('b * 'c) list
val fmap_fold :
  'a -> 'b -> 'c -> ('d -> 'e -> 'f -> 'f) -> ('d, 'e) fmap -> 'f -> 'f
val fmap_size : 'a -> 'b -> ('c, 'd) fmap -> int
val fmap_map : 'a -> ('b -> 'c) -> ('d, 'b) fmap -> ('d, 'c) fmap
