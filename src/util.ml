(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* A few generic utility functions *)

let compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  = fun f g x -> f (g (x))

let ( @* ) = compose

let default d = function Some x -> x | None -> d

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let string_split_chars chars str =
  let len = String.length str in
  let rec split pos =
    let rec lookup i =
      if i >= len then raise Not_found
      else if String.contains chars str.[i] then i
      else lookup (succ i)
    in
    try
      let i = lookup pos in
      if i > pos then String.sub str pos (i - pos) :: split (succ i)
      else split (succ i)
    with Not_found | Invalid_argument _ ->
        [ String.sub str pos (len - pos) ]
  in
  split 0

let is_prefix pfx str =
  let pfxlen = String.length pfx in
  let rec check i = i >= pfxlen || pfx.[i] = str.[i] && check (i+1) in
  String.length str >= pfxlen && check 0

let ends_with_escape s =
  let rec aux n = n >= 0 && s.[n] = '\\' && not (aux (n-1))
  in aux (String.length s - 1)

let count_leading_spaces s =
  let rec aux i =
    if i >= String.length s || s.[i] <> ' ' then i
    else aux (i+1)
  in
  aux 0

let shorten_string n s =
  match string_split '\n' s with
  | [] -> ""
  | [s] ->
      if String.length s <= n then s
      else
        let n1 = (n - 3) / 2 in
        let n2 = n - 3 - n1 in
        String.sub s 0 n1
        ^ "..."
        ^ String.sub s (String.length s - n2) n2
  | s1::r1::r ->
      let s2 =
        let rec last x = function x::r -> last x r | [] -> x in
        last r1 r
      in
      let l1 = String.length s1 and l2 = String.length s2 in
      let n1 = min l1 (max ((n-3) / 2)  (n-3 - l2)) in
      let n2 = min l2 (n - 3 - n1) in
      String.sub s1 0 n1
      ^ "..."
      ^ String.sub s2 (l2 - n2) n2

(* Hexadecimal printing of strings *)

let c0 = int_of_char '0'
let ca = int_of_char 'a'
let cA = int_of_char 'A'

let string_to_hex s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter  
    begin fun c ->
      let c = int_of_char c in
      let a, b = c / 16, c mod 16 in
      let char_of_hex = function
        | c when c <= 9 -> char_of_int (c0 + c)
        | c -> char_of_int (cA + c - 10)
      in
      Buffer.add_char buf (char_of_hex a);
      Buffer.add_char buf (char_of_hex b)
    end s;
  Buffer.contents buf

let string_of_hex h =
  let len = String.length h / 2 in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    let char_to_hex = function
      | c when c >= 'a' && c <= 'f' -> int_of_char c - ca + 10
      | c when c >= 'A' && c <= 'F' -> int_of_char c - cA + 10
      | c -> int_of_char c - c0
    in
    let a, b = char_to_hex h.[i * 2], char_to_hex h.[i * 2 + 1] in
    Buffer.add_char buf (char_of_int (a * 16 + b))
  done;
  Buffer.contents buf
