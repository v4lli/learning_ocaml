open Cmdliner
open Printf
open Char
open String
open Str

(* Trim num fields from the left of the string; helper function *)
let rec ltrim haystack needle num =
  if num = 0 then haystack else
    let occurence = index haystack needle in
    ltrim (sub haystack (occurence + 1)
      ((length haystack) - occurence - 1)) needle (pred num)

(* Find the position into the string of the separator for the num-th field *)
let rec find_nth_occurence haystack needle num pos =
  if num = 0 then pos - 1 else
    find_nth_occurence haystack needle (pred num)
      (succ (index_from haystack pos needle))

(* Return the num-th field, separated by needle, from haystack *)
let get_field_from_string haystack needle num =
  let trimmed = ltrim haystack needle (num - 1) in
  if (contains trimmed needle) then
    let occurence = index haystack needle in
    sub trimmed 0 occurence
  else
    trimmed

(* Return a range of fields, separated by needle, from within haystack *)
let rec get_fields_from_string haystack needle l r =
  let ltrimmed = ltrim haystack needle (l - 1) in
  sub ltrimmed 0 (find_nth_occurence haystack needle (r - l + 1) 0)

(*
 * Return a tuple of integer options, denoting the "left" and "right" boundary
 * of the wanted field. None means 'open end' in that direction (i.e. all
 * preceding or following fields are wanted).
 *
 * Should be able to handle "5", "2-5", "5-", "-3".
 *)
let arg_to_tuple str =
  if contains str '-' then
    let dash = index str '-' in
    if (dash = 0) then
      (None, Some (int_of_string (sub str 1 (dash - 1))))
    else if (dash = (length str) - 1) then
      (Some (int_of_string (sub str 0 dash)), None)
    else
      (Some (int_of_string (sub str 0 dash)),
        Some (int_of_string (sub str (dash + 1)
        ((length str) - dash - 1))))
  else
    let same = int_of_string str in
    (Some same, Some same)

(* Split a string into a string list, separated by comma. *)
let split_args arg_str =
  List.map arg_to_tuple (Str.split (Str.regexp ",") arg_str)

(* Return a substring from haystack as specified by tuple and needle. *)
let cut_from_str haystack needle tuple =
  match tuple with
   | Some l, Some r -> if (l = r) then
                         (get_field_from_string haystack needle l)
                       else
                         (get_fields_from_string haystack needle l r)
   | None, Some r -> "implement me"
   | Some l, None -> "implement me"
   | _ -> ""

(* Return a comma-separated list of processed field strings *)
let assemble_field_strs input needle fields =
  String.concat "," (List.map (fun x -> cut_from_str input needle x) fields)

let main delim fields =
  let fields = split_args fields in
  let str = assemble_field_strs (read_line ()) delim fields in
  printf "%s\n" str

let delim =
  let doc = "Delimiter" in
  Arg.(value & opt char 'a' & info ["d"] ~docv:"DELIM" ~doc)
let field =
  let doc = "Field specification" in
  Arg.(value & opt string "1" & info ["f"] ~docv:"FIELD" ~doc)

let cmd =
  let doc = "cut clone in ocaml" in
  Term.(pure main $ delim $ field),
  Term.info "cut" ~version:"1.0" ~doc
let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
