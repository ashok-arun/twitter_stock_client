open Hashtbl

(* This list represents the custom keywords the user wants to look for
   and it's respective stock ticker *)
let custom_pairings_list =
  [ ("tesla", "TSLA"); ("gamestonk", "GME"); ("gamestop", "GME") ]

(* initializing dictionary containing mappings from key word to stock
   ticker*)
let custom_pairings = Hashtbl.create 64

(* converts list [lst] representation of mappings to hashtable
   representation *)
let hash_pairings lst =
  let rec hash_pairings_tr lst tbl =
    match lst with
    | h :: t ->
        Hashtbl.add tbl (fst h) (snd h);
        hash_pairings_tr t tbl
    | [] -> ()
  in
  hash_pairings_tr lst custom_pairings

(* takes in string [s] first searches for index of first occurence of
   '$<TICKER>'. Then clips off everything after first occurence and
   lastly searches for keywords in [custom_pairings]. If neither are
   found it returns empty string. *)
let custom_parse (s : string) =
  if Hashtbl.length custom_pairings = 0 then
    hash_pairings custom_pairings_list;
  if s = "" then ""
  else
    let regex_stock = Str.regexp "[$][a-zA-Z]+" in
    let ind =
      try Str.search_forward regex_stock s 0
      with Not_found -> String.length s - 1
    in
    let target_s = String.sub s 0 ind in
    let word_list =
      target_s |> String.lowercase_ascii |> String.split_on_char ' '
    in
    let res =
      List.find_opt
        (fun x -> Hashtbl.find_opt custom_pairings x <> None)
        word_list
    in
    match res with
    | None ->
        if ind = String.length s - 1 then ""
        else
          String.sub (Str.matched_string s) 1
            (String.length (Str.matched_string s) - 1)
    | Some x -> Hashtbl.find custom_pairings x
