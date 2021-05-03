open Stock
open Trade
open Letters
open Str
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt
open Lwt.Infix
open Str
open Twitter
open Sentiment
open Json_writer
open Email

(* Helper function to structure JSON properly when writing new tweets to
   file*)
let build_json body = "{\"accounts\": [" ^ body ^ "]  \n  }"

(* Helper regex to replace line breaks with the empty string (keeps JSON
   formatted properly)*)
let trim message =
  let r = Str.regexp "\n" in
  Str.global_replace r "" message

(* Manager.start runs this function: Scans through every account in
   accounts.json, and updates all with most recent tweet If the tweets
   are new, bullish, and match our market standards, we buy! TODO: Scan
   through current portfolio, checking to sell TODO: Run this function
   on a timer or cron job *)
let start =
  print_endline "Enter your email\n";
  print_string "> ";
  let email = read_line () in
  let rec main_driver portf new_json =
    match portf with
    | h :: t ->
        let tweet_prior = h |> member "last_tweet" |> to_string in
        let handle = h |> member "handle" |> to_string in
        let current_tweet = trim (most_recent_tweet handle) in
        let next_json =
          "{\"handle\": \"" ^ handle ^ "\", \"last_tweet\": \""
          ^ current_tweet ^ "\"}"
        in
        let final_json =
          if new_json <> "" then new_json ^ "," ^ next_json
          else next_json
        in
        let () =
          print_endline
            ( handle ^ ": " ^ tweet_prior ^ "(prior). Current: "
            ^ current_tweet )
        in
        if tweet_prior = current_tweet then main_driver t final_json
        else
          let ticker = get_tickers current_tweet in
          if ticker = "" || is_bullish current_tweet = false then
            main_driver t final_json
          else if
            let ticker_json = ticker_info_json ticker in
            check_stock ticker ticker_json
          then
            let () = Trader.buy ticker in
            let () =
              Lwt_main.run (send_email conf email ticker);
              print_endline (ticker ^ " bought!")
            in
            main_driver t final_json
          else main_driver t final_json
    | [] -> new_json
  in
  let portfolio =
    "accounts.json" |> Yojson.Basic.from_file |> member "accounts"
    |> to_list
  in
  let new_json_to_write = main_driver portfolio "" in
  Json_writer.update_json (build_json new_json_to_write) "accounts.json"
