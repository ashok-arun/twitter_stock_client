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

(* Begin Manager to scan through every twitter account, updating tweets
   in accounts.json, and purchasing relevent stocks*)
let () = Manager.start

(* let () = print_endline "Enter your email\n"; print_string "> "; let
   email = read_line () in Lwt_main.run (send_email conf email "AAPL") *)

(* let () = print_endline "Enter your TDAmeritrade APP APIKEY\n";
   print_string "> "; let apikey = read_line () in print_endline
   "Enter\n your TDAmeritrade Access Token\n"; print_string "> "; let
   token = read_line () in let bearer = get_bearer apikey token in
   print_endline (account_info bearer) *)

(* let () = let test_tweet = Twitter.most_recent_tweet "3110Project" in
   (* TODO: Verify whether the tweet actually mentions a real stock*)
   let ticker = Twitter.get_tickers test_tweet in print_endline
   (test_tweet ^ "...\n Ticker: " ^ if ticker = "" then "none" else
   ticker ^ ".... Bullish: " ^ string_of_bool (Sentiment.is_bullish
   test_tweet)) *)

(* let () = print_endline "Enter your TDAmeritrade APP APIKEY\n";
   print_string "> "; let apikey = read_line () in let ticker_json =
   Stock.ticker_info_json "tsla" apikey in let price = Stock.stock_price
   ticker_json "tsla" in let volume = Stock.stock_volume ticker_json
   "tsla" in print_endline ("tsla" ^ "... Price: " ^ Float.to_string
   price ^ "... Volume: " ^ Int.to_string volume) *)
