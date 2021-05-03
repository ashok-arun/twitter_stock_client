open Yojson.Basic.Util

type t = {
  sentiment_api_key : string;
  td_api_key : string;
  td_refresh_token : string;
  twitter_bearer : string;
  email_address : string;
  email_password : string;
  algo_trailing_stop : float;
  algo_max_price : float;
  algo_max_volume : int;
  algo_strategy : string;
}

let make_config filename =
  let conf = filename |> Yojson.Basic.from_file in
  {
    sentiment_api_key =
      conf |> member "Sentiment" |> member "api_key" |> to_string;
    td_api_key =
      conf |> member "TD_Ameritrade" |> member "api_key" |> to_string;
    td_refresh_token =
      conf |> member "TD_Ameritrade" |> member "refresh_token"
      |> to_string;
    twitter_bearer =
      conf |> member "Twitter" |> member "bearer" |> to_string;
    email_address =
      conf |> member "Email" |> member "address" |> to_string;
    email_password =
      conf |> member "Email" |> member "password" |> to_string;
    algo_trailing_stop =
      conf |> member "Algo" |> member "trailing_stop" |> to_float;
    algo_max_price =
      conf |> member "Algo" |> member "max_price" |> to_float;
    algo_max_volume =
      conf |> member "Algo" |> member "max_volume" |> to_int;
    algo_strategy =
      conf |> member "Algo" |> member "strategy" |> to_string;
  }

let config = make_config "config.json"

let sentiment_api_key = config.sentiment_api_key

let td_api_key = config.td_api_key

let td_refresh_token = config.td_refresh_token

let twitter_bearer = config.twitter_bearer

let email_address = config.email_address

let email_password = config.email_password

let algo_trailing_stop = config.algo_trailing_stop

let algo_max_price = config.algo_max_price

let algo_max_volume = config.algo_max_volume

let algo_strategy = config.algo_strategy

(* checks if configs are good *)
let check_configs () =
  let string_fields =
    [
      sentiment_api_key;
      td_api_key;
      td_refresh_token;
      twitter_bearer;
      email_address;
      email_password;
    ]
  in
  if List.exists (fun x -> x = "") string_fields then
    print_string
      "missing field in config.json (Twitter, Email, TD_Ameritrade or \
       Sentiment)"
  else if
    algo_max_price < 0.0
    || algo_trailing_stop < 0.0
    || algo_max_volume < 0
  then
    print_string
      "missing fields in Algo in config.json or invalid max_price, \
       trailing_stop, or max_volume"
  else if
    not
      (algo_strategy = "rolling_max" || algo_strategy = "moving_average")
  then
    print_string
      "invalid Algo strategy in config.json. Needs to be rolling_max \
       or moving_average"
  else print_string "configs are good"
