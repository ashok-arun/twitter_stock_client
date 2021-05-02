open Cohttp
open Yojson.Basic.Util
open Curl

let get_bearer apikey token =
  let headers =
    Header.init () |> fun h ->
    Header.add h "Content-Type" "application/x-www-form-urlencoded"
  in
  let data =
    [
      ("client_id", apikey);
      ("refresh_token", token);
      ("grant_type", "refresh_token");
    ]
  in
  let body = data_map data in
  let json =
    get_curl_data "https://api.tdameritrade.com/v1/oauth2/token" headers
      body
  in
  json |> Yojson.Basic.from_string |> member "access_token" |> to_string

let account_info bearer =
  let headers =
    Header.init () |> fun h ->
    Header.add h "Authorization" ("Bearer " ^ bearer)
  in
  get_curl "https://api.tdameritrade.com/v1/accounts" headers
