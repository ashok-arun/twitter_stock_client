open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* API Documentation: https://text2data.com/Integration *)
(* API Usage:
   https://text2data.com/Account/Manage?message=ChangeSecretSuccess *)

(* helper function which makes the payload for a HTTP Post request. It
   takes the payload generates as a tuple list [(arg, value)] and
   returns a string representation of a json to feed into Cohttp.
   Precondition: [lst] is non-empty *)
let of_json_string lst =
  let string_from_pair (a, b) = Printf.sprintf {|"%s" : "%s"|} a b in
  let rec of_json_string_tr acc = function
    | [] -> failwith "invalid lst passed into of_json_string"
    | [ x ] -> acc ^ string_from_pair x ^ "\n}"
    | x :: y -> of_json_string_tr (acc ^ string_from_pair x ^ ",\n") y
  in
  of_json_string_tr "{\n" lst

(* default args for text2data *)
let args =
  [
    ("IsTwitterContent", "true");
    ("PrivateKey", Config.sentiment_api_key);
    ("Secret", "secret");
    ("RequestIdentifier", "");
  ]

(* endpoint of text2data *)
let text2data_endpoint = "http://api.text2data.com/v3/analyze/"

let headers =
  Header.init () |> fun h ->
  Header.add h "Content-Type" "application/json"

(* makes a post request with endpoint, args and [phrase] *)
let get_post phrase =
  let updated_args = ("DocumentText", phrase) :: args in
  let post_body =
    Cohttp_lwt.Body.of_string (of_json_string updated_args)
  in
  let body =
    Client.post ~headers ~body:post_body
      (Uri.of_string text2data_endpoint)
    >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let code = resp |> Response.status |> Code.code_of_status in
    (code, body)
  in
  match Lwt_main.run body with
  | code, body -> if code = 200 then body else failwith "request failed"

(* Checks through response from http post request and returns true if
   DocSentimentPolarity is not equal to "-"*)
let is_bullish phrase =
  phrase |> get_post |> Yojson.Basic.from_string
  |> member "DocSentimentPolarity"
  |> to_string
  |> fun x -> x <> "-"
