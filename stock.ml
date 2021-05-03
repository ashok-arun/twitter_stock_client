open Cohttp
open Yojson.Basic.Util
open Curl

let headers =
  Header.init () |> fun h ->
  Header.add h "Content-Type" "application/json"

(* Retrieves a json object as a string of the [ticker] info*)
let ticker_info_json ticker =
  get_curl
    ( "https://api.tdameritrade.com/v1/marketdata/quotes?apikey="
    ^ Config.td_api_key ^ "&symbol=" ^ ticker )
    headers

(* Gets the price history of a stock from time called. Ticker must be in
   all caps. Assumes all parameters are valid to make request. See API
   Documentation for help *)
let get_price_history
    ?(period_type = "day")
    ?(period = "2")
    ?(freq_type = "minute")
    ?(freq = "1")
    ticker =
  let url =
    "https://api.tdameritrade.com/v1/marketdata/"
    ^ String.uppercase_ascii ticker
    ^ "/pricehistory" ^ "?apikey=" ^ Config.td_api_key ^ "&periodType="
    ^ period_type ^ "&period=" ^ period ^ "&frequencyType=" ^ freq_type
    ^ "&frequency=" ^ freq
  in
  get_curl url headers

(* "TSLA": { "assetType": "EQUITY", "assetMainType": "EQUITY", "cusip":
   "88160R101", "symbol": "TSLA", "description": "Tesla, Inc. - Common
   Stock", "bidPrice": 639, "bidSize": 500, "bidId": "P", "askPrice":
   639.01, "askSize": 100, "askId": "P", "lastPrice": 639.01,
   "lastSize": 0, "lastId": "P", "openPrice": 601.75, "highPrice":
   637.66, "lowPrice": 591.01, "bidTick": " ", "closePrice": 611.29,
   "netChange": 27.72, "totalVolume": 39432359, "quoteTimeInLong":
   1617148792835, "tradeTimeInLong": 1617148799448, "mark": 639,
   "exchange": "q", "exchangeName": "NASD", "marginable": true,
   "shortable": true, "volatility": 0.0251, "digits": 4, "52WkHigh":
   900.4, "52WkLow": 89.28, "nAV": 0, "peRatio": 836.1, "divAmount": 0,
   "divYield": 0, "divDate": "", "securityStatus": "Normal",
   "regularMarketLastPrice": 635.62, "regularMarketLastSize": 3290,
   "regularMarketNetChange": 24.33, "regularMarketTradeTimeInLong":
   1617134400591, "netPercentChangeInDouble": 4.5347,
   "markChangeInDouble": 27.71, "markPercentChangeInDouble": 4.533,
   "regularMarketPercentChangeInDouble": 3.9801, "delayed": true,
   "realtimeEntitled": false } *)

type info = {
  symbol : string;
  start : float;
  high : float;
  low : float;
  price : float;
  volume : int;
  close : float;
  change : float;
}

let info_of_json j =
  {
    symbol = j |> member "symbol" |> to_string;
    start = j |> member "openPrice" |> to_float;
    high = j |> member "highPrice" |> to_float;
    low = j |> member "lowPrice" |> to_float;
    price = j |> member "lastPrice" |> to_float;
    volume = j |> member "totalVolume" |> to_int;
    close = j |> member "closePrice" |> to_float;
    change = j |> member "netChange" |> to_float;
  }

let from_json ticker json =
  json |> Yojson.Basic.from_string
  |> member (String.uppercase_ascii ticker)
  |> info_of_json

let stock_price ticker json = (from_json ticker json).price

let stock_volume ticker json = (from_json ticker json).volume

let check_price ticker json =
  if Config.algo_max_price >= stock_price ticker json then true
  else false

let check_volume ticker json =
  if Config.algo_max_volume >= stock_volume ticker json then true
  else false

let check_stock ticker json =
  if check_price ticker json && check_volume ticker json then true
  else false
