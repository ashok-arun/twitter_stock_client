open Algo

let buy ticker api_key =
  let curr_time = Core.Time.now () in
  let string_time =
    Core.Time.to_sec_string curr_time Core.Time.Zone.utc
  in
  let rolling_max =
    Algo.calc_rolling_max ticker api_key |> string_of_float
  in
  (*TODO -> change with actual buying price*)
  let bought_price =
    AlgoUtils.get_current_price ticker api_key |> string_of_float
  in
  AlgoUtils.csv_add "stocks.csv"
    [ "BUY"; ticker; bought_price; string_time; rolling_max ]

let sell ticker api_key =
  let curr_time = Core.Time.now () in
  let string_time =
    Core.Time.to_sec_string curr_time Core.Time.Zone.utc
  in
  (*TODO -> change with actual buying price*)
  let bought_price =
    AlgoUtils.get_current_price ticker api_key |> string_of_float
  in
  AlgoUtils.csv_add "stocks.csv"
    [ "SELL"; ticker; bought_price; string_time; "-1" ]
