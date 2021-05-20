open Algo

let buy ticker =
  let curr_time = Core.Time.now () in
  let string_time =
    Core.Time.to_sec_string curr_time Core.Time.Zone.utc
  in
  let curr_price = AlgoUtils.get_current_price ticker in
  let rolling_max = Algo.calc_rolling_max ticker in

  let profit_target =
    AlgoUtils.calc_target curr_price rolling_max
      Config.algo_profit_target Config.algo_error_margin
  in

  (*TODO -> change with actual buying price*)
  let bought_price = curr_price |> string_of_float in
  AlgoUtils.csv_add "stocks.csv"
    [
      "BUY";
      ticker;
      bought_price;
      string_time;
      profit_target |> string_of_float;
    ]

let sell ticker =
  let curr_time = Core.Time.now () in
  let string_time =
    Core.Time.to_sec_string curr_time Core.Time.Zone.utc
  in
  (*TODO -> change with actual buying price*)
  let bought_price =
    AlgoUtils.get_current_price ticker |> string_of_float
  in
  AlgoUtils.csv_add "stocks.csv"
    [ "SELL"; ticker; bought_price; string_time; "-1" ]
