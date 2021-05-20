open Stock
open AlgoUtils

(* keeps track of highest values reached by stocks that are currently
   being held. Each entry is {Ticker : Highest_Price} *)
let high_dictionary = Hashtbl.create 1000

(* index of rolling max value in stocks.csv *)
let rolling_max_index = 4

(* takes in a stock [ticker] for TD Ameritrade and returns the highest
   price in a period defined by *)
let calc_rolling_max ticker =
  let highs = AlgoUtils.parse (Stock.get_price_history ticker) "high" in
  let max_high = function
    | [] ->
        invalid_arg
          "Could not calculate rolling max. Price history was empty"
    | h :: t -> List.fold_left max h t
  in
  max_high highs

(* checks if [current_price] falls below the *)
let hit_trailing_stop stock current_price =
  let max_price = Hashtbl.find high_dictionary stock in
  current_price
  < Float.sub max_price (Float.mul max_price Config.algo_trailing_stop)

(* checks "stocks.csv" if [current_price] of [stock] passing rolling max *)
let hit_rolling_max stock current_price =
  current_price
  >= ( member_of_last_purchase stock rolling_max_index "stocks.csv"
     |> float_of_string )

(* considers [stock] to have "broken" rolling max if it passes with a
   margin of [rebuy_thresh] *)
let breaks_rolling_max ?(rebuy_thresh = 0.02) stock =
  let rolling_max =
    member_of_last_purchase stock rolling_max_index "stocks.csv"
    |> float_of_string
  in
  get_current_price stock > rolling_max +. (rolling_max *. rebuy_thresh)

(* Takes in a stock ticker and decides if it should sell. Returns
   boolean tuple (sell_stock, keep_watching). It could decide to sell
   for two reasons: 1. It hits rolling max. 2. It hits trailing stop
   loss. If it hits rolling max, then (true, true), if it hits trailing
   stop, then (true, false), else (false, false) *)
let keep_stock stock =
  let current_price = AlgoUtils.get_current_price stock in
  AlgoUtils.log_high stock current_price high_dictionary;
  let stopped_out = hit_trailing_stop stock current_price in
  (* TODO -> deal with max not being higher than price *)
  let maxed_out = hit_rolling_max stock current_price in
  let sell_stock = if stopped_out || maxed_out then true else false in
  let keep_watching =
    if maxed_out && not stopped_out then true else false
  in
  (sell_stock, keep_watching)

(* looks at specified stock on watchlist and returns true if it should
   rebuy. The decision is made from whether or not it "breaks" from the
   rolling max *)
let rebuy stock = breaks_rolling_max stock
