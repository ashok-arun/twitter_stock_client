open OUnit2
open Twitter
open Sentiment
open Parser

(********************************************************************
  Helper Functions. 
 ********************************************************************)
let str_equality_test (name : string) input out : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal out input ~printer:(fun a -> a)

let bool_equality_test (name : string) input out : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal out input ~printer:(fun a ->
      if a then "true" else "false")

(********************************************************************
   End helper functions.
 ********************************************************************)

let twitter_tests =
  [
    bool_equality_test
      "Twitter recent tweet test to make sure API calls are functioning"
      (Twitter.most_recent_tweet "elonmusk" = "")
      false;
  ]

let sentiment_tests =
  [
    bool_equality_test "test if tweet <AMC to the moooon> is bullish"
      (Sentiment.is_bullish "AMC to the mooon")
      true;
  ]

let parser_test =
  [
    str_equality_test "testing if detects a tesla as TSLA"
      (Parser.custom_parse "I believe gamestonk is going to the moon")
      "GME";
    str_equality_test "testing for empty string if detects nothing"
      (Parser.custom_parse "I believe gameston is going to the moon")
      "";
    str_equality_test "testing for $<TICKER>"
      (Parser.custom_parse "$AMC to the moon")
      "AMC";
    str_equality_test "testing for keyword precedent of $<TICKER>"
      (Parser.custom_parse "Gamestonk and $AMC going to the moon")
      "GME";
    str_equality_test "testing regex $ with no ticker"
      (Parser.custom_parse "testing regex $")
      "";
    str_equality_test "empty tweet" (Parser.custom_parse "") "";
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ twitter_tests; sentiment_tests; parser_test ]

let _ = run_test_tt_main suite
