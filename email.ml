let email_address = Config.email_address

let email_password = Config.email_password

open Letters

let conf =
  Config.make ~username:email_address ~password:email_password
    ~hostname:"smtp.gmail.com" ~with_starttls:true
  |> Config.set_ca_cert "roots.pem"

let send_email config email ticker =
  let sender = email_address in
  let recipients = [ To email ] in
  let subject = "Order to Buy " ^ ticker ^ " Executed" in
  let body =
    Plain
      ( "The Twitter Stock Client application has executed and \
         fullfiled an order for [number of shares] shares of " ^ ticker
      ^ " at an average price of $[price bought at] on [date].\n\n\
         Your trade confirmation is avaiable in your TDAmeritrade \
         account history and the stock.csv file in your Twitter Stock \
         Client Repository.\n\n\
        \ Sincerely,\n\
        \ Twitter Stock Client Team" )
  in
  let mail = build_email ~from:sender ~recipients ~subject ~body in
  match mail with
  | Ok message -> send ~config ~sender ~recipients ~message
  | Error reason -> Lwt.fail_with reason

(* Your Order Has Been Executed!

   Hi Ashok,

   Your limit order to buy 5 shares of WORK was executed at an average
   price of $36.81 on July 1st 2019 at 2:35 PM.

   Your trade confirmation will be available in your order history on
   Robinhood in one trading day.

   If you have any questions, please visit our Help Center and we’d be
   more than happy to help!

   Sincerely, *)
