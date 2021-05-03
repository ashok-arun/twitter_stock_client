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
