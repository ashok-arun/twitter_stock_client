(* Writes a given message to a named file
    message: message to write to file (string)
    file: name of file to write to (string)
*)
let update_json message file = 
  let () =
    (* Write message to file *)
    let oc = open_out file in (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" message; (* write something *)   
    close_out oc;                     (* flush and close the channel *)
in print_endline "Written"