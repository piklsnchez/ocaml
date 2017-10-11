open SocketServer

let onmessage msg =
  Printf.printf "onmessage: %s\n" msg;
  match msg with
  "connect" -> sendmessage (Parse_wiki.get_list())
  | m -> ()

let _ =
  SocketServer.message := onmessage;
  start ()
