let magic        = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let sep          = Str.regexp_string ":"
let base64 s     = Cryptokit.transform_string (Cryptokit.Base64.decode()) s
let tobase64 s   = Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad()) s
let reply_format = format_of_string "HTTP/1.1 101 Switching Protocols\r
Upgrade: websocket\r
Connection: Upgrade\r
Sec-WebSocket-Accept: %s\r\n\r\n"

let parse_headers l =
  List.map (fun s -> let ll = Str.split sep s in ((List.hd ll), List.hd (List.tl ll))) l

let get_headers in_chan  =
  let rec do_header in_chan acc =
  let line = input_line in_chan in
  if (String.trim line) <> "" then
    do_header in_chan (line::acc)
  else
    parse_headers acc in
  do_header in_chan []

let out = ref stdout

let message = ref print_endline

(*bits 9 - 15 are message length if less than 126 bit 8 should be 0*)
let sendmessage msg =
  let len = String.length msg in
  Printf.printf "sendmessage (%Lu): %s...\n" (Int64.of_int len) (String.sub msg 0 20);
  match len with
  _ when len < 126 -> Printf.fprintf !out "%c%c%s" (char_of_int 129) (char_of_int len) msg
  | _ -> Printf.fprintf !out "%c%c%Lu%s" (char_of_int 129) (char_of_int 127)(*FIXME of course this needs to be binary*) (Int64.of_int len) msg;
  flush !out

let do_websocket i o =
  while true do
    ignore(input_byte i);
    let b = input_byte i in
    let payload = Bytes.create (b lxor 128) in
    let mask = Bytes.create 4 in
    ignore(input i mask 0 (Bytes.length mask));(* pull in mask *)
    ignore(input i payload 0 (Bytes.length payload));(* pull in message *)
    let result = Bytes.mapi (fun i c -> (char_of_int ((int_of_char c) lxor
    (int_of_char (Bytes.get mask (i mod 4)))))) payload in(* decode message *)
    !message result
  done

let do_upgrade o headers =
  let key = List.find (fun (k,v) -> k = "Sec-WebSocket-Key") headers in
  Printf.fprintf o reply_format (tobase64 (Cryptokit.hash_string
  (Cryptokit.Hash.sha1 ()) ((String.trim (snd key)) ^ magic)));
  flush o

let main i o =
  out := o;
  let req = input_line i in(*first line is request string GET*)
  print_endline req;
  if String.sub (req ^ "   ") 0 3 = "GET" then(* negotiate *)
    (get_headers i |> (do_upgrade o);
    do_websocket i o)
  else
    Printf.fprintf o "BAD REQUEST\n"

let start () = Unix.establish_server main (Unix.ADDR_INET (Unix.inet_addr_any, 1111))
