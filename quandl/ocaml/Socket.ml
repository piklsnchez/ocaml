open Unix;;
open Printf;;
open Str;;

(** write data to socket *)
let write_sock (sock, input) =
  ignore(write sock input 0 (String.length input));;

(** read data from the socket and return as string *)
let read_sock sock : string =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  let output = Buffer.create buf_size in
  let rec r () =
    match read sock buf 0 buf_size with
    | 0 -> Buffer.contents output
    | n -> Buffer.add_subbytes output buf 0 n; r() in 
  r();;

let comment_regexp =   Str.regexp "<!--.*-->";;
let start_tag_regexp = Str.regexp "<[^/]?[^>]+>";;
let end_tag_regexp =   Str.regexp "</[^>]+>";;
let text_regexp =      Str.regexp "[^<]+";;

let strip_newlines s =
  let buf = Buffer.create 4096 in
  String.iter (fun c -> Buffer.add_char buf (if c = '\n' then ' ' else c)) s;
  Buffer.contents buf;;

let get_body s =
  let body_regexp = Str.regexp_case_fold "<body.*</body>" in
  let body = strip_newlines s in
  let n = try
    Str.search_forward body_regexp body 0
  with Not_found ->
    print_endline s;
    assert false
  in
  Str.matched_string body;;

(** parse the input *)
let parse_html s =
  let buf = Buffer.create 4096 in
  let body = get_body s in
  let len = String.length body in
  let rec next pos =
    if pos < len &&
      (Str.string_partial_match start_tag_regexp body pos ||
       Str.string_partial_match end_tag_regexp body pos ||
       Str.string_partial_match text_regexp body pos)
    then
      let token = Str.matched_string body in
      Buffer.add_string buf (token ^ "\n");
      next (Str.match_end())
    else
      Buffer.contents buf in
  next 0;;

(** get domain name*)
let get_domain s = let tokens = Str.split (Str.regexp_string".") s in
  let size = List.length tokens in
  (List.nth tokens (size -2)) ^ "." ^ (List.nth tokens (size -1));;

(** http request template *)
let req_format = format_of_string "GET /%s HTTP/1.1
Accept: */*
Accept-Encoding: identity
Host: %s
User-Agent: Googlebot/2.1 (+http://www.google.com/bot.html)
Connection: close\r\n\r\n";;

(** Entry point arg 1 host arg 2 port arg 3 page*)
let main () = let addr = (gethostbyname (get_domain Sys.argv.(1))).h_addr_list.(0) in
  let port = int_of_string Sys.argv.(2) in
  let mysocket = socket PF_INET SOCK_STREAM 0 in
  connect mysocket (ADDR_INET(addr, port));
  write_sock (mysocket, (sprintf req_format Sys.argv.(3) Sys.argv.(1) ));
  shutdown mysocket SHUTDOWN_SEND;
  let output = read_sock mysocket in
  print_endline (parse_html output);;

(** swallow exceptions in main *)
let () = handle_unix_error main ();;
