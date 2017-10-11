open Str

let re = regexp "^\\(WIKI/[^,]+\\),\"\\(.*\\)\"$"
let templ = "\\1|\\2"
let delim = regexp "|"
let line_sep = regexp "\n"

let get_list () =
  let tick_file = open_in "WIKI_tickers.csv" in
  let tick_list = List.map
  (fun l -> try
    let t = Str.split_delim delim (global_replace re templ l) in
    "{\"symbol\":\""^(List.nth t 0)^"\",\"desc\":\""^(List.nth t 1)^"\"}"
  with Failure(_) -> l)
  (Str.split line_sep (really_input_string tick_file (in_channel_length tick_file))) in
  let result = "[\n" ^ (List.fold_left (fun l acc -> acc ^ "\n," ^ l)
  "{\"symbol\":\"symbol\",\"desc\":\"desc\"}"
  tick_list) ^ "\n]" in
  close_in tick_file;
  Message.create "load_database" result
