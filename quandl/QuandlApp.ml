open Quandl

let _ = Printexc.record_backtrace true; Random.self_init ()

type share = {symbol:string;date:string;price:float;quant:int}
let to_string s = Printf.sprintf "%s|%s|%f|%i" s.symbol s.date s.price s.quant
let to_json s = Printf.sprintf "{\"symbol\":%s,\"date\":%s,\"price\":%f,\"quant\":%i}" s.symbol s.date s.price s.quant

let do_header () = Printf.printf "Content-type: application/json\r\n\r\n"

let shares_filename = "/home/ubuntu/workspace/ocaml/quandl/MyShares.db"
let purse_filename = "/home/ubuntu/workspace/ocaml/quandl/purse.dat"
let log_channel = open_out_gen [Open_append;Open_creat] 0o600 "/home/ubuntu/workspace/ocaml/quandl/quandl_app.log"
let _ = at_exit (fun _ -> flush log_channel; close_out log_channel)

let ticker_db:(string, string) Hashtbl.t = get_tickers ()

let available_list = Hashtbl.fold (fun k v acc -> (k,v)::acc) ticker_db []

let display_available () = List.iter (fun (k,v) -> Printf.printf "%-15s %s\n" k v) available_list

let load_purse () = let purse_in = open_in_gen [Open_rdonly;Open_creat] 0o600 purse_filename in 
    let ret = if (in_channel_length purse_in) > 0
        then
            (Scanf.fscanf purse_in "%f" (fun f -> f))
        else
            1000.
    in
    close_in purse_in;
    ret
    
let load_shares_db () = let shares_in = open_in_gen [Open_rdonly;Open_creat] 0o600 shares_filename in
    let ret = if (in_channel_length shares_in) > 0
    then
        ((Marshal.from_channel shares_in): (string, share) Hashtbl.t)
    else
        Hashtbl.create 10
    in
    close_in shares_in;
    ret

let save_purse purse = let purse_out = open_out purse_filename in (Printf.fprintf purse_out "%f" purse); close_out purse_out

let save_shares_db db = let shares_out = open_out shares_filename in (Marshal.to_channel shares_out db []); close_out shares_out

let buy_order db sym quant: float = let tick = List.hd (get_eod sym) in let price = ((Random.float (tick.high -. tick.low)) +. tick.low) in
    (*transaction*)
    let s = {symbol=sym;date=tick.date;price=price;quant=quant} in
    Printf.fprintf log_channel "buy: %s\n" (to_json s); 
    (if (Hashtbl.mem db sym) then
        let old_s = Hashtbl.find db sym in
        let new_quant = old_s.quant + s.quant in
        let new_price = ((old_s.price *. (float old_s.quant)) +. (s.price *. (float s.quant))) /. (float new_quant) in
        Hashtbl.replace db sym {symbol=s.symbol;date=s.date;price=new_price;quant=new_quant}
    else
        Hashtbl.add db sym s);
    (*debit*)
    s.price *. (float s.quant)
    
let sell_order db sym quant: float = let tick = List.hd (get_eod sym) in let price = ((Random.float (tick.high -. tick.low)) +. tick.low) in
    (*transaction*)
    let s = {symbol=sym;date=tick.date;price=price;quant=(-1 * quant)} in
    Printf.fprintf log_channel "sell: %s\n" (to_json s);
    (if (Hashtbl.mem db sym) then
        let old_s = Hashtbl.find db sym in
        let new_quant = old_s.quant + s.quant in
        Hashtbl.replace db sym {symbol=s.symbol;date=s.date;price=old_s.price;quant=new_quant}
    else
        Hashtbl.add db sym s);
    (*credit*)
    s.price *. (float (-1 * s.quant))
    
let rec main () = let purse = load_purse () in
    Printf.printf "\n$%.2f > " purse;
    (match (read_line ()) with
    | "listall" -> display_available ()
    | "listown" -> Hashtbl.iter (fun k s -> Printf.printf "%-10s %7.2f %i\n" s.symbol s.price s.quant) (load_shares_db ())
    | "quit" -> exit 0
    | com -> if (String.sub (com ^ "   ") 0 3) = "buy"
        then
            let (sym, quant) = Scanf.sscanf com "buy %s %i" (fun s q -> (s,q)) in
            let db = load_shares_db () in
            save_purse (purse -. (buy_order db sym quant));
            save_shares_db db
        else if (String.sub (com ^ "    ") 0 4) = "sell"
        then 
            let (sym, quant) = Scanf.sscanf com "sell %s %i" (fun s q -> (s,q)) in
            let db = load_shares_db () in
            save_purse (purse +. (sell_order db sym quant));
            save_shares_db db
        else print_endline "Not a Command");
    main ()
    
let () = let args = try 
    Sys.getenv "QUERY_STRING" 
    with Not_found -> (let len = int_of_string (Sys.getenv "CONTENT_LENGTH") in
        let ret = String.create len in
        really_input stdin ret 0 len;
        ret) in
    match List.nth (List.find (fun l -> (List.hd l) = "fun") (List.fold_left (fun acc l -> (Str.split (Str.regexp_string "=") l)::acc) [] (Str.split (Str.regexp_string "&") args))) 1
    with "fetchTicker" -> do_header ();
        Printf.printf "{\"ticker\":[{\"symbol\":\"symbol\",\"description\":\"description\"}";
        (Hashtbl.iter (fun k v -> Printf.printf ",{\"symbol\":\"%s\",\"description\":\"%s\"}" k v) ticker_db);
        Printf.printf "]}";
        flush stdout
    | _ -> do_header (); print_endline "{\"ticker\":[{\"symbol\":\"null\",\"description\":\"nothing found\"}]}"; flush stdout