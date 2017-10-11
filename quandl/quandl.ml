let _ = Printexc.record_backtrace true
type eod = {symbol : string; date : string; opening : float; high : float; low : float; closeing : float; volume : float; dividend : float; split_rate : float; aopen : float; ahigh : float; alow : float; aclose : float; avolume : float};;
let to_string e = Printf.sprintf "%s|%s|%f|%f|%f|%f|%f|%f|%f|%f|%f|%f|%f|%f"
    e.symbol e.date e.opening e.high e.low e.closeing e.volume e.dividend
    e.split_rate e.aopen e.ahigh e.alow e.aclose e.avolume
let to_json e = Printf.sprintf "{\"symbol\":\"%s\",\"date\":\"%s\",\"opening\":%f,\"high\":%f,\"low\":%f,\"closing\":%f,\"volume\":%f,\"dividend\":%f,\"splitRate\":%f,\"aopen\":%f,\"ahigh\":%f,\"alow\":%f,\"aclose\":%f,\"avolume\":%f}"
    e.symbol e.date e.opening e.high e.low e.closeing e.volume e.dividend e.split_rate e.aopen e.ahigh e.alow e.aclose e.avolume

let db_filename = "/home/ubuntu/workspace/ocaml/quandl/tickersdb.db";;
    
let fetch_tickers () = let request = "GET /quandl-static-content/Ticker+CSV%27s/WIKI_tickers.csv HTTP/1.0\r\n
Accept: */*\r\n
Accept-Encoding: identity\r\n
Host: s3.amazonaws.com\r\n
User-Agent: Googlebot/2.1 (+http://www.google.com/bot.html)\r\n
Connection: close\r\n\r\n
" in
    let hostname = Unix.gethostbyname "s3.amazonaws.com" in
    let address = Unix.ADDR_INET (hostname.Unix.h_addr_list.(0), 80) in
    let (in_conn, out_conn) = Unix.open_connection address in
        output_string out_conn request;
        flush out_conn;
        let ticker_table = Hashtbl.create 4000 in
        while (String.sub ((input_line in_conn) ^ "      ") 0 6) <> "quandl" do () done;
        try(*symbol, description*)
            while true do
                Scanf.sscanf (input_line in_conn) "%s@,\"%s@\"" (fun s d -> Hashtbl.add ticker_table s d)
            done
        with End_of_file -> ();
        Unix.shutdown_connection in_conn;
        close_in in_conn;
    let file_channel = open_out_bin db_filename in
        Marshal.to_channel file_channel ticker_table [];
        close_out file_channel
    
    
let get_tickers () =
        let file_channel = open_in_bin db_filename in
        let db = ((Marshal.from_channel file_channel): (string, string) Hashtbl.t) in
        close_in file_channel;
        db
        
let get_eod
?(sort_order="descending")
?(rows=1)
?(trim_start="")
?(trim_end="")
?(column=0)
?(collapse="none")
?(transformation="none")
sym =
let request = Printf.sprintf "GET /api/v1/datasets/%s.csv?rows=%i&exclude_headers=false&auth_token=qYS6hrjXJRFBeLb1r-xd%s%s%s&collapse=%s&transform=%s HTTP/1.0\r\n
Accept: */*\r\n
Accept-Encoding: identity\r\n
Host: www.quandl.com\r\n
User-Agent: Googlebot/2.1 (+http://www.google.com/bot.html)\r\n
Connection: close\r\n
\r\n
" sym rows (if "" <> trim_start then "&trim_start=" ^ trim_start else "") (if "" <> trim_end then "&trim_end=" ^ trim_end else "") (if column <= 0 then "" else Printf.sprintf "&column=%i" column) collapse transformation in
(*print_endline request;*)
let ticker_hostname = Unix.gethostbyname "quandl.com" in
let address = Unix.ADDR_INET (ticker_hostname.Unix.h_addr_list.(0), 80) in
let (in_conn, out_conn) = Unix.open_connection address in
    output_string out_conn request;
    flush out_conn;
    while (String.sub ((input_line in_conn) ^ "         ") 0 9) <> "Date,Open" do () done;
    let rec build l =
        try
            build (Scanf.sscanf (input_line in_conn) "%s@,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f"
            (fun d o h l c v divi sp ao ah al ac av -> 
            {symbol=sym;date=d;opening=o;
            high=h;low=l;closeing=c;
            volume=v;dividend=divi;
            split_rate=sp;aopen=ao;
            ahigh=ah;alow=al;aclose=ac;
            avolume=av})::l)
        with End_of_file -> l
        | e -> prerr_endline (Printexc.to_string e);raise e
        in
    build []