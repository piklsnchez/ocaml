let io_fun i o = let out_chan = open_out "server.out" in
    try
    while true do
        Printf.fprintf out_chan "%s\n" (input_line i);
        flush out_chan
    done
    with _ -> close_out out_chan
    
let main () = Unix.establish_server io_fun (Unix.ADDR_INET (Unix.inet_addr_any, 1111))
let () = main()
