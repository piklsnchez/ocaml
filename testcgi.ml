let () = Printf.printf "Content-type: text/plain\r\n\r\n";
    ignore(Sys.command "/usr/bin/env > /tmp/env.txt");
    let len = int_of_string (Sys.getenv "CONTENT_LENGTH") in
    let s = String.create len in (really_input stdin s 0 len);
    Printf.printf "QueryString: %s\nStdin(%i):%s\n"
    (List.fold_left
        (fun a b -> a ^ (if a = "" then "" else ":") ^ b)
        ""
        (Str.split (Str.regexp_string "&")
            (try Sys.getenv "QUERY_STRING" with Not_found -> "")
        )
    )
    len
    s;
    flush stdout