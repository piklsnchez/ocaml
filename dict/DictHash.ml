open Printf
open Hashtbl

type matches = { pos : int; len : int; };;
let max = 22;;

let findstrings str = 
    let tbl = Hashtbl.create 62992 in
    let file = open_in "words" in
    try
        while true do 
            Hashtbl.add tbl (input_line file) 0
        done
    with End_of_file -> let s = Hashtbl.stats tbl in printf "%d;%d;%d\n" s.num_bindings s.num_buckets s.max_bucket_length;
    let rec get_next m_list = 
        let m = match (List.hd m_list) with Some x -> x in
        if Hashtbl.mem tbl (String.sub str m.pos m.len) 
        then
            (Some {pos=m.pos; len=m.len})::m_list
        else
            if m.len > 0
            then
                get_next ((Some {pos=m.pos; len=(m.len - 1)})::m_list)
            else
                None::m_list in
    (* if none we need to back up *)
    let rec print_word ma_list =
        let m_list = get_next ma_list in
        match List.hd m_list with
        | Some x -> printf "%s " (String.sub str x.pos x.len);
            if (x.pos + x.len) >= (String.length str)
            then
                print_endline ""
            else
                print_word ((Some {pos=(x.pos + x.len); len=(min ((String.length str) - (x.pos + x.len)) max)})::m_list);
        | None -> match List.hd (List.tl m_list) with
            | Some ma -> print_word ((Some {pos=ma.pos; len=(ma.len -1)})::m_list);
            | None -> print_endline "FAIL";
    print_word ((Some {pos=0; len=max})::[]);;
        
let () = findstrings Sys.argv.(1);;
(* input string "ilovepicklesinthemorning" find all the words that match the dictionary*)