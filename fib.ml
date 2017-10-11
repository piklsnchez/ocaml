open Printf
let rec fib i = if i < 2 then i else (fib (i-1)) + (fib (i-2));;
    
    
let () = let n = (float_of_string "5.0") -. 1. in
let sqrt5 = (sqrt 5.) in
let phi = (1. +. sqrt5) /. 2. in
let bin = ((phi ** (n+.1.)) -. ((1.-.phi) ** (n+.1.))) /. sqrt5 in
    printf "Content-type: text/plain\r\n\r\nbinet: %d\nrec: %d\n" (int_of_float bin) (fib (int_of_string "5"));;
