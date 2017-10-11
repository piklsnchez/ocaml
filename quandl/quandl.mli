type eod = {symbol : string; date : string; opening : float; high : float; low : float; closeing : float; volume : float; dividend : float; split_rate : float; aopen : float; ahigh : float; alow : float; aclose : float; avolume : float}
val to_string: eod -> string
val to_json: eod -> string
val fetch_tickers : unit -> unit
val get_tickers : unit -> (string, string) Hashtbl.t
val get_eod : ?sort_order:string -> ?rows:int -> ?trim_start:string -> ?trim_end:string -> ?column:int -> ?collapse:string -> ?transformation:string -> string -> eod list