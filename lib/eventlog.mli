type phase
type bucket
type counter_kind

type event_payload =
    Entry of { phase : phase; }
  | Exit of { phase : phase; }
  | Alloc of { count : int; bucket : bucket; }
  | Counter of { count : int; kind : counter_kind; }
  | Flush of { duration : int; }

type event = {
  payload : event_payload;
  timestamp : int;
  pid : int;
}

type endianness = Be | Le

type trace_header = {
  endianness : endianness;
  ocaml_trace_version : int;
}

type packet =
    Header of trace_header
  | Event of event

val string_of_gc_counter : counter_kind -> string
val string_of_alloc_bucket : bucket -> string
val string_of_phase : phase -> string

module Parser : sig

type decoder

val decoder : unit -> decoder 

val decode : decoder -> [> `Await
                        | `End
                        | `Error of [> `Msg of string ] | `Ok of packet ]

val src : decoder -> Bigstringaf.t -> int -> int -> bool -> unit

end 
