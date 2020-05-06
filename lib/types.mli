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

type packet =
    Header of endianness
  | Event of event

val string_of_gc_counter : counter_kind -> string
val string_of_alloc_bucket : bucket -> string
val string_of_phase : phase -> string

val gc_counter_of_int : int -> counter_kind
val alloc_bucket_of_int : int -> bucket 
val phase_of_int : int -> phase
