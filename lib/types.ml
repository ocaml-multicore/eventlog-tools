module R = Rresult.R

type phase = int
type bucket = int
type counter_kind = int

type event_payload =
    Entry of { phase : phase; }
  | Exit of { phase : phase; }
  | Alloc of { count : int; bucket : bucket; }
  | Counter of { count : int; kind : counter_kind; }
  | Flush of { duration : int; }

type event = {
  is_backup_thread : bool;
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

let string_of_phase i = Consts.phase.(i)
let string_of_gc_counter i = Consts.gc_counter.(i)
let string_of_alloc_bucket i = Consts.alloc_bucket.(i)

let phase_of_int i =
  if i < Array.length Consts.phase then
    R.return i
  else
    R.error_msgf "phase_of_int: invalid argument %d" i

let gc_counter_of_int i =
  if i < Array.length Consts.gc_counter then
    R.return i
  else
    R.error_msgf "gc_counter_of_int: invalid argument %d" i

let alloc_bucket_of_int i =
  if (i - 1) < Array.length Consts.alloc_bucket then
    R.return (i - 1)
  else
    R.error_msgf "alloc_bucket_of_int: invalid argument %d" i
