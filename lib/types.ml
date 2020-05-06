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
  payload : event_payload;
  timestamp : int;
  pid : int;
}

type endianness = Be | Le

type packet =
    Header of endianness
  | Event of event

let string_of_phase i =
  try
    Consts.phase.(i)
  with
  | _ -> failwith (Printf.sprintf "string_of_phase: invalid phase id %d" i)

let phase_of_int i =
  if i < Array.length Consts.phase then
    i
  else
    failwith (Printf.sprintf "phase_of_int: invalid argument %d" i)

let string_of_gc_counter i =
  try
    Consts.gc_counter.(i)
  with
  | _ -> failwith (Printf.sprintf "string_of_gc_counter: invalid phase %d" i)

let gc_counter_of_int i =
  if i < Array.length Consts.gc_counter then
    i
  else
    failwith (Printf.sprintf "gc_counter_of_int: invalid argument %d" i)

let string_of_alloc_bucket i =
  try
    Consts.alloc_bucket.(i - 1)
  with
  | _ -> failwith (Printf.sprintf "string_of_alloc_bucket: invalid bucket %d" i)

let alloc_bucket_of_int i =
  if i < Array.length Consts.alloc_bucket then
    i
  else
    failwith (Printf.sprintf "alloc_bucket_of_int: invalid argument %d" i)
