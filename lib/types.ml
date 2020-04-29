type phase = int
type bucket = int
type counter_kind = int

type endianness = [ `BE | `LE ]

type event_payload =
    Entry of { phase : string; }
  | Exit of { phase : string; }
  | Alloc of { count : int; bucket : string; }
  | Counter of { count : int; kind : string; }
  | Flush of { duration : int; }
            
type event = {
  payload : event_payload;
  timestamp : int;
  pid : int;
}
