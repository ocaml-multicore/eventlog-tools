open Angstrom

type phase = int
type bucket = int
type counter_kind = int

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

module Parser = struct
  
  let magic = LE.int32 0xc1fc1fc1l    
  let caml_trace_version = LE.int16 0x1 
  let stream_id = LE.int16 0x0 

  let entry_event =
    LE.int32 0x0l
    *> LE.any_int16 >>= fun phase ->
    return (Entry { phase = Consts.phase_to_string phase; }) 

  let exit_event =
    LE.int32 0x1l
    *> LE.any_int16 >>= fun phase ->
    return (Exit { phase = Consts.phase_to_string phase; })

  let counter_event =
    LE.int32 0x2l *>
    LE.any_int64 >>= fun count ->
    LE.any_int16 >>= fun kind ->
    return (Counter { count = Int64.to_int count;
                      kind = Consts.gc_counter_to_string kind; })

  let alloc_event =
    LE.int32 0x3l *>
    LE.any_int64 >>= fun count ->
    any_int8 >>= fun bucket ->
    return (Alloc { count = Int64.to_int count;
                    bucket = Consts.alloc_bucket_to_string bucket; })

  let flush_event =
    LE.int32 0x4l
    *> LE.any_int64 >>= fun duration ->
    return (Flush { duration = Int64.to_int duration; })

  let event_header =
    LE.any_int64 >>= fun timestamp ->
    LE.any_int32 >>= fun pid ->
    return (timestamp, pid)

  let parse_event =
    let event_parser =
      entry_event
      <|> exit_event 
      <|> alloc_event 
      <|> counter_event 
      <|> flush_event in
    event_header >>= fun (timestamp, pid) ->
    event_parser >>= fun payload ->
    return {payload; timestamp = Int64.to_int timestamp; pid = Int32.to_int pid; }

  let parse : event t =
    let header = magic *> caml_trace_version *> stream_id in
    (header *> parse_event) <|> parse_event 

  type t = {
    mutable buffer : Bigstringaf.t;
    mutable off : int;
    mutable len : int;
    mutable state : event Unbuffered.state;
    mutable complete : Unbuffered.more;
  }

  let rec decode d =
    match d.state with
    | Unbuffered.Done (i, v) ->
      d.off <- d.off + i;
      d.state <- Unbuffered.parse parse;
      `Ok v
    | Fail (_,_,msg) ->
      if d.complete = Unbuffered.Complete && d.off == d.len then
        `End
      else 
        `Error (`Msg msg)
    | Partial { committed; continue; } ->
      d.off <- d.off + committed;
      d.state <- continue d.buffer ~off:d.off ~len:(d.len - d.off) d.complete;
      match d.state with
      | Partial { committed=0; _ } -> `Await
      | _ -> decode d

  let decoder () =
    let buffer = Bigstringaf.empty in
    let len = 0x0 in
    let off = 0x0 in
    let state = Unbuffered.parse parse in
    let complete = Unbuffered.Incomplete in 
    { buffer; len; off; state; complete; }

  let src d src src_off src_len complete =
    let uncommited = d.len - d.off in
    let dst_len = (src_len + uncommited) in
    let dst = Bigstringaf.create dst_len in
    Bigstringaf.blit d.buffer ~src_off:d.off dst ~dst_off:0 ~len:uncommited; 
    Bigstringaf.blit src ~src_off dst ~dst_off:uncommited ~len:src_len; 
    d.buffer <- dst;
    d.off <- 0;
    d.len <- dst_len;
    d.complete <- if complete then Unbuffered.Complete else Unbuffered.Incomplete;

end 
