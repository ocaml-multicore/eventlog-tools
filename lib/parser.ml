open Angstrom
open Types

let magic_be = BE.int32 0xc1fc1fc1l >>= fun () -> return Be    
let magic_le = LE.int32 0xc1fc1fc1l >>= fun () -> return Le

let caml_trace_version e =
  let f =
    match e with
    | Le -> LE.int16
    | Be -> BE.int16
  in
  f 0x1

let stream_id e =
  let f =
    match e with
    | Le -> LE.int16
    | Be -> BE.int16
  in
  f 0x0

let parse_event e =
  let int32 = match e with Be -> BE.int32 | Le -> LE.int32 in
  let any_int16 = match e with Be -> BE.any_int16 | Le -> LE.any_int16 in
  let any_int32 = match e with Be -> BE.any_int32 | Le -> LE.any_int32 in
  let any_int64 = match e with Be -> BE.any_int64 | Le -> LE.any_int64 in
  let entry_event =
    int32 0x0l
    *> any_int16 >>= fun phase ->
    return (Entry { phase = phase_of_int phase; }) 
  in
  let exit_event =
    int32 0x1l
    *> any_int16 >>= fun phase ->
    return (Exit { phase = phase_of_int phase; })
  in
  let counter_event =
    int32 0x2l *>
    any_int64 >>= fun count ->
    any_int16 >>= fun kind ->
    return (Counter { count = Int64.to_int count;
                      kind = gc_counter_of_int kind; })
  in 
  let alloc_event =
    int32 0x3l *>
    any_int64 >>= fun count ->
    any_int8 >>= fun bucket ->
    return (Alloc { count = Int64.to_int count;
                    bucket = alloc_bucket_of_int bucket; })
  in
  let flush_event =
    int32 0x4l
    *> any_int64 >>= fun duration ->
    return (Flush { duration = Int64.to_int duration; })
  in
  let event_header =
    any_int64 >>= fun timestamp ->
    any_int32 >>= fun pid ->
    return (timestamp, pid)
  in
  let event_parser =
    entry_event
    <|> exit_event 
    <|> alloc_event 
    <|> counter_event 
    <|> flush_event
  in 
  event_header >>= fun (timestamp, pid) ->
  event_parser >>= fun payload ->
  Event {payload; timestamp = Int64.to_int timestamp; pid = Int32.to_int pid; }
  |> return
  
let parse_magic : endianness t = magic_be <|> magic_le  

let parse_header =
  parse_magic >>= fun endianness ->
  caml_trace_version endianness *> stream_id endianness >>= fun () ->
  return (Header endianness)

              
type decoder = {
  mutable buffer : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  mutable state : packet Unbuffered.state;
  mutable complete : Unbuffered.more;
  mutable parser : packet t; 
}

let rec decode d =
  match d.state with
  | Unbuffered.Done (i, v) ->
    d.off <- d.off + i;
    begin
      match v with
      | Header endianness ->
        d.parser <- parse_event endianness;
      | _ -> ()
    end;
    d.state <- Unbuffered.parse d.parser;
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
  let parser = parse_header in
  let state = Unbuffered.parse parse_header in
  let complete = Unbuffered.Incomplete in 
  { buffer; len; off; state; complete; parser; }

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
