(** caml_trace_version 0x1 *)

let phase = [|
  "compact/main";
  "compact/recompact";
  "explicit_gc/set";
  "explicit_gc/stat";
  "explicit_gc/minor";
  "explicit_gc/major";
  "explicit_gc/full_major";
  "explicit_gc/compact";
  "major";
  "major/roots";
  "major/sweep";
  "major/mark_roots";
  "major/mark_main";
  "major/mark_final";
  "major/mark";
  "major/mark_global_roots_slice";
  "major/roots_global";
  "major/roots_dynamic_global";
  "major/roots_local";
  "major/roots_c";
  "major/roots_finalised";
  "major/roots_memprof";
  "major/roots_hook";
  "major/check_and_compact";
  "minor";
  "minor/local_roots";
  "minor/ref_tables";
  "minor/copy";
  "minor/update_weak";
  "minor/finalized";
  "explicit_gc/major_slice";
  "domain/spawn";
  "domain/send_interrupt";
  "domain/idle_wait";
  "finalise_update_first";
  "finalise_update_last";
  "interrupt/gc";
  "interrupt/remote";
  "major/ephe_mark";
  "major/ephe_sweep";
  "major/finish_marking";
  "major/cycle_domains";
  "major/phase_change";
  "major/stw";
  "major/mark_opportunistic";
  "major/slice";
  "minor/clear";
  "minor/finalizers_oldify";
  "minor/global_roots";
  "minor/leave_barrier";
  "stw/api_barrier";
  "stw/handler";
  "stw/leader";
  "major/finish_sweeping";
  "minor/finalizers_admin";
  "minor/remembered_set";
  "minor/remembered_set_promote";
  "minor/local_roots_promote";
  "domain/condition_wait"
|]

let gc_counter = [|
  "alloc_jump";
  "force_minor/alloc_small";
  "force_minor/make_vect";
  "force_minor/set_minor_heap_size";
  "force_minor/weak";
  "force_minor/memprof";
  "major/mark/slice/remain";
  "major/mark/slice/fields";
  "major/mark/slice/pointers";
  "major/work/extra";
  "major/work/mark";
  "major/work/sweep";
  "minor/promoted";
  "request_major/alloc_shr";
  "request_major/adjust_gc_speed";
  "request_minor/realloc_ref_table";
  "request_minor/realloc_ephe_ref_table";
  "request_minor/realloc_custom_table";
|]

let alloc_bucket = [|
  "alloc 01";
  "alloc 02";
  "alloc 03";
  "alloc 04";
  "alloc 05";
  "alloc 06";
  "alloc 07";
  "alloc 08";
  "alloc 09";
  "alloc 10-19";
  "alloc 20-29";
  "alloc 30-39";
  "alloc 40-49";
  "alloc 50-59";
  "alloc 60-69";
  "alloc 70-79";
  "alloc 80-89";
  "alloc 90-99";
  "alloc large";
 |]
