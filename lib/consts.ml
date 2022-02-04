(** caml_trace_version 0x1 *)

let phase = [|
  "compact/main";
  "compact/recompact";
  "explicit/gc_set";
  "explicit/gc_stat";
  "explicit/gc_minor";
  "explicit/gc_major";
  "explicit/gc_full_major";
  "explicit/gc_compact";
  "major";
  "major/roots";
  "major/sweep";
  "major/mark/roots";
  "major/mark/main";
  "major/mark/final";
  "major/mark";
  "major/mark/global_roots_slice";
  "major_roots/global";
  "major_roots/dynamic_global";
  "major_roots/local";
  "major_roots/C";
  "major_roots/finalised";
  "major_roots/memprof";
  "major_roots/hook";
  "major/check_and_compact";
  "minor";
  "minor/local_roots";
  "minor/ref_tables";
  "minor/copy";
  "minor/update_weak";
  "minor/finalized";
  "explicit/gc_major_slice";
    "domain/spawn";
    "domain/send_interrupt";
    "domain/idle_wait";
    "finalise/update_first";
    "finalise/update_last";
    "interrupt/gc";
    "interrupt/remote";
    "major/ephe_mark";
    "major/ephe_sweep";
    "major/finish_marking";
    "major_gc/cycle_domains";
    "major_gc/phase_change";
    "major_gc/stw";
    "major/mark_opportunistic";
    "major/slice";
    "minor/clear";
    "minor/finalizers/oldify";
    "minor/global_roots";
    "minor/leave_barrier";
    "stw/api_barrier";
    "stw/handler";
    "stw/leader";
    "minor/clear";
    "minor_finalized";
    "minor_finalizers_oldify";
    "minor_global_roots";
    "minor_leave_barrier";
    "minor_local_roots";
    "minor_ref_tables";
    "minor_update_weak";
    "stw_api_barrier";
    "stw_handler";
    "stw_leader";
    "major_finish_sweeping";
    "minor_finalizers_admin"
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
