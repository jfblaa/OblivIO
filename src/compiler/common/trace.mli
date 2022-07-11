module M = Message

type print_when = ONTHEFLY | ATEXIT
type print_what = NOTHING | AGGREGATE | FULL

type trace

val empty_trace: print_when -> print_what -> trace
val add_send: float -> M.message -> trace -> unit
val add_receive: float -> M.message -> trace -> unit
val terminate: trace -> unit