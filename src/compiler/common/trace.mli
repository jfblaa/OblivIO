module M = Message

type trace_element =
  { time: float
  ; msg: M.message
  }

type trace

val empty_trace: trace
val add: trace_element -> trace -> unit
val to_list: trace -> trace_element list