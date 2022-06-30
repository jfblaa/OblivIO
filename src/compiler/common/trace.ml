module M = Message

type trace_element =
  { time: float
  ; msg: M.message
  }

type trace = {mutable elms: trace_element list}

let empty_trace = {elms=[]}

let add elm trace =
  trace.elms <- elm :: trace.elms

let to_list trace =
  List.rev trace.elms