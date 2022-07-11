module M = Message

type direction = INCOMING | OUTGOING
type print_when = ONTHEFLY | ATEXIT
type print_what = NOTHING | AGGREGATE | FULL

type trace_element =
  { time: float
  ; d: direction
  ; msg: M.message
  }

type trace = 
  { mutable elms: trace_element list
  ; mutable timeOfFirstMessage: float
  ; mutable timeOfLastSend: float
  ; mutable nIncomingMessages: int
  ; mutable nOutgoingMessages: int
  ; mutable incomingBytes: int64
  ; mutable outgoingBytes: int64
  ; print_when: print_when
  ; print_what: print_what
  }

let print_element {time;d;msg} = 
  let timeStr = Printf.sprintf "t=%f, " time in
  let dStr = 
    match d with
    | OUTGOING -> "-> "
    | INCOMING -> "<- " in
  let bStr = 
    Int.to_string @@ M.sizeOfMsgValue msg in
  print_endline @@ timeStr ^ dStr ^ bStr ^ " bytes: " ^ M.to_string msg

let print_aggregate trace =
  print_endline @@ String.concat ""
    [ "time of last send: "
    ; Printf.sprintf "%f, " trace.timeOfLastSend
    ; "#in: "
    ; Int.to_string trace.nIncomingMessages
    ; " ("
    ; Int64.to_string trace.incomingBytes
    ; " bytes)"
    ; ", #out: "
    ; Int.to_string trace.nOutgoingMessages
    ; " ("
    ; Int64.to_string trace.outgoingBytes
    ; " bytes)"
    ]

let empty_trace print_when print_what = 
  { elms=[]
  ; timeOfFirstMessage = 0.
  ; timeOfLastSend = 0.
  ; nIncomingMessages = 0
  ; nOutgoingMessages = 0
  ; incomingBytes = Int64.zero
  ; outgoingBytes = Int64.zero
  ; print_when
  ; print_what
  }

let add_send time msg trace =
  begin
  match trace.elms with
  | [] -> trace.timeOfFirstMessage <- time
  | _ -> ()
  end;
  trace.nOutgoingMessages <- 1 + trace.nOutgoingMessages;
  trace.outgoingBytes <- Int64.add (Int64.of_int @@ M.sizeOfMsgValue msg) trace.outgoingBytes;
  let time = time -. trace.timeOfFirstMessage in
  trace.timeOfLastSend <- time;
  let elm = {time;d=OUTGOING;msg} in
  trace.elms <- elm :: trace.elms;
  match trace.print_when with
  | ONTHEFLY ->
    begin
    match trace.print_what with
    | AGGREGATE ->
      print_aggregate trace
    | FULL ->
      print_element elm;
      print_aggregate trace
    | NOTHING -> ()
    end
  | ATEXIT -> ()

let add_receive time msg trace =
  begin
  match trace.elms with
  | [] -> trace.timeOfFirstMessage <- time
  | _ -> ()
  end;
  trace.nIncomingMessages <- 1 + trace.nIncomingMessages;
  trace.incomingBytes <- Int64.add (Int64.of_int @@ M.sizeOfMsgValue msg) trace.incomingBytes;
  let time = time -. trace.timeOfFirstMessage in
  let elm = {time;d=INCOMING;msg} in
  trace.elms <- elm :: trace.elms;
  match trace.print_when with
  | ONTHEFLY ->
    begin
    match trace.print_what with
    | AGGREGATE ->
      print_aggregate trace
    | FULL ->
      print_element elm;
      print_aggregate trace
    | NOTHING -> ()
    end
  | ATEXIT -> ()

let terminate trace =
  match trace.print_when with
  | ATEXIT ->
    begin
    match trace.print_what with
    | AGGREGATE ->
      print_aggregate trace
    | FULL ->
      List.iter print_element (List.rev trace.elms);
      print_aggregate trace
    | NOTHING -> ()
    end
  | ONTHEFLY -> ()