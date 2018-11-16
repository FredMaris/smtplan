class ['fluent, 'action] t (succes : bool) =
object
  inherit ['fluent, 'action] Plan.t

  val plan = ([] : 'action list)
  val length = 0

  method length = length
  method succes = succes
  method add_end action = {< plan = action :: plan ; length = succ length >}
  method add_begin action = {< plan = plan @ [action] ; length = succ length >}

  (*method nplan actions = length <- List.length actions ; {< plan = actions >}*)

  method to_string =
    let n = ref 0 in
    let s = ref "" in
      List.iter 
	(fun action ->
	   incr n;
	   s := !s ^ string_of_int !n ^ " : " ^ action#to_string ^ "\n") (List.rev plan) ;
      !s

  method to_ipc_string =
    let n = ref 0 in
    let s = ref "" in
      List.iter 
	(fun action ->
	   s := !s ^ string_of_int !n ^ ": (" ^ action#to_string ^ ") [1]\n";incr n) (List.rev plan) ;
      !s

  method valid init goal =
    let nb_actions = ref 0 in
    let rec valid state = function
	[] -> Array.iter (fun g -> if not(List.mem g state) then raise Exit) goal
      | action :: plan ->
	  incr nb_actions ;
	  Array.iter (fun f -> if not(List.mem f state) then raise Exit) action#prec ;
	  let nstate = ref state in
	    Array.iter (fun f -> nstate := Utils.delete f !nstate) action#del ;
	    Array.iter (fun f -> nstate := f :: !nstate) action#add ;
	    valid !nstate plan
    in
      try valid (Array.to_list init) (List.rev plan) ; (true, !nb_actions) with Exit -> (false, 0)

end

