class ['fluent, 'action] t (succes : bool) =
object
  inherit ['fluent, 'action] Plan.t

  val mutable plan = ([] : 'action list list)

  val mutable nb_actions = 0

  (* quality contient la valeur de la qualite du plan *)
  val mutable quality = max_int
  method quality = quality

  method succes = succes
  method add_end actions = plan <- plan @ [actions] ; {<plan = plan>}
  method add_begin actions = plan <- actions :: plan ; {<plan = plan>}
  method set_plan liste_actions =
    plan <- liste_actions ;
    nb_actions <- List.fold_right (fun e accu -> accu + List.length e) plan 0 ;
    {<plan = plan>}

  method set_nb_actions n = nb_actions <- n

  method plan = {<plan = plan>}
  method to_string =
    let n = ref 0 in
    let s = ref "" in
      List.iter 
	(fun actions ->
	   incr n ;
	   List.iter
	     (fun a ->
		if not a#is_noop then
		  s := !s ^ string_of_int !n ^ " : " ^ a#to_string ^ "\n") actions) plan ;
      !s

  method to_ipc_string =   let n = ref 0 in
    let s = ref "" in
    let c = ref 0 in
      List.iter 
	(fun actions ->
	   incr n;
	   List.iter 
	     (fun a ->
	       begin
		 c := !c+(a#quality);
		 if not a#is_noop then 
		   s := !s ^ string_of_int !n ^ " : " ^ a#to_string ^ "\n"
		end) 
	    actions) plan ;
	       
    (!s ^ "\nCout: " ^ (string_of_int !c) ^ "\n")

  method update_quality =
    if plan = [] then quality <- max_int ;
    quality <- List.fold_left (fun accu la -> accu + (List.fold_left (fun accu1 a -> accu1 + a#quality) 0 la)) 0 plan

  method valid init goal =
(*(true, nb_actions)*)
    let nb_actions = ref 0 in
    let rec valid state = function
	[] -> Array.iter (fun g -> if not(List.mem g state) then raise Exit) goal
      | actions :: plan ->
	  List.iter 
	  (fun a ->
	     Array.iter (fun f -> if not(List.mem f state) then raise Exit) a#prec) actions ;
	  let nstate = ref state in
	    List.iter 
	      (fun a ->
		 Array.iter (fun f -> nstate := Utils.delete f !nstate) a#del ;
		 Array.iter (fun f -> nstate := f :: !nstate) a#add ;
		 if not a#is_noop then incr nb_actions
	      ) actions ;
	    valid !nstate plan
    in
      try valid (Array.to_list init) plan ; (true, !nb_actions) with Exit -> (false, 0)

end

