module AtomTable = Hashtbl.Make(Atom)

type 'fluent candidate = {
  o : Domain.operator ;
  params : Symb.constant array ;
  mutable pos : int ;
}

class virtual ['fluent, 'action, 'plan] t =
object (self)
  constraint 'fluent = 'action #Node.fluent
  constraint 'action = 'fluent #Node.action
  constraint 'plan = ('fluent, 'action) #Plan.t

  (*
    Public datas
  *)

  val mutable actions = []
  val mutable actions_always = []
  val mutable actions_final = [| |]
  val mutable actions_always_final = [| |]
  val mutable fluents = []
  val mutable fluents_final = [| |]
  val mutable init_state = [| |]
  val mutable goal = [| |]
  val mutable nb_fluents = 0
  val mutable nb_actions = 0

  method actions = actions_final
  method set_actions act =
    actions <- act ;
    actions_final <- Array.of_list act ;
    nb_actions <- Array.length actions_final ;

  method actions_always = actions_always_final
  method fluents = fluents_final
  method init_state = init_state
  method goal = goal
  method nb_fluents = nb_fluents
  method nb_actions = nb_actions
  method nb_init = Array.length init_state
  method nb_goal = Array.length goal


  (* 
     Private datas
  *)

  val mutable domain = Domain.domain_void ;
  val mutable problem = Domain.problem_void ;
  val mutable goal_temp = Formula.Top
  val fluents_table = AtomTable.create 1000
  val constraints_table = AtomTable.create 100
  
  (* Private access !!! *)
  method domain = domain;
  method problem = problem;

(* Noms *)
  method domain_name = domain#name
  method problem_name = problem#name

  (*
    Virtual methods
  *)


  method virtual create_fluent : Atom.t -> 'fluent
  method virtual create_action : string -> Symb.constant array -> float -> int -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> 'action
  method virtual plan_succes : 'plan
  method virtual plan_fail : 'plan
  method virtual run : 'plan
  method virtual print_statistics : unit


  (*
    Domain and problem definition and creation 
  *)

  method private parse_domain = 
    (domain <- 
    let stream = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel stream in
      Parser.domain Lexer.token lexbuf);
      (*Utils.eprint "%s\n" domain#to_complete_istring*)

  method private parse_problem = 
    problem <-
    let stream = open_in Sys.argv.(2) in
    let lexbuf = Lexing.from_channel stream in
      Parser.problem Lexer.token lexbuf 

  (*
    Fluent creation
  *)

  method private search_fluent atom =
    try (AtomTable.find fluents_table atom, true) with 
      | Not_found ->
	  let fluent = self#create_fluent atom in
	    AtomTable.add fluents_table atom fluent ;
	    (fluent, false)

  method private search_fluenti atom =
    let present = (AtomTable.fold (fun a b c -> c || a#equal2 atom) fluents_table false) in
    try (AtomTable.find fluents_table atom, present) with 
      | Not_found ->
	  let fluent = self#create_fluent atom in
	    AtomTable.add fluents_table atom fluent ;
	    (fluent, present)

  (*
    Extraction of typing informations
  *)

  method private domains_creation =
    let unary_constraints = AtomTable.create 100 in
    let create_init_state init_state =
      Array.of_list 
	(List.fold_right 
	   (fun atom fluents ->
	      (**)if AtomTable.mem fluents_table atom then fluents 
	      else begin
		problem#add_atom_constants atom ;
		if atom#pred#typing then begin
		  if atom#nb_terms <= 1 then AtomTable.add unary_constraints atom () 
		  else AtomTable.add constraints_table atom () ;
		  fluents
		end
		else(**) fst (self#search_fluent atom) :: fluents
	      (**)end(**)
	   ) init_state [])
    in
    let simplify_goal =
      Formula.simplify(
	fun atom ->
	  let table = if atom#nb_terms <= 1 then unary_constraints else constraints_table in
	    if not (AtomTable.mem table atom) then begin
 	      Utils.eprint "\nImpossible goal : %s.\n\n" atom#to_string ;
	      exit 0
	    end)
    in
      init_state <- create_init_state problem#init ;
      goal_temp <- simplify_goal problem#goal ;
      problem#finalize


  (*
    Creation of actions and fluents
  *)


  method private create_actions =
    let candidates_table = AtomTable.create 5000 in
 (* let instanc atom params =
    let ts = atom#timeset in
      let f = fst (self#search_fluent (atom#instantiate params)) in
       f#atom#set_timeset ts;
       f in *)
    let calculate_duration params duration =
      let dur = FunctionFormula.calculate problem#functions_value_list params duration in
      (*Utils.print "%s -> %f\n" (FunctionFormula.to_string duration) dur;*)
      dur
    in
    let append_timedata atom params =
      let timeset = (FunctionFormula.calculate problem#functions_value_list params (fst atom#timeset_struct),FunctionFormula.calculate problem#functions_value_list params (snd atom#timeset_struct))
      in
       (*Utils.print "[%s,%s] -> [%f,%f]\n" (FunctionFormula.to_string (fst atom#timeset_struct)) (FunctionFormula.to_string (snd atom#timeset_struct)) (fst timeset) (snd timeset);*)
       atom#timedata#set_timeset timeset;
       atom#timedata
    in
    let rec treat_candidate = function {o = o ; params = params} as candidate ->
      if candidate.pos = 0 then
	let action = 
	  (self#create_action o#name params (calculate_duration params o#duration) o#quality
	     (Array.map (fun atom -> begin append_timedata atom params; ((fst (self#search_fluent (atom#instantiate params))),atom#timedata) end) o#prec)
	     (Array.map (fun atom -> begin append_timedata atom params; ((fst (self#search_fluent (atom#instantiate params))),atom#timedata) end) o#nprec)
	     (Array.map
		(fun atom -> begin append_timedata atom params; ((
		   let (fluent, present) = self#search_fluenti (atom#instantiate params) in
		     if not present then begin
		       try
			 let candidates = AtomTable.find candidates_table fluent#atom in
			   AtomTable.remove candidates_table fluent#atom ;
			   List.iter treat_candidate !candidates
		       with Not_found -> ()
		     end ;
		     fluent
		), atom#timedata) end) o#add)
	     (Array.map (fun atom -> begin append_timedata atom params; ((fst (self#search_fluent (atom#instantiate params))),atom#timedata) end) o#del))
	in 
	  actions <- action :: actions
      else begin
	candidate.pos <- candidate.pos - 1 ;
	let atom = o#prec.(candidate.pos)#instantiate params in
	  if AtomTable.mem fluents_table atom then treat_candidate candidate
	  else
	    let candidates =
	      try AtomTable.find candidates_table atom with
		| Not_found ->
		    let candidates = ref [] in
		      AtomTable.add candidates_table atom candidates ;
		      candidates
	    in
	      candidates := candidate :: !candidates
      end
    in
    let verify_equality_rules params eq_pred =
      Array.iter 
	(fun (t1, t2) -> 
	   if not(eq_pred params.(t1#num) (if t2#is_var then params.(t2#num) else t2))
	   then raise Not_found)
    in
    let nb_candidates = ref 0 in
    let treat_operator o =
      let nb_params = o#parameters#nb in
      let nb_precs = Array.length o#prec in
      let params = Array.copy o#parameters#vars in
      let rec search_params i =
	if i = nb_params then begin
	  if domain#equality then begin
	    verify_equality_rules params (==) o#equa ;
	    verify_equality_rules params (!=) o#diff
	  end ;
	  incr nb_candidates ;
	  treat_candidate {o = o ; params = Array.copy params ; pos = nb_precs}
	end
	else
	  let n = o#order.(i) in
	  let constraints = o#constraints.(n) in
	    Array.iter
	      (fun constant ->
		 try
		   if not domain#equality then
		     for j = 0 to i - 1 do
		       if params.(o#order.(j)) == constant then raise Not_found 
		     done ;
		   params.(n) <- constant ;
		   if List.for_all (fun atom -> AtomTable.mem constraints_table (atom#instantiate params)) constraints then
		     search_params (i + 1)
		 with
		   | Not_found -> ()
	      ) (o#parameters#domain n)
      in search_params 0
    in
      domain#operator_iter treat_operator ;
      (!nb_candidates, List.length actions, Array.length init_state)


  (*
    Final representation construction
  *)

  method private final_representation =
    let create_fluent atom =
      let (fluent, present) = self#search_fluent atom in
	if not present then begin Utils.eprint "\nUnreachable goal : %s %i\n\n" fluent#to_string (List.length actions) ; exit 0 end
	else fluent
    in
    let create_goal = function
      | Formula.Top -> Utils.eprint "\nProblem solved -> no action.\n\n" ; exit 0
      | Formula.PosLit atom -> [| create_fluent atom |]
      | Formula.Conjunct c -> 
	  Array.map (function 
		       | Formula.PosLit atom -> create_fluent atom
		       | _ -> assert false) c
      | _ -> assert false
    in 
    let init_action action = 
      try
	Array.iter (fun f -> if f#is_goal then action#set_rescue) action#del ;
	action#set_num nb_actions ;
	nb_actions <- nb_actions + 1 ;
	Array.iter (fun f -> f#add_consumer action) action#prec ;
	Array.iter (fun f -> f#add_producer action) action#add ;
	Array.iter (fun f -> f#add_deleter action) action#del ;
	true
      with Exit -> false
    in
      goal <- create_goal (goal_temp) ;
      Array.iter (fun f -> f#set_init) init_state ;
      Array.iter (fun f -> f#set_goal) goal ;
      actions_final <- Array.of_list (List.filter init_action actions) ;
      AtomTable.iter (fun atom fluent -> fluent#finalize) fluents_table ;
  (*    init_state <-
      Array.of_list
	(Array.fold_right
	   (fun fluent fluents ->
	      if fluent#deleters = [| |] then begin
		fluent#make_relevant ;
		Array.iter (fun a -> a#remove_prec fluent) fluent#consumers ;
		Array.iter (fun a -> a#remove_add fluent) fluent#producers ;
		fluents
	      end
	      else fluent :: fluents) init_state []) ; 
      goal <- Array.of_list (Array.fold_right (fun fluent fluents -> if not fluent#relevant then fluent :: fluents else fluents) goal []) ; *)
      if goal = [| |] then begin Utils.eprint "\nProblem solved -> no action.\n\n" ; exit 0 end ;	
      AtomTable.iter (fun atom fluent -> if not fluent#relevant then fluents <- fluent :: fluents) fluents_table ;
      fluents_final <- Array.of_list fluents ;
(*      Utils.eprint "fluents : %s" (Utils.string_of_list "\n" (fun f -> f#to_istring) fluents) ;
      Utils.eprint "fluents but : %s" (Utils.string_of_array "\n" (fun f -> f#to_istring) goal) ; *)
      actions_always_final <- Array.of_list (List.filter (fun a-> a#prec = [| |]) actions) ;

(*      List.iter (fun action -> Utils.eprint "\n%s" action#to_complete_istring) actions ; *)
      (*Array.iter (fun action -> Utils.eprint "\n%s" action#to_complete_istring) actions_final ;*)

      let n = ref 0 in 
	Array.iter (fun f -> f#set_num !n ; incr n) fluents_final ;
	nb_fluents <- !n ;
	n := 0 ;
	Array.iter (fun f -> f#set_num !n ; incr n) actions_final ;
	nb_actions <- !n ;
	Array.length init_state


  val mutable preparation_time = 0.
  val mutable final_parsing_time = 0.
  val mutable final_plan = ""

  method search =
    let  (search_time, plan) = Utils.my_time2 (fun () -> self#run) in
      if plan = self#plan_fail then 
	Utils.eprint "\nNo plan.\n\n"
      else
	(match plan#valid self#init_state self#goal with
	   | (true, nb) -> 
	       final_plan <- plan#to_ipc_string ; Utils.eprint "\n\n%s\nValid plan : %i actions.\n" plan#to_ipc_string nb 
	   | _ -> Utils.eprint "\n%s\n\n****************** INCORRECT PLAN ******************\n\n" plan#to_string) ;
      self#print_statistics ;
      Utils.eprint "Preparation time : %.2f\nSearch time : %.2f\nTotal time : %.2f\n\n" 
	preparation_time search_time (preparation_time +. search_time) ;


  initializer
  let (lexing_time, _) = Utils.my_time "\nParsing domain" (fun () -> self#parse_domain) in
  let (parsing_time, _) = Utils.my_time "Parsing problem" (fun () -> self#parse_problem) in
  let (domains_time, _) = Utils.my_time "Computing domains" (fun () -> self#domains_creation) in
    Utils.eprint "\n\n%s\n\n" problem#to_string ;
(**)    Utils.eprint "\n\n%s\n\n" domain#to_complete_istring ;

(*Utils.eprint "\nFUNCTIONS VALUES:\n"; List.iter (fun (atom,value) -> Utils.eprint " %s = %f\n" atom#to_string value) problem#functions_value_list; Utils.eprint "\n";*)

    if Array.length Sys.argv >= 4 && Sys.argv.(3) = "no" then exit 0 ;
    let (instantiation_time, (nb_candidates, nb_actions, nb_fluents_init)) =
      Utils.my_time "Instantiating actions" (fun () -> self#create_actions (*;
let action = new (Node#action "Init" [| |] 0 0 [| |] self#goal [| |]) in actions <- action :: actions
*)
) in
    let (final_repr_time, nb_fluents_init2) = 
      Utils.my_time "Computing final representation" (fun () -> self#final_representation) in
      Utils.eprint "\nParsing time : %.2f\n\
	  Domains creation  time : %.2f\n\
		    Instantiation time : %.2f -> %i candidates, %i actions\n\
		    Finalization time : %.2f -> %i fluents, %i init\n\n"
	(lexing_time +. parsing_time)
	domains_time
	instantiation_time nb_candidates nb_actions
	final_repr_time nb_fluents nb_fluents_init2 ;
      preparation_time <- lexing_time +. parsing_time +. domains_time +. instantiation_time +. final_repr_time ;
      final_parsing_time <- lexing_time +. parsing_time

end
