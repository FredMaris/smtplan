let max_level = 256
(* !!! attention !!!, c'est max_level défini dans le fichier pGraph qui est utilisé par les No-good et non max_level décrit ci-dessus *)

(*
  Node common
*)

class node_common =
object (self)

  val mutable level = -1
  method level = level
  method set_level nlevel = level <- nlevel

  val mutable n = 0
  method n = n
  method set_n nn = n <- nn

  val mutable n1 = 0
  method n1 = n1

  val mutable ln1 =[]
  method ln1 = ln1
  method set_n1 nn = n1 <- nn ; ln1 <- nn::ln1

  method sup_ln1 = ln1 <- List.tl ln1 ; if ln1=[] then n1 <- 0 else n1 <- List.hd ln1

  method is_n1 nn = List.mem nn ln1

end

(*
  Fluent nodes
*)

class ['action] fluent atom =
object (self)
  inherit node_common
  inherit ['action] Node.fluent atom

  val cons = Array.create max_level ([] : 'action list)
  val prod = Array.create max_level ([] : 'action list)
  val del = Array.create max_level ([] : 'action list)

  method cons level = cons.(level)
  method prod level = prod.(level)
  method del level = del.(level)

  val mutable new_cons = []
  val mutable new_prod = []
  val mutable new_del = []

  method new_cons = new_cons
  method new_prod = new_prod
  method new_del = new_del

  method init_level level =
    cons.(level) <- cons.(pred level) ;
    prod.(level) <- prod.(pred level) ;
    del.(level) <- del.(pred level) ;
    new_cons <- [] ;
    new_prod <- [] ;
    new_del <- []

  method add_cons level action =
    cons.(level) <- action :: cons.(level) ;
    new_cons <- action :: new_cons

  method add_prod level action =
    prod.(level) <- action :: prod.(level) ;
    new_prod <- action :: new_prod

  method add_del level action =
    del.(level) <- action :: del.(level) ;
    new_del <- action :: new_del

  method sort_prod level =
    prod.(level) <- List.sort 
      (fun a1 a2 -> 
	 if a1#is_noop then -1
	 else if a2#is_noop then 1
	 else match compare a1#level a2#level with 
	   | 0 -> -1
	   | c -> c) prod.(level)

  method noop = List.find (fun a -> a#is_noop) prod.(succ level)

end


(*
  Action nodes
*)

class ['fluent] action ope params duration quality prec add del =
object (self)
  inherit node_common
  inherit ['fluent] Node.action ope params duration quality prec add del
  val mutable valeur = new Valeur.valeur false quality
  val mutable is_noop = false
  method is_noop = is_noop
  method set_noop = is_noop <- true ; valeur#setValeur(0)
  method getValeur = valeur

end

(*
  Planning Graph
*)

class virtual ['fluent, 'action, 'plan] t =
object (self)
  inherit ['fluent, 'action, 'plan] PlanningData.t as pdata
  val mutable fluents = []
  val mutable deleted_fluents = []
  val mutable actions = []
  val mutable new_fluents = []
  val mutable new_actions = []
  val mutable nb_actions = 0
  val mutable nb_fluents = 0
  val mutable level = 0
  val mutable nb_goals = 0
  val mutable goal = []

  val mutable fmutex = [| |]
  val mutable amutex = [| |]
  val mutable fmutex_list = []

  val mutable perm_amutex = 0
  val mutable nb_amutex = 0
  val mutable nb_fmutex = 0
  val mutable old_fmutex = 0
  val mutable all_fluents = [| |]

  method level = level

  method print_statistics = ()

  initializer
    nb_goals <- Array.length pdata#goal ;
    goal <- Array.to_list pdata#goal ;
    fmutex <- Array.create (pdata#nb_fluents) [| |] ;
    amutex <- Array.create (pdata#nb_actions + pdata#nb_fluents)  [| |] ;
    all_fluents <- Array.create (pdata#nb_fluents) pdata#goal.(0) ;
    Array.iter self#add_fluent pdata#init_state ;

  method add_fluent fluent =
    fluent#set_n nb_fluents ;
    all_fluents.(fluent#n) <- fluent ;
    nb_fluents <- succ nb_fluents ;
    fluent#set_level level ;
    fluents <- fluent :: fluents ;
    new_fluents <- fluent :: new_fluents ;
    if fluent#is_goal then nb_goals <- pred nb_goals ;
    self#init_fmutex fluent

  method add_action action =
    action#set_n nb_actions ;
    nb_actions <- succ nb_actions ;
    action#set_level level ;
    actions <- action :: actions ;
    new_actions <- action :: new_actions ;
    Array.iter (fun f -> f#add_cons level action) action#prec ;
    Array.iter (fun f -> f#add_prod level action) action#add ;
    Array.iter (fun f -> if f#level = -1 then deleted_fluents <- f :: deleted_fluents ; f#add_del level action) action#del ;
    self#init_amutex action

  method consistant_fluents level fluents =
    Utils.prod_never1 (self#is_fmutex level) fluents

  val mutable precmutex_actions = []

  method find_actions =
    let propagate_action action =
      self#add_action action ;
      Array.iter (fun f -> if f#level = -1 then self#add_fluent f) action#add
    in
    let try_propagate_action action =
      if action#zero_prec_unsat then begin
	if self#consistant_fluents (pred level) (Array.to_list action#prec) then
	  propagate_action action
	else
	  precmutex_actions <- action :: precmutex_actions
      end
    in
    let propagate_fluent fluent = 
      let noop = (self#create_action ("NOOP-"^fluent#to_string) [| |] 1 0 [|fluent,(0,0)|] [|fluent,(0,0)|] [| |]) in
	self#add_action  noop ;
	noop#set_noop ;
	Array.iter try_propagate_action fluent#consumers
    in
    let nfluents = new_fluents in
      new_fluents <- [] ;
      new_actions <- [] ;
      List.iter propagate_fluent nfluents ;
      if level = 1 then Array.iter propagate_action pdata#actions_always ;
      precmutex_actions <- List.find_all
	(fun action -> 
	   if self#consistant_fluents (pred level) (Array.to_list action#prec) then begin
	     propagate_action action ; false end
	   else true) precmutex_actions
	
  method init_fmutex f = fmutex.(f#n) <- Array.create (succ f#n) (-1)
  method init_amutex a = amutex.(a#n) <- Array.create (succ a#n) 0

  method add_fmutex f1 f2 =
    nb_fmutex <- succ nb_fmutex ;
    fmutex.(max f1#n f2#n).(min f1#n f2#n) <- level ;
    fmutex_list <- (f1, f2) :: fmutex_list

  method add_amutex permanent a1 a2 =
    amutex.(max a1#n a2#n).(min a1#n a2#n) <-
    if permanent then begin perm_amutex <- succ perm_amutex ; max_int end
    else begin nb_amutex <- succ nb_amutex ; level end

  method is_fmutex level f1 f2 = fmutex.(max f1#n f2#n).(min f1#n f2#n) >= level
  method is_amutex level a1 a2 = amutex.(max a1#n a2#n).(min a1#n a2#n) >= level

  method extend_mutex =
  (* transforme tous les mutex en mutex permanent puisque level-off atteint *)
    let extend mutex =
      try
	for i = 0 to Array.length mutex -1 do
	  if mutex.(i) = [| |] then raise Exit ;
	  Array.iteri (fun j x -> if x = level then mutex.(i).(j) <- max_int) mutex.(i)
	done 
      with Exit -> ()
    in
      extend fmutex ;
      extend amutex
	
  method inconsistant_effect_mutex =
    let mutex a1 a2 =
      if a1 != a2 && not(self#is_amutex level a1 a2) then self#add_amutex true a1 a2
    in
      List.iter 
	(fun fluent ->
	   Utils.prod_apply2 mutex fluent#new_prod (fluent#del level) ;
	   Utils.prod_apply2 mutex (fluent#prod (pred level)) fluent#new_del
	) fluents

  method crossed_interactions_mutex =
    let mutex a1 a2 =
      if a1 != a2 && not(self#is_amutex level a1 a2) then self#add_amutex true a1 a2
    in
      List.iter 
	(fun fluent ->
	   Utils.prod_apply2 mutex fluent#new_cons (fluent#del level) ;
	   Utils.prod_apply2 mutex (fluent#cons (pred level)) fluent#new_del
	) fluents

  method concurrent_preconditions_mutex = 
    let mutex a1 a2 =
      if not(self#is_amutex level a1 a2) then self#add_amutex false a1 a2
    in
      List.iter 
	(fun (fluent1, fluent2) ->
	   Utils.prod_apply2 mutex (fluent1#cons level) (fluent2#cons level))
	fmutex_list
	
  method inconsistant_support_mutex =
    let mutex f1 f2 =
      if Utils.prod_for_all (self#is_amutex level) (f1#prod level) (f2#prod level) then
	self#add_fmutex f1 f2
    in
    let old_fmutex_list = fmutex_list in
      fmutex_list <- [] ;
      List.iter (fun (fluent1, fluent2) -> mutex fluent1 fluent2) old_fmutex_list ;
      Utils.prod_apply mutex new_fluents fluents

  method extend_common =
    level <- succ level ;
    List.iter (fun f -> f#init_level level) fluents ;
    deleted_fluents <- List.filter (fun f -> f#level = -1) deleted_fluents ;
    List.iter (fun f -> f#init_level level) deleted_fluents

  val mutable listeNiveaux = []
  val mutable listeNiveauxRecup = []

  method listeNiveaux = listeNiveaux

  method extend_level_off =
    self#retablirNiveau ;
    self#extend_common ;
    self#recupererNiveau ;

  (* A chaque fluent est attribué un numéro pour chaque niveau dans lequel il est présent. Ce numéro, n1, a comme valeur numeroFluentRecup + n ou n est le numero du fluent independant du niveau *)

  val mutable numeroFluent = 1
  val mutable numeroFluentRecup = 1
  val mutable numeroAction = 1
  val mutable numeroActionRecup = 1

  method recupererNiveau =
    let recupere mutex numero =
      let list_mut = ref [] in
	for i = 0 to Array.length mutex -1 do
	  Array.iteri (fun j x -> if x >= level then list_mut := (i + numero, j + numero) :: !list_mut) mutex.(i)
	done ;
      !list_mut
    in
      numeroFluentRecup <- numeroFluent ;
      numeroActionRecup <- numeroAction ;
      List.iter (fun e -> e#set_n1 numeroFluent ;
	numeroFluent <- succ numeroFluent) (List.rev fluents) ;
      List.iter (fun e -> e#set_n1 numeroAction ;
	numeroAction <- succ numeroAction) (List.rev actions) ;
      let lf = List.map (fun e -> new FluentGP.fluentGP e#n1 e#to_string) fluents in
      if (level > 0) then
	let la = List.map (fun e -> let a = new ActionGP.actionGP e#n1 e#to_string e#getValeur in a#setNiveau level ; a) actions in
	let lma = recupere amutex (numeroActionRecup) in
	let lmf = recupere fmutex (numeroFluentRecup) in
	let laf = List.concat (List.map (fun e -> List.map (fun g -> g#n1, e#n1) (e#prod level)) fluents) in
	let lfa =
	  if level == 1 then []
	  else List.fold_right (fun e accu -> if e#level < level then (List.map (fun g -> (List.nth e#ln1 1, g#n1)) (e#cons level)) @ accu else accu) fluents []
	in
	  listeNiveaux <- (new NiveauGP.niveauGP lf la laf lfa lmf lma level) :: listeNiveaux ;
	  listeNiveauxRecup <-(new NiveauGP.niveauGP lf la laf lfa lmf lma level)::listeNiveauxRecup;
        else begin
	  listeNiveaux <- (new NiveauGP.niveauGP lf [] [] [] [] [] level) :: listeNiveaux ;
	  listeNiveauxRecup <- (new NiveauGP.niveauGP lf [] [] [] [] [] level)::listeNiveauxRecup;
	  end

  method printNiveau = print_string (List.hd listeNiveaux)#toString

  method listeButs = List.map (fun e -> new FluentGP.fluentGP e#n1 e#to_string) (List.filter (fun e -> e#is_goal) fluents)

  method recupererActions (la : ActionCSP.actionCSP list) =
(* Utils.eprint "\nrecupererActions" ; *)
   let tab = Array.create level [] in
   try List.iter (fun e ->  let i = e#getNiveau in
     Utils.eprint "\n%s e#getNumero e#getNiveau %i" e#toString e#getNiveau ;
     if i > 0 then
       (let a1 = List.find (fun g -> g#is_n1 e#getNumero) actions in
	Utils.eprint "\t%s a1#num %i" a1#to_string a1#num ;
	if not a1#is_noop then tab.(i-1) <- tab.(i-1) @ [a1])) la ;
     Array.to_list tab
    with Not_found -> Utils.eprint "\nimpossible recup action" ; failwith ("impossible recup action")

  method extend =
    self#extend_common ;
    (* print_string("nb actions : "^string_of_int(nb_actions)^" nb fluents : "^string_of_int(nb_fluents)^"\n"); *)
    nb_amutex <- 0 ;
    old_fmutex <- nb_fmutex ;
    nb_fmutex <- 0 ;
    self#find_actions ;
    List.iter (fun fluent -> fluent#sort_prod level) fluents ;
    self#inconsistant_effect_mutex ;
    self#crossed_interactions_mutex ;
    self#concurrent_preconditions_mutex ;
    self#inconsistant_support_mutex ;
    self#recupererNiveau

  method goals_found = nb_goals = 0 && self#consistant_fluents level goal

  method level_off = level > 0 && nb_fmutex = old_fmutex && new_fluents = []

  method extend_until_goals ?(m1 = ignore) ?(m2 = ignore) () =
    let goals_found = ref self#goals_found in
      m1 (flush stderr) ;
      while not !goals_found && not self#level_off do
	self#extend ;
	goals_found := self#goals_found ;
	m2 (flush stderr) ;
      done ;
      !goals_found

  method extend_until_level_off ?(m1 = ignore) ?(m2 = ignore) () =
    m1 (flush stderr) ;
    while not self#level_off do
      self#extend ;
      m2 (flush stderr)
    done ;
    self#goals_found

  method extend_until_level_off_or_level ?(m1 = ignore) ?(m2 = ignore) lev =
    let goal_time = ref (-1) in
      m1 (flush stderr) ;
      while (not self#level_off) && level < lev do
	if !goal_time = -1 && self#goals_found then goal_time := level ;
	self#extend ;
	m2 (flush stderr)
      done ;
      (self#goals_found, !goal_time)

  method solveTool quality =
    let m4 = fun () -> Utils.eprint "\nsolve begin\n\n" in
    let (succes, lstaction) = Gpcsp.toWcspFile "test.wcsp" self#listeNiveaux self#listeButs quality in
    if (begin m4 (flush stderr) ; succes end)
    then
      let pplan = new ParallelPlan.t succes
      in begin
        (* Utils.eprint "\nICI" ; *)
	let planint = self#recupererActions lstaction in
	ignore (pplan#set_plan planint) ;
        (* Utils.eprint "\nnouveau plan trouvé quality = %i\n" pplan#quality ; *)
	pplan end
    else self#plan_fail
	
  method retablirNiveau =
(*!!!!!!!!   listeNiveaux <- listeNiveauxRecup *)
      listeNiveaux <- List.map (fun e -> new NiveauGP.niveauGP e#getListeFluents e#getListeActions e#getListeArcsActionFluent e#getListeArcsFluentAction e#getListeMutexFluents e#getListeMutexActions e#getNumero) listeNiveauxRecup

  method retablirNiveau2 =
   List.iter (fun e -> e#sup_ln1) fluents ;
   List.iter (fun e -> e#sup_ln1) actions ;
   listeNiveauxRecup <- List.tl listeNiveauxRecup ;
   numeroFluent <- numeroFluentRecup ;
   numeroAction <- numeroActionRecup ;
   self#retablirNiveau ;
   self#recupererNiveau ;

  method run =
    let m1 = fun () -> Utils.eprint "\n\nBuilding the planning-graph...\nLevel 0 - Fluents : %i\n" nb_fluents in
    let m2 = fun () -> Utils.eprint "Level %i - Fluents : %i - Actions : %i - FMutex : %i - AMutex : %i\n"
      level nb_fluents nb_actions nb_fmutex (perm_amutex + nb_amutex) in
    let m3 = fun () -> Utils.eprint "No solution.\n" in
    let m4 = fun () -> Utils.eprint "\nGraph has leveled off.\n\n" in

    let goals_found = self#extend_until_goals ~m1:m1 ~m2:m2 () in
    let rplan = ref self#plan_fail in
    try
      if not goals_found then  m3 (flush stderr)
      else begin
	while not self#level_off do
	  (* cherche première solution *)
	  let plan = if level >= 0 then self#solveTool !rplan#quality else self#plan_fail in
	  if plan#succes then begin
	    plan#update_quality ;
	    (* met à jour la meilleure qualité trouvée *)
	    if !rplan#quality > plan#quality then (begin
	      Utils.eprint "not self#level_off solution Success Rquality %i quality %i\n" !rplan#quality plan#quality ;
              rplan := plan end) ;
	    (* succes -> fin de l'algorithme *)
	    raise Exit end  ;
	  self#extend ;
	  m2 (flush stderr)
	done ;
	m4 (flush stderr) ;
	self#extend_mutex ;
	(* Utils.eprint "\nself#extend_mutex ok" ; *)
	self#retablirNiveau2 ;
	while true do
	(* Utils.eprint "\nwhile true" ; *)
	  let plan = if level >= 0 then self#solveTool !rplan#quality else self#plan_fail in
	  if plan#succes then begin
		plan#update_quality ;
		if !rplan#quality > plan#quality then (begin
		  Utils.eprint "self#level_off solution Success Rquality %i quality %i\n" !rplan#quality plan#quality ;
		  rplan := plan end) ;
		raise Exit end ;
	  (* Utils.eprint "\nself#extend_level_off" ; *)
	  self#extend_level_off ;
	  (* Utils.eprint "\nm2" ; *)
	  m2 (flush stderr) ;
	  (* Utils.eprint "\nok" ; *)
	done
      end ;
      !rplan
    with Exit -> !rplan

end