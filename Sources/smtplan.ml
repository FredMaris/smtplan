class node_common =
object (self)

  val mutable n = Array.make 256 0
  method n level = n.(level)
  method set_n level nn = n.(level) <- nn

  val mutable level = max_int
  method level = level
  method set_level nlevel = level <- nlevel

end

class fluent atom =
object (self)
  inherit node_common
  inherit [action] Node.fluent atom

  val string = 
    let bs = Bytes.of_string (atom#pred#to_string ^ 
    (if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms)))
(*    ^ "[" ^ (string_of_int (fst atom#timeset)) ^ ";" ^ (string_of_int (snd atom#timeset)) ^ "]" *) in
    for i = 0 to (Bytes.length bs) - 1 do
     if (Bytes.get bs i) = '-' then (Bytes.set bs i '_');
    done;
    Bytes.to_string bs

  val mutable neglevel = max_int
  method neglevel = neglevel
  method set_neglevel nneglevel = neglevel <- nneglevel

  method to_string = string
end


and action name params duration quality prec nprec add del condadd =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del condadd

  val mutable maxlevel = -1

  val string = 
    let bs = Bytes.of_string (name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params)) in
    for i = 0 to (Bytes.length bs) - 1 do
     if (Bytes.get bs i) = '-' then (Bytes.set bs i '_');
    done;
    Bytes.to_string bs

  method maxlevel = maxlevel
  method set_maxlevel maxl = maxlevel <- maxl

  method to_string = string

  method to_complete_string = 
    let string_of_fluent_array fluents =
      Utils.string_of_array "#" Utils.to_string fluents in
      "s$Prec$" ^ string ^ "#" ^ string_of_fluent_array prec ^ "#\n" ^
      "s$Add$" ^ string ^ "#" ^ string_of_fluent_array add ^ "#\n" ^
      "s$Del$" ^ string ^ "#" ^ string_of_fluent_array del ^ "#\n"
end


class plan succes = 
object 
  inherit [fluent, action] SequentialPlan.t succes
end

class ['fluent, 'action, 'plan] tsp_common =
object
  method create_fluent = new fluent
  method create_action = new action
  val plan_succes = new plan true
  val plan_fail = new plan false
  method plan_succes = plan_succes
  method plan_fail = plan_fail
end


class t =
object (self)
  inherit [fluent, action, plan] PlanningData.t  as pdata
  inherit [fluent, action, plan] tsp_common

  val mutable solved = false
  val mutable nb = 0
  val mutable nbc = 0
  val mutable rpg_max_level = 0

  method print_statistics = ()
  method run = self#plan_fail




  method search = (* search_real : supprimer _real pour obtenir SMTPLAN original *)
    let (search_time,_) = Utils.my_time "Searching plan (SMT-PLAN algorithm)" (fun () -> self#notimed_search) in
      Utils.eprint "Total search time : %.2f\n" search_time

  method notimed_search =

    let setminlevel = if Array.length Sys.argv > 4 then int_of_string Sys.argv.(4) else 1 in

    let f_exists (f : 'fluent) (a_set : 'fluent array) = 
      Array.fold_left (fun t (fluent : 'fluent) ->
        (fluent#atom#equal f#atom) || t
      ) false a_set
    in

    let get_f_timedata (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
    (*let timedata_null = (new Timedata.t) in*)
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then timedata
        else t
      ) (snd (Array.get a_iset 0)) a_iset
    in

    let get_f_ce_time fstsnd (c,e) a_icondset = 
      let r = (Array.fold_left (fun t x -> if ((fst (fst x))#atom#equal c#atom) && ((fst (snd x))#atom#equal e#atom) then (snd (fstsnd x))#timeset else t) (-1.0,-1.0) a_icondset) in
      if r == (-1.0,-1.0) then begin Utils.eprint "STM-PLAN Fatal Error : timelabel not found\n"; exit 0 end
      else r
    in

    let get_f_time f a_iset = 
      let r = (Array.fold_left (fun t x -> if ((fst x)#atom#equal f#atom) then (snd x)#timeset else t) (-1.0,-1.0) a_iset) in
      if r == (-1.0,-1.0) then begin Utils.eprint "STM-PLAN Fatal Error : timelabel not found\n"; exit 0 end
      else r
    in
    let get_f f a_iset =
      Array.fold_left (fun c x -> if ((fst x)#atom#equal f#atom) then (fst x) else c) f a_iset
    in
    let current_level = ref 0 in
    let build_rpg = (* Building the Temporal Relaxed Planning Graph *)
      Utils.print "\nBuilding the Temporal Relaxed Planning Graph ...\n";
      Array.iter (fun f ->
                   (if (List.exists (fun p ->
                                        (p#atom#equal f#atom))
                        (Array.to_list pdata#init_state))
                    then f#set_level 0))
      pdata#fluents;
      while (not (List.for_all (fun p -> 
                    (List.exists (fun f ->
                       (f#atom#equal p#atom) && (f#level <= !current_level))
                    (Array.to_list pdata#fluents)))
                  (Array.to_list pdata#goal)))
      && (!current_level<=255) do
        Array.iter (fun a -> (if (a#level > !current_level) &&
          (Array.fold_left (fun b prec -> b &&
            prec#level <= !current_level)
           (* Array.fold_left (fun c f -> c ||
              ((f#atom#equal prec#atom) && (f#level <= !current_level)))
            false pdata#fluents) *)
          true a#prec)
          then begin
               a#set_level (succ !current_level);
               Array.iter (fun f ->
                   if (f#level > !current_level) && (List.exists (fun add ->
                                 (f#atom#equal add#atom))
                                 ((Array.to_list a#add) @ (Array.fold_left (fun l (c,f) -> if c#level <= !current_level then f :: l else l) [] a#condadd)) )
                   then f#set_level (succ !current_level))
               pdata#fluents;
               Array.iter (fun f ->
                   if (f#neglevel > !current_level) && (List.exists (fun del ->
                                 (f#atom#equal del#atom))
                                 (Array.to_list a#del))
                   then f#set_neglevel (succ !current_level))
               pdata#fluents;
               end))
          pdata#actions;
        (*Array.iter (fun a -> if (a#level > !current_level) &&
               (List.for_all (fun prec ->
                    (List.exists (fun f ->
                       (f#atom#equal prec#atom) && (f#level <= !current_level))
                    (Array.to_list pdata#fluents)))
                  (Array.to_list a#prec))
               then a#set_level (succ !current_level))
        pdata#actions;*)
 
       current_level := succ !current_level
      done;
      rpg_max_level <- !current_level;
Utils.print "Goal found at level %d.\n" !current_level;
(*Array.iter (fun a -> Utils.print "%s(level[%d])\n" a#to_string a#level) pdata#actions;
Array.iter (fun f -> Utils.print "%s(level[%d],neglevel[%d])\n" f#to_istring f#level f#neglevel) pdata#fluents;*)

    in

(* 
    let solver = (new Smtsolver.t) in
    let smtwrite = solver#smtwrite in

      solver#set_smtfilename "smtplan.smt";  *)
      build_rpg;

(* TEST - RECHERCHE COMPLETE : *) (* rpg_max_level <- 1; *)

while not solved do

if setminlevel <= rpg_max_level then begin (* Begin search if current maxlevel >= minlevel *)

(* Reducing Graph *)
  let subgoals = ref [] in
  let newsubgoals = ref [] in
    subgoals := (Array.to_list pdata#goal);
    for i = rpg_max_level downto 1 do
      List.iter (fun (f : 'fluent) ->
        Array.iter (fun (a : 'action) ->
          if (a#maxlevel < i) then
          begin
            a#set_maxlevel i;
            Array.iter (fun (p : 'fluent) ->
              newsubgoals := p :: !newsubgoals;
            ) a#prec
          end
        ) (Array.append f#producers f#condproducers)
      ) !subgoals;
      subgoals := (List.append !newsubgoals !subgoals);
      newsubgoals := [];
    done;

      Utils.print "\nSearching plan with SMT encoding at level %d\n" rpg_max_level;
      

(*let encode_float x = "(/ " ^ string_of_int (int_of_float (x*.1000000.0)) ^ " 1000000)"
in*)
let encode_float x = (string_of_float x) ^ "0"
in 

    let solver = (new Smtsolver.t) in
    let smtwrite = solver#smtwrite in

     (* solver#set_smtfilename "smtplan.smt"; *)
      solver#open_smtwrite;

(*     smtwrite (Printf.sprintf "%s\n" solver#stringtest);
      solver#close_smtwrite;
      if solver#launch then Utils.print "STM_SAT\n";
      exit 0; *)

(** OPTION UNSAT-CORE **)
  (*    smtwrite "(set-option :produce-unsat-cores true)\n"; *)

      solver#smtwrite "(set-logic QF_RDL)\n(declare-fun St_spy_variable () Real)\n(declare-fun t_Init () Real)\n(declare-fun t_Goal () Real)\n";
(*      solver#smtwrite (Printf.sprintf "(benchmark default.smt\n:source {\nSMTPLAN2015 automated Planning to SMT-LIB v2.0 encoding\nby F.Maris, IRIT - Universite Paul Sabatier, Toulouse\nBenchmark encoded from planning problem\ndomain : %s, problem : %s\n}\n (set-logic QF_RDL)\n:(declare-fun St_spy_variable () Real)\n(declare-fun t_Init () Real)\n(declare-fun t_Goal () Real)\n" pdata#domain_name pdata#problem_name); *)

      nb <- 2;

      for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nb <- nb + 1 ; a#set_n i nb ; smtwrite (Printf.sprintf "(declare-fun t_%s_%i () Real)\n" a#to_string i) end) pdata#actions ;
      done ;
      
for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(declare-fun t_CondBegin_%s_%s_%d () Real)\n" f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_CondEnd_%s_%s_%d () Real)\n" f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_VCondBegin_%s_%s_%d () Real)\n" f#to_string a#to_string i);
(*			smtwrite (Printf.sprintf "(declare-fun t_VNCondBegin_%s_%s_%d () Real)\n" f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_VNCondEnd_%s_%s_%d () Real)\n" f#to_string a#to_string i); *)
			nb <- nb + 3; (* + 2 more for conditional effects *)
		) a#prec;
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(declare-fun t_AddBegin_%s_%s_%d () Real)\n" a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_AddEnd_%s_%s_%d () Real)\n" a#to_string f#to_string i);
			nb <- nb + 2;
		) a#add;
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(declare-fun t_DelBegin_%s_%s_%d () Real)\n" a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_DelEnd_%s_%s_%d () Real)\n" a#to_string f#to_string i);
			nb <- nb + 2;
		) a#del;
		Array.iter (fun (c,e) ->
			smtwrite (Printf.sprintf "(declare-fun t_CECondBegin_%s_%s_%s_%d () Real)\n" c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_CECondEnd_%s_%s_%s_%d () Real)\n" c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_CEAddBegin_%s_%s_%s_%d () Real)\n" a#to_string c#to_string e#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_CEAddEnd_%s_%s_%s_%d () Real)\n" a#to_string c#to_string e#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_VCECondBegin_%s_%s_%s_%d () Real)\n" c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_VNCECondBegin_%s_%s_%s_%d () Real)\n" c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(declare-fun t_VNCECondEnd_%s_%s_%s_%d () Real)\n" c#to_string e#to_string a#to_string i);
			nb <- nb + 7;
		) a#condadd;
		end;
	) pdata#actions;
done;

	Array.iter (fun f ->
		smtwrite (Printf.sprintf "(declare-fun t_AddBegin_Init_%s () Real)\n" f#to_string);
		nb <- (succ nb);
	) pdata#init_state;

	Array.iter (fun f ->
		smtwrite (Printf.sprintf "(declare-fun t_CondBegin_%s_Goal () Real)\n" f#to_string);
		smtwrite (Printf.sprintf "(declare-fun t_VCondBegin_%s_Goal () Real)\n" f#to_string);
		nb <- nb + 2;
	) pdata#goal;


     Utils.print "  Number of real variables : %d\n" nb;




(* ENCODING AXIOMS *)


(*Utils.print "%s\n" pdata#problem#attribute_spaces#to_string;
Utils.print "%s\n" pdata#problem#atom_types#to_string;*)


(*	pdata#operator_iter (fun op -> Utils.print "%s: \n" op#name;
	(*	let pos = ref (-1) in
    	  Utils.string_of_array "," (fun s -> incr pos ; s#to_string ^ ":" ^ Utils.to_string ptypes.(!pos)) op#parameters#vars;
	*)	Array.iter (fun p -> Utils.print "%s( " p#pred#name;
			Array.iter (fun term -> Utils.print "%s " term#name;
			) p#terms;
			Utils.print ") ";
		) op#prec;
		Utils.print "\n";
	);
*)



(* Axiom (A0) : Action Events Definition Constraints *)

for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin
		Array.iter (fun f ->
	        let fts_prec =  fst (get_f_time f a#iprec)  in
	        let fte_prec =  snd (get_f_time f a#iprec)  in
		    smtwrite (Printf.sprintf "(assert (= t_CondBegin_%s_%s_%d (+ t_%s_%d %s)))\n" f#to_string a#to_string i a#to_string i (encode_float fts_prec));
		    smtwrite (Printf.sprintf "(assert (= t_CondEnd_%s_%s_%d (+ t_%s_%d %s)))\n" f#to_string a#to_string i a#to_string i (encode_float fte_prec));
		) a#prec;
		Array.iter (fun f ->
	        let fts_add =  fst (get_f_time f a#iadd)  in
	        let fte_add =  snd (get_f_time f a#iadd)  in
		    smtwrite (Printf.sprintf "(assert (= t_AddBegin_%s_%s_%d (+ t_%s_%d %s)))\n" a#to_string f#to_string i a#to_string i (encode_float fts_add));
		    smtwrite (Printf.sprintf "(assert (= t_AddEnd_%s_%s_%d (+ t_%s_%d %s)))\n" a#to_string f#to_string i a#to_string i (encode_float fte_add));
		) a#add;
		Array.iter (fun f ->
	        let fts_del =  fst (get_f_time f a#idel)  in
	        let fte_del =  snd (get_f_time f a#idel)  in
		    smtwrite (Printf.sprintf "(assert (= t_DelBegin_%s_%s_%d (+ t_%s_%d %s)))\n" a#to_string f#to_string i a#to_string i (encode_float fts_del));
		    smtwrite (Printf.sprintf "(assert (= t_DelEnd_%s_%s_%d (+ t_%s_%d %s)))\n" a#to_string f#to_string i a#to_string i (encode_float fte_del));
		) a#del;
		end;
	) pdata#actions
done;



(* Axiom (A1) : Action Events Activation *)

for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_CondBegin_%s_%s_%d 0)))\n" a#to_string i f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_CondEnd_%s_%s_%d 0)))\n" a#to_string i f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_VCondBegin_%s_%s_%d 0)))\n" a#to_string i f#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_VCondBegin_%s_%s_%d 0) (>= t_%s_%d 0)))\n" f#to_string a#to_string i a#to_string i);
			nbc <- nbc + 4;
		) a#prec;
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_AddBegin_%s_%s_%d 0)))\n" a#to_string i a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_AddEnd_%s_%s_%d 0)))\n" a#to_string i a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_AddBegin_%s_%s_%d 0) (>= t_%s_%d 0)))\n" a#to_string f#to_string i a#to_string i);
			nbc <- nbc + 3;
		) a#add;
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_DelBegin_%s_%s_%d 0)))\n" a#to_string i a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (>= t_DelEnd_%s_%s_%d 0)))\n" a#to_string i a#to_string f#to_string i);
			smtwrite (Printf.sprintf "(assert (=> (>= t_DelBegin_%s_%s_%d 0) (>= t_%s_%d 0)))\n" a#to_string f#to_string i a#to_string i);
			nbc <- nbc + 3;
		) a#del;
		Array.iter (fun (c,e) ->		
			smtwrite (Printf.sprintf "(assert (=> (>= t_CEAddBegin_%s_%s_%s_%d 0) (>= t_%s_%d 0)))\n" a#to_string c#to_string e#to_string i a#to_string i);
			nbc <- nbc + 1;
		) a#condadd;
		end;
	) pdata#actions;
done;
	Array.iter (fun f ->
		smtwrite (Printf.sprintf "(assert (>= t_CondBegin_%s_Goal 0))\n" f#to_string);
		smtwrite (Printf.sprintf "(assert (>= t_VCondBegin_%s_Goal 0))\n" f#to_string);
		nbc <- nbc + 2;
	) pdata#goal;
	Array.iter (fun f ->
	(* Modifier pour prendre en compte état initial daté *)
		smtwrite (Printf.sprintf "(assert (= t_AddBegin_Init_%s 0))\n" f#to_string);
		nbc <- nbc + 1;
	) pdata#init_state;

(* Axiom : Virtual Events Ordering for Conditions *)

for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "(assert (=> (>= t_%s_%d 0) (<= t_VCondBegin_%s_%s_%d t_CondBegin_%s_%s_%d)))\n" a#to_string i f#to_string a#to_string i f#to_string a#to_string i);
			nbc <- nbc + 1;
		) a#prec;
	) pdata#actions;
done;
	Array.iter (fun f ->
		smtwrite (Printf.sprintf "(assert (<= t_VCondBegin_%s_Goal t_CondBegin_%s_Goal))\n" f#to_string f#to_string);
	    nbc <- nbc + 1;
	) pdata#goal;


(* Axiom : Condition production *)

      for i = 1 to rpg_max_level do
        Array.iter (fun b -> if (b#level <= i) && (b#maxlevel >= i) then begin
          Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
            if ((Array.length f#producers) <> 0) || init_exists then begin
            nbc <- succ nbc;
            smtwrite (Printf.sprintf "(assert (or (< t_%s_%i 0)" b#to_string i);
            Array.iter (fun a ->
              for j = 1 to i - 1 do
                if (a#level <= j) && (a#maxlevel >= j) then begin
                 smtwrite (Printf.sprintf " (= t_VCondBegin_%s_%s_%d t_AddBegin_%s_%s_%d)" f#to_string b#to_string i a#to_string f#to_string j)
                end
              done
            ) f#producers;
            Array.iter (fun a ->
              Array.iter (fun (c,e) -> if f#atom#equal e#atom then
                for j = 1 to i - 1 do
                  if (a#level <= j) && (a#maxlevel >= j) then begin
                   smtwrite (Printf.sprintf " (= t_VCondBegin_%s_%s_%d t_CEAddBegin_%s_%s_%s_%d)" f#to_string b#to_string i a#to_string c#to_string e#to_string j)
 (*  ; Utils.print " (= t_VCondBegin_%s_%s_%d t_CEAddBegin_%s_%s_%s_%d)" f#to_string b#to_string i a#to_string c#to_string e#to_string j *)
                  end
                done
              ) a#condadd
            ) f#condproducers;
            if init_exists then
              smtwrite (Printf.sprintf " (= t_VCondBegin_%s_%s_%d t_AddBegin_Init_%s)" f#to_string b#to_string i f#to_string);
            smtwrite "))\n";
            end
          ) b#prec end
        ) pdata#actions
      done;
      Array.iter (fun f -> let init_exists = (Array.fold_left (fun c x -> (x#atom#equal f#atom) || c) false pdata#init_state) in
        if ((Array.length f#producers) <> 0) || ((Array.length f#condproducers) <> 0) || init_exists then begin
        nbc <- succ nbc;
(*!!! ATTENTION: peut-être pb si un seul élément dans le or *)
        smtwrite "(assert (or";
        Array.iter (fun a ->
          for j = 1 to rpg_max_level do
            if (a#level <= j) && (a#maxlevel >= j) then begin
             smtwrite (Printf.sprintf " (= t_VCondBegin_%s_Goal t_AddBegin_%s_%s_%d)" f#to_string a#to_string f#to_string j)
            end
          done
        ) f#producers;
        Array.iter (fun a ->
          Array.iter (fun (c,e) ->  if f#atom#equal e#atom then
            for j = 1 to rpg_max_level do
               if (a#level <= j) && (a#maxlevel >= j) then begin
               smtwrite (Printf.sprintf " (= t_VCondBegin_%s_Goal t_CEAddBegin_%s_%s_%s_%d)" f#to_string a#to_string c#to_string e#to_string j)
  (* ; Utils.print " (= t_VCondBegin_%s_Goal t_CEAddBegin_%s_%s_%s_%d)" f#to_string a#to_string c#to_string e#to_string j *)
(*; Utils.print "%s: (%s->%s) %s\n" a#to_string c#to_string e#to_string f#to_string *)
                end
            done
          ) a#condadd
        ) f#condproducers;
        if init_exists then
          smtwrite (Printf.sprintf " (= t_VCondBegin_%s_Goal t_AddBegin_Init_%s)" f#to_string f#to_string);
        smtwrite "))\n";
        end
      ) pdata#goal;


(* Axiom : Condition protection *)


for i=1 to rpg_max_level do
	Array.iter (fun b -> if (b#level <= i) && (b#maxlevel >= i) then
		Array.iter (fun f ->
			for j=1 to rpg_max_level do
				Array.iter (fun a -> if (a#level <= j) && (a#maxlevel >= j) (**) && ((i!=j) || (a#n j)!=(b#n i)) (**) then begin
					smtwrite (Printf.sprintf "(assert (or (< t_%s_%d 0) (< t_%s_%d 0) (< t_DelEnd_%s_%s_%d t_VCondBegin_%s_%s_%d) (< t_CondEnd_%s_%s_%d t_DelBegin_%s_%s_%d)))\n" b#to_string i a#to_string j a#to_string f#to_string j f#to_string b#to_string i f#to_string b#to_string i a#to_string f#to_string j);
			        nbc <- nbc + 1;
			      end;
				) f#deleters;
			done;
		) b#prec;
	) pdata#actions;
done;
		Array.iter (fun f ->
			for j=1 to rpg_max_level do
				Array.iter (fun a -> if (a#level <= j) && (a#maxlevel >= j) (* && ((i!=j) || (a#n j)!=(b#n i)) *) then begin
					smtwrite (Printf.sprintf "(assert (or (< t_%s_%d 0) (< t_DelEnd_%s_%s_%d t_VCondBegin_%s_Goal)))\n" a#to_string j a#to_string f#to_string j f#to_string);
					nbc <- nbc + 1;
				  end;
				) f#deleters;
			done;
		) pdata#goal;


(* Axiom : Contradictory effects exclusion *)


for i=1 to rpg_max_level do
	Array.iter (fun b -> if (b#level <= i) && (b#maxlevel >= i) then
		Array.iter (fun f ->
			for j=1 to rpg_max_level do
				Array.iter (fun a -> if (a#level <= j) && (a#maxlevel >= j) && ((i!=j) || (a#n j)!=(b#n i)) then begin
					smtwrite (Printf.sprintf "(assert (or (< t_%s_%d 0) (< t_%s_%d 0) (< t_DelEnd_%s_%s_%d t_AddBegin_%s_%s_%d) (< t_AddEnd_%s_%s_%d t_DelBegin_%s_%s_%d)))\n" b#to_string i a#to_string j a#to_string f#to_string j b#to_string f#to_string i b#to_string f#to_string i a#to_string f#to_string j);
					nbc <- nbc + 1;
				  end;
				) f#deleters;
			done;
		) b#add;
	) pdata#actions;
done;


(* Axioms : Conditional effects, virtual events *)


for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin
		Array.iter (fun (c,e) ->
		
		(* Virtual events for conditional effects *)
		
			smtwrite (Printf.sprintf "(assert (or (< t_%s_%d 0) (>= t_VCECondBegin_%s_%s_%s_%d 0) (>= t_VNCECondBegin_%s_%s_%s_%d 0)))\n" a#to_string i c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (or (< t_VCECondBegin_%s_%s_%s_%d 0) (<= t_VCECondBegin_%s_%s_%s_%d t_CECondBegin_%s_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (<= t_VNCECondBegin_%s_%s_%s_%d t_VNCECondEnd_%s_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (<= t_CECondBegin_%s_%s_%s_%d t_VNCECondEnd_%s_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i);
			smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (<= t_VNCECondEnd_%s_%s_%s_%d t_CECondEnd_%s_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i);
			nbc <- nbc + 5;

(* redondant *)
	(*		smtwrite (Printf.sprintf "(assert (or (< t_CEAddBegin_%s_%s_%s_%d 0) (>= t_VCECondBegin_%s_%s_%s_%d 0)))\n" a#to_string c#to_string e#to_string i c#to_string e#to_string a#to_string i);
*)
			
		(* Enabling conditional effects *)
		
			(* CE Condition production *)
			
	        let init_exists = (Array.fold_left (fun res x -> (x#atom#equal c#atom) || res) false pdata#init_state) in
              if ((Array.length c#producers) <> 0) || ((Array.length c#condproducers) <> 0) || init_exists then begin
              nbc <- succ nbc;
              smtwrite (Printf.sprintf "(assert (or (< t_VCECondBegin_%s_%s_%s_%d 0)" c#to_string e#to_string a#to_string i);
              Array.iter (fun b ->
                for j = 1 to i - 1 do
                  if (b#level <= j) && (b#maxlevel >= j) then begin
                   smtwrite (Printf.sprintf " (= t_VCECondBegin_%s_%s_%s_%d t_AddBegin_%s_%s_%d)" c#to_string e#to_string a#to_string i b#to_string c#to_string j)
                  end
                done
              ) c#producers;
              Array.iter (fun b ->
                for j = 1 to i - 1 do
                  if (b#level <= j) && (b#maxlevel >= j) then begin
                   Array.iter (fun (c2,e2) -> if e2#atom#equal c#atom then
                     smtwrite (Printf.sprintf " (= t_VCECondBegin_%s_%s_%s_%d t_CEAddBegin_%s_%s_%s_%d)" c#to_string e#to_string a#to_string i b#to_string c2#to_string c#to_string j)
                   ) b#condadd
                  end
                done
              ) c#condproducers;
              if init_exists then
                smtwrite (Printf.sprintf " (= t_VCECondBegin_%s_%s_%s_%d t_AddBegin_Init_%s)" c#to_string e#to_string a#to_string i c#to_string);
              smtwrite "))\n";
              end;
            
            (* CE Condition protection *)
            
            for j=1 to rpg_max_level do
				Array.iter (fun b -> if (b#level <= j) && (b#maxlevel >= j) && ((i!=j) || (b#n j)!=(a#n i)) then begin
					smtwrite (Printf.sprintf "(assert (or (< t_VCECondBegin_%s_%s_%s_%d 0) (< t_%s_%d 0) (< t_DelEnd_%s_%s_%d t_VCECondBegin_%s_%s_%s_%d) (< t_CECondEnd_%s_%s_%s_%d t_DelBegin_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i b#to_string j b#to_string c#to_string j c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i b#to_string c#to_string j);
					nbc <- nbc + 1;
				  end;
				) c#deleters;
			done;
			
			(* CE Effect production *)
			
			let fts_add =  fst (get_f_ce_time snd (c,e) a#icondadd)  in
			let fte_add =  snd (get_f_ce_time snd (c,e) a#icondadd)  in
			smtwrite (Printf.sprintf "(assert (or (< t_VCECondBegin_%s_%s_%s_%d 0) (= t_CEAddBegin_%s_%s_%s_%d (+ t_%s_%d %s))))\n" c#to_string e#to_string a#to_string i a#to_string c#to_string e#to_string i a#to_string i (encode_float fts_add));
			smtwrite (Printf.sprintf "(assert (or (< t_VCECondBegin_%s_%s_%s_%d 0) (= t_CEAddEnd_%s_%s_%s_%d (+ t_%s_%d %s))))\n" c#to_string e#to_string a#to_string i a#to_string c#to_string e#to_string i a#to_string i (encode_float fte_add));
			nbc <- nbc + 2;


        (* Disabling conditional effects *)
        
			(* CE Condition destruction *)
			
	        let init_exists = (Array.fold_left (fun res x -> (x#atom#equal c#atom) || res) false pdata#init_state) in
              if ((Array.length c#deleters) <> 0) || (not init_exists) then begin
              nbc <- succ nbc;
              smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0)" c#to_string e#to_string a#to_string i);
              Array.iter (fun b ->
                for j = 1 to i - 1 do
                  if (b#level <= j) && (b#maxlevel >= j) then begin
                   smtwrite (Printf.sprintf " (= t_VNCECondBegin_%s_%s_%s_%d t_DelBegin_%s_%s_%d)" c#to_string e#to_string a#to_string i b#to_string c#to_string j)
                  end
                done
              ) c#deleters;
              if (not init_exists) then
                smtwrite (Printf.sprintf " (= t_VNCECondBegin_%s_%s_%s_%d 0)" (* "t_DelBegin_Init_%s)" *) c#to_string e#to_string a#to_string i);
              smtwrite "))\n";
              end;
            
            (* CE Negation of condition protection *)
            
            for j=1 to rpg_max_level do
				Array.iter (fun b -> if (b#level <= j) && (b#maxlevel >= j) && ((i!=j) || (b#n j)!=(a#n i)) then begin
					smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (< t_%s_%d 0) (< t_AddEnd_%s_%s_%d t_VNCECondBegin_%s_%s_%s_%d) (< t_VNCECondEnd_%s_%s_%s_%d t_AddBegin_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i b#to_string j b#to_string c#to_string j c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i b#to_string c#to_string j);
					nbc <- nbc + 1;
				  end;
				) c#producers;
				Array.iter (fun b -> if (b#level <= j) && (b#maxlevel >= j) && ((i!=j) || (b#n j)!=(a#n i)) then
                   Array.iter (fun (c2,e2) -> if e2#atom#equal c#atom then begin
					  smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (< t_%s_%d 0) (< t_CEAddEnd_%s_%s_%s_%d t_VNCECondBegin_%s_%s_%s_%d) (< t_VNCECondEnd_%s_%s_%s_%d t_CEAddBegin_%s_%s_%s_%d)))\n" c#to_string e#to_string a#to_string i b#to_string j b#to_string c2#to_string c#to_string j c#to_string e#to_string a#to_string i c#to_string e#to_string a#to_string i b#to_string c2#to_string c#to_string j);
					  nbc <- nbc + 1;
					end;
				   ) b#condadd
				) c#condproducers;
			done;
			
			(* CE Disable effect *)
			
			smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (< t_CEAddBegin_%s_%s_%s_%d 0)))\n" c#to_string e#to_string a#to_string i a#to_string c#to_string e#to_string i);
			smtwrite (Printf.sprintf "(assert (or (< t_VNCECondBegin_%s_%s_%s_%d 0) (< t_CEAddEnd_%s_%s_%s_%d 0)))\n" c#to_string e#to_string a#to_string i a#to_string c#to_string e#to_string i);
			nbc <- nbc + 2;        
 
		) a#condadd;
		end;
	) pdata#actions;
done;




(*****


(* Encoding rule n°1 : Init state and Goal *)

smtwrite ":formula\n( and\n(= St_spy_variable (+ 1 t_Init))\nInit Goal\n";
nbc <- 2;



(* Encoding rule n°5 : plan boundaries  *)

smtwrite "(>= t_Goal t_Init)\n";
nbc <- succ nbc;
for i = 1 to rpg_max_level do
Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nbc <- nbc + 2 ; (*a#set_n i nb ;*) let last_effect_time = a#duration (* a modifier pour prendre en compte les effets hors intervalle *) in smtwrite (Printf.sprintf "(or (not %s%i) (>= t_%s%i t_Init))\n(or (not %s%i) (>= (- t_Goal t_%s%i) %s))\n" a#to_string i a#to_string i a#to_string i a#to_string i (encode_float last_effect_time)) end) pdata#actions ;
done ;

*****)





(** OPTION UNSAT-CORE *)
 (*smtwrite "(check-sat)\n(get-unsat-core)\n"; *)


Utils.print "  Number of clauses : %d\n" nbc;

 smtwrite "\n(check-sat)\n(get-value (";

for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin nb <- nb + 1 ; a#set_n i nb ; smtwrite (Printf.sprintf "t_%s_%i " a#to_string i) end) pdata#actions ;
done ;
(*for i = 1 to rpg_max_level do
	Array.iter (fun a -> if (a#level <= i) && (a#maxlevel >= i) then begin
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "t_CondBegin_%s_%s_%d " f#to_string a#to_string i)
		) a#prec;
		Array.iter (fun (c,e) ->
			smtwrite (Printf.sprintf "t_VCECondBegin_%s_%s_%s_%d " c#to_string e#to_string a#to_string i)
		) a#condadd;
		Array.iter (fun f ->
			smtwrite (Printf.sprintf "t_DelBegin_%s_%s_%d " a#to_string f#to_string i)
		) a#del;
		end;
	) pdata#actions;
done;
Array.iter (fun f ->
	smtwrite (Printf.sprintf "t_VCondBegin_%s_Goal " f#to_string)
) pdata#goal;
*)		

smtwrite "))\n";


solver#close_smtwrite;
solved <- solver#launch; (* SOLVER TEST : *) (* if rpg_max_level > 2 then solved <- true; *)

(* TEST STOP 1 : *) (* solved <- true; *)

end; (* End search if current maxlevel >= minlevel *)

if solved then begin
 Utils.print "Floating plan found at level %d (SAT).\n" rpg_max_level;
end else begin
 if setminlevel <= rpg_max_level then Utils.print "No floating plan (UNSAT).\n";
    let expand_rpg = (* Expanding the Temporal Relaxed Planning Graph to next level *)
      Utils.print "Expanding the Temporal Relaxed Planning Graph from level %d to level %d ...\n" rpg_max_level (succ rpg_max_level);
        Array.iter (fun a -> (if (a#level > rpg_max_level) &&
          (Array.fold_left (fun b prec -> b &&
            Array.fold_left (fun c f -> c ||
              ((f#atom#equal prec#atom) && (f#level <= rpg_max_level)))
            false pdata#fluents)
          true a#prec)
          then begin
               a#set_level (succ rpg_max_level);
               Array.iter (fun f ->
                   if (f#level > rpg_max_level) && (List.exists (fun add ->
                                 (f#atom#equal add#atom))
                                 ((Array.to_list a#add) @ (Array.fold_left (fun l (c,f) -> if c#level <= !current_level then f :: l else l) [] a#condadd)) )
                   then f#set_level (succ rpg_max_level))
               pdata#fluents;
               Array.iter (fun f ->
                   if (f#neglevel > rpg_max_level) && (List.exists (fun del ->
                                 (f#atom#equal del#atom))
                                 (Array.to_list a#del))
                   then f#set_neglevel (succ rpg_max_level))
               pdata#fluents;
               end))
          pdata#actions;
      rpg_max_level <- succ rpg_max_level;
 in
 expand_rpg;
end;

done

(*Utils.print "\ndone.\n"*)
(*
      Utils.eprint "Number of clauses : %i\n" (nbc - 1)
*)
end
