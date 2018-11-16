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
    let s = atom#pred#to_string ^ 
    (if atom#nb_terms = 0 then "" else "_" ^  (Utils.string_of_array "_" Utils.to_string atom#terms))
 (*   ^ "[" ^ (string_of_int (fst atom#timeset)) ^ ";" ^ (string_of_int (snd atom#timeset)) ^ "]" *) in
    for i = 0 to (String.length s) - 1 do
     if (String.get s i) = '-' then (String.set s i '_');
    done;
    s

  val mutable agenda_pos = []
  val mutable agenda_neg = []
  val mutable neglevel = max_int
  val mutable no_op = new action "none" [| |] 0.0 0 [| |] [| |] [| |] [| |]

  method neglevel = neglevel
  method set_neglevel nneglevel = neglevel <- nneglevel

  method to_string = string
  
  method no_op = no_op
  method set_no_op f = no_op <- new action ("no_op_" ^ f#to_string) [| |] 0.0 0 [|(f,new Timedata.t (0.0,0.0) 0)|] [| |] [|(f,new Timedata.t (0.0,0.0) 0)|] [| |]
  
end


and action name params duration quality prec nprec add del condadd =
object (self)
  inherit node_common
  inherit [fluent] Node.action name params duration quality prec nprec add del condadd

  val mutable ident_num = 0
  method ident_num = ident_num
  method set_ident_num idn = ident_num <- idn

  val string =
    let s = name ^ if Array.length params = 0 then "" else "_" ^ (Utils.string_of_array "_" Utils.to_string params) in
    for i = 0 to (String.length s) - 1 do
     if (String.get s i) = '-' then (String.set s i '_');
    done;
    s

  method to_string = string

  method presentation_string = "(" ^ name ^ if Array.length params = 0 then ")" else " " ^ (Utils.string_of_array " " Utils.to_string params) ^ ")"

(*
  method to_complete_string = 
    let string_of_fluent_array fluents =
      Utils.string_of_array "#" Utils.to_string fluents in
      "s$Prec$" ^ string ^ "#" ^ string_of_fluent_array prec ^ "#\n" ^
      "s$Add$" ^ string ^ "#" ^ string_of_fluent_array add ^ "#\n" ^
      "s$Del$" ^ string ^ "#" ^ string_of_fluent_array del ^ "#\n" *)
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


let positive_print_int i node = Utils.print "%i " (node#n i)
let positive_print_string i node = Utils.print "%s(%i) " node#to_string i
let negative_print_int i node = Utils.print "-%i " (node#n i)
let negative_print_string i node = Utils.print "-%s(%i) " node#to_string i
let endl_int () = Utils.print "0\n"
let endl_string () = Utils.print "\n"



class t =
object (self)
  inherit [fluent, action, plan] PlanningData.t  as pdata
  inherit [fluent, action, plan] tsp_common

  (* val mutable solver = (new Smtsolver.t) *)
  val options = if Array.length Sys.argv < 4 then "" else Sys.argv.(4)
  val debug_mode = false
  val mutable solved = false
  val mutable nb = 0
  val mutable nbc = 0
  val mutable rpg_max_level = 0
  val mutable plan = []
  val mutable goals = []
  val mutable search_level = 0
val mutable depth_counter = ref 0

  method print_statistics = ()
  method run = self#plan_fail

  method virtual create_action : string -> Symb.constant array -> float -> int -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> 'action

(*  method search =
    let (search_time,_) = Utils.my_time "Searching plan (TLP-GP algorithm)" (fun () -> self#notimed_search) in
      Utils.eprint "Total search time : %.2f\n" search_time *)

  method search = (* notimed_search *)

    (* Utils.print "Searching plan (TLP-GP algorithm) .....\n"; *)

    let get_f_time_bound (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then (timedata#closed_left,timedata#closed_right)
        else t
      ) (true,true) a_iset
    in

    let get_f_timedata_code (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
      Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
        if (fluent#atom#equal f#atom)
        then timedata#code
        else t
      ) (-1) a_iset
    in

    let get_f_time f a_iset = 
      Array.fold_left (fun t x -> if ((fst x)#atom#equal f#atom) then (snd x)#timeset else t) (-1.0,-1.0) a_iset
    in
    (*let get_f f a_iset =
      Array.fold_left (fun c x -> if ((fst x)#atom#equal f#atom) then (fst x) else c) f a_iset
    in*)
    
    (* Initialize No_ops *)
    let init_no_op =
      Array.iter (fun f -> f#set_no_op f) pdata#fluents;
    in init_no_op ;
    
    
    let current_level = ref 0 in
    let build_rpg = (* Building the Temporal Relaxed Planning Graph *)
      Utils.print "Building the Temporal Relaxed Planning Graph ...\n";
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
      && (!current_level<=1024) do
 (*     while (Array.fold_left (fun a f -> a &&
                    (Array.fold_left (fun b p -> b ||
                       ((p#atom#equal f#atom) && (f#level <= !current_level)))
                    false pdata#goal))
                  true pdata#goal)
      && (!current_level<=255) do *)
        Array.iter (fun a -> (if (a#level > !current_level) &&
          (Array.fold_left (fun b prec -> b &&
            Array.fold_left (fun c f -> c ||
              ((f#atom#equal prec#atom) && (f#level <= !current_level)))
            false pdata#fluents)
          true a#prec)
          then begin
               a#set_level (succ !current_level);
               Array.iter (fun f ->
                   if (f#level > !current_level) && (List.exists (fun add ->
                                 (f#atom#equal add#atom))
                                 (Array.to_list a#add))
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

      build_rpg;

let timedata_null = new Timedata.t (0.0,0.0) 0 in

let action_init =
  self#create_action "Init" [| |] 0.0 0 [| |] [| |] (Array.of_list (Array.fold_left (fun x f -> (f,f#atom#timedata(*timedata_null*)) :: x) [] pdata#init_state)) [| |]
in

let action_goal =
  self#create_action "Goal" [| |] 0.0 0 (Array.of_list (Array.fold_left (fun x f -> (f,timedata_null) :: x) [] pdata#goal)) [| |] [| |] [| |]
in


(*let get_f_timedata_list (f : 'fluent) (a_iset : (('fluent*Timedata.t) array)) = 
  Array.fold_left (fun t ((fluent,timedata) : ('fluent*Timedata.t)) ->
    if (fluent#atom#equal f#atom)
    then timedata :: t
    else t
  ) [] a_iset
in*)

(* Affichage Etat Initial et But *)

(*Array.iter (fun x -> Utils.print "Init_State: %s\n" x#to_string) pdata#init_state;
Utils.print "%s\n" action_init#to_complete_istring;
Array.iter (fun x -> Utils.print "Goal: %s\n" x#to_string) pdata#goal;*)


let print_agenda agenda =
  List.iter (fun (f, ag_pos, ag_neg) ->
    Utils.print "Agenda(%s):\n" f#to_string;
    if ag_pos = [] then Utils.print "  Empty\n" else
    List.iter (fun (t,t_a,la,da,t_b,lb,db) ->
      match t with
       | (true,true) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (true,false) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
       | (false,true) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (false,false) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
    ) ag_pos;
    Utils.print "Agenda(Not_%s):\n" f#to_string;
    if ag_neg = [] then Utils.print "  Empty\n" else
    List.iter (fun (t,t_a,la,da,t_b,lb,db) ->
      match t with
       | (true,true) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (true,false) -> Utils.print "  [t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
       | (false,true) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f]\n" t_a la da t_b lb db;
       | (false,false) -> Utils.print "  ]t_%s_%d + %f ; t_%s_%d + %f[\n" t_a la da t_b lb db;
    ) ag_neg;
  ) agenda
in

let get_agenda pos f agenda =
  let (fluent,ag_pos,ag_neg) = List.find (fun (fl,ap,an) -> f#atom#equal fl#atom) agenda in
    if pos then ag_pos else ag_neg
in

let agenda_append pos f (t,t_a,la,da,t_b,lb,db) agenda =
 (* if pos then (Utils.print "Ajout Agenda(%s) : " f#to_string) else (Utils.print "Ajout Agenda(Not_%s) : " f#to_string);
  (match t with
   | (true,true) -> Utils.print "[t_%s%d + %f ; t_%s%d + %f]\n" t_a la da t_b lb db
   | (true,false) -> Utils.print "[t_%s%d + %f ; t_%s%d + %f[\n" t_a la da t_b lb db
   | (false,true) -> Utils.print "]t_%s%d + %f ; t_%s%d + %f]\n" t_a la da t_b lb db
   | (false,false) -> Utils.print "]t_%s%d + %f ; t_%s%d + %f[\n" t_a la da t_b lb db);*)
  List.map (fun (fluent,ag_pos,ag_neg) ->
    if (f#atom#equal fluent#atom) then
      if pos then (fluent,((t,t_a,la,da,t_b,lb,db) :: ag_pos),ag_neg)
      else (fluent,ag_pos,((t,t_a,la,da,t_b,lb,db) :: ag_neg))
    else (fluent,ag_pos,ag_neg)
  ) agenda
in

let comp_change comp = if options = "b" then (if (String.get comp 0 = '<') then Printf.sprintf "<=" else Printf.sprintf ">=") else comp in

let comp_apply comp f1 f2 =
  match comp with
   |"<" -> f1 < f2
   |"<=" -> f1 <= f2
   |">" -> f1 > f2
   |">=" -> f1 >= f2
   | _ -> false
in

let actions_distinct t_a la t_b lb =
(* let result= *)
  ((la <> lb) || ((String.compare t_a t_b) <> 0))
(* in
 if result then Printf.printf "actions_distinct: %s_%d <> %s_%d\n" t_a la t_b lb;
 result *)
in

let constraints_append_stc (comp,t_a,la,t_b,lb,ab) constraints =
  (*if debug_mode then Utils.print "Add STC : (t_%s_%d - t_%s_%d %s %f)\n" t_a la t_b lb comp ab;*)
  ((comp,t_a,la,t_b,lb,ab) :: (fst constraints),(snd constraints))
in

let constraints_append_dtc (comp1,t_a,la,t_b,lb,ab) (comp2,t_c,lc,t_d,ld,cd) constraints =
  (*if debug_mode then Utils.print "Add DTC : (t_%s_%d - t_%s_%d %s %f) v (t_%s_%d - t_%s_%d %s %f)\n" t_a la t_b lb comp1 ab t_c lc t_d ld comp2 cd;*)
  let actions_compare = ((actions_distinct t_a la t_b lb),(actions_distinct t_c lc t_d ld)) in
  match actions_compare with
   |(false,false) -> if (comp_apply comp1 0.0 ab)
                      then constraints (* dtc is true *)
                      else if (comp_apply comp2 0.0 cd)
                           then constraints (* dtc is true *)
                           else begin Utils.print "Inconsistency found in constraints set.\n"; ((fst constraints),((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) :: (snd constraints)) end (* dtc is false !!! *)
   |(false,true) -> if (comp_apply comp1 0.0 ab)
                      then constraints (* dtc is true *)
                      else (constraints_append_stc (comp2,t_c,lc,t_d,ld,cd) constraints) (* simplify dtc to stc *)
   |(true,false) -> if (comp_apply comp2 0.0 cd)
                      then constraints (* dtc is true *)
                      else (constraints_append_stc (comp1,t_a,la,t_b,lb,ab) constraints) (* simplify dtc to stc *)
   |(true,true) -> ((fst constraints),((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) :: (snd constraints))
in

(*let plan_append action level plan =
  (action,level) :: plan
in*)

let found_in_plan (a,i) plan =
  if (List.exists (fun (aa,ii) -> (a#ident_num = aa#ident_num) && (i = ii)) plan)
  then true else false (*begin Utils.print "[%s%d] BINGO !\n" a#to_string i; true end else begin Utils.print "[%s%d] NOT BINGO !!\n" a#to_string i; false end*)
in

let encode_float x = "(/ " ^ string_of_int (int_of_float (x*.1000000.0)) ^ " 1000000)"
in

let solve_constraints constraints plan variables =
begin
  let solver = (new Smtsolver.t) in
  let smtwrite = solver#smtwrite in
    solver#open_smtwrite;
    smtwrite (Printf.sprintf "(benchmark default.smt\n:source {\nTLP-GP automated DTP to SMT-LIB encoding for planning\nby F.Maris and P.Regnier, IRIT - Universite Paul Sabatier, Toulouse\n}\n:logic QF_RDL\n:extrafuns ((St_spy_variable Real))\n:extrafuns ((t_Init_0 Real))\n:extrafuns ((t_Goal_%d Real))\n" (succ rpg_max_level));
    List.iter (fun (a,i) ->
      if (a#ident_num <> 1) then smtwrite (Printf.sprintf ":extrafuns ((t_%s_%i Real))\n" a#to_string i);
    ) plan;
    List.iter (fun (var,i) ->
      smtwrite (Printf.sprintf ":extrafuns ((t_%s_%i Real))\n" var i)
    ) variables;
    smtwrite (Printf.sprintf ":formula ( and\n(= St_spy_variable (+ 1 t_Init_0))\n(>= t_Goal_%d t_Init_0)\n" (succ rpg_max_level));
    (* Temporal boundaries *)
    List.iter (fun (a,i) ->
      let first_prec_time = 0 (* a modifier pour prendre en compte les effets hors intervalle *)
      in
      let last_effect_time = a#duration (* a modifier pour prendre en compte les effets hors intervalle *) in
      smtwrite (Printf.sprintf "(>= (- t_%s_%i t_Init_0) %d)\n(>= (- t_Goal_%d t_%s_%i) %s)\n" a#to_string i first_prec_time (succ rpg_max_level) a#to_string i (encode_float last_effect_time))
    ) plan;
    List.iter (fun (var,i) ->
      smtwrite (Printf.sprintf "(>= t_%s_%i t_Init_0)\n(>= t_Goal_%d t_%s_%i)\n" var i (succ rpg_max_level) var i) (* modifier pour prise en compte de la durée d'intervalle anywhere *)
    ) variables;
    smtwrite "\n";
    (* STP Constraints *)
    List.iter (fun (comp,t_a,la,t_b,lb,ab) ->
      smtwrite (Printf.sprintf "(%s (- t_%s_%d t_%s_%d) %s)\n" comp t_a la t_b lb (encode_float ab))
    ) (fst constraints);
    smtwrite "\n";
    (* DTP Constraints *)
    List.iter (fun ((comp1,t_a,la,t_b,lb,ab),(comp2,t_c,lc,t_d,ld,cd)) ->
       smtwrite (Printf.sprintf "(or (%s (- t_%s_%d t_%s_%d) %s) (%s (- t_%s_%d t_%s_%d) %s))\n" (comp_change comp1) t_a la t_b lb (encode_float ab) (comp_change comp2) t_c lc t_d ld (encode_float cd))
    ) (snd constraints);
    smtwrite "))\n";
    solver#close_smtwrite;
    solver#launch
end
in

(* Search strategy for choosing actions *)
let get_producers (fluent:'fluent) (level : int)=
  let prodlist = ref []
  in
    Array.iter (fun a ->
(*      for i = (level - 1) downto (a#level) do *)
        prodlist := (a,   (*i*) level   ) :: !prodlist;
(*      done *)
    ) fluent#producers;
(**) if (fluent#level < level-1) then
      begin
       Utils.print "%s\n" fluent#no_op#to_string;
       prodlist := (fluent#no_op,level) :: !prodlist;
      end;
  if (level<=1) && (Array.fold_left (fun x f -> (fluent#atom#equal f#atom) || x) false pdata#init_state) then prodlist := (action_init,0) :: !prodlist;
  !prodlist;
in

(*  Array.fold_left (fun x a ->
    let prodlist = ref [] in
    for i=(level - 1) downto 1 do
      prodlist := ((a,i) :: !prodlist)
    done;
    (List.append prodlist x)
  ) [] fluent#producers
in *)

let rec iter_producers funct (agenda,plan,constraints,c) = function
  | [] ->
      (agenda,plan,constraints,false)
  | prod :: prod_tl ->
      let (ag,pl,co,cc) = (funct (agenda,plan,constraints,c) prod) in if cc then (ag,pl,co,true) else (iter_producers funct (agenda,plan,constraints,c) prod_tl)
in

let add_goals ((a,i) : 'action * int) goals_tl =
  Array.fold_left (fun goals_list p -> (p,a,i) :: goals_list) goals_tl a#prec
in

(* FONCTION LINKS *)
let rec links (aux :
(('fluent*'action*int) list) 
* (('fluent *(((bool*bool)*(*'action*)string*int*float*(*'action*)string*int*float) list)*(((bool*bool)*(*'action*)string*int*float*(*'action*)string*int*float) list )) list)
* (('action * int) list) 
* ((( string * (*'action*)string * int * (*'action*)string * int * float)list)* 
((( string * (*'action*)string * int * (*'action*)string * int * float)*
( string * (*'action*)string * int * (*'action*)string * int * float)) list))
* ((string * int) list)
(** * int **)
)

= match aux with
  | ([],agenda,plan,constraints,variables (**,0**) ) ->
    (agenda,plan,constraints,true)
(*  | ([],agenda,plan,constraints,variables (**,depth**) ) ->
    (agenda,plan,constraints,(solve_constraints constraints plan variables)) *)
  | ((p,b,j) :: goals_tl,agenda,plan,constraints,variables (**,depth**) ) ->
    iter_producers (fun (ag,pl,co,c) (a,i) ->
let (aux_agenda,aux_constraints,aux_variables) =
(* Pour chaque effet negatif Not_f de A, ajouter un intervalle d'effet à Agenda(Not_f) *)
(Array.fold_left (fun (c_ag3,c_co3,variables_aux1) neff ->
   ((if not (found_in_plan (a,i) plan) then
       let agenda_aux1=(agenda_append false neff ((get_f_time_bound neff a#idel),a#to_string,i,(fst (get_f_time neff a#idel)),a#to_string,i,(snd (get_f_time neff a#idel))) c_ag3) in
         (***if (get_f_timedata_code neff a#idel)==10 then (* dans le cas d'une transition, ajouter également un intervalle semi-ouvert de protection à Agenda(f) *)
           (agenda_append true neff (((fst (get_f_time_bound neff a#idel)),false),a#to_string,i,(fst (get_f_time neff a#idel)),a#to_string,i,(snd (get_f_time neff a#idel))) agenda_aux1)
         else***) agenda_aux1
     else c_ag3),
(* Pour chaque intervalle de Agenda(f), poser une contrainte de non recouvrement *)
    (let constraints_aux1=(List.fold_left (fun c_co4 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
      let ft_adel = (fst (get_f_time neff a#idel)) (* del begins at start *) in
       let not_auth_tx31 = (actions_distinct a#to_string i t_bb ib) in
       let not_auth_tx32 = (actions_distinct a#to_string i t_aa ia) in
       (constraints_append_dtc
         (if (tb > ft_adel) then ((if (not_auth_tx31) && (snd typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_adel)) else ((if (not_auth_tx31) && (snd typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_adel -. tb)))
         (if (ft_adel > ta) then ((if (not_auth_tx32) && (fst typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_adel -. ta)) else ((if (not_auth_tx31) && (fst typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta -. ft_adel)))
         c_co4)
    ) c_co3 (get_agenda true neff c_ag3)) in
      (***if (get_f_timedata_code neff a#idel)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement pour chaque intervalle de Agenda(Not_f) *)
        (List.fold_left (fun c_co4 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
          let ft_adel_start = (fst (get_f_time neff a#idel)) in
          let ft_adel_end = (snd (get_f_time neff a#idel)) (* del at end *) in
            (constraints_append_dtc
              (if (tb > ft_adel_start) then ((if (snd typ) && (fst (get_f_time_bound neff a#idel)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_adel_start)) else ((if (snd typ) && (fst (get_f_time_bound neff a#idel)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_adel_start - tb)))
              (if (ft_adel_end > ta) then ((if (fst typ) && (snd (get_f_time_bound neff a#idel)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_adel_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound neff a#idel)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_adel_end)))
              c_co4)
         ) constraints_aux1 (get_agenda false neff c_ag3))
      else***)
        constraints_aux1),
     variables_aux1
   )
)
(* Pour chaque effet positif f de A, ajouter un intervalle d'effet à Agenda(f) *)
(Array.fold_left (fun (c_ag1,c_co1,variables_aux2) eff ->
  if not (eff#atom#equal p#atom) then
   ((if not (found_in_plan (a,i) plan) then
       let agenda_aux2=(agenda_append true eff ((get_f_time_bound eff a#iadd),a#to_string,i,(fst (get_f_time eff a#iadd)),a#to_string,i,(snd (get_f_time eff a#iadd))) c_ag1) in
         (***if (get_f_timedata_code eff a#iadd)==10 then (* dans le cas d'une transition, ajouter également un intervalle semi-ouvert de protection à Agenda(Not_f) *)
           (agenda_append false eff (((fst (get_f_time_bound eff a#iadd)),false),a#to_string,i,(fst (get_f_time eff a#iadd)),a#to_string,i,(snd (get_f_time eff a#iadd))) agenda_aux2)
         else***) agenda_aux2
    else c_ag1),
(* Pour chaque intervalle de Agenda(Not_f), poser une contrainte de non recouvrement *)
    (let constraints_aux2=(List.fold_left (fun c_co2 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
      let ft_aadd = (fst (get_f_time eff a#iadd)) (* add begins at start *) in
       let not_auth_tx21 = (actions_distinct a#to_string i t_bb ib) in
       let not_auth_tx22 = (actions_distinct a#to_string i t_aa ia) in
       (*let simplify2 = (if not_auth_tx21 then ,) in*)
       (constraints_append_dtc
         (if (tb > ft_aadd) then ((if (not_auth_tx21) && (snd typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_aadd)) else ((if (not_auth_tx21) && (snd typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd -. tb)))
         (if (ft_aadd > ta) then ((if (not_auth_tx22) && (fst typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd -. ta)) else ((if (not_auth_tx22) && (fst typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta -. ft_aadd)))
         c_co2)
    ) c_co1 (get_agenda false eff c_ag1)) in
      (***if (get_f_timedata_code eff a#iadd)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement pour chaque intervalle de Agenda(f) *)
       (List.fold_left (fun c_co2 (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let ft_aadd_start = (fst (get_f_time eff a#iadd)) in
         let ft_aadd_end = (snd (get_f_time eff a#iadd)) (* add at end *) in
          (constraints_append_dtc
            (if (tb > ft_aadd_start) then ((if (snd typ) && (fst (get_f_time_bound eff a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_aadd_start)) else ((if (snd typ) && (fst (get_f_time_bound eff a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_start - tb)))
            (if (ft_aadd_end > ta) then ((if (fst typ) && (snd (get_f_time_bound eff a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound eff a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_aadd_end)))
            c_co2)
       ) constraints_aux2 (get_agenda true eff c_ag1))
      else***) constraints_aux2),
    variables_aux2
  )
  else (c_ag1,c_co1,variables_aux2)
)
(* Poser un intervalle de maintien de précondition à Agenda(p) *)

(let (variables_aux3 : ((string*int) list) ref) = ref [] in
 let (t_a,ft_aadd_ag,ft_aadd_co) =
 let atdc = (get_f_timedata_code p a#iadd) in
   match atdc with
    |0 -> (a#to_string,(fst (get_f_time p a#iadd)),(fst (get_f_time p a#iadd))) (* eff_OVER : p certain au debut de l'intervalle, protection au debut de l'intervalle *)
    |1 -> (a#to_string,(fst (get_f_time p a#iadd)),(snd (get_f_time p a#iadd))) (* eff_SOMEWHERE : p certain a la fin de l'intervalle, protection au debut de l'intervalle *)
    |2 -> variables_aux3 := ("where__" ^ a#to_string ^ "__produces__" ^ p#to_string,i) :: !variables_aux3; ("where__" ^ a#to_string ^ "__produces__" ^ p#to_string,0.0,0.0) (* eff_ANYWHERE : prendre en compte nouvelle variable temporelle !!! a faire !!! *)
    |10 -> (a#to_string,(fst (get_f_time p a#iadd)),(snd (get_f_time p a#iadd))) (* trans_OVER : p certain a la fin de l'intervalle, protection au debut de l'intervalle *)
    |_ -> (a#to_string ^ "__UNKNOWN_INTERVAL_TYPE",0.0,0.0)
   in
  let (t_b,ft_bprec_ag,ft_bprec_co) =
  let btdc = (get_f_timedata_code p b#iprec) in
    match btdc with
     |0 -> (b#to_string,(snd (get_f_time p b#iprec)),(fst (get_f_time p b#iprec)))
     |1 -> (b#to_string,(snd (get_f_time p b#iprec)),(fst (get_f_time p b#iprec)))
     |2 -> variables_aux3 := ("where__" ^ b#to_string ^ "__needs__" ^ p#to_string,j) :: !variables_aux3; ("where__" ^ b#to_string ^ "__needs__" ^ p#to_string,0.0,0.0) (* prec_ANYWHERE :  *)
     |_ -> (b#to_string ^ "__UNKNOWN_INTERVAL_TYPE",0.0,0.0)
  in
let new_agenda1 = (agenda_append true p (((fst (get_f_time_bound p a#iadd)),(snd (get_f_time_bound p b#iprec))),t_a,i,ft_aadd_ag,t_b,j,ft_bprec_ag) ag (* agenda_feed *)) in
(((***if (get_f_timedata_code p a#iadd)==10 then (* dans le cas d'une transition, ajouter également un intervalle de protection à Agenda(Not_p) *)
  (agenda_append false p (((fst (get_f_time_bound p a#iadd)),false),t_a,i,ft_aadd_ag,t_a,i,ft_aadd_co) new_agenda1)
  else***) new_agenda1),
(* Pour chaque intervalle de Agenda(Not_p), poser une contrainte de non recouvrement *)
(let constraints_aux3=(List.fold_left (fun clist (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let not_auth_tx = (actions_distinct b#to_string j t_aa ia) in
           (constraints_append_dtc
             (if (tb > ft_aadd_ag) then ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb -. ft_aadd_ag)) else ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_ag -. tb)))
             (if (ft_bprec_ag > ta) then ((if (not_auth_tx) && (fst typ) && (fst (get_f_time_bound p b#iprec)) then ">" else ">="),t_aa,ia,b#to_string,j,(ft_bprec_ag -. ta)) else ((if (not_auth_tx) && (fst typ) && (fst (get_f_time_bound p b#iprec)) then "<" else "<="),b#to_string,j,t_aa,ia,(ta -. ft_bprec_ag)))
             clist)
        )
(* Poser une contrainte de précédence entre A et B *)
  (if (ft_aadd_co >= ft_bprec_co) then (constraints_append_stc (">=",b#to_string,j,a#to_string,i,(ft_aadd_co -. ft_bprec_co)) co (* constraints_feed *))
  else (constraints_append_stc ("<=",a#to_string,i,b#to_string,j,(ft_bprec_co -. ft_aadd_co)) co (* constraints_feed *)))
  (get_agenda false p new_agenda1)
) in
        (***if (get_f_timedata_code p a#iadd)==10 then (* dans le cas d'une transition, poser également une contrainte de non recouvrement de l'intervalle de transition pour chaque intervalle de Agenda(p) *)
(List.fold_left (fun clist (typ,t_aa,ia,ta,t_bb,ib,tb) ->
         let (ft_aadd_start,ft_aadd_end) = (get_f_time p a#iadd) (* add at end *) in
           (constraints_append_dtc
             (if (tb > ft_aadd_start) then ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then ">" else ">="),a#to_string,i,t_bb,ib,(tb - ft_aadd_start)) else ((if (snd typ) && (fst (get_f_time_bound p a#iadd)) then "<" else "<="),t_bb,ib,a#to_string,i,(ft_aadd_start - tb)))
             (if (ft_aadd_end > ta) then ((if (fst typ) && (snd (get_f_time_bound p a#iadd)) then ">" else ">="),t_aa,ia,a#to_string,i,(ft_aadd_end - ta)) else ((if (fst typ) && (snd (get_f_time_bound p a#iadd)) then "<" else "<="),a#to_string,i,t_aa,ia,(ta - ft_aadd_end)))
             clist)
        ) constraints_aux3 (get_agenda true p new_agenda1))
        else***) constraints_aux3),
(List.append !variables_aux3 variables)
))
a#add) a#del) in
      let (aux_plan,found) = (if not (found_in_plan (a,i) plan) then ((a,i) :: plan,false) else (plan,true)) in

(**)
      if solve_constraints aux_constraints aux_plan aux_variables then
        if not found then
        links (((add_goals (a,i) goals_tl) : (('fluent * 'action * int) list)),
              aux_agenda,
              aux_plan,
              aux_constraints,
              aux_variables)
        else
        links ((goals_tl : (('fluent * 'action * int) list)),
              aux_agenda,
              plan,
              aux_constraints,
              aux_variables)
      else
        (ag,pl,co,false)


 (**     let (csolved,go_west,newdepth) = if (depth == 3) then let csol = (solve_constraints aux_constraints aux_plan) in (csol,csol,0) else (false,true,succ depth) in

     (* if solve_constraints aux_constraints aux_plan then *)
       if go_west then


        if not (found_in_plan (a,i) plan) then
        let (ag_return, pl_return, co_return, sol_return) =
        links (((add_goals (a,i) goals_tl) : (('fluent * 'action * int) list)),
              aux_agenda,
              aux_plan,
              aux_constraints,
              newdepth)
         in if ((depth > 0) && (not sol_return)) then
              if (solve_constraints aux_constraints aux_plan) then
                (aux_agenda,aux_plan,aux_constraints,true)
              else
                (ag,pl,co,false)
            else (ag_return, pl_return, co_return, sol_return)
        else
        let (ag_return, pl_return, co_return, sol_return) =
        links ((goals_tl : (('fluent * 'action * int) list)),
              aux_agenda,
              plan,
              aux_constraints,
              newdepth)
         in if ((depth > 0) && (not sol_return)) then
              if (solve_constraints aux_constraints aux_plan) then
                (aux_agenda,plan,aux_constraints,true)
              else
                (ag,pl,co,false)
            else (ag_return, pl_return, co_return, sol_return)
      else
        (ag,pl,co,false)

(*end*) **)


    ) (agenda,plan,constraints,false) (get_producers p j)
in




goals <- List.map (fun f -> (f,false)) (Array.to_list pdata#goal);

(*links ([(action_init,0);(action_goal,rpg_max_level + 1)],
       (Array.fold_left (fun x f -> ((f,[],[]) :: x) ) [] pdata#fluents),
       [],
       ([],[])
      );*)

(*if solve_constraints [] then Utils.print "TLP-GP_SAT" else Utils.print "TLP-GP_UNSAT";*)



(*
(* Prend en entrée la liste des buts et renvoie un couple (premier but, liste modifiée) *)
let choose_goal goals_list = 
in*)

action_init#set_ident_num 1;
action_goal#set_ident_num 2;
let it = ref 2 in Array.iter (fun a -> it:=(succ !it); a#set_ident_num !it;) pdata#actions;

while not solved do
      Utils.print "\nSearching floating plan using TLP-GP algorithm at level %d ...\n" rpg_max_level;

(*Utils.print "[SPECIAL ACTION_INIT]\n%s\n" action_init#to_complete_istring;
Utils.print "[SPECIAL ACTION_GOAL]\n%s\n" action_goal#to_complete_istring;*)

(*exemple:  List.fold_left (fun goals_list p -> (p,a,i) :: goals_list) goals_tl a#prec*)

let (agenda,plan,constraints,sol) =
links ((Array.fold_left (fun goals_list p -> (p,action_goal,(rpg_max_level + 1)) :: goals_list) [] pdata#goal),
       (Array.fold_left (fun x f -> ((f,[],[]) :: x) ) [] pdata#fluents),
       [],
       ([],[]),
       [] (**,
       0 **)
      )
in
solved <- sol;

  (* print_agenda agenda; *)

if solved then begin
 Utils.print "Floating plan found at level %d.\n" rpg_max_level;
(* List.iter (fun (a,i) ->
   Utils.print "[%s](level %d)\n" a#to_string i;
 ) plan; *)
 Utils.print "Constraints Set: #STC=%d / #DTC=%d\n"
   (List.length (fst constraints))
   (List.length (snd constraints));
 if debug_mode then print_agenda agenda;
 let extractor = (new Smtsolver_extractplan.t) in
  let (plan_times,solver_factor) = extractor#launch in
   if plan_times = [] then
   begin
    Utils.print "SMT Solver did not return a static plan.\n--------------------------------------------\nACTIONS (in the floating plan)\n--------------------------------------------\n";
    List.iter (fun (a,i) ->
      Utils.print "[%s](level %d)\n" a#to_string i;
    ) plan;
   end else begin
    Utils.print "STATIC PLAN:\n--------------------------------------------\npossible start time: (ACTION) [duration]\n--------------------------------------------\n";
    (*List.iter (fun (var_name, level_num, start_time) -> Utils.print "%s (%d) [%d.%06d]\n" var_name level_num ((fst start_time)/(1000000*solver_factor*(snd start_time))) (((fst start_time) mod (1000000*solver_factor*(snd start_time)))/(1000000*solver_factor*(snd start_time)))) plan_times;*)
    (*List.iter (fun (var_name, level_num, start_time) -> Utils.print "%s (%d) [%d.%06d]\n" var_name level_num (start_time/solver_factor) (1000000*(start_time mod solver_factor)/solver_factor)) plan_times;*)
    let current_time = ref (-1.0) in
    List.iter (fun (var_name, level_num, start_time) ->
      let v_action =
       if var_name = "Init" then action_init else if var_name = "Goal" then action_goal else
        (Array.fold_left (fun r a -> if a#to_string = var_name then a else r)) action_init pdata#actions
      in
      if !current_time = start_time then
       Utils.print "%s  %s [%f] {level %d}\n" (String.make (String.length (Printf.sprintf "%f" (!current_time/.(float_of_int solver_factor)))) ' ') v_action#presentation_string v_action#duration level_num
      else begin
       current_time := start_time;
       Utils.print "%f: %s [%f] {level %d}\n" (start_time/.(float_of_int solver_factor)) v_action#presentation_string v_action#duration level_num
      end) plan_times
   end;
(* BEGIN writing plan to output file *)
   if plan_times <> [] then
   begin
    let planfile = open_out "solution-plan.txt" in
    output_string planfile (Printf.sprintf "\n; Makespan: %f\n\n" (List.fold_left (fun t (var_name, level_num, start_time) -> if (String.compare var_name "Goal") = 0 then (start_time/.(float_of_int solver_factor)) else t) 0.0 plan_times));
    let current_time = ref (-1.0) in
    List.iter (fun (var_name, level_num, start_time) ->
      let (no_hide,v_action) =
       if var_name = "Init" then (false,action_init) else if var_name = "Goal" then (false,action_goal) else
        (true,(Array.fold_left (fun r a -> if a#to_string = var_name then a else r)) action_init pdata#actions)
      in
      if no_hide then begin
       (*if !current_time = start_time then
        output_string planfile (Printf.sprintf "%s  %s [%f]\n" (String.make (String.length (Printf.sprintf "%f" (!current_time/.(float_of_int solver_factor)))) ' ') v_action#presentation_string v_action#duration)
       else begin*)
        current_time := start_time;
        output_string planfile (Printf.sprintf "%f: %s [%f]\n" (start_time/.(float_of_int solver_factor)) v_action#presentation_string v_action#duration)
       (*end*)
      end) plan_times;
    close_out_noerr planfile
   end else begin
    let planfile = open_out "solution-plan.txt" in
     output_string planfile (Printf.sprintf "; Floating plan exists ...\n; The solver did not return a static plan.\n" );
     close_out_noerr planfile
   end;
(* END writing plan to output file *)
   Utils.print "--------------------------------------------\nMakespan: ";
   if plan_times = [] then Utils.print "?\n"
    else Utils.print "%f\n" (List.fold_left (fun t (var_name, level_num, start_time) -> if (String.compare var_name "Goal") = 0 then (start_time/.(float_of_int solver_factor)) else t) 0.0 plan_times);
   Utils.print "--------------------------------------------\n\n";
end else begin
 Utils.print "No floating plan at level %d.\n" rpg_max_level;
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
                                 (Array.to_list a#add))
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

end