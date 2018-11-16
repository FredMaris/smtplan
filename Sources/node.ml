class ['fluent] action name (params : Symb.constant array) (duration : float) (quality : int) (p_prec : ('fluent * Timedata.t) array) (p_nprec : ('fluent * Timedata.t) array) (p_add : ('fluent * Timedata.t) array) (p_del : ('fluent * Timedata.t) array) (p_condadd : (('fluent * Timedata.t)*('fluent * Timedata.t)) array) =
object (self)
  val mutable num = 0
  val mutable prec = Array.map (fun (a,b) -> a) p_prec
  val mutable nprec = Array.map (fun (a,b) -> a) p_nprec
  val mutable add = Array.map (fun (a,b) -> a) p_add
  val mutable del = Array.map (fun (a,b) -> a) p_del
  val mutable condadd = Array.map (fun ((c,_),(e,_)) -> (c,e)) p_condadd
(*  val mutable add = Array.of_list (Array.fold_left (fun ladd (l,f,_) -> if (List.length l)=0 then (f :: ladd) else ladd) [] p_add)
  val mutable del = Array.of_list (Array.fold_left (fun ldel (l,f,_) -> if (List.length l)=0 then (f :: ldel) else ldel) [] p_del)
*)
  val mutable iprec = p_prec
  val mutable inprec = p_nprec
  val mutable iadd = p_add
  val mutable idel = p_del
  val mutable icondadd = p_condadd
(*  val mutable iadd = Array.of_list (Array.fold_left (fun ladd (l,f,t) -> if (List.length l)=0 then ((f,t) :: ladd) else ladd) [] p_add)
  val mutable idel = Array.of_list (Array.fold_left (fun ldel (l,f,t) -> if (List.length l)=0 then ((f,t) :: ldel) else ldel) [] p_del)
*)
  val mutable nb_prec_unsat = Array.length p_prec
  val mutable relevant = false
  val mutable rescue = false

  method num = num
  method set_num n = num <- n
  method params = params
  method duration = duration
  method quality = quality
  method prec = prec
  method nprec = nprec
  method add = add
  method del = del
  method condadd = condadd
  method iprec = iprec
  method inprec = inprec
  method iadd = iadd
  method idel = idel
  method icondadd = icondadd
  method relevant = relevant
  method make_relevant = relevant <- true
  method make_irrelevant = relevant <- false
  method is_rescue = rescue
  method set_rescue = rescue <- true

  method remove_prec fluent =
    prec <- Utils.array_remove_elem fluent prec ;
    iprec <- Utils.array_remove_elem2 fluent iprec ;
    self#decr_nb_prec_unsat
  method remove_add fluent = add <- Utils.array_remove_elem fluent add

  method to_string =
    name ^ (if Array.length params = 0 then "" else " " ^ (Utils.string_of_array " " (fun s -> s#to_string) params))

  method to_complete_string =
    let string_of_fluent_array fluents =
      Utils.string_of_array " " (fun (f : 'fluent) -> f#to_string) fluents in
      self#to_string ^ 
    "\n\tQuality : " ^ (string_of_int quality) ^
    "\n\tPrec : " ^ string_of_fluent_array prec ^
    "\n\tNPrec : " ^ string_of_fluent_array nprec ^
    "\n\tAdd : " ^ string_of_fluent_array add ^
    "\n\tDel : " ^ string_of_fluent_array del

  method to_complete_istring =
    let string_of_fluent_array fluents =
      Utils.string_of_array " " (fun (f,timedata) -> f#to_string ^ (if timedata#closed_left then "[" else "]") ^ string_of_float (fst timedata#timeset) ^ ";" ^ string_of_float (snd timedata#timeset) ^ (if timedata#closed_right then "]" else "[")) fluents in
      self#to_string ^ 
    "\n\tQuality : " ^ (string_of_int quality) ^
    "\n\tiPrec : " ^ string_of_fluent_array iprec ^
    "\n\tiNPrec : " ^ string_of_fluent_array inprec ^
    "\n\tiAdd : " ^ string_of_fluent_array iadd ^
    "\n\tiDel : " ^ string_of_fluent_array idel

  method zero_prec_unsat = nb_prec_unsat = 0 or (nb_prec_unsat <- pred nb_prec_unsat ; nb_prec_unsat = 0)
  method decr_nb_prec_unsat = nb_prec_unsat <- pred nb_prec_unsat
  method nb_prec_unsat = nb_prec_unsat

  method init_nb_prec_unsat =  nb_prec_unsat <- Array.length prec

end


class ['action] fluent (atom : Atom.t) =
object(fluent)
  val mutable num = 0
  val mutable producers_temp = []
  val mutable consumers_temp = []
  val mutable deleters_temp = []
  val mutable condproducers_temp = [] 
  val mutable producers = [| |]
  val mutable consumers = [| |]
  val mutable deleters = [| |]
  val mutable condproducers = [| |] 
  val mutable relevant = false
  val mutable is_init = false
  val mutable is_goal = false
  method num = num
  method set_num n = num <- n
  method atom = atom
  method relevant = relevant
  method make_relevant = relevant <- true
  method make_irrelevant = relevant <- false
  method is_init = is_init
  method is_goal = is_goal
  method set_init = is_init <- true
  method set_goal = is_goal <- true
  method to_string = atom#to_string
  method to_istring = atom#to_istring
  method producers = producers
  method consumers = consumers
  method deleters = deleters
  method condproducers = condproducers
  method add_producer (action : 'action) = producers_temp <- action :: producers_temp
  method add_consumer (action : 'action) = consumers_temp <- action :: consumers_temp
  method add_deleter (action : 'action) = deleters_temp <- action :: deleters_temp
  method add_condproducer (action : 'action) = condproducers_temp <- action :: condproducers_temp 
  method finalize = 
    producers <- Array.of_list producers_temp ;
    producers_temp <- [] ;
    consumers <- Array.of_list consumers_temp ;
    consumers_temp <- [] ;
    deleters <- Array.of_list deleters_temp ;
    deleters_temp <- [];
    condproducers <- Array.of_list condproducers_temp ;
    condproducers_temp <- [] ; 
end

