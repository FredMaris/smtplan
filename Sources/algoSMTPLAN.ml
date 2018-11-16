class ['action] fluent atom =
object (self)
  inherit ['action] PGraphSMTPLAN.fluent atom

  val mutable value = false
  method is_required = value
  method require = value <- true
  method deny = value <- false

end

class ['fluent] action ope params duration quality prec add del =
object (self)
  inherit ['fluent] PGraphSMTPLAN.action ope params duration quality prec add del

  val value = Array.create PGraphSMTPLAN.max_level false
  method is_used level = value.(level)
  method use level = value.(level) <- true
  method exclude level = value.(level) <- false

end

class virtual ['fluent, 'action, 'plan] t =
object (self)
  inherit ['fluent, 'action, 'plan] PGraphSMTPLAN.t

  val mutable nb_calls = 0
  val mutable nb_calls2 = 0

  method find_plan level goals =
    nb_calls <- nb_calls + 1 ;
    if level = 0 then self#plan_succes
    else
      if self#is_nogood goals level then self#plan_fail
      else
	let plan = self#assign_goals level [] (List.sort (fun f1 f2 -> compare f2#level f1#level) goals) in
	  if not plan#succes then self#add_nogood goals level ;
	  plan

  method assign_goals level actions =
    nb_calls2 <- nb_calls2 +1 ;
    function
      [] ->
	let new_goals = ref [] in
	List.iter (fun action ->
		     Array.iter (fun fluent ->
				   if not(List.memq fluent !new_goals) then
				     new_goals := fluent :: !new_goals
				) action#prec
		  ) actions ;
	  let plan = self#find_plan (level - 1) !new_goals in
	    if plan#succes then plan#add_end actions else plan
    | g :: goals ->
	List.fold_left (fun plan producer ->
	    if not plan#succes && not (List.exists (self#is_amutex level producer) actions) then
	      self#assign_goals level (if List.memq producer actions then actions else producer :: actions) goals
	    else plan) self#plan_fail (g#prod level)

  method solve =
    Utils.eprint "Searching GP way at level %i... " self#level ; flush stderr ;
    let plan = self#find_plan self#level (Array.to_list self#goal) in
      Utils.eprint "%i - %i - %i.\n" nb_calls nb_calls2 self#nb_nogoods ;
      flush stderr ;
      plan

end
