class fluent atom =
object
  inherit [action] AlgoSMTPLAN.fluent atom
end

and action ope params duration quality prec add del =
object
  inherit [fluent] AlgoSMTPLAN.action ope params duration quality prec add del
end

class plan succes = 
object 
  inherit [fluent, action] ParallelPlan.t succes

end

class ['fluent, 'action, 'plan] gp_common =
object
  method create_fluent = new fluent
  method create_action = new action
  val plan_succes = new plan true
  val plan_fail = new plan false
  method plan_succes = plan_succes
  method plan_fail = plan_fail

end

class gp =
object
  inherit [fluent, action, plan] gp_common
  inherit [fluent, action, plan] AlgoSMTPLAN.t
  inherit [fluent] NogoodTable.c
end
