class virtual ['fluent, 'action, 'plan] t :
object
  constraint 'fluent = 'action #Node.fluent
  constraint 'action = 'fluent #Node.action
  constraint 'plan = ('fluent, 'action) #Plan.t

  method virtual create_fluent : Atom.t -> 'fluent
  method virtual create_action : string -> Symb.constant array -> float -> int -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> ('fluent*Timedata.t) array -> (('fluent*Timedata.t)*('fluent*Timedata.t)) array -> 'action
  method virtual plan_succes : 'plan
  method virtual plan_fail : 'plan
  method virtual run : 'plan
  method virtual print_statistics : unit
 
  method actions : 'action array
  method set_actions : 'action list -> unit
  method actions_always : 'action array
  method fluents : 'fluent array
  method init_state : 'fluent array
  method goal : 'fluent array
  method nb_fluents : int
  method nb_actions : int
  method nb_init : int
  method nb_goal : int

  method domain_name : string
  method problem_name : string
  method operator_iter : (Domain.operator -> unit) -> unit
  method problem : Domain.problem

  method search : unit
end
