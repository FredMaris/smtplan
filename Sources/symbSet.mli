class t :
object
  method predicates : Symb.predicate array
  method constants : Symb.constant array
  method variables : Symb.variable array

  method nb_pred : int
  method nb_const : int
  method nb_var : int

  method create_predicate : string -> Symb.predicate
  method create_constant : string -> Symb.constant
  method create_variable : string -> Symb.variable    

  method reset_var_table : unit

  method finalize : unit
end
