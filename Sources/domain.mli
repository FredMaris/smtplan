class operator :
  string -> Typeset.parameters -> FunctionFormula.t -> int -> Formula.t -> Formula.t ->
object
  method name : string
  method parameters : Typeset.parameters
  method duration : FunctionFormula.t
  method quality : int
  method prec : Atom.t array
  method nprec : Atom.t array
  method add : Atom.t array
  method del : Atom.t array
  method equa : (Symb.term * Symb.term) array
  method diff : (Symb.term * Symb.term) array
  method constraints : Atom.t list array
  method order : int array
  method condadd : (Atom.t * Atom.t) array
  method ncondadd : (Atom.t * Atom.t) array
  method conddel : (Atom.t * Atom.t) array
  method nconddel : (Atom.t * Atom.t) array

  method to_string : string
  method to_complete_string : string
  method to_complete_istring : string
  method to_creation_string : string

  method create_action_struct : unit
end

class domain :
  string -> string list -> operator array ->
object
  method name : string
  method equality : bool

  method to_string : string
  method to_complete_string : string
  method to_complete_istring : string

  method operator_iter : (operator -> unit) -> unit
end

class problem :
  string -> domain -> Symb.constant list -> Atom.t list -> Formula.t -> SymbSet.t -> Typeset.attribute_space_set -> (Atom.t * float) list ->
object
  method name : string
  method domain : domain
  method init : Atom.t list
  method goal : Formula.t
  method functions_value_list : (Atom.t * float) list
  
  method attribute_spaces : Typeset.attribute_space_set
  method atom_types : Typeset.atom_types

  method to_string : string
  method to_complete_string : string

  method add_atom_constants : Atom.t -> unit
  method finalize : unit
end

val domain_void : domain
val problem_void : problem
