type att_space
type param_type

class attribute_space_set :
  SymbSet.t ->
  object
    method to_string : string
    method create_att_space : Symb.predicate -> int -> att_space
    method add_constants : Atom.t -> unit
    method add_builtin_types : Symb.predicate -> Symb.constant -> unit
    method create_ptype : Symb.variable -> param_type
    method create_domains : unit
    method finalize : unit
  end

class parameters :
  Symb.variable list ->
  attribute_space_set ->
  object
    method vars : Symb.variable array
    method vars_list : Symb.variable list
    method nb : int
    method domain : int -> Symb.constant array
    method ctypes : int -> Symb.ctype list
    method to_string : string

    method add_transition : Atom.t -> unit
    method add_attribute_space : Atom.t -> unit
    method fix_encountered_ctypes : unit
  end

class atom_types :
  SymbSet.t ->
  object
    method atom_types_array : Symb.ctype array list array
    method to_string : string
    method add_atom_type : Atom.t -> bool
    method is_atom_type : Atom.t -> bool
  end
