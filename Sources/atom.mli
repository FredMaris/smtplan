class t : Symb.predicate -> Symb.term array -> Timedata.t -> (FunctionFormula.t * FunctionFormula.t) ->
object ('atom)
  method pred : Symb.predicate
  method terms : Symb.term array
  method timedata : Timedata.t
  method timeset_struct : (FunctionFormula.t * FunctionFormula.t)
(*  method timeset : int * int
  method set_timeset : int * int -> unit *)
  method nb_terms : int
  method to_string : string
  method to_istring : string
  method equal : 'atom -> bool
  method equal2 : 'atom -> bool
  method instantiate : Symb.term array -> 'atom
  method equal_names : 'atom -> bool
  method add_compat : 'atom -> bool
  method is_compat : 'atom -> bool
  method get_comp : Symb.term list -> t list
  method iteri : (int -> Symb.term -> unit) -> unit
end

val equal : t -> t -> bool
val hash : t -> int
