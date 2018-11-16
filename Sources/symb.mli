class symb : 
  string -> int ->
object
  method name : string
  method num : int
  method to_string : string
end

class predicate : 
  string -> int ->
object 
  inherit symb
  method typing : bool
 (* method timed : bool
  method time : int * int*)
  method arity : int
  method untype : unit
(*  method set_time : int * int -> unit *)
  method set_arity : int -> unit
  method builtin_types : predicate list
  method add_builtin_type : predicate -> unit
  method init_compat : int -> unit
  method get_compat : (predicate * (int * int) list) list array
  method add_compat : predicate -> ((int * int) list) -> unit
  method is_compat : predicate -> ((int * int) list) -> bool
end

class virtual term : 
  string -> int ->
object
  inherit symb
  method builtin_type : predicate
  method set_builtin_type : predicate -> unit
  method has_builtin_type : bool
  method virtual ctype : ctype
  method virtual set_ctype : ctype -> unit
  method virtual is_var : bool
end

and ctype :
object
  method to_string : string
  method to_complete_string : string
  method add_constant : term -> unit
  method constants : term list
end


class constant : 
  string -> int ->
object
  inherit term
  method ctype : ctype
  method set_ctype : ctype -> unit
  method is_var : bool
end

class variable : 
  string -> int ->
object
  inherit term
  method ctype : ctype
  method set_ctype : ctype -> unit
  method is_var : bool
end


module TermSet :
sig
  type symbol_set
  class  c :
  object ('c)
    method set : symbol_set
    method elements : term list
    method mem : term -> bool
    method add : term -> unit
    method inter : 'c -> 'c
    method iter : (term -> unit) -> unit
  end
end

