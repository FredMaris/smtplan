class symb =
  let table = Hashtbl.create 100 in
    fun (name : string) (num : int) ->
object
  val name =
    try Hashtbl.find table name with
      | Not_found -> 
	  Hashtbl.add table name name ;
	  name
  method name = name
  method num = num
  method to_string = name
end 

class predicate name num =
object 
  inherit symb name num
  val mutable typing = true
(*  val mutable timed = false*)
(*  val mutable time = (0,0)*)
  val mutable arity = -1
  method typing = typing
(*  method timed = timed*)
(*  method time = time*)
  method arity = arity
  method untype = typing <- false
(*  method set_time ttime = begin timed <- true; time <- ttime end*)

  method set_arity a = 
    if arity <> -1 && a <> arity then begin
      Printf.printf "\n\nPredicate %s is expected to be of arity %i, not %i...\n\n" name arity a ;
      exit 0
    end ;
    arity <- a

  val mutable builtin_types = []
  method builtin_types = builtin_types
  method add_builtin_type (ntype : predicate) = builtin_types <- ntype :: builtin_types

   
      (* 
	 compatibility treatment
      *)

  val mutable compat = [| |]

  method init_compat nb =
    compat <- Array.create (nb + 1) []

  method get_compat = compat
      
  method add_compat (pred : predicate) (equalities : (int * int) list) =
    compat.(pred#num) <- (pred, equalities) :: compat.(pred#num)

  method is_compat (pred : predicate) (equalities : (int * int) list) =
    List.mem (pred, equalities) compat.(pred#num)
end


let pred_void = new predicate "" (-1)

class virtual term name num =
object
  inherit symb name num
  val mutable builtin_type = pred_void
  method builtin_type = builtin_type
  method set_builtin_type ntype = builtin_type <- ntype
  method has_builtin_type = builtin_type != pred_void
  method virtual ctype : ctype
  method virtual set_ctype : ctype -> unit
  method virtual is_var : bool
end

and ctype =
  let nb = ref (-2) in
object (self)
  val num = (incr nb ; !nb)
  val mutable constants = ([] : term list)
  method to_string = "T" ^ string_of_int num
  method to_complete_string = 
    self#to_string ^ " = {" ^ Utils.string_of_list "," Utils.to_string
      (List.sort (fun c1 c2 -> compare c1#to_string c2#to_string) constants) ^ "}"
  method add_constant constant = constants <- constant :: constants
  method constants = constants
end

let void = new ctype

class constant name num =
object
  inherit term name num
  val mutable ctype = void
  method ctype = ctype
  method set_ctype nctype = ctype <- nctype
  method is_var = false
end

class variable name num =
object
  inherit term name num
  val mutable ctype = void
  method ctype = ctype
  method set_ctype nctype = ctype <- nctype
  method is_var = true
end

module TermSet =
struct
  module OrderedTerm =
  struct
    type t = term
    let compare a b = compare a#num b#num
  end
    
  module SymbolSet = Set.Make(OrderedTerm)

  type symbol_set = SymbolSet.t

  class c = 
  object (_ : 'c)
    val mutable set = SymbolSet.empty
    method set = set
    method mem constant = SymbolSet.mem constant set
(**    method copy = {< >}*)
    method add constant = set <- SymbolSet.add constant set
(*    method cardinal = SymbolSet.cardinal set*)
    method elements = SymbolSet.elements set
    method inter (set2 : 'c) = {< set = SymbolSet.inter set set2#set >}
(*    method union (set2 : 'c) = set <- SymbolSet.union set set2#set*)
    method iter f = SymbolSet.iter f set
  end
end
