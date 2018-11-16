class attribute_space =
  let nb = ref 0 in
    fun pred pos ->
object (self)
  inherit Symb.TermSet.c

  val num = (incr nb ; !nb)
  val mutable transitions = []

  method num = num

  method to_string = pred ^ "(" ^ string_of_int pos ^ ")"
  method to_complete_string =
    "Attribute space  : " ^ self#to_string ^ " = {" ^ 
    Utils.string_of_list "," Utils.to_string (List.sort (fun c1 c2 -> compare c1#to_string c2#to_string) self#elements) ^ "}\n" ^
    if transitions = [] then "No transition rules" 
    else "Transition rules :\n   " ^ self#to_string ^
      Utils.string_of_list ("\n   " ^ self#to_string) Utils.to_string transitions
    
  method add_transition enablers consequence =
    let enablers = List.sort (fun e1 e2 -> compare e1#num e2#num) enablers in
      if not(List.exists (fun transition -> transition#add_consequence enablers consequence) transitions) then
	transitions <- new transition_rule enablers consequence :: transitions

  method add_constant constant =
    if not(self#mem constant) then begin
      self#add constant ;
      List.iter (fun rule -> rule#propagate constant) transitions
    end
end

and transition_rule (enablers : attribute_space list) (consequence : attribute_space) =
object
  val mutable consequences = [consequence]

  method to_string = 
    (if enablers <> [] then " " else "") ^ Utils.string_of_list " " Utils.to_string enablers ^ " --> " ^ 
    Utils.string_of_list " " Utils.to_string consequences

  method add_consequence enab consequence = 
    if enab <> enablers then false 
    else begin
      if not(List.memq consequence consequences) then
	consequences <- consequence :: consequences ;
      true
    end

  method propagate constant =
    if List.for_all (fun attribute_space -> attribute_space#mem constant) enablers then
      List.iter (fun attribute_space -> attribute_space#add_constant constant) consequences
end



let generic_attribute_space = new attribute_space "generic_attribute_space" 0


class ptype (var : Symb.variable) =
object
  val mutable domain = [| |]
  val mutable ctypes = ([] : Symb.ctype list)
  val mutable attribute_spaces = [generic_attribute_space]
  val mutable encountered_ctypes = []

  method domain = domain
  method ctypes = ctypes

  method to_string = Utils.string_of_list " U " Utils.to_string (List.sort (fun t1 t2 -> compare t1#to_string t2#to_string) ctypes)

  method add_ctype ctype =
    ctypes <- ctype :: ctypes

  method add_attribute_space attribute_space = 
    if List.hd attribute_spaces == generic_attribute_space then
      attribute_spaces <- [attribute_space]
    else
      if not(List.memq attribute_space attribute_spaces) then
	attribute_spaces <- attribute_space :: attribute_spaces

  method add_transition transition = 
    if not(List.memq transition attribute_spaces) then
      List.iter
	(fun attribute_space -> 
	   attribute_space#add_transition (Utils.delete attribute_space attribute_spaces) transition
	) attribute_spaces

  method add_encountered_ctype =
    if not(List.memq var#ctype encountered_ctypes) then
      encountered_ctypes <- var#ctype :: encountered_ctypes

  method remove_unused_ctypes =
    ctypes <- Utils.delete_all_pred (fun ctype -> not(List.memq ctype encountered_ctypes)) ctypes

  method create_domain =
    domain <- Array.of_list (List.concat (List.map (fun ctype -> ctype#constants) ctypes))

  method domain_set =
    List.fold_left 
      (fun inter attribute_space -> 
	 inter#inter attribute_space
      ) (List.hd attribute_spaces) (List.tl attribute_spaces)

end

type att_space = attribute_space
type param_type = ptype

type caracteristic = {
  mutable ptypes : ptype list ;
  mutable attribute_spaces : attribute_space list
}


class attribute_space_set (symb_set : SymbSet.t) = 
object (self)
  val att_space_table = Hashtbl.create 10
  val mutable att_space_list = []
  val mutable ptypes_list = []
  val mutable ctypes_list = []

  method to_string = 
    Utils.string_of_list "\n\n" Utils.to_complete_string att_space_list ^
    "\n\nObject types : \n\n" ^ Utils.string_of_list "\n" Utils.to_complete_string (List.rev ctypes_list)

  method create_att_space (pred : Symb.predicate) pos =
    try Hashtbl.find att_space_table (pred, pos) with
      | Not_found ->
	  let attribute_space = new attribute_space pred#name pos in
	    att_space_list <- attribute_space :: att_space_list ;
 	    Hashtbl.add att_space_table (pred, pos) attribute_space ;
	    attribute_space

  method add_constants (atom : Atom.t) =
    atom#iteri
      (fun pos constant ->
	   let attribute_space = self#create_att_space atom#pred pos in
	     attribute_space#add_constant constant)

  method add_builtin_types pred constant =
    (self#create_att_space pred 0)#add_constant constant ;
    List.iter (fun pred -> self#add_builtin_types pred constant) pred#builtin_types      

  method create_ptype var = 
    let ptype = new ptype var in
      ptypes_list <- ptype :: ptypes_list ;
      ptype

  method create_domains = 
    List.iter (fun ptype -> ptype#remove_unused_ctypes ;  ptype#create_domain) ptypes_list

  method finalize =
    let ctypes_table = Hashtbl.create 10 in
    let attribute_spaces_table = Hashtbl.create 10 in
    let caracteristics = Array.map (fun _ -> {ptypes = [] ; attribute_spaces = []}) symb_set#constants in

      Array.iter generic_attribute_space#add_constant symb_set#constants ;

      List.iter
	(fun ptype ->
	   ptype#domain_set#iter
	   (fun constant ->
	      let caracteristic = caracteristics.(constant#num) in
		caracteristic.ptypes <- ptype :: caracteristic.ptypes)) 
      ptypes_list ;

      List.iter
	(fun attribute_space ->
	   attribute_space#iter
	   (fun constant ->
	      let caracteristic = caracteristics.(constant#num) in
		caracteristic.attribute_spaces <- attribute_space :: caracteristic.attribute_spaces)
	) att_space_list ;

      Array.iteri
	(fun pos {ptypes = ptypes ; attribute_spaces = attribute_spaces} ->
	   let ctype = 
	     if ptypes = [] then
	       try Hashtbl.find attribute_spaces_table attribute_spaces with
		 | Not_found -> 
		     let ctype = new Symb.ctype in
		       Hashtbl.add attribute_spaces_table attribute_spaces ctype ;
		       ctypes_list <- ctype :: ctypes_list ;
		       ctype 
	     else
	       try Hashtbl.find ctypes_table ptypes with
		 | Not_found -> 
		     let ctype = new Symb.ctype in
		       Hashtbl.add ctypes_table ptypes ctype ;
		       ctypes_list <- ctype :: ctypes_list ;
		       List.iter (fun ptype -> ptype#add_ctype ctype) ptypes ;
		       ctype
	   in
	   let constant = symb_set#constants.(pos) in
	     ctype#add_constant constant ;
	     constant#set_ctype ctype
	) caracteristics
end


class parameters vars_list (attribute_spaces : attribute_space_set) =
  let vars = Array.of_list vars_list in
object (self)
  val ptypes = Array.map (fun var -> attribute_spaces#create_ptype var) vars

  method nb = Array.length vars
  method vars = vars
  method vars_list = vars_list
  method domain pos = ptypes.(pos)#domain
  method ctypes pos = ptypes.(pos)#ctypes

  initializer 
    Array.iter 
      (fun var -> 
	 if var#has_builtin_type then 
 	   let attribute_space = attribute_spaces#create_att_space var#builtin_type 0 in
 	     ptypes.(var#num)#add_attribute_space attribute_space
      ) vars

  method to_string =
    let pos = ref (-1) in
      Utils.string_of_array "," (fun s -> incr pos ; s#to_string ^ ":" ^ Utils.to_string ptypes.(!pos)) vars

  method add_attribute_space (atom : Atom.t) =
    atom#iteri
      (fun pos term ->
 	 let attribute_space = attribute_spaces#create_att_space atom#pred pos in
 	   if term#is_var then ptypes.(term#num)#add_attribute_space attribute_space
 	   else attribute_space#add_constant term)

  method add_transition (atom : Atom.t) =
    atom#iteri 
      (fun pos term -> 
	 let attribute_space = attribute_spaces#create_att_space atom#pred pos in
	   if term#is_var then ptypes.(term#num)#add_transition attribute_space
	   else attribute_space#add_constant term)

  method fix_encountered_ctypes =
    Array.iter (fun ptype -> ptype#add_encountered_ctype) ptypes
end




class atom_types (symb_set : SymbSet.t) =
object
  val atom_types_array = Array.create symb_set#nb_pred []

  method atom_types_array = atom_types_array

  method to_string = 
    let p = ref (-1) in
      "Atom types :\n" ^ 
      Utils.string_of_array "" 
	(fun atom_types ->
	   incr p ;
	   let pred = symb_set#predicates.(!p) in
	     if pred#typing then "" else
	       "\n" ^ pred#to_string ^ " : " ^
	       Utils.string_of_list " " (fun ctypes -> "(" ^ Utils.string_of_array "," Utils.to_string ctypes ^ ")") atom_types
	) atom_types_array
	
  method add_atom_type (atom : Atom.t) =
    let atom_type  = Array.map (fun term -> term#ctype) atom#terms in
      if List.mem atom_type atom_types_array.(atom#pred#num) then false
      else begin
	atom_types_array.(atom#pred#num) <- atom_type :: atom_types_array.(atom#pred#num) ;
	true
      end

  method is_atom_type (atom : Atom.t) =
    List.mem (Array.map (fun term -> term#ctype) atom#terms) atom_types_array.(atom#pred#num)
end
