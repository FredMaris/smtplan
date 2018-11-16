class operator name (parameters : Typeset.parameters) (duration : FunctionFormula.t) (quality : int) precondition effect = 
object (self)
  val constraints = Array.create parameters#nb []
  val mutable cons = parameters#vars_list
  val mutable prec = [||]
  val mutable nprec = [||]
  val mutable add = [||]
  val mutable del = [||]
  val mutable equa = [||]
  val mutable diff = [||]
  val mutable order = [||]
  val mutable condadd = [||]
  val mutable ncondadd = [||]
  val mutable conddel = [||]
  val mutable nconddel = [||]

  method name = name
  method parameters = parameters
  method duration = duration
  method quality = quality
  method constraints = constraints
  method prec = prec
  method nprec = nprec
  method add = add
  method del = del
  method equa = equa
  method diff = diff
  method order = order
  method condadd = condadd
  method ncondadd = ncondadd
  method conddel = conddel
  method nconddel = nconddel

  method to_creation_string =
    "(:action " ^ name ^
    "\n\t:parameters (" ^ parameters#to_string ^
    "\n\t:quality (" ^ (string_of_int quality) ^
    ")\n\t:precondition " ^ Formula.to_string precondition ^
    "\n\t:effect " ^ Formula.to_string effect ^ ")"

  method to_string =
    name ^ " (" ^ parameters#to_string ^ ")"
      
  method to_complete_string =
    "(:action " ^ name ^
    "\n\t:parameters (" ^ parameters#to_string ^ 
    "\n\t:quality (" ^ (string_of_int quality) ^
    ")\n\t:precs " ^ Utils.string_of_array " " Utils.to_string prec ^ 
    "\n\t:cons " ^ Utils.string_of_array " - " (Utils.string_of_list " " Utils.to_string) constraints ^
    "\n\t:adds " ^ Utils.string_of_array " " Utils.to_string add ^ 
    "\n\t:dels " ^ Utils.string_of_array " " Utils.to_string del

  method to_complete_istring =
    let string_of_fluent_array fluents =
      Utils.string_of_array " " (fun f -> f#to_istring) fluents in
    let string_of_condeff_array condeffs =
      Utils.string_of_array " " (fun (c,e) -> "<" ^ c#to_istring ^ "->" ^ e#to_istring ^ ">") condeffs in
    "(:action " ^ name ^
    "\n\t:parameters (" ^ parameters#to_string ^ 
    "\n\t:quality (" ^ (string_of_int quality) ^
    "\n\tiPrec : " ^ string_of_fluent_array prec ^
    "\n\tiNPrec : " ^ string_of_fluent_array nprec ^
    "\n\tiAdd : " ^ string_of_fluent_array add ^
    "\n\tiDel : " ^ string_of_fluent_array del ^
    "\n\tiCondAdd : " ^ string_of_condeff_array condadd ^
    "\n\tiNCondAdd : " ^ string_of_condeff_array ncondadd ^
    "\n\tiCondDel : " ^ string_of_condeff_array conddel ^
    "\n\tiNCondAdd : " ^ string_of_condeff_array nconddel

      
  method create_action_struct =
    let precondition =
      Formula.simplify
	(fun atom -> 
	   parameters#add_attribute_space atom ;
	   if atom#nb_terms >= 2 then begin
	     let min = ref max_int in
	       Array.iter (fun t -> if t#num < !min then min := t#num) atom#terms ;
	       constraints.(!min) <- atom :: constraints.(!min) ;
	       Array.iter (fun v -> cons <- Utils.delete v cons ) atom#terms
	   end) precondition
    in
    let oprec = ref [] in
    let onprec = ref [] in
    let oadd = ref [] in
    let odel = ref [] in
    let ocondeff = ref [] in
    let odiff = ref [] in
    let oequa = ref [] in
    let rec create pos neg = function
      | Formula.Top -> ()
      | Formula.PosLit atom -> if atom#pred#name = "=" then oequa := atom :: !oequa else pos := atom :: !pos
      | Formula.NegLit atom -> if atom#pred#name = "=" then odiff := atom :: !odiff else neg := atom :: !neg
      | Formula.Conjunct formulas -> Array.iter (create pos neg) formulas
      | Formula.When _ -> failwith "Unauthorized use of 'when' in :condition.\n"
    in
    
    let create_when pos neg ceff = function
      | (Formula.PosLit atomc, Formula.PosLit atome) -> ceff := ((atomc,atome),(true,true)) :: !ceff
      | (Formula.NegLit atomc, Formula.PosLit atome) -> ceff := ((atomc,atome),(false,true)) :: !ceff
      | (Formula.PosLit atomc, Formula.NegLit atome) -> ceff := ((atomc,atome),(true,false)) :: !ceff
      | (Formula.NegLit atomc, Formula.NegLit atome) -> ceff := ((atomc,atome),(false,false)) :: !ceff
    in
    let rec create_eff pos neg ceff = function
      | Formula.Top -> ()
      | Formula.PosLit atom -> if atom#pred#name = "=" then oequa := atom :: !oequa else pos := atom :: !pos
      | Formula.NegLit atom -> if atom#pred#name = "=" then odiff := atom :: !odiff else neg := atom :: !neg
      | Formula.Conjunct formulas -> Array.iter (create_eff pos neg ceff) formulas
      | Formula.When (c,e) -> (* begin Utils.print "Conditional effect 'when'.\n"; *) create_when pos neg ceff (c,e) (* end *)
    in
    
    let create_equality_rules list =
      Array.of_list
	(List.map 
	   (fun atom -> 
	      let t1 = atom#terms.(0) in
	      let t2 = atom#terms.(1) in
		if t1#is_var then (t1, t2) else (t2, t1)
	   ) list)
    in
    
(* *)    Utils.print "%s\nPREC:\n%s\nEFF:\n%s\n" name (Formula.to_string precondition) (Formula.to_string effect);

      create oprec onprec precondition ;
      create_eff oadd odel ocondeff effect ;
      prec <- Array.of_list !oprec ;
      nprec <- Array.of_list !onprec ;
      add <- Array.of_list !oadd ;
      del <- Array.of_list !odel ;
      equa <- create_equality_rules !oequa ;
      diff <- create_equality_rules !odiff ;
      
      condadd <- Array.of_list (List.fold_left (fun l ((c,e),(posc,pose)) -> if (posc && pose) then (c,e) :: l else l) [] !ocondeff) ;
      ncondadd <- Array.of_list (List.fold_left (fun l ((c,e),(posc,pose)) -> if ((not posc) && pose) then (c,e) :: l else l) [] !ocondeff) ;
      conddel <- Array.of_list (List.fold_left (fun l ((c,e),(posc,pose)) -> if (posc && (not pose)) then (c,e) :: l else l) [] !ocondeff) ;
      nconddel <- Array.of_list (List.fold_left (fun l ((c,e),(posc,pose)) -> if ((not posc) && (not pose)) then (c,e) :: l else l) [] !ocondeff) ;

      Array.iter parameters#add_attribute_space prec ;
      Array.iter parameters#add_transition add ;
      (* find order *)
      let p = ref parameters#vars_list in
	List.iter (fun v -> p := Utils.delete v !p) cons ;
	order <- Array.of_list (List.map (fun s -> s#num) (List.rev_append !p cons))
(*; Utils.print "OPERATOR######%s\n" name;
Array.iter (fun f -> Utils.print "PREC######%s\n" f#to_istring) prec;
Array.iter (fun f -> Utils.print "ADD######%s\n" f#to_istring) add;
Array.iter (fun f -> Utils.print "DEL######%s\n" f#to_istring) del;*)
end


class domain name requirements (operators : operator array) = 
object (self)
  val equality = List.mem ":equality" requirements

  method name = name
  method equality = equality

  method to_string =
    "Operator types :\n\n" ^ Utils.string_of_array "\n" Utils.to_string operators 

  method to_complete_string = 
    "(define (domain " ^ name ^ ")\n" ^
    Utils.string_of_array "\n" Utils.to_complete_string operators

  method to_complete_istring = 
    "(define (domain " ^ name ^ ")\n" ^
    Utils.string_of_array "\n" (fun o -> o#to_complete_istring) operators

  method operator_iter f = Array.iter f operators

  initializer
    self#operator_iter  (fun o ->  o#create_action_struct)
end


class problem name (domain : domain) objects init goal symb_set (attribute_spaces : Typeset.attribute_space_set) (functions_value_list : (Atom.t * float) list) =
object
  val atom_types = new Typeset.atom_types symb_set

  method name = name
  method domain = domain
  method init = init
  method goal = goal
  method functions_value_list = functions_value_list
  
  method attribute_spaces = attribute_spaces
  method atom_types = atom_types

  method to_string = attribute_spaces#to_string ^ "\n\n" ^ atom_types#to_string

  method to_complete_string =
    "(define (problem " ^ name ^
    ")\n  (:domain " ^ domain#name ^
    ")\n  (:init " ^ (Utils.string_of_list " " Utils.to_string init) ^
    ")\n  (:goal " ^ Formula.to_string goal ^ ")\n"

  method add_atom_constants = attribute_spaces#add_constants

  method finalize =
    let compute_encountered_ctypes () =
      let change = ref true in
      let variable_types = ref [] in
      let rec get_variable_types o vtypes = function
	| -1 -> variable_types := (o, Array.of_list vtypes) :: !variable_types
	| pos -> 
	    List.iter (fun ctype -> get_variable_types o (ctype :: vtypes) (pos - 1)) 
	    (o#parameters#ctypes pos)
      in
      let try_variable_types (o, vtypes) =
	try
	  Array.iteri (fun pos v -> v#set_ctype vtypes.(pos)) o#parameters#vars ;
	  Array.iter (fun atom -> if not(atom_types#is_atom_type atom) then raise Exit) o#prec ;
	  Array.iter (fun atom -> change := atom_types#add_atom_type atom or !change) o#add ;
	  o#parameters#fix_encountered_ctypes
	with
	  | Exit -> variable_types := (o, vtypes) :: !variable_types
      in
	List.iter (fun atom -> if not atom#pred#typing then ignore (atom_types#add_atom_type atom)) init ;
	domain#operator_iter (fun o -> get_variable_types o [] (o#parameters#nb - 1)) ;
	while !change && !variable_types <> [] do
	  change := false ;
	  let variable_types2 = !variable_types in
	    variable_types := [] ;
	    List.iter try_variable_types variable_types2
	done 
    in
      List.iter (fun c -> if c#has_builtin_type then attribute_spaces#add_builtin_types c#builtin_type c) objects ;
      symb_set#finalize ;
      attribute_spaces#finalize ;
      compute_encountered_ctypes () ;
      attribute_spaces#create_domains ;
end


let domain_void = new domain "" [] [||]
let problem_void = new problem "" domain_void [] [] Formula.Top (new SymbSet.t) (new Typeset.attribute_space_set (new SymbSet.t)) []
