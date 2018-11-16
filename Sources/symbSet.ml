class t =
object (self)
  val pred_table = Hashtbl.create 10
  val const_table = Hashtbl.create 100
  val var_table = Hashtbl.create 10

  val mutable var_list = []

  val mutable predicates = [| |]
  val mutable constants = [| |]
  val mutable variables = [| |]
  
  val mutable nb_pred = 0
  val mutable nb_const = 0
  val mutable nb_var = 0
  val mutable num_var = 0

  method predicates = predicates
  method constants = constants
  method variables = variables

  method nb_pred = nb_pred
  method nb_const = nb_const
  method nb_var = nb_var

  method create_predicate name =
    try Hashtbl.find pred_table name with
      | Not_found -> 
	  let symbol = new Symb.predicate name nb_pred in
	    Hashtbl.add pred_table name symbol ; 
	    nb_pred <- succ nb_pred ;
	    symbol

  method create_constant name =
    try Hashtbl.find const_table name with
      | Not_found -> 
	  let constant = new Symb.constant name nb_const in
	    Hashtbl.add const_table name constant ; 
	    nb_const <- succ nb_const ;
	    constant

  method create_variable (name : string) =
    try Hashtbl.find var_table name with
      | Not_found -> 
	  nb_var <- succ nb_var ;
	  let variable = new Symb.variable ("x" ^ string_of_int nb_var) num_var in
	    Hashtbl.add var_table name variable ;
	    num_var <- succ num_var ;
	    variable
    
  method reset_var_table =
    var_list <- var_list @ List.sort (fun v1 v2 -> compare v1#num v2#num) (Utils.hashtbl_elements var_table) ;
    Hashtbl.clear var_table ;
    num_var <- 0

  method finalize =
    predicates <- Array.of_list (List.sort (fun p1 p2 -> compare p1#num p2#num) (Utils.hashtbl_elements pred_table)) ;
    constants <- Array.of_list (List.sort (fun c1 c2 -> compare c1#num c2#num) (Utils.hashtbl_elements const_table)) ;
    variables <- Array.of_list var_list ;
    Hashtbl.iter (fun k pred -> pred#init_compat nb_pred) pred_table
end
