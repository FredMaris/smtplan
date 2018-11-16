let vname = "??V"

class t (pred : Symb.predicate) (terms : Symb.term array) (timedata : Timedata.t) (timeset_struct : (FunctionFormula.t * FunctionFormula.t)) =
object (self : 'atom)
  val pred = pred
  val terms = terms
  val mutable timedata = timedata
  val timeset_struct = timeset_struct

  method pred = pred
  method terms = terms
  method timedata = timedata
  method timeset_struct = timeset_struct
(*  method timeset = timedata#timeset
  method set_timeset (ts : int * int) = timedata#set_timeset ts *)
  method nb_terms = pred#arity

  initializer 
    pred#set_arity (Array.length terms);


  method to_string =
    "(" ^ pred#to_string ^ 
    (if terms = [| |] then ")" else " " ^  (Utils.string_of_array " " (fun s -> s#to_string) terms) ^ ")")
   (* ^ "[" ^ string_of_int (fst timeset) ^ ";" ^ string_of_int (snd timeset) ^ "]"*)

  method to_istring =
    "(" ^ pred#to_string ^ 
    (if terms = [| |] then ")" else " " ^  (Utils.string_of_array " " (fun s -> s#to_string) terms) ^ ")")
    (*^ "[" ^ string_of_int (fst timedata#timeset) ^ ";" ^ string_of_int (snd timedata#timeset) ^ "]"*)
    ^ timedata#to_complete_string

  method instantiate params = (* Utils.print "Instantiate: %s\n" self#to_string; flush stderr; *)
    {< terms = Array.map (fun t -> if t#is_var then params.(t#num) else t) terms ; timedata = timedata >}

  method equal (atom : 'atom) = pred == atom#pred && Utils.eq_array terms atom#terms (*&& fst timeset == fst atom#timeset && snd timeset == snd atom#timeset*)

  method equal2 (atom : 'atom) = pred == atom#pred && Utils.eq_array terms atom#terms

  method equal_names (atom : 'atom) = 
    pred == atom#pred && 
	  try
	    Array.iteri (fun i s -> if s#name != atom#terms.(i)#name then raise Exit) terms ;
	    true
	  with Exit -> false

  method add_compat (atom : 'atom) =
    let equalities1 = ref [] in
    let equalities2 = ref [] in
      Array.iteri 
	(fun i t1 ->
	   atom#iteri 
	   (fun j t2 ->
	      if t1== t2 then begin
		equalities1 := (i, j) :: !equalities1 ;
		equalities2 := (j, i) :: !equalities2
	      end
	   )
	) terms ;
      if not(pred#is_compat atom#pred !equalities1) then begin
	(*Printf.printf "%s -- %s\n" self#to_string atom#to_string ;*)
	pred#add_compat atom#pred !equalities1 ;
	atom#pred#add_compat pred !equalities2 ;
	true
      end
      else false

  method is_compat (atom : 'atom) =
    let equalities = ref [] in
      Array.iteri 
	(fun i t1 ->
	   atom#iteri 
	   (fun j t2 ->
	      if t1 == t2 then equalities := (i, j) :: !equalities)
	) terms ;
      pred#is_compat atom#pred !equalities

  method get_comp (vars : Symb.term list) =
    let atoms = ref [] in
    let v = new Symb.variable vname 0 in
      Array.iter
	(fun equalities ->
	   List.iter 
	   (fun (p, equalities) ->
	      let nt = Array.make p#arity v in
		List.iter (fun (a, b) -> nt.(b) <- terms.(a)) equalities ;
		atoms := new t p nt timedata timeset_struct :: !atoms;
		let change = ref false in
		let rec create tab pos =
		  if pos = Array.length tab then begin
		    if !change then atoms := new t p tab timedata timeset_struct :: !atoms
		  end
		  else
		  if pos < Array.length tab then begin
		    if tab.(pos) == v then
		      List.iter (fun v -> 
				   let copy = Array.copy tab in
				     tab.(pos) <- v ;
				     change := true ;
				     create copy (pos + 1)) vars ;
		  (*else*)
		    create tab (pos + 1)
		  end
		in 
		  create nt 0)
	   equalities)
	pred#get_compat ;
      !atoms
	   
  method iteri f = Array.iteri f terms
end

let equal a1 a2 = a1#equal a2

(*let hash a = 
  let v = ref a#pred#num in
    Array.iter (fun c -> v := c#num + !v * !Symb.nb_const) a#terms ; !v*)

let hash a = Hashtbl.hash (a#pred#num, a#terms)
