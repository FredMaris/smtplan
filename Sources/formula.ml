type t = Top | PosLit of Atom.t | NegLit of Atom.t | Conjunct of t array | When of (t*t) | Forall of ((Symb.variable list)*t)


let rec to_string = function
  | Top -> "()"
  |  PosLit a -> a#to_string
  | NegLit a -> "(not " ^ a#to_string ^ ")"
  | Conjunct c -> "(and " ^ (Utils.string_of_array " " to_string c) ^ ")"
  | When (c,e) -> "(when " ^ (to_string c) ^ " " ^ (to_string e) ^ ")"
  | Forall (variables,formula) -> "(forall " ^ (String.concat " " (List.map (fun v -> v#to_string) variables)) ^ ")" ^ to_string formula

let rec simplify simplify_func = function
  | PosLit atom when atom#pred#typing ->
      simplify_func atom ;
      Top
  | When (c, PosLit atome) when atome#pred#typing ->
      simplify_func atome ;
      Top
  | Conjunct formulas -> begin
      match 
	Array.fold_right (fun f formulas -> 
			  match simplify simplify_func f with
			    | Top -> formulas
			    | formula -> formula :: formulas) formulas []
      with
	| [] -> Top
	| [formula] -> formula
	| formulas -> Conjunct (Array.of_list formulas)
    end
  | formula -> formula


(*let rec flattening_aux vars = function
  | Top -> Top
  | PosLit a -> PosLit (a#instanciate vars)
  | NegLit a -> NegLit (a#instanciate vars)
  | When (c,e) -> When (flattening_aux vars c, flattening_aux vars e)
  | Forall ([variable],formula) -> Conjunct (Array.of_list (List.map (fun v -> flattening_aux (v :: vars) formula) [variable] 
(*  | Forall (v::variables,formula) -> Conjunct *)

let flattening = flattening_aux [] *)