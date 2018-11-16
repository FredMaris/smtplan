type t = Funct of (Symb.predicate * Symb.term array * Timedata.t) | Number of float | Add of (t * t) | Sub of (t * t) | Multiply of (t * t) | Divide of (t * t)

let preatom_to_string (pred,terms,timedata) =
    "(" ^ pred#to_string ^ 
    (if terms = [| |] then ")" else " " ^  (Utils.string_of_array " " (fun s -> s#to_string) terms) ^ ")")

let preatom_equal pred1 terms1 (pred,terms,timedata) =
 (*let r =*)
    pred1 == pred && Utils.eq_array terms1 terms
 (*in
 Utils.print "%s %s %s\n" (preatom_to_string (pred,terms,timedata)) (if r then "=" else "<>") (preatom_to_string (pred1,terms1,timedata));
 r*)

let preatom_instantiate params (pred,terms,timedata) =
 (*let r =*)
    (pred,Array.map (fun t -> if t#is_var then params.(t#num) else t) terms,timedata)
 (*in
 Utils.print "%s -i-> %s\n" (preatom_to_string (pred,terms,timedata)) (preatom_to_string r);
 r*)

let rec to_string = function
  | Funct a -> (preatom_to_string a)
  | Number f -> (string_of_float f)
  | Add (x,y) -> "(+ " ^ (to_string x) ^ " " ^ (to_string y) ^ ")"
  | Sub (x,y) -> "(- " ^ (to_string x) ^ " " ^ (to_string y) ^ ")"
  | Multiply (x,y) -> "(* " ^ (to_string x) ^ " " ^ (to_string y) ^ ")"
  | Divide (x,y) -> "(/ " ^ (to_string x) ^ " " ^ (to_string y) ^ ")"

let rec functions_zero = function
  | Funct a -> 0.0
  | Number f -> f
  | Add (x,y) -> (functions_zero x) +. (functions_zero y)
  | Sub (x,y) -> (functions_zero x) -. (functions_zero y)
  | Multiply (x,y) -> (functions_zero x) *. (functions_zero y)
  | Divide (x,y) -> (functions_zero x) /. (functions_zero y)

let rec calculate functions_value_list params = function
  | Funct a -> List.fold_left (fun c (atom,value) -> if preatom_equal atom#pred atom#terms (preatom_instantiate params a) then value else c) 0.0 functions_value_list
  | Number f -> f
  | Add (x,y) -> (calculate functions_value_list params x) +. (calculate functions_value_list params y)
  | Sub (x,y) -> (calculate functions_value_list params x) -. (calculate functions_value_list params y)
  | Multiply (x,y) -> (calculate functions_value_list params x) *. (calculate functions_value_list params y)
  | Divide (x,y) -> (calculate functions_value_list params x) /. (calculate functions_value_list params y)
