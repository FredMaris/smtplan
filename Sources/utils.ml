let begin_time = ref 0.

(* supprime la 1ere occurence d'un element $f$ dans une liste $lf$ *)
let rec remove f lf = match lf with
   []->[]
  |(e::l)-> if e#equal f then l else e::(remove f l) ;;

let string_of_list sep f lis =
  String.concat sep (List.map f lis)

let string_of_list_iter sep f l = 
  let lt = ref l in
  let res = ref "" in
  begin
    for i = 0 to List.length l -1 do
      res := !res ^ sep ^ (f (List.hd !lt));
      lt := List.tl !lt;
    done;
    !res
  end;;

let string_of_array sep f tab =
  string_of_list sep f (Array.to_list tab)

let string_of_hashtbl sep f tab =
  let list = ref [] in
    Hashtbl.iter (fun k v -> list := (f k v) :: !list) tab ;
    String.concat sep !list

let to_string o = o#to_string

let array_remove_elem e array =
  let res = Array.make (Array.length array - 1) array.(0) in
  let i = ref 0 in
    for j = 0 to (Array.length res) do
      if array.(j) != e then begin 
	res.(!i) <- array.(j) ;
	incr i
      end
    done;
    res

let array_remove_elem2 e array =
  let res = Array.make (Array.length array - 1) array.(0) in
  let i = ref 0 in
    for j = 0 to (Array.length res) do
      if fst array.(j) != e then begin 
	res.(!i) <- array.(j) ;
	incr i
      end
    done;
    res

let eq_array a1 a2  =
  let i = ref 0 in 
  let eq = ref true in
    while !eq && !i < Array.length a1 do
      eq := a1.(!i) == a2.(!i) ; incr i
    done ; 
    !eq

let hashtbl_elements table =
  Hashtbl.fold (fun _ data list -> data :: list) table []

(*
let hashtbl_elements table =
  let elts = ref [] in 
    Hashtbl.iter (fun _ data -> elts := data :: !elts) table ;
    List.rev(!elts)
*)

let rec delete e = function
  | [] -> []
  | f :: l -> if e == f then l else f :: delete e l

let rec delete_pred p = function
  | [] -> []
  | f :: l -> if p f then l else f :: delete_pred p l

let rec delete_all_pred p = function
  | [] -> []
  | f :: l -> 
      let rest = delete_all_pred p l in
	if p f then rest else f :: rest

let prod_apply f l1 l2 =
  let l = ref l2 in
    List.iter (fun e1 -> l := List.tl !l ; List.iter (f e1) !l) l1

let prod_apply2 f l1 l2 =
  List.iter (fun e1 -> List.iter (f e1) l2) l1

let prod_for_all f l1 l2 =
  List.for_all (fun e1 -> List.for_all (f e1) l2) l1

let prod_never1 f l =
  let l2 = ref l in
    List.for_all (fun e1 -> l2 := List.tl !l2 ; List.for_all (fun e2 -> not(f e1 e2)) !l2) l

let array_prod_iter f a =
  for i = 0 to Array.length a - 2 do
    for j = i + 1 to Array.length a - 1 do
      f a.(i) a.(j)
    done
  done

let array_prod_iter_eq f a =
  for i = 0 to Array.length a - 1 do
    for j = i to Array.length a - 1 do
      f a.(i) a.(j)
    done
  done

let array_prod_iter_all f a =
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      f a.(i) a.(j)
    done
  done

let array_prod_iter2 f a1 a2 =
  for i = 0 to Array.length a1 - 1 do
    for j = 0 to Array.length a2 - 1 do
      f a1.(i) a2.(j)
    done
  done

let array_prod_always f a =
  try
    for i = 0 to Array.length a - 1 do
      for j = i + 1 to Array.length a - 1 do
	if not(f a.(i) a.(j)) then raise Exit
      done
    done ;
    true
  with Exit -> false

let eprint = Printf.eprintf
let print = Printf.eprintf

let get_time () = (Unix.times ()).Unix.tms_utime

let my_time message f = 
  eprint "%s..... " message ; flush stderr ;
(*  let start = Unix.gettimeofday () in*)
  let start = get_time () in
  let result = f () in
    eprint "done.\n" ; 
    flush stderr ;
    (get_time () -. start, result)

let my_time2 f = 
(*  let start = Unix.gettimeofday () in*)
  let start = get_time () in
  let result = f () in
    (get_time () -. start, result)

let to_string x = x#to_string
let to_complete_string x = x#to_complete_string

let print_channel ch str = 
 (* output ch str 0 (String.length str) *)
  output_string ch str