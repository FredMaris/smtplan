type node ={
  value : int ;
  mutable links : node_set
}
and node_set = Empty | Node of node_set * node * node_set * int

let compare_nodes {value = v1} {value = v2} = compare v1 v2


module Set =
struct

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

    let height = function
        Empty -> 0
      | Node(_, _, _, h) -> h

    (* Creates a new node with left son l, value x and right son r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

    let create l x r =
      let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
      Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

    let bal l x r =
      let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Set.bal"
        | Node(ll, lv, lr, _) ->
            if height ll >= height lr then
              create ll lv (create lr x r)
            else begin
              match lr with
                Empty -> invalid_arg "Set.bal"
              | Node(lrl, lrv, lrr, _)->
                  create (create ll lv lrl) lrv (create lrr x r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Set.bal"
        | Node(rl, rv, rr, _) ->
            if height rr >= height rl then
              create (create l x rl) rv rr
            else begin
              match rl with
                Empty -> invalid_arg "Set.bal"
              | Node(rll, rlv, rlr, _) ->
                  create (create l x rll) rlv (create rlr rv rr)
            end
      end else
        Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as bal, but repeat rebalancing until the final result
       is balanced. *)

    let rec join l x r =
      match bal l x r with
        Empty -> invalid_arg "Set.join"
      | Node(l', x', r', _) as t' ->
          let d = height l' - height r' in
          if d < -2 || d > 2 then join l' x' r' else t'

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assumes | height l - height r | <= 2. *)

    let rec merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
          bal l1 v1 (bal (merge r1 l2) v2 r2)

    (* Same as merge, but does not assume anything about l and r. *)

    let rec concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
          join l1 v1 (join (concat r1 l2) v2 r2)

    (* Splitting *)

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, r, _) ->
          let c = compare_nodes x v in
          if c = 0 then (l, Some v, r)
          else if c < 0 then
            let (ll, vl, rl) = split x l in (ll, vl, join rl v r)
          else
            let (lr, vr, rr) = split x r in (join l v lr, vr, rr)

    (* Implementation of the set operations *)

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec mem x = function
        Empty -> false
      | Node(l, v, r, _) ->
          let c = compare_nodes x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec add x = function
        Empty -> Node(Empty, x, Empty, 1)
      | Node(l, v, r, _) as t ->
          let c = compare_nodes x v in
          if c = 0 then t else
          if c < 0 then bal (add x l) v r else bal l v (add x r)

    let singleton x = Node(Empty, x, Empty, 1)

    let rec remove x = function
        Empty -> Empty
      | Node(l, v, r, _) ->
          let c = compare_nodes x v in
          if c = 0 then merge l r else
          if c < 0 then bal (remove x l) v r else bal l v (remove x r)

    let rec union s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
          if h1 >= h2 then
            if h2 = 1 then add v2 s1 else begin
              let (l2, _, r2) = split v1 s2 in
              join (union l1 l2) v1 (union r1 r2)
            end
          else
            if h1 = 1 then add v1 s2 else begin
              let (l1, _, r1) = split v2 s1 in
              join (union l1 l2) v2 (union r1 r2)
            end

    let rec inter s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              concat (inter l1 l2) (inter r1 r2)
          | (l2, Some _, r2) ->
              join (inter l1 l2) v1 (inter r1 r2)

    let rec diff s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, _), t2) ->
          match split v1 t2 with
            (l2, None, r2) ->
              join (diff l1 l2) v1 (diff r1 r2)
          | (l2, Some _, r2) ->
              concat (diff l1 l2) (diff r1 r2)

    let rec compare_aux l1 l2 =
        match (l1, l2) with
        ([], []) -> 0
      | ([], _)  -> -1
      | (_, []) -> 1
      | (Empty :: t1, Empty :: t2) ->
          compare_aux t1 t2
      | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
          let c = compare_nodes v1 v2 in
          if c <> 0 then c else compare_aux (r1::t1) (r2::t2)
      | (Node(l1, v1, r1, _) :: t1, t2) ->
          compare_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
      | (t1, Node(l2, v2, r2, _) :: t2) ->
          compare_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)

    let compare s1 s2 =
      compare_aux [s1] [s2]

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
          let c = compare_nodes v1 v2 in
          if c = 0 then
            subset l1 l2 && subset r1 r2
          else if c < 0 then
            subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
          else
            subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2

    let rec iter f = function
        Empty -> ()
      | Node(l, v, r, _) -> iter f l; f v; iter f r

    let rec fold f s accu =
      match s with
        Empty -> accu
      | Node(l, v, r, _) -> fold f l (f v (fold f r accu))

    let rec for_all p = function
        Empty -> true
      | Node(l, v, r, _) -> p v && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node(l, v, r, _) -> p v || exists p l || exists p r

    let filter p s =
      let rec filt accu = function
        | Empty -> accu
        | Node(l, v, r, _) ->
            filt (filt (if p v then add v accu else accu) l) r in
      filt Empty s

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty -> accu
        | Node(l, v, r, _) ->
            part (part (if p v then (add v t, f) else (t, add v f)) l) r in
      part (Empty, Empty) s

    let rec cardinal = function
        Empty -> 0
      | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

    let rec elements_aux accu = function
        Empty -> accu
      | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s

    let rec min_elt = function
        Empty -> raise Not_found
      | Node(Empty, v, r, _) -> v
      | Node(l, v, r, _) -> min_elt l

    let rec max_elt = function
        Empty -> raise Not_found
      | Node(l, v, Empty, _) -> v
      | Node(l, v, r, _) -> max_elt r

    let choose = min_elt
end


let create () = Empty


let rec get x = function
    Empty -> raise Not_found
  | Node(l, v, r, _) ->
      let c = compare x v.value in
	if c = 0 then v else get x (if c < 0 then l else r)


let rec iter_sup f1 f2 x = function
    Empty -> ()
  | Node(l, v, r, _) (*as set*) -> 
      let c = compare x v.value in
	if c < 0 then iter_sup f1 f2 x l
	else if c = 0 then f1 v
	else begin iter_sup f1 f2 x l ; f2 v ; iter_sup f1 f2 x r end


let add l s = 
  let rec add s = function
      [] -> s
    | e :: l -> 
	try 
	  let node = get e s in
	    node.links <- add node.links l ;
	    s
	with Not_found -> 
	  Set.add {value = e ; links = add Empty l} s
  in add s l


let is_subsumed l s = 
  let rec is_subsumed l = function
      Empty -> true
    | s -> let m = (Set.max_elt s).value in
	let rec aux = function 
	    [] -> false
	  | e :: l -> 
	      e <= m && ((try is_subsumed  l (get e s).links with Not_found -> false) or aux l) 
	in aux l
  in s <> Empty && is_subsumed l s

let alone = function
    Empty -> false
  | Node(l, _, r, _) -> l =Empty && r = Empty

let remove_subsumed l s =
  let rec remove s = function
      [] -> Empty
    | e :: l as list ->
	let ns = ref s in
	  iter_sup
	    (fun node ->  (*equality*)
	       if l = [] or node.links <> Empty then
		 let set = remove node.links l in
		   if set = Empty then ns := Set.remove node !ns else node.links <- set)
	    (fun node ->
	       if node.links <> Empty then
		 let set = remove node.links list in
		   if set = Empty then ns := Set.remove node !ns else node.links <- set)
	    e s ;
	  !ns
  in remove s l


let add_min l s = add l (remove_subsumed l s)



let rec cardinal s =
  Set.fold (fun node nb -> (if node.links = Empty then 1 else 0) + nb + cardinal node.links) s 0

(*
let rec print i = function
  Empty -> print_newline ()
  | s -> 
      List.iter (fun node -> Printf.printf "(%i) %i " i node.value ; print (i+1) node.links) 
      (Set.elements s)

let s = create ()
(*
let s = add_min [2;5;7;8] s
let s = add_min [2;5;8;9] s
let s = add_min [2;5;9;10] s
let s = add_min [5;7;8] s
let s = add_min [5;7;9] s
let s = add_min [5;9;10] s
let s = add_min [5;9;12;14] s
*)
let s = add_min [5;10;14;15;18] s
let s = add_min [5;10;12;13;15;16] s

let _ = 
  print 0 s ; 
  Printf.printf "%i  %b\n" (cardinal s) (is_subsumed [1;5;6;10] s) ;
  let s = add_min [5;10;12] s in
    print 0 s ;
*)


