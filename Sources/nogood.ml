class virtual ['fluent] c =
object

  val table = Array.make PGraph.max_level (ISetSet.create ())

  method add_nogood (fluents : 'fluent list) level = 
    let nums = List.sort compare (List.map (fun a -> a#n) fluents) in
      table.(level) <- ISetSet.add_min nums table.(level)
  
  method is_nogood (fluents : 'fluent list) level =
    let nums = List.sort compare (List.map (fun a -> a#n) fluents) in
      ISetSet.is_subsumed nums table.(level)

  method nb_nogoods = 
    Array.fold_left (fun nb nogoods -> nb + ISetSet.cardinal nogoods) 0 table

end

