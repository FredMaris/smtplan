let _ = 
  Utils.begin_time := Utils.get_time () ;
  let print_help () =
    Utils.eprint "\nUsage: %s DOMAIN PROBLEM [ALGORITHM]\n
DOMAIN: strips temporal planning domain expressed in (typed) PDDL
PROBLEM: strips temporal planning problem expressed in (typed) PDDL
ALGORITHM: one of the following:
\t- tlp-gp : Temporally Lifted Progression GraphPlan (TLP-GP-1)
\t- smtplan : Sat Modulo Theory temporal PLANner (TLP-GP-2)

\n" Sys.argv.(0) ;
    exit 0
  in
  let nb_args = Array.length Sys.argv in
  let algo = 
    if nb_args < 3 then print_help ()
    else
      if nb_args = 3 then "smtplan" else Sys.argv.(3) in
    match algo with
(*      | "tlp-gp" -> (new Tlpgp.t)#search
 *)
      | "smtplan" -> (new Smtplan.t)#search
      | x -> Utils.eprint "\n%s : search procedure unknown.\n" x ; print_help ()
