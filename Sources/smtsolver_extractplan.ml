class t =
object (self)

val mutable smtfilename = "default.smt"
val mutable planfile = Unix.openfile "plan.txt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640

method set_smtfilename filename = smtfilename <- Printf.sprintf "%s" filename

method launch =
  Utils.print "Searching static plan ...\n"; flush stderr;
  ignore (Sys.command (Printf.sprintf "smt-solver-mac/mathsat -input=smt -model < %s | grep t_ | grep = > plan.txt" smtfilename));
  let file = "plan.txt" in
  let varlist = ref [] in
  let solver_factor = ref 1 in
  let ic = open_in file in
   begin
    try 
     while true do let line = input_line ic in
      if (String.get line 0) = 'S' then begin
       let cut_pos = String.index line '=' in
        solver_factor := (int_of_string (String.sub line (cut_pos+2) ((String.length line)-cut_pos-2)))
       end
       else begin
         let cut_pos = String.index line '=' in
         let level_pos = String.rindex line '_' in
         let start_time =
           if String.contains line '/' then
             let cut2_pos = String.index line '/' in
             ((float_of_string (String.sub line (cut_pos+2) (cut2_pos-cut_pos-2)))/.(float_of_string (String.sub line (cut2_pos+1) ((String.length line)-cut2_pos-1))))
           else (float_of_string (String.sub line (cut_pos+2) ((String.length line)-cut_pos-2)))
          (*(int_of_string (String.sub line (cut_pos+2) ((String.length line)-cut_pos-2)))*) in
         let level_num = (int_of_string (String.sub line (level_pos+1) (cut_pos-level_pos-2))) in
          varlist := ((String.sub line 2 (level_pos-2)), level_num, start_time) :: !varlist;
       end;
     done
    with e ->
     close_in_noerr ic;
   end;
  varlist := (List.sort (fun (s1,l1,d1) (s2,l2,d2) -> let comp = (compare d1 d2) in if comp = 0 then (compare l1 l2) else comp) !varlist);
  (!varlist,!solver_factor);
end
