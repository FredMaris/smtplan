class t =
object (self)

val mutable smtfilename = "default.smt"
val mutable smtfile = Unix.openfile "default.smt" [Unix.O_TRUNC;Unix.O_CREAT;Unix.O_WRONLY] 0o640

val smtout = Unix.pipe()
(* val sstdin = Unix.dup Unix.stdin *)
val sstdout = Unix.dup Unix.stdout

val mutable result = ""

val mutable model = []

(* method sstdin = sstdin *)
method sstdout = sstdout


method set_smtfilename filename = smtfilename <- Printf.sprintf "%s" filename


method smtout_r = fst smtout
method smtout_w = snd smtout

method open_smtwrite = ()

method smtwrite s =
  ignore (Unix.write smtfile (Bytes.of_string s) 0 (String.length s))

method close_smtwrite = 
  Unix.close smtfile;
  
method model = model

method launch =
flush stderr; flush stdout;
match Unix.fork () with
    0 -> let ipid=Unix.getpid() in
         Unix.close self#smtout_r;
         (* *) Utils.print "SMT Solver processing ...\n"; flush stderr; (* *)
	 Unix.dup2 self#smtout_w Unix.stdout;
        (* let rsmtfile = (Unix.openfile "default.smt" [Unix.O_RDONLY] 0o640) in
         Unix.dup2 rsmtfile Unix.stdin; *)
	(* ignore (Unix.execvp "smt-solver-mac/mathsat" [|"-input=smt"|]); *)
	(* if 0 == (Sys.command (Printf.sprintf "smt-solver-mac/mathsat -input=smt2 < %s" smtfilename)) then exit 0; *)
	 if 0 == (Sys.command (Printf.sprintf "smt-solver-mac/yices-smt2 %s" smtfilename)) then exit 0;

	(* Unix.dup2 self#sstdin Unix.stdin;
         Unix.close rsmtfile; *)
	 Unix.dup2 self#sstdout Unix.stdout;
	 Utils.print "Error : launching SMT solver.\n"; flush stderr;
	 Unix.close self#smtout_w;
	 exit 0;
  | pid  -> Unix.close self#smtout_w;
           (* Unix.close self#sstdin; *)
            Unix.close self#sstdout;
	    ignore (Unix.waitpid [] pid);
	    begin
		try
	    	let c = Unix.in_channel_of_descr self#smtout_r in 
	      	result <- input_line c;
			if (String.compare result "sat") = 0 then
	      	begin
				(* *) Utils.print "SMT solver result : %s\n" result; (* *)
	(*			while true do
					Utils.print "%s\n" (input_line c);
				done
	 *)
	(*	*)		let lexbuf = Lexing.from_channel c in
				model <- Smtoutparser.model Smtoutlexer.token lexbuf;
			    List.iter (fun (variable,value) -> if value>=0.0 then Utils.print "%s = %f\n" variable value) model;
(*	*)		end
		with
			End_of_file -> Utils.print "CRASH!\n";
		end;

		Unix.close self#smtout_r;
                (*Printf.printf "Child [%d] returns %s\n" pid result;*)
		if (String.compare result "sat") = 0
		then true
		else false



end