%{

  let action_relative_end_time = ref (FunctionFormula.Number 1.0)

  let current_time_set = ref ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))

  let current_time_bound = ref (true,true)

  let ti_code = ref 0

  let ti_min_dur = ref 0.0

  let functions_value_list = ref []

  let parse_error s = Utils.eprint " Syntax error at line %i\n\n" !Lexer.line

  let symb_set = new SymbSet.t
  let attribute_spaces = new Typeset.attribute_space_set symb_set

  let domain = ref Domain.domain_void

  let constants_in_domain = ref []

  let create_domain name requirements operators =
    Lexer.line := 1 ;
    Utils.eprint "%s..... " name ;
    domain := new Domain.domain name requirements (Array.of_list operators) ;
    !domain

  let create_operator name params descr =
    symb_set#reset_var_table ;
    let (duration, quality, prec, eff) = descr in
    let parameters =  new Typeset.parameters params attribute_spaces in
      new Domain.operator (String.uppercase name) parameters duration quality prec eff

  let create_problem name domain_name objects init goal =
    Utils.eprint "%s..... " name ;
    if domain_name <> !domain#name then begin
      Utils.eprint "\n\nProblem %s not for domain %s !\n\n" name !domain#name ;
      exit 0 
    end ;
    new Domain.problem name !domain objects init goal symb_set attribute_spaces !functions_value_list

  let create_atom pred terms timeset =
    let pred = symb_set#create_predicate pred in
      if !Lexer.is_effect then pred#untype ;
      let timedata = ref (new Timedata.t timeset !ti_code) in
      (!timedata)#set_closed_left (fst !current_time_bound);
      (!timedata)#set_closed_right (snd !current_time_bound);
      (!timedata)#set_min_dur !ti_min_dur;
      new Atom.t pred (Array.of_list terms) !timedata timeset

  let create_preatom pred terms timeset =
    let pred = symb_set#create_predicate pred in
      if !Lexer.is_effect then pred#untype ;
      let timedata = ref (new Timedata.t timeset !ti_code) in
      (!timedata)#set_closed_left (fst !current_time_bound);
      (!timedata)#set_closed_right (snd !current_time_bound);
      (!timedata)#set_min_dur !ti_min_dur;
      (pred,(Array.of_list terms),!timedata)

  let create_typed_term_list variables pred =
    List.iter (fun t -> t#set_builtin_type (symb_set#create_predicate pred)) variables ;
    variables

  let set_action_relative_end_time dur =
    action_relative_end_time := dur ;
    dur

  let set_current_time_set timeset =
    current_time_set := timeset ;
    timeset

  let formula_time formulas time_set =
   List.map (fun p -> (p, time_set)) formulas

  let create_function name var_list =
   (name,var_list)

%}

%token DEFINE DOMAIN REQUIREMENTS CONSTANTS TYPES FUNCTIONS ACTION DURATIVE_ACTION PARAM DURATION AT BEFORE AFTER START END OVER ALL SOMEWHERE ANYWHERE MIN_DUR ASSIGN INCREASE DECREASE CONSUME PRODUCE QUALITY PREC EFFECT NOT AND TYPE
%token LP RP EQUAL ADD MULTIPLY DIVIDE LH RH INF SUP
%token PROBLEM PDOMAIN OBJECTS INIT GOAL METRIC MINIMIZE TOTALTIME
%token FORALL WHEN
%token <string> VAR IDENT REQUIREMENT
%token <int> INTEGER
%token <float> RATIONAL
%start domain problem
%type <Domain.domain> domain
%type <Domain.problem> problem
%%

domain:
| LP DEFINE LP DOMAIN IDENT RP LP REQUIREMENTS requirements LP TYPES types LP CONSTANTS typed_constant_list operator_list { constants_in_domain := $15; create_domain $5 $9 $16 }
;| LP DEFINE LP DOMAIN IDENT RP LP REQUIREMENTS requirements LP TYPES types LP CONSTANTS typed_constant_list ;LP FUNCTIONS functions_list operator_list { constants_in_domain := $15; create_domain $5 $9 $19 }

| LP DEFINE LP DOMAIN IDENT RP LP REQUIREMENTS requirements LP TYPES types operator_list { create_domain $5 $9 $13 }
;| LP DEFINE LP DOMAIN IDENT RP LP REQUIREMENTS requirements LP TYPES types LP FUNCTIONS functions_list ;operator_list { create_domain $5 $9 $16 }

| LP DEFINE LP DOMAIN IDENT RP LP REQUIREMENTS requirements operator_list { create_domain $5 $9 $10 }
| LP DEFINE LP DOMAIN IDENT RP LP TYPES types operator_list { create_domain $5 [] $10 }
| LP DEFINE LP DOMAIN IDENT RP operator_list { create_domain $5 [] $7 }

requirements:
| RP { [] }
| REQUIREMENT requirements { $1 :: $2 }

operator_list:
| RP { [] }
| LP ACTION operator operator_list { $3 :: $4 }
| LP DURATIVE_ACTION operator operator_list { $3 :: $4 }


operator:
| IDENT PARAM LP typed_variable_list operator_strips RP { set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator $1 $4 $5 }
| IDENT PARAM LP variable_list operator_strips RP { set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator $1 $4 $5 }
| IDENT operator_strips RP { set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator $1 [] $2 }

operator_strips:
| duration quality operator_strips_cont { ($1, $2, fst $3, snd $3) }
| duration operator_strips_cont { ($1, 0, fst $2, snd $2) }
| quality operator_strips_cont { ((FunctionFormula.Number 1.0), $1, fst $2, snd $2) }
| operator_strips_cont { ((FunctionFormula.Number 1.0), 0, fst $1, snd $1) }

operator_strips_cont:
| operator_strips_cont_prec formula operator_strips_cont_eff formula { ($2, $4) }
| operator_strips_cont_eff formula { (Formula.Top, $2) }

operator_strips_cont_prec:
| PREC { current_time_bound := (true,false); ti_code := 0; set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 1.0)) }

operator_strips_cont_eff:
| EFFECT { current_time_bound := (true,true); ti_code := 0; set_current_time_set ((FunctionFormula.Number 1.0),(FunctionFormula.Number 1.0)) }

duration:
| DURATION function_formula { set_action_relative_end_time $2 }
| DURATION LP EQUAL VAR function_formula RP { set_action_relative_end_time $5 }

function_formula:
| number { FunctionFormula.Number $1 }
| LP ADD function_formula function_formula RP { FunctionFormula.Add ($3,$4) }
| LP TYPE function_formula function_formula RP { FunctionFormula.Sub ($3,$4) }
| LP MULTIPLY function_formula function_formula RP { FunctionFormula.Multiply ($3,$4) }
| LP DIVIDE function_formula function_formula RP { FunctionFormula.Divide ($3,$4) }
| LP IDENT typed_variable_list { FunctionFormula.Funct (create_preatom $2 $3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) }
| LP IDENT variable_list { FunctionFormula.Funct (create_preatom $2 $3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) }


quality:
| QUALITY INTEGER { $2 }

problem:
| LP DEFINE LP PROBLEM IDENT RP
      LP PDOMAIN IDENT RP
      LP OBJECTS typed_constant_list
      init_definition
      goal_definition { create_problem $5 $9 (!constants_in_domain @ $13) $14 $15 }
| LP DEFINE LP PROBLEM IDENT RP
      LP PDOMAIN IDENT RP
      LP OBJECTS constant_list
      init_definition
      goal_definition { create_problem $5 $9 (!constants_in_domain @ $13) $14 $15 }
| LP DEFINE LP PROBLEM IDENT RP
    LP PDOMAIN IDENT RP
      init_definition
      goal_definition { create_problem $5 $9 [] $11 $12 }

init_definition:
| LP INIT atom_list { $3 }
| LP INIT mixed_atom_list { $3 }

goal_definition:
| LP GOAL formula RP metric { $3 }
;| LP GOAL timed_formula RP metric { $3 }

metric:
| RP { }
| LP METRIC MINIMIZE LP TOTALTIME RP RP { }

formula:
| LP AND formula_list { Formula.Conjunct (Array.of_list $3) }
| LP AND LP time_set timed_formula_list RP { Formula.Conjunct (Array.of_list $5) }
| LP time_set LP NOT LP atom RP RP { Formula.NegLit $6 }
| LP NOT LP atom RP { Formula.NegLit $4 }
| LP time_set LP atom RP { Formula.PosLit $4 }
| LP atom { Formula.PosLit $2 }
| LP WHEN LP conditional_effect { Formula.When $4 }
| LP FORALL LP typed_variable_list formula { Formula.Forall ($4,$5) }

conditional_effect:
| atom LP atom RP { (Formula.PosLit $1, Formula.PosLit $3) }
| atom LP NOT LP atom RP RP { (Formula.PosLit $1, Formula.NegLit $5) }
| NOT LP atom RP LP atom RP { (Formula.NegLit $3, Formula.PosLit $6) }
| NOT LP atom RP LP NOT LP atom RP RP { (Formula.NegLit $3, Formula.NegLit $8) }
| time_set LP atom RP LP time_set LP atom RP RP { (Formula.PosLit $3, Formula.PosLit $8) }
| time_set LP atom RP LP time_set LP NOT LP atom RP RP RP { (Formula.PosLit $3, Formula.NegLit $10) }
| time_set LP NOT LP atom RP RP LP time_set LP atom RP RP { (Formula.NegLit $5, Formula.PosLit $11) }
| time_set LP NOT LP atom RP RP LP time_set LP NOT LP atom RP RP RP { (Formula.NegLit $5, Formula.NegLit $13) }


timed_formula:
| LP AND timed_formula_list { Formula.Conjunct (Array.of_list $3) }
| LP NOT LP atom RP { Formula.NegLit $4 }
| LP atom { Formula.PosLit $2 }


formula_list:
| RP { [] }
| formula formula_list { $1 :: $2 }


timed_formula_list:
| RP { [] }
| timed_formula timed_formula_list { $1 :: $2 }


atom_list:
| RP { [] }
| LP atom atom_list { $2 :: $3 }


timed_atom_list:
| RP { [] }
| timed_atom timed_atom_list { $1 :: $2 }


mixed_atom_list:
| RP { [] }
| LP atom mixed_atom_list { $2 :: $3 }
| timed_atom mixed_atom_list { $1 :: $2 }
| LP EQUAL LP atom number RP mixed_atom_list { functions_value_list := ($4,$5) :: !functions_value_list; $7 }


atom:
| IDENT term_list  { create_atom $1 $2 !current_time_set }
| EQUAL term_list  { create_atom "=" $2 !current_time_set }
| AT term_list { create_atom "at" $2 !current_time_set }
| INF EQUAL LP funct INTEGER RP { create_atom (fst $4) (snd $4) !current_time_set }
| SUP EQUAL LP funct INTEGER RP { create_atom (fst $4) (snd $4) !current_time_set }
| INF LP funct INTEGER RP { create_atom (fst $3) (snd $3) !current_time_set }
| INF LP funct INTEGER RP { create_atom (fst $3) (snd $3) !current_time_set }
| ASSIGN LP funct INTEGER RP { create_atom (fst $3) (snd $3) !current_time_set }
| INCREASE LP funct INTEGER RP { create_atom (fst $3) (snd $3) !current_time_set }
| DECREASE LP funct INTEGER RP { create_atom (fst $3) (snd $3) !current_time_set }


timed_atom:
| LP time_set LP atom RP { $4 }
| LP time_set produced_timed_atom RP { $3 }
| LP time_set consumed_timed_atom RP { $3 }


produced_timed_atom:
| LP PRODUCE LP atom RP { ti_code := 10; $4 }

consumed_timed_atom:
| LP CONSUME LP atom RP { ti_code := 10; $4 }

time_set:
| AT timepoint { current_time_bound := (true,true); ti_code := 0; set_current_time_set ($2,$2) }
| BEFORE timepoint { current_time_bound := (true,false); ti_code := 0; set_current_time_set ($2,$2) }
| AFTER timepoint { current_time_bound := (false,true); ti_code := 0; set_current_time_set ($2,$2) }
| OVER time_interval { ti_code := 0; set_current_time_set $2 }
| SOMEWHERE time_interval { ti_code := 1; set_current_time_set $2 }
| ANYWHERE time_interval { ti_code := 2; ti_min_dur := 0.0; set_current_time_set $2 }
| MIN_DUR number ANYWHERE time_interval { ti_code := 2; ti_min_dur := $2; set_current_time_set $4 }
| TYPE SUP OVER time_interval { ti_code := 10; set_current_time_set $4 }

timepoint:
| START { (FunctionFormula.Number 0.0) }
| END { !action_relative_end_time }
| number { (FunctionFormula.Number $1) }
| LP ADD timepoint timepoint RP { FunctionFormula.Add ($3,$4) }
| LP TYPE timepoint timepoint RP { FunctionFormula.Sub ($3,$4) }

time_interval:
| ALL { current_time_bound := (true,false); ((FunctionFormula.Number 0.0),!action_relative_end_time) }
| LH timepoint timepoint RH { current_time_bound := (true,true); ($2,$3) }
| LH timepoint timepoint LH { current_time_bound := (true,false); ($2,$3) }
| RH timepoint timepoint RH { current_time_bound := (false,true); ($2,$3) }
| RH timepoint timepoint LH { current_time_bound := (false,false); ($2,$3) }


types:
| types_aux2 { }
| types_aux IDENT types {
    let ptype = symb_set#create_predicate $2 in
      List.iter (fun p -> p#add_builtin_type ptype) $1 }

types_aux:
| TYPE { [] }
| IDENT types_aux { symb_set#create_predicate $1 :: $2 }

types_aux2:
| RP { }
| IDENT types_aux2 { }


typed_constant_list:
| typed_constant_list_aux IDENT RP  { create_typed_term_list $1 $2 }
| typed_constant_list_aux IDENT typed_constant_list  { create_typed_term_list $1 $2 @ $3 }

typed_constant_list_aux:
| TYPE { [] }
| constant typed_constant_list_aux { $1 :: $2 }


typed_variable_list:
| typed_variable_list_aux IDENT RP { create_typed_term_list $1 $2 }
| typed_variable_list_aux IDENT typed_variable_list { create_typed_term_list $1 $2 @ $3 }

typed_variable_list_aux:
| TYPE { [] }
| variable typed_variable_list_aux { $1 :: $2 }


functions_list:
| RP { [] }
| LP funct functions_list { $2 :: $3 }


funct:
| IDENT typed_variable_list { create_function $1 $2 }
| IDENT variable_list { create_function $1 $2 }


instanciated_funct:
| IDENT typed_constant_list { create_function $1 $2 }
| IDENT constant_list { create_function $1 $2 }


term_list:
| RP { [] }
| term term_list { $1 :: $2 }

variable_list:
| RP { [] }
| variable variable_list { $1 :: $2 }

constant_list:
| RP { [] }
| constant constant_list { $1 :: $2 }

term:
| VAR { symb_set#create_variable $1 }
| IDENT { symb_set#create_constant $1 }
| INTEGER { symb_set#create_constant (string_of_int $1) }

variable:
| VAR { symb_set#create_variable $1 }

constant:
| IDENT { symb_set#create_constant $1 }
| INTEGER { symb_set#create_constant (string_of_int $1) }

number:
| INTEGER { float_of_int $1 }
| RATIONAL { $1 }
