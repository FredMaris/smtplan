type token =
  | DEFINE
  | DOMAIN
  | REQUIREMENTS
  | CONSTANTS
  | TYPES
  | FUNCTIONS
  | ACTION
  | DURATIVE_ACTION
  | PARAM
  | DURATION
  | AT
  | BEFORE
  | AFTER
  | START
  | END
  | OVER
  | ALL
  | SOMEWHERE
  | ANYWHERE
  | MIN_DUR
  | ASSIGN
  | INCREASE
  | DECREASE
  | CONSUME
  | PRODUCE
  | QUALITY
  | PREC
  | EFFECT
  | NOT
  | AND
  | TYPE
  | LP
  | RP
  | EQUAL
  | ADD
  | MULTIPLY
  | DIVIDE
  | LH
  | RH
  | INF
  | SUP
  | PROBLEM
  | PDOMAIN
  | OBJECTS
  | INIT
  | GOAL
  | METRIC
  | MINIMIZE
  | TOTALTIME
  | FORALL
  | WHEN
  | VAR of (string)
  | IDENT of (string)
  | REQUIREMENT of (string)
  | INTEGER of (int)
  | RATIONAL of (float)

open Parsing;;
let _ = parse_error;;
# 2 "Sources/smtoutparser.mly"

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

# 141 "Sources/smtoutparser.ml"
let yytransl_const = [|
  257 (* DEFINE *);
  258 (* DOMAIN *);
  259 (* REQUIREMENTS *);
  260 (* CONSTANTS *);
  261 (* TYPES *);
  262 (* FUNCTIONS *);
  263 (* ACTION *);
  264 (* DURATIVE_ACTION *);
  265 (* PARAM *);
  266 (* DURATION *);
  267 (* AT *);
  268 (* BEFORE *);
  269 (* AFTER *);
  270 (* START *);
  271 (* END *);
  272 (* OVER *);
  273 (* ALL *);
  274 (* SOMEWHERE *);
  275 (* ANYWHERE *);
  276 (* MIN_DUR *);
  277 (* ASSIGN *);
  278 (* INCREASE *);
  279 (* DECREASE *);
  280 (* CONSUME *);
  281 (* PRODUCE *);
  282 (* QUALITY *);
  283 (* PREC *);
  284 (* EFFECT *);
  285 (* NOT *);
  286 (* AND *);
  287 (* TYPE *);
  288 (* LP *);
  289 (* RP *);
  290 (* EQUAL *);
  291 (* ADD *);
  292 (* MULTIPLY *);
  293 (* DIVIDE *);
  294 (* LH *);
  295 (* RH *);
  296 (* INF *);
  297 (* SUP *);
  298 (* PROBLEM *);
  299 (* PDOMAIN *);
  300 (* OBJECTS *);
  301 (* INIT *);
  302 (* GOAL *);
  303 (* METRIC *);
  304 (* MINIMIZE *);
  305 (* TOTALTIME *);
  306 (* FORALL *);
  307 (* WHEN *);
    0|]

let yytransl_block = [|
  308 (* VAR *);
  309 (* IDENT *);
  310 (* REQUIREMENT *);
  311 (* INTEGER *);
  312 (* RATIONAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\009\000\009\000\009\000\011\000\011\000\011\000\013\000\013\000\
\013\000\013\000\017\000\017\000\018\000\020\000\015\000\015\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\016\000\
\022\000\022\000\022\000\023\000\023\000\024\000\024\000\028\000\
\028\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\029\000\029\000\029\000\030\000\030\000\032\000\032\000\
\026\000\026\000\035\000\035\000\027\000\027\000\027\000\027\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\036\000\036\000\036\000\039\000\040\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\041\000\
\041\000\041\000\041\000\041\000\042\000\042\000\042\000\042\000\
\042\000\007\000\007\000\044\000\044\000\043\000\043\000\008\000\
\008\000\045\000\045\000\012\000\012\000\047\000\047\000\010\000\
\010\000\038\000\038\000\049\000\049\000\037\000\037\000\014\000\
\014\000\025\000\025\000\050\000\050\000\050\000\048\000\046\000\
\046\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\004\000\007\000\008\000\011\000\016\000\
\019\000\013\000\016\000\010\000\010\000\007\000\001\000\002\000\
\001\000\004\000\004\000\006\000\006\000\003\000\003\000\002\000\
\002\000\001\000\004\000\002\000\001\000\001\000\002\000\006\000\
\001\000\005\000\005\000\005\000\005\000\003\000\003\000\002\000\
\015\000\015\000\012\000\003\000\003\000\005\000\005\000\001\000\
\007\000\003\000\006\000\008\000\005\000\005\000\002\000\004\000\
\005\000\004\000\007\000\007\000\010\000\010\000\013\000\013\000\
\016\000\003\000\005\000\002\000\001\000\002\000\001\000\002\000\
\001\000\003\000\001\000\002\000\001\000\003\000\002\000\007\000\
\002\000\002\000\002\000\006\000\006\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\005\000\005\000\002\000\
\002\000\002\000\002\000\002\000\002\000\004\000\004\000\001\000\
\001\000\001\000\005\000\005\000\001\000\004\000\004\000\004\000\
\004\000\001\000\003\000\001\000\002\000\001\000\002\000\003\000\
\003\000\001\000\002\000\003\000\003\000\001\000\002\000\001\000\
\003\000\002\000\002\000\002\000\002\000\001\000\002\000\001\000\
\002\000\001\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\148\000\000\000\002\000\001\000\000\000\
\000\000\003\000\000\000\146\000\147\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\004\000\007\000\008\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yysindex = "\008\000\
\225\254\000\000\236\254\000\000\217\254\000\000\000\000\236\254\
\224\254\000\000\230\254\000\000\000\000\239\254\207\254\226\254\
\000\000\240\254\243\254\207\254\242\254\207\254\244\254\000\000\
\245\254\250\254\207\254\000\000\251\254\252\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\012\000\000\000\244\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yytablesize = 29
let yytable = "\011\000\
\003\000\019\000\018\000\020\000\015\000\012\000\013\000\023\000\
\001\000\025\000\016\000\005\000\006\000\009\000\029\000\017\000\
\021\000\022\000\024\000\010\000\026\000\027\000\012\000\013\000\
\012\000\013\000\028\000\030\000\031\000"

let yycheck = "\032\001\
\032\001\032\001\015\000\016\000\031\001\055\001\056\001\020\000\
\001\000\022\000\037\001\032\001\033\001\053\001\027\000\033\001\
\033\001\031\001\033\001\008\000\033\001\033\001\055\001\056\001\
\055\001\056\001\033\001\033\001\033\001"

let yynames_const = "\
  DEFINE\000\
  DOMAIN\000\
  REQUIREMENTS\000\
  CONSTANTS\000\
  TYPES\000\
  FUNCTIONS\000\
  ACTION\000\
  DURATIVE_ACTION\000\
  PARAM\000\
  DURATION\000\
  AT\000\
  BEFORE\000\
  AFTER\000\
  START\000\
  END\000\
  OVER\000\
  ALL\000\
  SOMEWHERE\000\
  ANYWHERE\000\
  MIN_DUR\000\
  ASSIGN\000\
  INCREASE\000\
  DECREASE\000\
  CONSUME\000\
  PRODUCE\000\
  QUALITY\000\
  PREC\000\
  EFFECT\000\
  NOT\000\
  AND\000\
  TYPE\000\
  LP\000\
  RP\000\
  EQUAL\000\
  ADD\000\
  MULTIPLY\000\
  DIVIDE\000\
  LH\000\
  RH\000\
  INF\000\
  SUP\000\
  PROBLEM\000\
  PDOMAIN\000\
  OBJECTS\000\
  INIT\000\
  GOAL\000\
  METRIC\000\
  MINIMIZE\000\
  TOTALTIME\000\
  FORALL\000\
  WHEN\000\
  "

let yynames_block = "\
  VAR\000\
  IDENT\000\
  REQUIREMENT\000\
  INTEGER\000\
  RATIONAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_var_values) in
    Obj.repr(
# 95 "Sources/smtoutparser.mly"
                        ( _2 )
# 364 "Sources/smtoutparser.ml"
               : (string * float) list))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "Sources/smtoutparser.mly"
     ( [] )
# 370 "Sources/smtoutparser.ml"
               : 'list_of_var_values))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_value) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_var_values) in
    Obj.repr(
# 99 "Sources/smtoutparser.mly"
                               ( _1 :: _2 )
# 378 "Sources/smtoutparser.ml"
               : 'list_of_var_values))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'number) in
    Obj.repr(
# 102 "Sources/smtoutparser.mly"
                     ( (_2,_3))
# 386 "Sources/smtoutparser.ml"
               : 'var_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    Obj.repr(
# 103 "Sources/smtoutparser.mly"
                                ( (_2, -._5))
# 394 "Sources/smtoutparser.ml"
               : 'var_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'number) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    Obj.repr(
# 104 "Sources/smtoutparser.mly"
                                         ( (_2,_5 /. _6) )
# 403 "Sources/smtoutparser.ml"
               : 'var_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'number) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    Obj.repr(
# 105 "Sources/smtoutparser.mly"
                                                    ( (_2,-._7 /. _9) )
# 412 "Sources/smtoutparser.ml"
               : 'var_value))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 4 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : 'typed_constant_list) in
    let _16 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 109 "Sources/smtoutparser.mly"
                                                                                                                          ( constants_in_domain := _15; create_domain _5 _9 _16 )
# 423 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 14 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 10 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 7 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 4 : 'typed_constant_list) in
    let _18 = (Parsing.peek_val __caml_parser_env 1 : 'functions_list) in
    let _19 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 110 "Sources/smtoutparser.mly"
                                                                                                                                                        ( constants_in_domain := _15; create_domain _5 _9 _19 )
# 435 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    let _13 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 112 "Sources/smtoutparser.mly"
                                                                                         ( create_domain _5 _9 _13 )
# 445 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 4 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : 'functions_list) in
    let _16 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 113 "Sources/smtoutparser.mly"
                                                                                                                       ( create_domain _5 _9 _16 )
# 456 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'requirements) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 115 "Sources/smtoutparser.mly"
                                                                          ( create_domain _5 _9 _10 )
# 465 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 116 "Sources/smtoutparser.mly"
                                                            ( create_domain _5 [] _10 )
# 474 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 117 "Sources/smtoutparser.mly"
                                             ( create_domain _5 [] _7 )
# 482 "Sources/smtoutparser.ml"
               : 'domain))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "Sources/smtoutparser.mly"
     ( [] )
# 488 "Sources/smtoutparser.ml"
               : 'requirements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'requirements) in
    Obj.repr(
# 121 "Sources/smtoutparser.mly"
                           ( _1 :: _2 )
# 496 "Sources/smtoutparser.ml"
               : 'requirements))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "Sources/smtoutparser.mly"
     ( [] )
# 502 "Sources/smtoutparser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 125 "Sources/smtoutparser.mly"
                                   ( _3 :: _4 )
# 510 "Sources/smtoutparser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 126 "Sources/smtoutparser.mly"
                                            ( _3 :: _4 )
# 518 "Sources/smtoutparser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 130 "Sources/smtoutparser.mly"
                                                        ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 _4 _5 )
# 527 "Sources/smtoutparser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 131 "Sources/smtoutparser.mly"
                                                  ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 _4 _5 )
# 536 "Sources/smtoutparser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 132 "Sources/smtoutparser.mly"
                           ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 [] _2 )
# 544 "Sources/smtoutparser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'quality) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 135 "Sources/smtoutparser.mly"
                                        ( (_1, _2, fst _3, snd _3) )
# 553 "Sources/smtoutparser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'duration) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 136 "Sources/smtoutparser.mly"
                                ( (_1, 0, fst _2, snd _2) )
# 561 "Sources/smtoutparser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'quality) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 137 "Sources/smtoutparser.mly"
                               ( ((FunctionFormula.Number 1.0), _1, fst _2, snd _2) )
# 569 "Sources/smtoutparser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 138 "Sources/smtoutparser.mly"
                       ( ((FunctionFormula.Number 1.0), 0, fst _1, snd _1) )
# 576 "Sources/smtoutparser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'operator_strips_cont_prec) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips_cont_eff) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 141 "Sources/smtoutparser.mly"
                                                                     ( (_2, _4) )
# 586 "Sources/smtoutparser.ml"
               : 'operator_strips_cont))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips_cont_eff) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 142 "Sources/smtoutparser.mly"
                                   ( (Formula.Top, _2) )
# 594 "Sources/smtoutparser.ml"
               : 'operator_strips_cont))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "Sources/smtoutparser.mly"
       ( current_time_bound := (true,false); ti_code := 0; set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 1.0)) )
# 600 "Sources/smtoutparser.ml"
               : 'operator_strips_cont_prec))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "Sources/smtoutparser.mly"
         ( current_time_bound := (true,true); ti_code := 0; set_current_time_set ((FunctionFormula.Number 1.0),(FunctionFormula.Number 1.0)) )
# 606 "Sources/smtoutparser.ml"
               : 'operator_strips_cont_eff))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'function_formula) in
    Obj.repr(
# 151 "Sources/smtoutparser.mly"
                            ( set_action_relative_end_time _2 )
# 613 "Sources/smtoutparser.ml"
               : 'duration))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 152 "Sources/smtoutparser.mly"
                                            ( set_action_relative_end_time _5 )
# 621 "Sources/smtoutparser.ml"
               : 'duration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 155 "Sources/smtoutparser.mly"
         ( FunctionFormula.Number _1 )
# 628 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 156 "Sources/smtoutparser.mly"
                                              ( FunctionFormula.Add (_3,_4) )
# 636 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 157 "Sources/smtoutparser.mly"
                                               ( FunctionFormula.Sub (_3,_4) )
# 644 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 158 "Sources/smtoutparser.mly"
                                                   ( FunctionFormula.Multiply (_3,_4) )
# 652 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 159 "Sources/smtoutparser.mly"
                                                 ( FunctionFormula.Divide (_3,_4) )
# 660 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 160 "Sources/smtoutparser.mly"
                               ( FunctionFormula.Funct (create_preatom _2 _3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) )
# 668 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 161 "Sources/smtoutparser.mly"
                         ( FunctionFormula.Funct (create_preatom _2 _3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) )
# 676 "Sources/smtoutparser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 165 "Sources/smtoutparser.mly"
                  ( _2 )
# 683 "Sources/smtoutparser.ml"
               : 'quality))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _15 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 172 "Sources/smtoutparser.mly"
                      ( create_problem _5 _9 (!constants_in_domain @ _13) _14 _15 )
# 694 "Sources/smtoutparser.ml"
               : 'problem))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 2 : 'constant_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _15 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 177 "Sources/smtoutparser.mly"
                      ( create_problem _5 _9 (!constants_in_domain @ _13) _14 _15 )
# 705 "Sources/smtoutparser.ml"
               : 'problem))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 181 "Sources/smtoutparser.mly"
                      ( create_problem _5 _9 [] _11 _12 )
# 715 "Sources/smtoutparser.ml"
               : 'problem))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom_list) in
    Obj.repr(
# 184 "Sources/smtoutparser.mly"
                    ( _3 )
# 722 "Sources/smtoutparser.ml"
               : 'init_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 185 "Sources/smtoutparser.mly"
                          ( _3 )
# 729 "Sources/smtoutparser.ml"
               : 'init_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'metric) in
    Obj.repr(
# 188 "Sources/smtoutparser.mly"
                            ( _3 )
# 737 "Sources/smtoutparser.ml"
               : 'goal_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timed_formula) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'metric) in
    Obj.repr(
# 189 "Sources/smtoutparser.mly"
                                   ( _3 )
# 745 "Sources/smtoutparser.ml"
               : 'goal_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "Sources/smtoutparser.mly"
     ( )
# 751 "Sources/smtoutparser.ml"
               : 'metric))
; (fun __caml_parser_env ->
    Obj.repr(
# 193 "Sources/smtoutparser.mly"
                                        ( )
# 757 "Sources/smtoutparser.ml"
               : 'metric))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula_list) in
    Obj.repr(
# 196 "Sources/smtoutparser.mly"
                      ( Formula.Conjunct (Array.of_list _3) )
# 764 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'timed_formula_list) in
    Obj.repr(
# 197 "Sources/smtoutparser.mly"
                                           ( Formula.Conjunct (Array.of_list _5) )
# 772 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'time_set) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 198 "Sources/smtoutparser.mly"
                                   ( Formula.NegLit _6 )
# 780 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 199 "Sources/smtoutparser.mly"
                    ( Formula.NegLit _4 )
# 787 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'time_set) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 200 "Sources/smtoutparser.mly"
                         ( Formula.PosLit _4 )
# 795 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 201 "Sources/smtoutparser.mly"
          ( Formula.PosLit _2 )
# 802 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'conditional_effect) in
    Obj.repr(
# 202 "Sources/smtoutparser.mly"
                                ( Formula.When _4 )
# 809 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typed_variable_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 203 "Sources/smtoutparser.mly"
                                           ( Formula.Forall (_4,_5) )
# 817 "Sources/smtoutparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 206 "Sources/smtoutparser.mly"
                  ( (Formula.PosLit _1, Formula.PosLit _3) )
# 825 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 207 "Sources/smtoutparser.mly"
                            ( (Formula.PosLit _1, Formula.NegLit _5) )
# 833 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'atom) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 208 "Sources/smtoutparser.mly"
                            ( (Formula.NegLit _3, Formula.PosLit _6) )
# 841 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'atom) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 209 "Sources/smtoutparser.mly"
                                      ( (Formula.NegLit _3, Formula.NegLit _8) )
# 849 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'atom) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'time_set) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 210 "Sources/smtoutparser.mly"
                                                ( (Formula.PosLit _3, Formula.PosLit _8) )
# 859 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 12 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 10 : 'atom) in
    let _6 = (Parsing.peek_val __caml_parser_env 7 : 'time_set) in
    let _10 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    Obj.repr(
# 211 "Sources/smtoutparser.mly"
                                                          ( (Formula.PosLit _3, Formula.NegLit _10) )
# 869 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 12 : 'time_set) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'atom) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'time_set) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 212 "Sources/smtoutparser.mly"
                                                          ( (Formula.NegLit _5, Formula.PosLit _11) )
# 879 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 15 : 'time_set) in
    let _5 = (Parsing.peek_val __caml_parser_env 11 : 'atom) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : 'time_set) in
    let _13 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    Obj.repr(
# 213 "Sources/smtoutparser.mly"
                                                                    ( (Formula.NegLit _5, Formula.NegLit _13) )
# 889 "Sources/smtoutparser.ml"
               : 'conditional_effect))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timed_formula_list) in
    Obj.repr(
# 217 "Sources/smtoutparser.mly"
                            ( Formula.Conjunct (Array.of_list _3) )
# 896 "Sources/smtoutparser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 218 "Sources/smtoutparser.mly"
                    ( Formula.NegLit _4 )
# 903 "Sources/smtoutparser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 219 "Sources/smtoutparser.mly"
          ( Formula.PosLit _2 )
# 910 "Sources/smtoutparser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 223 "Sources/smtoutparser.mly"
     ( [] )
# 916 "Sources/smtoutparser.ml"
               : 'formula_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula_list) in
    Obj.repr(
# 224 "Sources/smtoutparser.mly"
                       ( _1 :: _2 )
# 924 "Sources/smtoutparser.ml"
               : 'formula_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 228 "Sources/smtoutparser.mly"
     ( [] )
# 930 "Sources/smtoutparser.ml"
               : 'timed_formula_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timed_formula_list) in
    Obj.repr(
# 229 "Sources/smtoutparser.mly"
                                   ( _1 :: _2 )
# 938 "Sources/smtoutparser.ml"
               : 'timed_formula_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 233 "Sources/smtoutparser.mly"
     ( [] )
# 944 "Sources/smtoutparser.ml"
               : 'atom_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom_list) in
    Obj.repr(
# 234 "Sources/smtoutparser.mly"
                    ( _2 :: _3 )
# 952 "Sources/smtoutparser.ml"
               : 'atom_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 238 "Sources/smtoutparser.mly"
     ( [] )
# 958 "Sources/smtoutparser.ml"
               : 'timed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timed_atom_list) in
    Obj.repr(
# 239 "Sources/smtoutparser.mly"
                             ( _1 :: _2 )
# 966 "Sources/smtoutparser.ml"
               : 'timed_atom_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 243 "Sources/smtoutparser.mly"
     ( [] )
# 972 "Sources/smtoutparser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 244 "Sources/smtoutparser.mly"
                          ( _2 :: _3 )
# 980 "Sources/smtoutparser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 245 "Sources/smtoutparser.mly"
                             ( _1 :: _2 )
# 988 "Sources/smtoutparser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 246 "Sources/smtoutparser.mly"
                                             ( functions_value_list := (_4,_5) :: !functions_value_list; _7 )
# 997 "Sources/smtoutparser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 250 "Sources/smtoutparser.mly"
                   ( create_atom _1 _2 !current_time_set )
# 1005 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 251 "Sources/smtoutparser.mly"
                   ( create_atom "=" _2 !current_time_set )
# 1012 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 252 "Sources/smtoutparser.mly"
               ( create_atom "at" _2 !current_time_set )
# 1019 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 253 "Sources/smtoutparser.mly"
                                ( create_atom (fst _4) (snd _4) !current_time_set )
# 1027 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 254 "Sources/smtoutparser.mly"
                                ( create_atom (fst _4) (snd _4) !current_time_set )
# 1035 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 255 "Sources/smtoutparser.mly"
                          ( create_atom (fst _3) (snd _3) !current_time_set )
# 1043 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 256 "Sources/smtoutparser.mly"
                          ( create_atom (fst _3) (snd _3) !current_time_set )
# 1051 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 257 "Sources/smtoutparser.mly"
                             ( create_atom (fst _3) (snd _3) !current_time_set )
# 1059 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 258 "Sources/smtoutparser.mly"
                               ( create_atom (fst _3) (snd _3) !current_time_set )
# 1067 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 259 "Sources/smtoutparser.mly"
                               ( create_atom (fst _3) (snd _3) !current_time_set )
# 1075 "Sources/smtoutparser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'time_set) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 263 "Sources/smtoutparser.mly"
                         ( _4 )
# 1083 "Sources/smtoutparser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'produced_timed_atom) in
    Obj.repr(
# 264 "Sources/smtoutparser.mly"
                                     ( _3 )
# 1091 "Sources/smtoutparser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'consumed_timed_atom) in
    Obj.repr(
# 265 "Sources/smtoutparser.mly"
                                     ( _3 )
# 1099 "Sources/smtoutparser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 269 "Sources/smtoutparser.mly"
                        ( ti_code := 10; _4 )
# 1106 "Sources/smtoutparser.ml"
               : 'produced_timed_atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 272 "Sources/smtoutparser.mly"
                        ( ti_code := 10; _4 )
# 1113 "Sources/smtoutparser.ml"
               : 'consumed_timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 275 "Sources/smtoutparser.mly"
               ( current_time_bound := (true,true); ti_code := 0; set_current_time_set (_2,_2) )
# 1120 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 276 "Sources/smtoutparser.mly"
                   ( current_time_bound := (true,false); ti_code := 0; set_current_time_set (_2,_2) )
# 1127 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 277 "Sources/smtoutparser.mly"
                  ( current_time_bound := (false,true); ti_code := 0; set_current_time_set (_2,_2) )
# 1134 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 278 "Sources/smtoutparser.mly"
                     ( ti_code := 0; set_current_time_set _2 )
# 1141 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 279 "Sources/smtoutparser.mly"
                          ( ti_code := 1; set_current_time_set _2 )
# 1148 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 280 "Sources/smtoutparser.mly"
                         ( ti_code := 2; ti_min_dur := 0.0; set_current_time_set _2 )
# 1155 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 281 "Sources/smtoutparser.mly"
                                        ( ti_code := 2; ti_min_dur := _2; set_current_time_set _4 )
# 1163 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 282 "Sources/smtoutparser.mly"
                              ( ti_code := 10; set_current_time_set _4 )
# 1170 "Sources/smtoutparser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 285 "Sources/smtoutparser.mly"
        ( (FunctionFormula.Number 0.0) )
# 1176 "Sources/smtoutparser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    Obj.repr(
# 286 "Sources/smtoutparser.mly"
      ( !action_relative_end_time )
# 1182 "Sources/smtoutparser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 287 "Sources/smtoutparser.mly"
         ( (FunctionFormula.Number _1) )
# 1189 "Sources/smtoutparser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 288 "Sources/smtoutparser.mly"
                                ( FunctionFormula.Add (_3,_4) )
# 1197 "Sources/smtoutparser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 289 "Sources/smtoutparser.mly"
                                 ( FunctionFormula.Sub (_3,_4) )
# 1205 "Sources/smtoutparser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    Obj.repr(
# 292 "Sources/smtoutparser.mly"
      ( current_time_bound := (true,false); ((FunctionFormula.Number 0.0),!action_relative_end_time) )
# 1211 "Sources/smtoutparser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 293 "Sources/smtoutparser.mly"
                            ( current_time_bound := (true,true); (_2,_3) )
# 1219 "Sources/smtoutparser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 294 "Sources/smtoutparser.mly"
                            ( current_time_bound := (true,false); (_2,_3) )
# 1227 "Sources/smtoutparser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 295 "Sources/smtoutparser.mly"
                            ( current_time_bound := (false,true); (_2,_3) )
# 1235 "Sources/smtoutparser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 296 "Sources/smtoutparser.mly"
                            ( current_time_bound := (false,false); (_2,_3) )
# 1243 "Sources/smtoutparser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux2) in
    Obj.repr(
# 300 "Sources/smtoutparser.mly"
             ( )
# 1250 "Sources/smtoutparser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'types_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 301 "Sources/smtoutparser.mly"
                        (
    let ptype = symb_set#create_predicate _2 in
      List.iter (fun p -> p#add_builtin_type ptype) _1 )
# 1261 "Sources/smtoutparser.ml"
               : 'types))
; (fun __caml_parser_env ->
    Obj.repr(
# 306 "Sources/smtoutparser.mly"
       ( [] )
# 1267 "Sources/smtoutparser.ml"
               : 'types_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux) in
    Obj.repr(
# 307 "Sources/smtoutparser.mly"
                  ( symb_set#create_predicate _1 :: _2 )
# 1275 "Sources/smtoutparser.ml"
               : 'types_aux))
; (fun __caml_parser_env ->
    Obj.repr(
# 310 "Sources/smtoutparser.mly"
     ( )
# 1281 "Sources/smtoutparser.ml"
               : 'types_aux2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux2) in
    Obj.repr(
# 311 "Sources/smtoutparser.mly"
                   ( )
# 1289 "Sources/smtoutparser.ml"
               : 'types_aux2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 315 "Sources/smtoutparser.mly"
                                    ( create_typed_term_list _1 _2 )
# 1297 "Sources/smtoutparser.ml"
               : 'typed_constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list) in
    Obj.repr(
# 316 "Sources/smtoutparser.mly"
                                                     ( create_typed_term_list _1 _2 @ _3 )
# 1306 "Sources/smtoutparser.ml"
               : 'typed_constant_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 319 "Sources/smtoutparser.mly"
       ( [] )
# 1312 "Sources/smtoutparser.ml"
               : 'typed_constant_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constant) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list_aux) in
    Obj.repr(
# 320 "Sources/smtoutparser.mly"
                                   ( _1 :: _2 )
# 1320 "Sources/smtoutparser.ml"
               : 'typed_constant_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 324 "Sources/smtoutparser.mly"
                                   ( create_typed_term_list _1 _2 )
# 1328 "Sources/smtoutparser.ml"
               : 'typed_variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 325 "Sources/smtoutparser.mly"
                                                    ( create_typed_term_list _1 _2 @ _3 )
# 1337 "Sources/smtoutparser.ml"
               : 'typed_variable_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 328 "Sources/smtoutparser.mly"
       ( [] )
# 1343 "Sources/smtoutparser.ml"
               : 'typed_variable_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list_aux) in
    Obj.repr(
# 329 "Sources/smtoutparser.mly"
                                   ( _1 :: _2 )
# 1351 "Sources/smtoutparser.ml"
               : 'typed_variable_list_aux))
; (fun __caml_parser_env ->
    Obj.repr(
# 333 "Sources/smtoutparser.mly"
     ( [] )
# 1357 "Sources/smtoutparser.ml"
               : 'functions_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'funct) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'functions_list) in
    Obj.repr(
# 334 "Sources/smtoutparser.mly"
                          ( _2 :: _3 )
# 1365 "Sources/smtoutparser.ml"
               : 'functions_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 338 "Sources/smtoutparser.mly"
                            ( create_function _1 _2 )
# 1373 "Sources/smtoutparser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 339 "Sources/smtoutparser.mly"
                      ( create_function _1 _2 )
# 1381 "Sources/smtoutparser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list) in
    Obj.repr(
# 343 "Sources/smtoutparser.mly"
                            ( create_function _1 _2 )
# 1389 "Sources/smtoutparser.ml"
               : 'instanciated_funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant_list) in
    Obj.repr(
# 344 "Sources/smtoutparser.mly"
                      ( create_function _1 _2 )
# 1397 "Sources/smtoutparser.ml"
               : 'instanciated_funct))
; (fun __caml_parser_env ->
    Obj.repr(
# 348 "Sources/smtoutparser.mly"
     ( [] )
# 1403 "Sources/smtoutparser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 349 "Sources/smtoutparser.mly"
                 ( _1 :: _2 )
# 1411 "Sources/smtoutparser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 352 "Sources/smtoutparser.mly"
     ( [] )
# 1417 "Sources/smtoutparser.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 353 "Sources/smtoutparser.mly"
                         ( _1 :: _2 )
# 1425 "Sources/smtoutparser.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 356 "Sources/smtoutparser.mly"
     ( [] )
# 1431 "Sources/smtoutparser.ml"
               : 'constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constant) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant_list) in
    Obj.repr(
# 357 "Sources/smtoutparser.mly"
                         ( _1 :: _2 )
# 1439 "Sources/smtoutparser.ml"
               : 'constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 360 "Sources/smtoutparser.mly"
      ( symb_set#create_variable _1 )
# 1446 "Sources/smtoutparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 361 "Sources/smtoutparser.mly"
        ( symb_set#create_constant _1 )
# 1453 "Sources/smtoutparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 362 "Sources/smtoutparser.mly"
          ( symb_set#create_constant (string_of_int _1) )
# 1460 "Sources/smtoutparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 365 "Sources/smtoutparser.mly"
      ( symb_set#create_variable _1 )
# 1467 "Sources/smtoutparser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 368 "Sources/smtoutparser.mly"
        ( symb_set#create_constant _1 )
# 1474 "Sources/smtoutparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 369 "Sources/smtoutparser.mly"
          ( symb_set#create_constant (string_of_int _1) )
# 1481 "Sources/smtoutparser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 372 "Sources/smtoutparser.mly"
          ( float_of_int _1 )
# 1488 "Sources/smtoutparser.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 373 "Sources/smtoutparser.mly"
           ( _1 )
# 1495 "Sources/smtoutparser.ml"
               : 'number))
(* Entry model *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let model (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * float) list)
