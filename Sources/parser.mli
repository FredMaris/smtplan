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

val domain :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Domain.domain
val problem :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Domain.problem
