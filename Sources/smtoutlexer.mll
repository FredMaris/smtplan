{
  open Smtoutparser
  let lev_paren = ref 0
  let is_effect = ref false
  let line = ref 1
}

let a = ['a' 'A']
let b = ['b' 'B']
let c = ['c' 'C']
let d = ['d' 'D']
let e = ['e' 'E']
let f = ['f' 'F']
let g = ['g' 'G']
let h = ['h' 'H']
let i = ['i' 'I']
let j = ['j' 'J']
let k = ['k' 'K']
let l = ['l' 'L']
let m = ['m' 'M']
let n = ['n' 'N']
let o = ['o' 'O']
let p = ['p' 'P']
let q = ['q' 'Q']
let r = ['r' 'R']
let s = ['s' 'S']
let t = ['t' 'T']
let u = ['u' 'U']
let v = ['v' 'V']
let w = ['w' 'W']
let x = ['x' 'X']
let y = ['y' 'Y']
let z = ['z' 'Z']
let letter = ['a'-'z' 'A'-'Z']
let special = ['-' '_']
let number = ['0'-'9']
let identifier = (special | number)* letter (letter | special | number)*
let integer = number+

rule token = parse
    | d e f i n e                   { DEFINE }

    | d o m a i n                   { DOMAIN }
    | ':' r e q u i r e m e n t s   { REQUIREMENTS }
    | ':' t y p e s                 { TYPES }
    | ':' c o n s t a n t s           { CONSTANTS }
    | ':' p r e d i c a t e s       { incr lev_paren; skip_paren lexbuf }
    | ':' f u n c t i o n s         { incr lev_paren; skip_paren lexbuf }
    | ':' a c t i o n               { ACTION }
    | ':' d u r a t i v e '-' a c t i o n  { DURATIVE_ACTION }
    | ':' p a r a m e t e r s       { PARAM }
    | ':' d u r a t i o n           { DURATION }
    | ':' p r e c o n d i t i o n   { is_effect := false ; PREC }
    | ':' c o n d i t i o n         { is_effect := false ; PREC }
    | ':' e f f e c t               { is_effect := true ; EFFECT }
    | ':' q u a l i t y             { QUALITY }

    | p r o b l e m                 { is_effect := false ; PROBLEM }
    | ':' d o m a i n               { PDOMAIN }
    | ':' o b j e c t s             { OBJECTS }
    | ':' i n i t                   { INIT }
    | ':' g o a l                   { GOAL }
    | ':' m e t r i c               { METRIC }

    | m i n i m i z e               { MINIMIZE }
    | t o t a l '-' t i m e         { TOTALTIME }

    | a t                           { AT }
    | b e f o r e                   { BEFORE }
    | a f t e r                     { AFTER }
    | s t a r t                     { START }
    | e n d                         { END }
    | o v e r                       { OVER }
    | a l l                         { ALL }
    | s o m e w h e r e             { SOMEWHERE }
    | a n y w h e r e               { ANYWHERE }
    | m i n i m a l '-' d u r a t i o n { MIN_DUR }

    | a s s i g n                   { ASSIGN }
    | i n c r e a s e               { INCREASE }
    | d e c r e a s e               { DECREASE }
    | c o n s u m e                 { CONSUME }
    | p r o d u c e                 { PRODUCE }
    
    | f o r a l l                   { FORALL }
    | w h e n                       { WHEN }

    | n o t                         { NOT }
    | a n d                         { AND }
    | '?' identifier                { VAR (String.lowercase (Lexing.lexeme lexbuf)) }
    | ':' identifier                { REQUIREMENT (String.lowercase (Lexing.lexeme lexbuf)) }
    | identifier                    { IDENT (String.lowercase (Lexing.lexeme lexbuf)) }
    | integer                       { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
    | integer '.' integer           { RATIONAL (float_of_string (Lexing.lexeme lexbuf)) }
    | '-'                           { TYPE }

    | [' ' '\t']                    { token lexbuf }
    | '\n'                          { incr line ; token lexbuf }
    | ';'                           { comments_token lexbuf }
    | '('                           { LP }
    | ')'                           { RP }
    | '='                           { EQUAL }
    | '+'                           { ADD }
    | '*'                           { MULTIPLY }
    | '/'                           { DIVIDE }
    | '['                           { LH }
    | ']'                           { RH }
    | '<'                           { INF }
    | '>'                           { SUP }
    | _                             { token lexbuf (* skip unexpected token *) }

and comments_token = parse 
    '\n'                            { incr line ; token lexbuf }
  | _                               { comments_token lexbuf }

and skip_paren = parse
    '('                             { if !lev_paren = 0 
                                      then token lexbuf
                                      else begin incr lev_paren ; skip_paren lexbuf end }
  | ')'                             { decr lev_paren ; skip_paren lexbuf }
  | ';'                             { comments_paren lexbuf }
  | '\n'                            { incr line ; skip_paren lexbuf }
  | _                               { skip_paren lexbuf }

and comments_paren = parse 
    '\n'                            { incr line ; skip_paren lexbuf }
  | _                               { comments_paren lexbuf }

