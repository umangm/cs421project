{
open Definitions;;
open TailRecPicoMLparse;;

exception EndInput

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let hex = ['0' - '9' 'a' - 'f']
let ident_char = letter | numeric | '_' | '\''
let string_char = ident_char | ' ' | '~' | '`' | '!' | '@' | '#' | '$' | '%' | '^' | '&'
  | '*' | '(' | ')' | '-' | '+' | '=' | '{' | '[' | '}' | ']'
  | '|' | ':' | ';' | '<' | ',' | '>' | '.' | '?' | '/' 

      rule token = parse
        | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
        | eof             { EOF }
          (* binary operators *)
        | "+"    { PLUS }
        | "-"    { MINUS }
        | "*"    { TIMES }
        | "/"    { DIV }
        | "+."   { DPLUS }
        | "-."   { DMINUS }
        | "*."   { DTIMES }
        | "/."   { DDIV }
        | "^"    { CARAT }
        | "::"   { DCOLON }
        | "<"    { LT }
        | ">"    { GT }
        | "="    { EQUALS }
        | ">="   { GEQ }
        | "<="   { LEQ }
        | "<>"   { NEQ }
        | "mod"  { MOD }
        | "**"   { EXP }
          (* monadic operators *)
        | "fst"          { FST }
        | "snd"          { SND }
        | "hd"           { HEAD }
        | "tl"           { TAIL }
        | "print_string" { PRINT }
        | "~"            { NEG }
          (* top-level/let-exp keywords *)
        | "let"   { LET }
        | "rec"   { REC }
        | "in"    { IN }
        | ";;"    { DSEMI }
          (* tuple/list symbols *)
        | "("     { LPAREN }
        | ")"     { RPAREN }
        | ","     { COMMA }
        | "["     { LBRAC }
        | "]"     { RBRAC }
        | ";"     { SEMI }
          (* boolean operators *)
        | "&&"    { LOGICALAND }
        | "||"    { LOGICALOR }
          (* if-then-else keywords *)
        | "if"    { IF }
        | "then"  { THEN }
        | "else"  { ELSE }
          (* function keywords *)
        | "fun"   { FUN }
        | "->"    { ARROW }
          (* exception handling keywords *)
        | "raise" { RAISE }
        | "try"   { TRY }
        | "with"  { WITH }
        | "|"     { PIPE }
        | "_"     { UNDERSCORE }
          (* named constants *)
        | "true"  { TRUE }
        | "false" { FALSE }
        | "[]"    { NIL }
        | "()"    { UNIT }
          (* numeric constants *)
        | numeric+ as s { INT (int_of_string s) }
        | numeric+'.'(numeric*) as s { FLOAT (float_of_string s) }
        | "0b"(('0'|'1')+) as s { INT (int_of_string s) }
        | "0x"(hex+) as s { INT (int_of_string s) }
        | numeric+'.'(numeric*)'e'(numeric+) as s   { FLOAT (float_of_string s) }
          (* identifiers *)
        | lowercase (ident_char*) as s { IDENT s }
          (* string literals *)
        | "\""   { string "" lexbuf }
      and string ins = parse
        | string_char* as s { string (ins ^ s) lexbuf }
        | "\""  { STRING ins }
        | "\\\\" {string (ins ^ "\\") lexbuf }
        | "\\\'" {string (ins ^ "\'") lexbuf }
        | "\\\"" {string (ins ^ "\"") lexbuf }
        | "\\t" {string (ins ^ "\t") lexbuf }
        | "\\n" {string (ins ^ "\n") lexbuf }
        | "\\r" {string (ins ^ "\r") lexbuf }
        | "\\b" {string (ins ^ "\b") lexbuf }
        | "\\\ " {string (ins ^ "\ ") lexbuf }
        | "\\"(('0'|'1')numeric numeric as s )
            {string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }
        | "\\"('2'['0' - '4']numeric as s) 
            {string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }
        | "\\"("25"['0' - '5'] as s)
            {string (ins ^ String.make 1 (char_of_int (int_of_string s))) lexbuf }

            (* your rules go here *)


            {(* do not modify this function: *)
              let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () =
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)

let get_all_token_options s =
  let b = Lexing.from_string (s^"\n") in
  let rec g () =
    match (try Some (token b) with _ -> None) with Some EOF -> []
      | None -> [None]
      | t -> t :: g () in
  g ()

 }
