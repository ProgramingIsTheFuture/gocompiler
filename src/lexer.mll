{
  open Parser
}

rule token = parse
  | ['\n' ' ' '\t'] { token lexbuf }
  | '+' { ADD }
  | '=' { EQUAL }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPARENT }
  | ')' { RPARENT }
  | ',' { COMMA }
  | "return" { RETURN }
  | "var" { LET }
  | "func" { FUNC }
  | ['0'-'9']+ as s { VALUE (int_of_string s) }
  | ['A'-'Z' 'a'-'z']+ as name
    { NAME name }
  | eof { EOF }
  | _ as c { failwith (Format.sprintf "Char %c is invalid" c) }
