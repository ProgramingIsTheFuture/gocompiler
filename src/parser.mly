%{
  open Ast

  let typ_of_str t = if t = "int" then TInt else failwith (Format.sprintf "Invalid type %s" t)
%}

%token LET FUNC
%token RETURN
%token <string> NAME
%token <int> VALUE
%token ADD EQUAL
%token LBRACE RBRACE
%token LPARENT RPARENT
%token COMMA
%token EOF

%start code

%type <stmt list> code

%%

param:
  | n = NAME t = NAME { n, (typ_of_str t) }

params:
  | LPARENT RPARENT { [] }
  | LPARENT p = separated_nonempty_list(COMMA, param) RPARENT 
    { p }

op:
  | v1 = expr ADD v2 = expr { Op (v1, "+", v2) }

apply:
  | n = NAME LPARENT e = separated_nonempty_list(COMMA, expr) RPARENT
    { 
      Apply (n, e)
    } 

expr:
  | LPARENT e = expr RPARENT { e }
  | v = VALUE { Const v }
  | n = NAME { Var n }
  | o = op { o }
  | a = apply { a }

stmt:
  | RETURN e = expr { Return e }
  | LET n = NAME EQUAL e = expr { Let (n, None, e) }
  | LET n = NAME t = NAME EQUAL e = expr 
    {
      let t = typ_of_str t in
      Let (n, Some t, e)
    }
  | FUNC n = NAME p = params t = NAME? LBRACE s = list(stmt) RBRACE 
    { 
      let t = match t with | Some t -> typ_of_str t | None -> TVoid in
      Func (n, p, t, s) 
    }
  | e = expr {Let ("_", None, e)}

code: s = list(stmt) EOF { s } 

