open Ast

let code =
  [
    Func
      ( "add",
        [ ("a", TInt); ("b", TInt) ],
        TInt,
        [ Return (Op (Var "a", "+", Var "b")) ] );
    Func
      ( "main",
        [],
        TVoid,
        [
          Let ("a", Some TInt, Const 10);
          Let ("b", None, Const 25);
          Let ("_", None, Apply ("add", [ Var "a"; Var "b" ]));
        ] );
  ]

let code_typed =
  [
    Func
      ( "add",
        [ ("a", TInt); ("b", TInt) ],
        TInt,
        [ Return (Op (Var "a", "+", Var "b")) ] );
    Func
      ( "main",
        [],
        TVoid,
        [
          Let ("a", Some TInt, Const 10);
          Let ("b", Some TInt, Const 25);
          Let ("_", Some TInt, Apply ("add", [ Var "a"; Var "b" ]));
        ] );
  ]

let _ =
  let source_code =
    "func add(a int, b int) int {\n\
    \  return a + b\n\
     }\n\n\
     func main() {\n\
    \  var a int = 10 \n\
    \  var b = 25\n\n\
    \  add(a, b)\n\
     }"
  in
  let lex = Lexing.from_string source_code in
  assert (code = Parser.code Lexer.token lex)

let () =
  let lex = Lexing.from_channel (open_in "main.go") in
  let code_from_source = Parser.code Lexer.token lex in
  assert (code = code_from_source)

let () =
  let lex = Lexing.from_channel (open_in "main.go") in
  let typed_code =
    Parser.code Lexer.token lex
    |> Typechecker.check Typechecker.ctx Typechecker.fun_ctx
  in
  assert (code_typed = typed_code)
