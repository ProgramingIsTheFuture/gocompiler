open Ast 

let code = 
  [
    Func
      ( "add",
        [ ("a", TInt); ("b", TInt) ],
        Some TInt,
        [ Return (Op (Var "a", "+", Var "b")) ] );
    Func
      ( "main",
        [],
        None,
        [
          Let ("a", Some TInt, Const 10);
          Let ("b", None, Const 25);
          Let ("_", None, Apply ("add", [ Var "a"; Var "b" ]));
        ] );
  ];;

let _ =
  let source_code = 
"func add(a int, b int) int {
  return a + b
}

func main() {
  var a int = 10 
  var b = 25

  add(a, b)
}"
  in
  let lex = Lexing.from_string source_code in
  assert (code = (Parser.code Lexer.token lex))

let () = 
  let lex = Lexing.from_channel (open_in "main.go") in
  let code_from_source = Parser.code Lexer.token lex in
  assert (code = code_from_source)

