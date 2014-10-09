
let trans input output =

  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.stmts Lexer.token lexbuf in
  close_in inp;

  let ast = Translate.trans_s(Ast.SList(ast)) in

  let out = open_out output in
  Printer.print_s (Format.formatter_of_out_channel out) ast;
  close_out out

let _ =

  trans Sys.argv.(1) Sys.argv.(2)
  (*
  print Utils.exec("g++ a.cpp");
  print Utils.exec("./a.out")
  *)
