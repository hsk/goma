open Ast
open Utils

let trans input output =
  let inp = open_in (input) in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.stmts Lexer.token lexbuf in
  let (ast:s) = Translate.trans_s(SList(ast)) in
  let cpp =
    let buf = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buf in
    Printer.print_s formatter ast;
    Format.fprintf formatter "@?";
    Buffer.contents buf
  in
  asm_open output;
  asm(cpp);
  asm_close()

let _ =

  trans Sys.argv.(1) Sys.argv.(2)
  (*  print_exec("g++ a.cpp");
  print_exec("./a.out")
*)
