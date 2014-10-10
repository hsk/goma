%{
open Ast

let addBlock = function
  | (SBlock _ as b) -> b
  | b -> SBlock [b]

%}

%token <int> INT
%token <string> ID
%token SUB
%token ADD
%token SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE
%token PRINT
%token EOF
%token COLON COMMA
%token ASSIGN
%token RETURN
%token <string> INCLUDE
%token <string> STRING
%token STRUCT THIS DOT
%token IF ELSE
%token IMPLEMENT RIMPLEMENT TRAIT
%token ARROW MEMBER FARROW
%token LT GT LE GE
%token MUL AMP DIV
%token CAST NEW AT
%right ASSIGN
%right CAST
%left LT GT LE GE
%left ADD SUB
%left MUL DIV
%left NEW
%left prec_app
%left DOT ARROW MEMBER
%left AT
%type <Ast.s list> stmts
%start stmts

%%

typ:
| MUL typ { TPtr $2 }
| ID { Ty $1 }
| LPAREN RPAREN ARROW ID { TFun(Ty $4, []) }

typs:
| typ { [$1]}
| typ COMMA typs { $1 :: $3 }


simple_exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { EInt($1) }
| STRING
    { EString($1) }
| ID { EVar($1)}

exps:
| exp {[$1]}
| exp COMMA exps { $1 :: $3 }

exp:
| simple_exp { $1 }
| exp ASSIGN exp
    { EBin($1, "=", $3) }

| exp ADD exp
    { EBin($1, "+", $3) }
| exp SUB exp
    { EBin($1, "-", $3) }
| exp MUL exp
    { EBin($1, "*", $3) }
| exp DIV exp
    { EBin($1, "/", $3) }
| exp DOT exp
    { EBin($1, ".", $3) }
| exp MEMBER exp
    { EBin($1, "->", $3) }
| exp LT exp
    { EBin($1, "<", $3) }
| exp GT exp
    { EBin($1, ">", $3) }
| exp LE exp
    { EBin($1, "<=", $3) }
| exp GE exp
    { EBin($1, ">=", $3) }
| AMP exp { EPre("&", $2)}
| MUL exp { EPre("*", $2)}
| NEW exp { EPre("new", $2)}
| AT exp { EBin(EVar "self", "->", $2)}
| exp CAST typ { ECast($3, $1)}
| exp FARROW ID DOT ID LPAREN RPAREN
    { ECallM($3, EBin($1, "->", EVar $5), []) }

| init { $1 }

inits:
| init {[$1]}
| init COMMA inits { $1 :: $3 }

init:
| ID LPAREN RPAREN {
    ECall(EVar $1, [])
}
| ID LPAREN exps RPAREN {
    ECall(EVar $1, $3)
}
| ID LPAREN exps COMMA RPAREN {
    ECall(EVar $1, $3)
}

stmts:
| stmt {[$1]}
| stmt stmts { $1 :: $2 }

stmt:
| exp { SExp($1) }
| def { $1 }
| RETURN exp { SRet($2) }
| RETURN { SRet EEmpty }
| LBRACE RBRACE { SBlock [] }
| LBRACE stmts RBRACE { SBlock $2 }
| IF LPAREN exp RPAREN stmt { SIf($3, $5, SEmpty)}
| IF LPAREN exp RPAREN stmt ELSE stmt { SIf($3, $5, $7)}

defs:
| def {[$1]}
| def defs { $1 :: $2 }

def:
| INCLUDE { SInclude($1) }
| ID LPAREN RPAREN COLON ID ASSIGN stmt{
    SFun(Ty $5, $1, [], addBlock $7)
}
| ID LPAREN prms RPAREN COLON ID ASSIGN stmt{
    SFun(Ty $6, $1, $3, addBlock $8)
}
| ID COLON ID LPAREN exps RPAREN { SLet(Ty $3, ECall(EVar $1, $5), EEmpty) }
| ID COLON ID { SLet(Ty $3, EVar $1, EEmpty) }
| ID COLON typ ASSIGN exp { SLet($3, EVar $1, $5) }
| ID STRUCT LBRACE RBRACE { SStruct($1, "", []) }
| ID STRUCT LBRACE str_mems RBRACE { SStruct($1, "", $4) }
| ID STRUCT LPAREN RPAREN {
        SStruct($1, "",[(Ty "", SCon([],[],SBlock []))])
    }
| ID STRUCT LPAREN prms RPAREN {
    let mems = List.map begin fun (ty,id) ->
        (ty, SExp(EVar id))
    end $4 in
    let inits = List.map begin fun (ty,id) ->
        ECall(EVar id, [EVar id])
    end $4 in
    SStruct($1, "",(Ty "", SCon($4,inits,SBlock []))::mems)
}
| ID RIMPLEMENT ID LPAREN RPAREN {
        SStruct($3, $1,[(Ty "", SCon([],[],SBlock []))])
    }
| ID RIMPLEMENT ID LPAREN prms RPAREN {
    let mems = List.map begin fun (ty,id) ->
        (ty, SExp(EVar id))
    end $5 in
    let inits = List.map begin fun (ty,id) ->
        ECall(EVar id, [EVar id])
    end $5 in
    SStruct($3, $1,(Ty "", SCon($5,inits,SBlock []))::mems)
}

| ID TRAIT LBRACE trait_defs RBRACE { STrait($1, $4) }
| ID IMPLEMENT ID LBRACE defs RBRACE { SImpl($3, $1, $5) }
| ID RIMPLEMENT ID LBRACE defs RBRACE { SImpl($1, $3, $5) }

trait_defs:
| trait_def {[$1]}
| trait_def trait_defs { $1 :: $2 }

trait_def:
| ID LPAREN RPAREN COLON ID{
    TFun(Ty $5, []), SExp(EVar $1)
}
| ID LPAREN prms RPAREN COLON ID {
    TFun(Ty $6, List.map (fun (t,_)->t) $3), SExp(EVar $1)
}

prms:
| ID COLON typ { [$3, $1] }
| ID COLON typ COMMA prms { ($3, $1)::$5 }

str_mems:
| str_mem { [$1] }
| str_mem str_mems { $1::$2 }

str_mem:
| ID COLON LPAREN typs RPAREN ARROW ID { (TFun(Ty $7, $4), SExp(EVar $1)) }
| ID COLON LPAREN RPAREN ARROW ID { (TFun(Ty $6, []), SExp(EVar $1)) }
| ID COLON typ { ($3, SExp(EVar $1)) }
| THIS LPAREN prms RPAREN COLON inits ASSIGN stmt { (Ty "", SCon($3,$6, addBlock $8)) }

| error
    { failwith
      (Printf.sprintf "parse error near characters %d-%d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
