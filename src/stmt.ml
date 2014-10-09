open Syntax
open Exp
open Ty

open Format

type t = 
  | SBlock of t list
  | SIf of Exp.e * t * t
  | SEmpty
  | SExp of Exp.e
  | SRet of Exp.e
  | SFun of Ty.t * string * (Ty.t * string) list * t
  | SInclude of string
  | SLet of Ty.t * Exp.e * Exp.e
  | SStruct of string * string * (Ty.t * t) list
  | SCon of (Ty.t * string) list * Exp.e list * t
  | STrait of  string * (Ty.t * t) list
  | SImpl of string * string * t list
  | SList of t list

let rec print ppf (t:t):unit = 
  let rec print sp ppf = function
  | SBlock ls ->
    fprintf ppf "{\n%a\n%s}" (print_ls "\n" (print (sp^"  "))) ls sp
  | SIf(e1,e2,SEmpty) ->
    fprintf ppf "%sif (%a)%a%s"
      sp
      (Exp.print "") e1
      (print2 sp "\n") e2
      sp
  | SIf(e1,e2,e3) ->
    fprintf ppf "%sif (%a)%a%selse%a"
      sp
      (Exp.print "" ) e1
      (print2 sp "\n") e2 
      sp
      (print2 sp "") e3 
  | SFun (t, id, ts, e2) ->
    fprintf ppf "%a %s(%a)%a"
      (Ty.print "" "") t
      id
      (print_ls ", " (fun ppf (t,a) -> fprintf ppf "%a %s" (Ty.print "" "") t a)) ts 
      (print2 sp "\n") e2
  | SStruct (id, super, ts) ->
    fprintf ppf "%sstruct %s%s{\n%s\n%a};\n"
      sp
      id
      (if super = "" then "" else ":" ^ super)
      sp
      (fun ppf ts -> List.iter (print_mem sp ppf) ts) ts
  | SList ls ->
    List.iter(fun t -> fprintf ppf "%a@." (print sp) t) ls
  | SEmpty -> ()
  | SExp e -> fprintf ppf "%a;" (Exp.print sp) e
  | SRet e -> fprintf ppf "%sreturn %a;" sp (Exp.print "") e
  | SInclude s -> fprintf ppf "#include %s" s
  | SLet (t, id, Exp.EEmpty) ->
    fprintf ppf "%s%a %a;"
      sp
      (Ty.print "" "") t
      (Exp.print "") id
  | SLet (t, id, e) ->
    fprintf ppf "%s%a %a = %a;"
      sp
      (Ty.print "" "") t
      (Exp.print "") id
      (Exp.print "") e
  | SCon(tis,[],e) ->
    fprintf ppf "(%a)%a"
      (print_ls ", " (fun ppf (t,i)-> fprintf ppf "%a %s" (Ty.print "" "") t i)) tis
      (print ("  "^sp)) e
  | SCon(tis,es,e) ->
    fprintf ppf "(%a):%a%a"
      (print_ls ", " (fun ppf (t,i)-> fprintf ppf "%a %s" (Ty.print "" "") t i)) tis
      (print_ls ", " (Exp.print "")) es 
      (print ("  "^ sp)) e
  | _ -> assert false
  and print2 sp ed ppf e =
    match e with
    | SBlock ls ->
      fprintf ppf " %a%s"
        (print sp) e
        (if ed <> "" then " " else "")
    | _ ->
      fprintf ppf "\n%a%s"
        (print (sp^"  ")) e
        ed
  and print_mem sp ppf = function
    | (Ty.TFun(_,_) as t),SExp(s) ->
      let buf = Buffer.create 1024 in
      let formatter = (Format.formatter_of_buffer buf) in 
      Exp.print "" formatter s;
      fprintf formatter "@?";
      let ss = Buffer.contents buf in 
      fprintf ppf "%a;\n"
        (Ty.print ss ("  "^sp)) t 
    | (t,s) ->
      fprintf ppf "%a %a\n"
        (Ty.print "" ("  "^sp)) t
        (print "") s
  in print "" ppf t

let prints ppf (ts:t list) =
  print_ls "\n" print ppf  ts

