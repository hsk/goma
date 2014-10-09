open Format
open Ast

let rec print_ls sep p ppf = function
  | [] -> ()
  | [x] -> p ppf x
  | x::xs ->
    fprintf ppf "%a%s%a" p x sep (print_ls sep p) xs

let rec print_t pp sp ppf = function

  | Ty(s) ->
    fprintf ppf "%s%s"
      sp
      s

  | TPtr(t) ->
    fprintf ppf "%a*"
      (print_t pp sp) t

  | TFun(r,ts) ->
    fprintf ppf "%a(*%s)(%a)"
      (print_t "" sp) r
      pp
      (print_ls ", " (print_t "" "")) ts

let rec print_e sp ppf = function

  | EEmpty ->
    ()

  | EInt i ->
    fprintf ppf "%s%d"
      sp
      i

  | EVar i ->
    fprintf ppf "%s%s"
      sp
      i

  | EString i ->
    fprintf ppf "%s%s"
      sp
      i

  | EPre(op,e1) ->
    fprintf ppf "%s(%s %a)"
      sp
      op
      (print_e "") e1

  | EBin(e1,op,e2) ->
    fprintf ppf "%s(%a %s %a)"
      sp
      (print_e "") e1
      op
      (print_e "") e2

  | ECall(e1,es) ->
    fprintf ppf "%a(%a)"
      (print_e sp) e1
      (print_ls ", " (print_e "")) es

  | ECallM(i,e1,es) ->
    fprintf ppf "%a(%a)"
      (print_e sp) e1
      (print_ls ", " (print_e "")) es

  | EArr(e1,es) ->
    fprintf ppf "%a[%a]"
      (print_e sp) e1
      (print_ls ", " (print_e "")) es

  | ECast(t,e) ->
    fprintf ppf "((%a)%a)"
      (print_t "" sp) t
      (print_e "") e


let rec print_s ppf (s:s):unit = 
  let rec print sp ppf = function

    | SEmpty ->
      ()

    | SExp e ->
      fprintf ppf "%a;"
        (print_e sp) e

    | SRet e ->
      fprintf ppf "%sreturn %a;"
        sp
        (print_e "") e

    | SInclude s ->
      fprintf ppf "#include %s"
        s

    | SList ls ->
      List.iter begin fun t ->
        fprintf ppf "%a@." (print sp) t
      end ls

    | SBlock ls ->
      fprintf ppf "{\n%a\n%s}"
        (print_ls "\n" (print (sp ^ "  "))) ls
        sp

    | SLet (t, id, EEmpty) ->
      fprintf ppf "%s%a %a;"
        sp
        (print_t "" "") t
        (print_e "") id

    | SLet (t, id, e) ->
      fprintf ppf "%s%a %a = %a;"
        sp
        (print_t "" "") t
        (print_e "") id
        (print_e "") e

    | SCon(tis, [], e) ->
      let f ppf (t,i) =
        fprintf ppf "%a %s"
          (print_t "" "") t
          i
      in
      fprintf ppf "(%a)%a"
        (print_ls ", " f) tis
        (print ("  "^sp)) e

    | SCon(tis, es, e) ->
      let f ppf (t,i) =
        fprintf ppf "%a %s"
          (print_t "" "") t
          i
      in
      fprintf ppf "(%a):%a%a"
        (print_ls ", " f) tis
        (print_ls ", " (print_e "")) es 
        (print ("  "^ sp)) e

    | SIf(e1, e2, SEmpty) ->
      fprintf ppf "%sif (%a)%a%s"
        sp
        (print_e "") e1
        (print_block sp "\n") e2
        sp

    | SIf(e1, e2, e3) ->
      fprintf ppf "%sif (%a)%a%selse%a"
        sp
        (print_e "" ) e1
        (print_block sp "\n") e2 
        sp
        (print_block sp "") e3 

    | SFun (t, id, ts, e2) ->
      let f ppf (t,a) =
        fprintf ppf "%a %s"
          (print_t "" "") t
          a
      in
      fprintf ppf "%a %s(%a)%a"
        (print_t "" "") t
        id
        (print_ls ", " f) ts 
        (print_block sp "\n") e2

    | SStruct (id, super, ts) ->
      let f ppf ts =
        List.iter (print_member sp ppf) ts
      in
      fprintf ppf "%sstruct %s%s{\n%s\n%a};\n"
        sp
        id
        (if super = "" then "" else ":" ^ super)
        sp
        f ts

    | _ -> assert false

  and print_block sp ed ppf = function

    | SBlock ls as e ->
      fprintf ppf " %a%s"
        (print sp) e
        (if ed <> "" then " " else "")

    | e ->
      fprintf ppf "\n%a%s"
        (print (sp^"  ")) e
        ed

  and print_member sp ppf = function

    | (TFun(_,_) as t),SExp(s) ->
      let ss =
        let buf = Buffer.create 1024 in
        let formatter = (Format.formatter_of_buffer buf) in 
        print_e "" formatter s;
        fprintf formatter "@?";
        Buffer.contents buf
      in
      fprintf ppf "%a;\n"
        (print_t ss ("  "^sp)) t 

    | (t,s) ->
      fprintf ppf "%a %a\n"
        (print_t "" ("  "^sp)) t
        (print "") s

  in
    print "" ppf s
