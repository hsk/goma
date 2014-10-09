open Syntax
open Ty
open Exp
open Stmt

let rec trans_exp (e:e):e =
  match e with
  | EBin(e1,op,e2) -> EBin(trans_exp e1,op,trans_exp e2)
  | EPre(op,e1) -> EPre(op, trans_exp e1)
  | ECall(e1,es) -> ECall(trans_exp e1,List.map trans_exp es)
  | ECallM(i,e1,es) ->
    begin match trans_exp e1 with
    | EBin(e1,"->",fn) ->
      let ea = ECast(Ty (i^"*"),
        EArr(EBin(EVar(i^"_v"),"->",EVar "data"), [EBin(e1,".",EVar "id")])) in
      ECall(EBin(ea,"->",fn),(ECast(Ty "Class*", EPre("&",e1)))::(List.map trans_exp es))
    | t -> assert false
    end        
  | EArr(e1,es) -> ECall(trans_exp e1,List.map trans_exp es)
  | ECast(t,e) -> ECast(t, trans_exp e)
  | EInt i -> e
  | EString _ -> e
  | EVar _ -> e
  | EEmpty -> e

let rec trans_fun i i2 (t:t):t =
  match t with
  | SFun(t1,id,ls,SBlock b) ->
    SFun(
      t1,
      i^"_"^i2^"_"^id,
      (Ty "Class*","self_")::ls,

      SBlock(
        (SLet(Ty (i2 ^ "*"), EVar "self", ECast(Ty (i2 ^ "*"), EVar "self_")))::
        (List.map (trans_fun i i2) b)
      )

    )
  | t -> trans_stmt(t)

and trans_stmt(t:t):t =
  match t with
  | SList(ls) -> SList(List.map trans_stmt ls)
  | STrait(id,ts) ->

    let ts =
      List.map begin function
        | (TFun(t1,ts),t) -> (TFun(t1,(Ty "Class*")::ts),t)
        | (ty,t) -> (ty,t)
      end ts
    in
    SList([
      SStruct(id,"",ts);
      (*
        Vec* Fib2_v = newVec();
      *)
      SLet(Ty "Vec*", EVar (id ^ "_v"), ECall(EVar"newVec",[]))
    ])
  | SImpl(id,id2,ss) ->
    let ls = (List.fold_right begin fun s ls ->
      match s with
      | SFun(_, name ,_,_) ->
        (SExp(EBin(EBin(EVar "impl", "->", EVar name), "=", EPre("&", EVar(id^"_"^id2^"_"^name)))))::ls
      | _ -> ls
      end ss [SRet(EVar "impl")] )
    in
    let ss = List.map (trans_fun id id2) ss in
    SList(ss@[
      SFun(Ty(id^"*"), "new"^id^"_"^id2,[],SBlock(
        (SLet(Ty(id), EPre("*", EVar "impl"), EPre("new",ECall(EVar id,[]))))::
        (SExp(ECall(EVar "setVec", [ EVar(id^"_v"); EVar (id2^"_classId"); ECast(Ty "void*",EVar "impl") ])))::
        ls
      ));
      (*
        "^id^"* "^id^"_Int_ = new"^id^"_Int();
      *)
      SLet(Ty(id^"*"), EVar (id^"_"^id2^"_"), ECall(EVar("new"^id^"_"^id2),[]))

    ])
  | SInclude _ -> t
  | SLet(t,e1,e2) -> SLet(t, trans_exp e1, trans_exp e2)
  | SStruct(v,"",tts) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar"Class_genId",[]));

      SStruct(v,
        "",
        (Ty "int", SExp(EVar "id")) ::
        (List.map begin fun(ty,t) ->
          let ty,t = match ty,t with
            | (Ty "", SCon(tyss,es,t1)) ->
              let id = ECall(EVar "id",[EVar(v^"_classId")]) in

              (Ty v, SCon(tyss,id::es,t1))
            | _ -> ty,t
          in
          (ty, trans_stmt t)
        end tts))
    ]
  | SStruct(v,super,tts) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar"Class_genId",[]));

      SStruct(v,
        super,
        (List.map begin fun(ty,t) ->
          let ty,t = match ty,t with
            | (Ty "", SCon(tyss,es,t1)) ->
              let id = EBin(EVar "id","=",EVar(v^"_classId")) in

              (Ty v, SCon(tyss,es,SBlock[SExp(id);t1]))
            | _ -> ty,t
          in
          (ty, trans_stmt t)
        end tts))
    ]
  | SExp(e) -> SExp(trans_exp e)
  | SCon(tyss,es,t1) ->
    SCon(tyss,es,t1)
  | SBlock(l) -> SBlock(List.map trans_stmt l)
  | SEmpty -> t
  | SIf(e1,t1,t2) -> SIf(trans_exp e1, trans_stmt t1, trans_stmt t2)
  | SRet(e) -> SRet(trans_exp e)
  | SFun(ty, id, tyis, t2) -> SFun(ty, id, tyis, trans_stmt t2)

