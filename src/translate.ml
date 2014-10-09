open Ast

let rec trans_e = function

  | EInt _ as e -> e

  | EString _ as e -> e

  | EVar _ as e -> e

  | EEmpty as e -> e

  | EPre(op, e) ->
    EPre(op, trans_e e)

  | EBin(e1, op, e2) ->
    EBin(trans_e e1, op, trans_e e2)

  | EArr(e, es) ->
    ECall(trans_e e, List.map trans_e es)

  | ECast(t, e) ->
    ECast(t, trans_e e)

  | ECall(e, es) ->
    ECall(trans_e e, List.map trans_e es)

  | ECallM(i, e, es) ->
    begin match trans_e e with
      | EBin(e, "->", fn) ->
        let ea =
          ECast(
            Ty (i^"*"),
            EArr(
              EBin(EVar(i^"_v"),"->",EVar "data"),
              [EBin(e,".",EVar "id")]))
        in
        ECall(
          EBin(ea,"->",fn),
          ECast(Ty "Class*", EPre("&",e))::List.map trans_e es)
      | _ ->
        assert false
    end

let rec trans_s = function

  | SExp(e) ->
    SExp(trans_e e)

  | SInclude _ as t -> t

  | SLet(t, e1, e2) ->
    SLet(t, trans_e e1, trans_e e2)

  | SCon(tis,es,s) ->
    SCon(tis,es,s)

  | SBlock(ss) ->
    SBlock(List.map trans_s ss)

  | SEmpty as s -> s

  | SIf(e,s1,s2) ->
    SIf(trans_e e, trans_s s1, trans_s s2)

  | SRet(e) ->
    SRet(trans_e e)

  | SFun(t, i, tis, s) ->
    SFun(t, i, tis, trans_s s)

  | SList(ss) ->
    SList(List.map trans_s ss)

  | SStruct(v,"",tss) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar "Class_genId", []));
      SStruct(
        v,
        "",
        (Ty "int", SExp(EVar "id")) ::
        (List.map begin function
          | (Ty "", SCon(tis,es,s1)) ->
            let id = ECall(EVar "id",[EVar(v^"_classId")]) in
            (Ty v, trans_s (SCon(tis, id::es, s1)))
          | t, s ->
            t, trans_s s
        end tss))
    ]

  | SStruct(v,super,tss) ->
    SList[
      SLet(Ty "int", EVar (v ^ "_classId"), ECall(EVar"Class_genId",[]));
      SStruct(v,
        super,
        (List.map begin function
          | (Ty "", SCon(tis,es,s1)) ->
            let id = EBin(EVar "id","=",EVar(v^"_classId")) in
            (Ty v, trans_s (SCon(tis,es,SBlock[SExp(id);s1])))
          | (t, s) -> (t, trans_s s)
        end tss))
    ]

  | STrait(id,ts) ->
    let ts =
      List.map begin function
        | TFun(t, ts), s -> TFun(t, Ty "Class*"::ts), s
        | t, s           -> t, s
      end ts
    in
    SList([
      SStruct(id, "", ts);
      SLet(Ty "Vec*", EVar (id ^ "_v"), ECall(EVar "newVec", []))
    ])

  | SImpl(id, id2, ss) ->
    let rec trans_fun i i2 = function
      | SFun(t, id, ls, SBlock ss) ->
        SFun(
          t,
          i^"_"^i2^"_"^id,
          (Ty("Class*"),"self_")::ls,
          SBlock(
            SLet(
              Ty (i2 ^ "*"),
              EVar "self",
              ECast(Ty (i2 ^ "*"), EVar "self_"))
            :: List.map (trans_fun i i2) ss
          )

        )
      | s -> trans_s s
    in
    let ls = List.fold_right begin fun s ls ->
      match s with
      | SFun(_, name, _, _) ->
        SExp(
          EBin(
            EBin(EVar "impl", "->", EVar name),
            "=",
            EPre("&", EVar(id^"_"^id2^"_"^name))))::ls
      | _ -> ls
      end ss [SRet(EVar "impl")]
    in
    SList(
      List.map (trans_fun id id2) ss @ [
        SFun(
          Ty(id^"*"),
          "new"^id^"_"^id2,
          [],
          SBlock(
            SLet(
              Ty(id),
              EPre("*", EVar "impl"),
              EPre("new", ECall(EVar id,[])))::
            SExp(ECall(
              EVar "setVec",
              [
                EVar (id^"_v");
                EVar (id2^"_classId");
                ECast(Ty "void*",EVar "impl") ]))
            :: ls
        ));
        SLet(
          Ty (id^"*"),
          EVar (id^"_"^id2^"_"),
          ECall(EVar("new"^id^"_"^id2), []))

      ]
    )
