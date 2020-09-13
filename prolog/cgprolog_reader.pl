

:- use_module(library(logicmoo/dcg_meta)).

:- multifile_data(cg_test_data/2). 
%:- multifile_data(skip_cg_test_data/2). 
:- multifile_data(cg/2).

%:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).


cg_demo :- make, forall((cg_test_data(Attribs,X), \+ memberchk(failing,Attribs)),do_cg_test(Attribs,X)).
cg_reader_tests :- make, forall(cg_test_data(Attribs,X),do_cg_test(Attribs,X)).

do_cg_test( Attribs,_):- memberchk(skip,Attribs),!.
do_cg_test( Attribs,X):- memberchk(xcall,Attribs),!, call_cg(xtext(X)).
do_cg_test(_Attribs,X):- assert_cg(xtext(X)).


assert_cg(X):- newId(Id),!,locally(nb_setval(cgid,Id), pred_cg(assert_cg_real,X)).
assert_cg_real(X):- nb_current(cgid,Id), print_cg(Id:X),  ain(cg(Id,X)).

call_cg(X):- pred_cg(call_cg_real,X).
call_cg_real(X):- print_cg(X),call(cg(_,X)).


pred_cg(Pred, Error):- var(Error),!, trace_or_throw(pred_cg(Pred, Error)).
pred_cg(Pred, X):- is_list(X),maplist(pred_cg(Pred),X).
pred_cg(Pred, cg(CG)):- !, pred_cg(Pred, CG).
pred_cg(Pred, cg(CG)):- wdmsg(pred_cg(Pred, CG)), !, call(Pred,CG).
%pred_cg(Pred, toks(Toks)):- catch(parse_cg(CG,Toks,[]),_,fail),pred_cg(Pred, cg(CG)),!.
pred_cg(Pred, toks(Toks)):- parse_cg(CG,Toks,[]),pred_cg(Pred, cg(CG)),!.
pred_cg(Pred, X):- wdmsg(pred_cg(Pred, X)), fail.
pred_cg(Pred, xtext(X)):- cg_df_to_term(X,Y),!, pred_cg(Pred, Y),!.
pred_cg(_, _):- !.
pred_cg(Pred, Error):- trace_or_throw(pred_cg(Pred, Error)).


print_cg(X):- is_list(X),!, maplist(print_cg,X).
print_cg(X):- nl,wdmsg(display(X)),nl.


:- use_module(library(dcg/basics)).

end_symbol--> `-`, !, end_symbol.
end_symbol-->  [C],!, { \+code_type(C, prolog_identifier_continue) }.
end_symbol--> \+ [_].
prolog_id_conted([])--> dcg_peek(end_symbol),!.
prolog_id_conted([C|T])--> [C], !,prolog_id_conted(T).

tokenize_cg_w(HT)--> blank,!,tokenize_cg_w(HT).

tokenize_cg_w(Name) --> dcg_peek(`'`),!,single_quoted_string(Str),{atom_codes(Name,Str)}.
tokenize_cg_w(String) --> dcg_peek(`"`),!,double_quoted_string(String).
tokenize_cg_w(Op)--> {sent_op_chars(Op,Chars)},Chars,!.
tokenize_cg_w('?'(UNAME)) --> `?`,!,prolog_id_conted(CL),{ atom_codes(Name, CL)},!,{upcase_atom(Name,UNAME)}.
tokenize_cg_w(T)--> dcg_basics:number(T),!.
tokenize_cg_w(Name)--> prolog_id_conted(CL), !,{ atom_codes(Name, CL)},!.
tokenize_cg_w(Name)--> [C],{ atom_codes(Name, [C])},!.

tokenize_cg(HT)--> blank,!,tokenize_cg(HT).
tokenize_cg([],S,E):- S=[],!,E=[].
tokenize_cg([H|T])--> tokenize_cg_w(H),!,tokenize_cg(T).
tokenize_cg([])-->[],!.                                             

parse_cg(List) --> concept(S),!, post_concept(S,S,List).

post_concept(Sticky,S,List) --> ci('-'),
  dcg_peek(ci('<-');ci('-');ci('->')),!,graph_listnode(Sticky,S,List).
post_concept(Sticky,Subj,[t(Rel,Subj,Obj)|List]) --> rel_right2(Rel),!,concept(Obj),graph_listnode(Sticky,Obj,List).
post_concept(Sticky,Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel),!,concept(Obj),graph_listnode(Sticky,Subj,List).
post_concept(Sticky,Obj, [t(Rel,Subj,Obj)|List]) --> rel_left2(Rel),!,concept(Subj),graph_listnode(Sticky,Obj,List).
post_concept(Sticky,Obj, [t(Rel,Subj,Obj)|List]) --> rel_left(Rel),!,concept(Subj),graph_listnode(Sticky,Subj,List).

graph_listnode(_Sticky,_Subj,[]) --> dcg_peek(ci(']')),!.
graph_listnode(_Sticky,_Subj,[]) --> ci('.'),!.
graph_listnode(Sticky,_Subj,List) --> ci(','),!,graph_listnode(Sticky,Sticky,List).
graph_listnode(Sticky,Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel), concept(Obj), 
  (ci(',');dcg_peek(ci('-'))) ,!, graph_listnode(Sticky,Subj,List).

graph_listnode(Sticky,Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel), concept(Obj), graph_listnode(Sticky,Obj,List).
graph_listnode(Sticky,Obj,[t(Rel,Subj,Obj)|List]) --> rel_left(Rel), concept(Subj), graph_listnode(Sticky,Subj,List).

graph_listnode(Sticky,Subj,[t(Rel,Subj,Obj)|List]) --> rel_right2(Rel), concept(Obj), graph_listnode(Sticky,Sticky,List).
graph_listnode(Sticky,Obj,[t(Rel,Subj,Obj)|List]) --> rel_left2(Rel), concept(Subj), graph_listnode(Sticky,Sticky,List).

graph_listnode(_Sticky,_,[])--> ((\+ [_]);['.']).

rel_right(Rel)-->ci('-'),rel(Rel),ci('->').
rel_right2(Rel)-->ci('->'),rel(Rel),ci('->').
rel_left(Rel)-->ci('<-'),rel(Rel),ci('-').
rel_left2(Rel)-->ci('<-'),rel(Rel),ci('<-').

rel(C)--> ['('],word_tok_loose(C),[')'].
rel(C)--> ['<'],word_tok_loose(C),['>'].
rel(C)--> word_tok_loose(C).

word_tok_loose(DC)-->[C],{atom(C),downcase_atom(C,DC)}.

nonword_tok(X):- atom(X),upcase_atom(X,UC),downcase_atom(X,DC),!,UC==DC. 

word_tok('#'(X))--> ['#'],word_tok(X),!.
word_tok('#')--> ['#'], !.
word_tok('*'(X))--> ['*'],word_tok(X),!.
word_tok('*')--> ['*'], !.
word_tok(?(Var)) --> [?(Var)],!.
word_tok(Value) --> [Value],{number(Value)},!.
word_tok(*)--> [*], !.
word_tok(?(Var)) --> [?(Var)],!.
word_tok(X)--> [X], !, {atom(X), \+ nonword_tok(X)}.

quant(X) --> [X], {nonword_tok(X)}.

                                                  
concept('*')--> [*], !.
concept(?(Var)) --> [?(Var)],!.
concept(C)-->concept0(C0),(([I],{integer(I)})->{C=n(C0,'#'(I))};{C=C0}),!.


concept0(C)--> ci('['), dcg_peek([P1,P2]), concept_innerds_3a(P1,P2,C),!.
concept0(C)--> ci('['), concept_innerds_1(C), ci(']'),!.
concept0(crel(C))--> rel(C),!.

%concept0(C)--> word_tok(C),!.
%concept0(C)--> word_tok_loose(C),!.

concept_innerds_1(all(C))--> cw(C), ci('@'),  ci('every'),!.
concept_innerds_1(n(C,'#'(V))) --> cw(C),        ci('#'), cw(V).
concept_innerds_1(n(C,'#'(V))) --> cw(C),ci(':'),ci('#'), cw(V).
concept_innerds_1(cg(C, Grph)) --> cw(C),ci(':'), dcg_peek(['[']), parse_cg(Grph).
concept_innerds_1(c(C, OP, V)) --> cw(C),ci(':'), [OP], cw(V).
concept_innerds_1(cg01(C,Grph))--> cw(C),ci('='), parse_cg(Grph).
concept_innerds_1(cot(C,OP,V)) --> cw(C),quant(OP) ,{nonword_tok(OP)},cw(V),!.
concept_innerds_1(ct(C,V))--> cw(C), ci(':'), cw(V).
concept_innerds_1(ct(C,V))--> cw(C), cw(V).
concept_innerds_1(itype(C,V))--> [ C ], !, concept_innerds_cont(V).
concept_innerds_1(type(C,V))--> [ C, ':' ], !, concept_innerds_cont(V).
concept_innerds_1(v(V))   --> cw(V),!.

concept_innerds_3a(P1,']',entity(C))--> !,[P1,']'],!,{cw(C,[P1],[])}.
concept_innerds_3a(P1,P2,C)--> concept_innerds_3b(P1,P2,C),[']'].

concept_innerds_3b(_P1,':',ct4(C,V))--> cw(C),[':'],cw(V),!.
concept_innerds_3b(_P1,_P2,cot4(C,OP,V))--> cw(C),[OP],{nonword_tok(OP)},cw(V),!.
concept_innerds_3b(_P1,'=',cg4(C,SubGraph))--> cw(C),['='], parse_cg(SubGraph),!.
concept_innerds_3b(_P1,_P2,entity(C))-->  cw(C),!.

sent_op_chars(Op,Chars):- sent_op(Op),atom_codes(Op,Chars).

% these must be before:
sent_op('::'). sent_op(':-').
sent_op('->'). sent_op('<-').
% these
sent_op('-').  sent_op(':').
sent_op('<').  sent_op('>').
% then..
sent_op('{').  sent_op('}').
sent_op('[').  sent_op(']').
sent_op('(').  sent_op(')').
sent_op(',').  sent_op(';').
sent_op('.').  sent_op('=').
sent_op('@').  sent_op('#').
sent_op('^').  sent_op('*').
sent_op('~').  sent_op('$').

cw(H,[H|T],T):- \+ sent_op(H).

ci(CI)-->[C],{atom(C),upcase_atom(C,UC),CI=UC}.

concept_innerds_cont(C)--> concept_innerds_1(C).


cg_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['\r'='\n'],Str,Str0),
  atom_codes(Str0,Codes),
  must_or_rtrace(tokenize_cg(Toks,Codes,[])),
  Out = toks(Toks).


:- set_dcg_meta_reader_options(file_comment_reader, cg_comment_expr).
cg_comment_expr(X) --> cspace,!,cg_comment_expr(X).
cg_comment_expr('$COMMENT'(Expr,I,CP)) --> comment_expr_5(Expr,I,CP),!.
comment_expr_5(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
comment_expr_5(T,N,CharPOS) -->  {cmt_until_eoln(Text)},Text,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.
cmt_until_eoln(`//`).
cmt_until_eoln(`;;`).
cmt_until_eoln(`%`).

%:- pop_operators.

:- fixup_exports.

cg_test_data([reader, level(0), sowa], "[Mat]1-(equal)->[Thingy #1]").
cg_test_data([reader, level(0)], "[Cat: @every]-(On)->[Mat]").
cg_test_data([reader, level(1)], "['Man':imad]<-agnt-['Drive']-obj->['Car']").
cg_test_data([reader, level(1)], "[Cat#1]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([reader, level(1)], "[Cat: ?x]-(equal)->M1-(On)->[Mat]").
cg_test_data([reader, level(1)], "[Cat: ?x]-(On)->[Mat]").
cg_test_data([reader, level(1)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([reader, level(1)], "[Mat #1]<- (on)- [Cat #1]").
cg_test_data([reader, level(1)], "[Mat]<-(On)-[Cat: ?x]").
cg_test_data([reader, level(1)], "[Thingy #1]<-(equal)-[Mat #1]").
cg_test_data([reader, level(2)], "[Cat #1]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([reader, level(2)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([reader, level(2)], "[Thingy #1] <- (equal) -[Mat #1]<- (on)- [Cat#1]").
cg_test_data([reader, level(3)], "[Cat: @every]->(On)->[Mat]").
cg_test_data([reader, level(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
cg_test_data([reader, level(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").

cg_test_data([reader, level(3)], "
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").


cg_test_data([xcall, level(0), funky_syntax], "?x -(equal)-> [Thingy #1]").

cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x] -(equal)-> [Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[Mat ?x]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0)], "[Cat: ?x]-(On)->[Mat #1]-(equal)->[Thingy #2]").

cg_test_data([reader, level(3)], "[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
cg_test_data([reader, level(3)], "[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").

cg_test_data([reader, level(4)], "

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").

cg_test_data([reader, level(4)], "
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").


cg_test_data([reader, level(3)], "
[Begin]-
        -obj->[Session],
        -srce->[Proposition = 
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John] 
               ],
        -agnt->[Person : John]").


cg_test_data([reader, level(3)], "
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").


cg_test_data([failing, reader, level(3)], "[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?z)").

cg_test_data([skip, reader, level(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]").

cg_test_data([failing,reader, level(3)], "
[CEILING]1->(BETWEEN)-
              2<-[FLOOR]
              3->[MAT],.").

cg_test_data([failing,reader, level(3)], "[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table").

cg_test_data([failing,reader, level(4)], "
[Person: Tom]1111111111<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").



