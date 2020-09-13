

:- use_module(library(logicmoo/dcg_meta)).

:- multifile_data(cg_test_data/2). 
:- multifile_data(cg/2).

:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).


cg_reader_tests :- make, forall(cg_test_data(_,X),assert_cg(xtext(X))).
cg_reader_tests2 :- make, forall((cg_test_data([reader,level(0)],X)),assert_cg(xtext(X))).

cg_demo :- make, forall(cg_test_data([xcall,level(0)],X),(call_cg(X))).


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
tokenize_cg_w('[')--> `[`,!.
tokenize_cg_w('<-')--> `<-`,!.
tokenize_cg_w('->')--> `->`,!.
tokenize_cg_w(Name)--> [C], {member(C,`[()]*@-=:<>,.$#`)},!,{ atom_codes(Name, [C])}.
%tokenize_cg_w(Name)--> dcg_used_chars(((`[` ; `(` ;`)` ; `]` ; `*`; `@`; `=`; `,`; `.`)), CL),!,{ atom_codes(Name, CL)}.
tokenize_cg_w('?'(UNAME)) --> `?`,!,prolog_id_conted(CL),{ atom_codes(Name, CL)},!,{upcase_atom(Name,UNAME)}.
tokenize_cg_w(Name) --> dcg_peek(`'`),!,single_quoted_string(Str),{atom_codes(Name,Str)}.
tokenize_cg_w(String) --> dcg_peek(`"`),!,double_quoted_string(String).
tokenize_cg_w(T)--> dcg_basics:number(T),!.
tokenize_cg_w(Name)--> prolog_id_conted(CL), !,{ atom_codes(Name, CL)},!.
tokenize_cg_w(Name)--> [C],{ atom_codes(Name, [C])},!.

tokenize_cg(HT)--> blank,!,tokenize_cg(HT).
tokenize_cg([],S,E):- S=[],!,E=[].
tokenize_cg([H|T])--> tokenize_cg_w(H),!,tokenize_cg(T).
tokenize_cg([])-->[],!.                                             

dcg_look(Grammar,List,List):- (var(Grammar)->((N=2;N=1;between(3,20,N)),length(Grammar,N)); true),phrase(Grammar,List,_),!.

parse_cg(List) --> concept(S),!, post_concept(S,List).

post_concept(S,List) --> ['-'], dcg_look(['-']),!,graph_listnode(S,List).
post_concept(Subj,[t(Rel,Subj,Obj)|List]) --> rel_right2(Rel),!,concept(Obj),graph_listnode(Obj,List).
post_concept(Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel),!,concept(Obj),graph_listnode(Subj,List).
post_concept(Obj, [t(Rel,Subj,Obj)|List]) --> rel_left(Rel),!,concept(Subj),graph_listnode(Subj,List).

graph_listnode(Subj,List) --> [','],!,graph_listnode(Subj,List).
graph_listnode(Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel), concept(Obj), 
  ([','];dcg_look(['-'])) ,!, graph_listnode(Subj,List).
graph_listnode(Subj,[t(Rel,Subj,Obj)|List]) --> rel_right(Rel), concept(Obj), graph_listnode(Obj,List).
graph_listnode(Obj,[t(Rel,Subj,Obj)|List]) --> rel_left(Rel), concept(Subj), graph_listnode(Subj,List).
graph_listnode(_,[])--> ((\+ [_]);['.']).

rel_right(Rel)-->['-'],rel(Rel),['->'].
rel_right2(Rel)-->['->'],rel(Rel),['->'].
rel_left(Rel)-->['<-'],rel(Rel),['-'].

rel(C)--> ['('],word_tok_loose(C),[')'].
rel(C)--> word_tok_loose(C).

word_tok_loose(DC)-->[C],{atom(C),downcase_atom(C,DC)}.

nonword_tok(X):- atom(X),upcase_atom(X,UC),downcase_atom(X,DC),!,UC==DC. 

                                                  

concept(co(Type,'#'(Num)))--> ['[', Type, ':', '#', Num, ']'],!.
concept(co(Type,'#'(Num)))--> ['[', Type, '#', Num, ']'],!.
concept(all(Type))--> ['[', Type, ':', '@', 'every', ']'],!.
concept(entity(C))--> ['[',C,']'],!.
concept(ct(Type,Word))--> ['[',Type,':',Word,']'],!.
concept(cot(Type,OP,Word))--> ['[',Type,':',OP,Word,']'],!.
concept(cot(Type,OP,Word))--> ['[',Type,OP,Word,']'],!.
concept(ct(Type,Word))--> ['[',Type,Word,']'],!.
concept(cg(Concept,SubGraph))--> ['[',Concept,'='], parse_cg(SubGraph), [']'],!.
concept(?(Var)) --> [?(Var)].

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


cg_test_data([reader, level(0)], "[Cat: @every]-(On)->[Mat]").
cg_test_data([reader, level(0)], "[Mat #1]-(equal)->[Thingy #1]").
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
cg_test_data([reader, level(3)], "[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?z)").
cg_test_data([xcall, level(0), funky_syntax], "?x -(equal)-> [Thingy #1]").

cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x] -(equal)-> [Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[Mat ?x]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0)], "[Cat: ?x]-(On)->[Mat #1]-(equal)->[Thingy #2]").

cg_test_data([reader, level(4)], "

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").


cg_test_data([reader, level(3)], "
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").

cg_test_data([reader, level(3)], "[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table").

skip_cg_test_data([reader, level(3)], "
[Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
       -obj -> [Key : enter]-partOf->[Keyboard],
       -agnt -> [Person : John] ],
        -agnt->[Person : John]").

skip_cg_test_data([reader, level(4)], "
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2])").

skip_cg_test_data([reader, level(4)], "
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").
     skip_cg_test_data([reader, level(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]").


:- pop_operators.

:- fixup_exports.

