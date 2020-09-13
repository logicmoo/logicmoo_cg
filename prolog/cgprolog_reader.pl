

:- use_module(library(logicmoo/dcg_meta)).

:- multifile_data(cg_test_data/2). 
:- multifile_data(cg/2).

:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).


cg_reader_tests :- make, forall((cg_test_data([reader,level(0)],X)),assert_cg(text(X))).
cg_reader_tests2 :- make, forall((cg_test_data([reader,level(0)],X)),assert_cg(text(X))).

cg_demo :- make, forall(cg_test_data([xcall,level(0)],X),(call_cg(X))).


assert_cg(X):- !,newId(Id),locally(nb_setval(cgid,Id), pred_cg(assert_cg_real,X)).
assert_cg_real(X):- nb_current(cgid,Id), print_cg(Id:X),  ain(cg(Id,X)).

call_cg(X):- pred_cg(call_cg_real,X).
call_cg_real(X):- print_cg(X),call(cg(X)).


pred_cg(Pred, Error):- var(Error),!, trace_or_throw(pred_cg(Pred, Error)).
pred_cg(Pred, X):- is_list(X),maplist(pred_cg(Pred),X).
pred_cg(Pred, text(X)):- cg_df_to_term(X,Y),!, pred_cg(Pred, Y).
pred_cg(Pred, toks(Toks)):- must_or_rtrace(parse_cg(CG,Toks,[])), pred_cg(Pred, cg(CG)).
pred_cg(Pred, cg(CG)):- wdmsg(pred_cg(Pred, CG)), !, call(Pred,CG).
pred_cg(Pred, Error):- trace_or_throw(pred_cg(Pred, Error)).


print_cg(X):- is_list(X),!, maplist(print_cg,X).
print_cg(X):- nl,display(X),nl.


:- use_module(library(dcg/basics)).
prolog_id_conted([C|T])--> [C], {(C=45;code_type(C, prolog_identifier_continue))},!,prolog_id_conted(T).
prolog_id_conted([])-->[].

tokenize_cg('[')--> `[`,!.

tokenize_cg('<-')--> `<-`,!.
tokenize_cg('->')--> `->`,!.
tokenize_cg(Name)--> [C], {member(C,`[()]*@-=:,.$#`)},!,{ atom_codes(Name, [C])}.
%tokenize_cg(Name)--> dcg_used_chars(((`[` ; `(` ;`)` ; `]` ; `*`; `@`; `=`; `,`; `.`)), CL),!,{ atom_codes(Name, CL)}.
tokenize_cg(var(Name)) --> `?`,prolog_id_conted(CL),{ atom_codes(Name, CL)},!.
tokenize_cg(var(Name)) --> `'`,read_until_string
tokenize_cg(T)--> dcg_basics:number(T),!.
tokenize_cg(Name)--> prolog_id_conted(CL), !,{ atom_codes(Name, CL)},!.
tokenize_cg(Name)--> [C],{ atom_codes(Name, [C])},!.

tokenize_cg_list([],S,E):- S=[],!,E=[].
tokenize_cg_list(HT)--> blank,!,tokenize_cg_list(HT).
tokenize_cg_list([H|T])--> tokenize_cg(H),!,tokenize_cg_list(T).
tokenize_cg_list([])-->[],!.                                             

dcg_look(Grammar,List,List):- (var(Grammar)->((N=2;N=1;between(3,20,N)),length(Grammar,N)); true),phrase(Grammar,List,_),!.

parse_cg(List) --> concept(S),['-'], dcg_look(['-']),!,graph_listnode(S,List).
parse_cg([rel(Rel,Subj,Obj)|List]) --> concept(Subj),['-'], rel(Rel),['->'],!,concept(Obj),graph_listnode(Obj,List).
parse_cg([rel(Rel,Subj,Obj)|List]) --> concept(Obj),['<-'], rel(Rel),['-'],!,concept(Subj),graph_listnode(Subj,List).

graph_listnode(Subj,[rel(Rel,Subj,Obj)|List]) --> ['-'],rel(Rel),['->'], concept(Obj), ([','];dcg_look(['-'])) ,!, graph_listnode(Subj,List).
graph_listnode(Subj,[rel(Rel,Subj,Obj)|List]) --> ['-'],rel(Rel),['->'], concept(Obj), graph_listnode(Obj,List).
graph_listnode(Obj,[rel(Rel,Subj,Obj)|List]) --> ['<-'],rel(Rel),['-'], concept(Subj), graph_listnode(Subj,List).
graph_listnode(_,[])--> ((\+ [_]);['.']).

rel(C)--> ['(',C,')'].
concept(entity(C)):- ['[',C,']'],!.
concept(ct(Type,Word)):- ['[',Type,':',Word,']'],!.
concept(cg(Concept,SubGraph))--> ['[',Concept,'='], parse_cg(SubGraph),[']'],!.

cg_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['//'='%'],Str,Str0),
  atom_codes(Str0,Codes),
  tokenize_cg(Toks,Codes,[]),
  Out = toks(Toks).


cg_test_data([reader, level(0)], "[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([reader, level(1)], "['Man':imad]<-agnt-['Drive']-obj->['Car']").
cg_test_data([reader, level(1)], "[Cat: #1]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([reader, level(1)], "[Cat: ?x]-(equal)->M1-(On)->[Mat]").
cg_test_data([reader, level(1)], "[Cat: ?x]-(On)->[Mat]").
cg_test_data([reader, level(1)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([reader, level(1)], "[Mat #1]<- (on)- [Cat: #1]").
cg_test_data([reader, level(1)], "[Mat]<-(On)-[Cat: ?x]").
cg_test_data([reader, level(1)], "[Thingy #1]<-(equal)-[Mat #1]").
cg_test_data([reader, level(2)], "[Cat #1]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([reader, level(2)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([reader, level(2)], "[Thingy #1] <- (equal) -[Mat #1]<- (on)- [Cat: #1]").
cg_test_data([reader, level(3)], "[Cat: @every]->(On)->[Mat]").

cg_test_data([reader, level(3)], "[Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2])").

cg_test_data([reader, level(3)], "[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table").

cg_test_data([reader, level(3)], "
[Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
       -obj -> [Key : enter]-partOf->[Keyboard],
       -agnt -> [Person : John] ],
        -agnt->[Person : John]").

cg_test_data([reader, level(4)], "
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2])").

cg_test_data([reader, level(4)], "
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").

cg_test_data([reader, level(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
      -obj->[Apple],
      -manr->[Fast],
      -agnt->[Man]").

cg_test_data([reader, level(4)], "

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").

cg_test_data([xcall, level(0), funky_syntax], "?x -(equal)-> [Thingy #1]").

cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "?x -(On)->[Mat #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x] -(equal)-> [Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[?x]-(On)->[Mat #1]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0), funky_syntax], "[Mat ?x]-(equal)->[Thingy #1]").
cg_test_data([xcall, level(0)], "[Cat: ?x]-(On)->[Mat #1]-(equal)->[Thingy #1]").

:- pop_operators.

:- fixup_exports.

