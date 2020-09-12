/*:- current_op(X,Y,'->'),cginput:op(X,Y,'<-').
:- current_op(X,Y,'->'),cginput:op(X,Y,'-').
*/
:- forall(current_op(X,Y,(+)),op(X,Y,(*))).
:- forall(current_op(X,Y,(+)),op(X,Y,(?))).
:- forall(current_op(X,Y,(+)),op(X,Y,(@))).

:- multifile_data(cg/1).

:- current_op(X,Y,'->'),push_operators([op(X,Y,'<-')]).

cg_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['XXX'='XXX'],Str,Str0),
  with_only_operators(
   [%op(900,xfy,'<-'),op(1000,yfx,'->'),op(1100,xfy,'-'),op(1110,xfx,'-'),op(1100,yfx,'-'),op(500,xfx,':'),
    % op(1170,yfx,'<-'),op(1150,yfx,'->'),
    % op(1000,yfx,'<-'),op(1000,yfx,'->'),    
   op(900,xfy,'<-'),
   op(900,yfx,'<-'),
   op(900,xfx,'<-'), 
   op(900,yfx,'->'), 
   op(900,xfy,'->'),
   op(900,xfx,'->'), 
    op(1000,xfx,'-'),
    op(1100,fy,'-'),
    
    op(1200,xfx,':'),op(1200,xfx,'='),op(300,fx,'?'),op(300,fx,'*'),op(300,yfx,'@'),op(300,fx,'@'),op(300,yfx,'?'),op(300,yfx,'*')],
     read_term_from_atom(Str0,Out,[variable_names(Vs)])), maplist(call,Vs),!.
  

cg_reader_tests :- make, forall(cg_reader_text(X),(assert_cg(X))).

cg_demo:- !.

ground_variables_as_atoms([],_Vars):-!.
ground_variables_as_atoms(_,[]):-!.
ground_variables_as_atoms(Vs,[N=V|Vars]):-
  ground_variables_as_atoms(Vs,Vars),
  (member_eq0(V, Vs) -> V = N ; true).

term_expansion(In,IS, Out,OS):- notrace((compound(In), In= cg(Stuff), nonvar(Stuff),nb_current(cg_term_expand,true))),
   prolog_load_context('term',Term), % dmsg(Term=In),
   Term=@=In,    
   nb_current('$variable_names',Vars), 
   term_variables(Stuff,Vs),!,
   ground_variables_as_atoms(Vs,Vars),
   current_why(UU),IS=OS,
   Out = (:- with_current_why(UU, assert_cg(cg(Stuff)))).


begin_cg:- style_check(-singleton), nb_setval(cg_term_expand,true).


:- current_op(X,Y,'->'),cginput:op(X,Y,'<-').
%:- current_op(X,Y,'->'),X2 is X + 1, cginput:op(X2,Y,'-').
%:- current_op(X,Y,(+)),cginput:op(X,Y,(*)).
%:- cginput:current_op(X,Y,(*)),cginput:op(X,Y,(?)).
%:- cginput:current_op(X,Y,(*)),cginput:op(X,Y,(@)).



not_oper(S):-  compound(S), compound_name_arity(S,F,_),member(F,['<-','-','->']),!,fail.
not_oper(_).

assert_cg(X):- \+ compound(X),cg_df_to_term(X,Y),!,assert_cg(cg(Y)).
assert_cg(cg(CG)):- dmsg(CG), !,newId(Id),locally(nb_setval(cgid,Id),(reop_cg(CG,New),assert_cg(New))).
%assert_cg(Reop):- New\=@=Reop,!,assert_cg(New).
assert_cg(X):- is_list(X),maplist(assert_cg,X).
assert_cg(X):- reop_cg_post(X,Y), X\=@= Y, assert_cg(Y).
% assert_cg(-(S,->(P,O))):- not_oper(S),not_oper(P),not_oper(O), assert_cg(spo(S,P,O)).
% assert_cg(S->S):- 
assert_cg(X):- % format("~N~p.~n",[X]),
  nl,display(X),nl,ain(cg(X)).

reop_cg(In,Out):- reop_cg_pre(In,M1),reop_cg_mid(M1,M2),reop_cg_post(M2,Out).

reop_cg_pre(In,Out):- \+ compound(In),!, Out=In.
reop_cg_pre([Thing],Out):- !, reop_cg_pre(entity(Thing),Out).
reop_cg_pre([Type:Thing],Out):- !, reop_cg_pre(type_thing(Type,Thing),Out).
reop_cg_pre(In,Out):- is_list(In),!,maplist(reop_cg_pre,In,Out).
reop_cg_pre(In,Out):- In=..[OP|AB],maplist(reop_cg_pre,AB,AABB),Out=..[OP|AABB].
reop_cg_pre(OIn,OIn).

reop_cg_mid(In,Out):- format(chars(Chars),' ~q . ',[In]),cg_df_to_term(Chars,Out),
  ignore((fail,Out\=@=In, with_no_operators((nl,display(bf(In)),nl,display(af(Out)),nl)))),!.

reop_cg_post(In,Out):- is_entity(In),!,Out=In.
reop_cg_post(In,Out):- \+ compound(In),!, Out=In.
reop_cg_post(In,Out):- is_list(In),!,Out=In.
reop_cg_post(In,Out):- is_list(In),!,maplist(reop_cg_post,In,Out).
reop_cg_post(-(S,->(P,O)),SPO):- not_oper(S),not_oper(P),not_oper(O),!, SPO = (spo(S,P,O)).
reop_cg_post(-(<-(O,P),S),SPO):- not_oper(S),not_oper(P),not_oper(O),!, SPO = (spo(S,P,O)).

reop_cg_post(-(SPOS,->(P,O)),[SPOS,SPO]):- right_side(SPOS,S), SPO = (spo(S,P,O)).
reop_cg_post(-(<-(O,P), SPOS),[SPOS,SPO]):- left_side(SPOS,S), SPO = (spo(S,P,O)).

%reop_cg_post(-(<-(O,P),SPOS),[SPOS,SPO]):- left_side(SPOS,S), SPO = (spo(S,P,O)).
reop_cg_post(In,Out):- In=..[OP|AB],maplist(reop_cg_post,AB,AABB),Out=..[OP|AABB].
reop_cg_post(OIn,OIn).

left_side(SPOS,S):- is_entity(SPOS),!,S = SPOS.
left_side(spo(SPO,_,_),S):- !, left_side(SPO,S),!. 
left_side(SPOS,S):- arg(1,SPOS,A1),!,left_side(A1,S).

right_side(SPOS,S):- is_entity(SPOS),!,S = SPOS.
%right_side(spo(SPO,_,_),S):- !, right_side(SPO,S),!. 
right_side(SPOS,S):- arg(2,SPOS,A1),!,right_side(A1,S).

is_entity(Atom):- atom(Atom).
is_entity(entity(_)).
is_entity(type_thing(_,_)).

dcg_used_chars(DCG1, O, S, E):- phrase(DCG1,S, E),!,O=S.
% dcg_both(DCG1,DCG2, S, E):- phrase(DCG1,S, E),!,phrase(DCG2,S, E).
%  ['Man':imad]<-agnt-['Drive']-obj->['Car']                                

:- use_module(library(http/dcg_basics)).
prolog_id_conted([C|T])--> [C], {(C=45;code_type(C, prolog_identifier_continue))},!,prolog_id_conted(T).
prolog_id_conted([])-->[].

tokenize_cg('[')--> `[`,!.

tokenize_cg('<-')--> `<-`,!.
tokenize_cg('->')--> `->`,!.
tokenize_cg(Name)--> [C], {member(C,`[()]*@-=:,.$#`)},!,{ atom_codes(Name, [C])}.
%tokenize_cg(Name)--> dcg_used_chars(((`[` ; `(` ;`)` ; `]` ; `*`; `@`; `=`; `,`; `.`)), CL),!,{ atom_codes(Name, CL)}.
tokenize_cg(var(Name)) --> `?`,prolog_id_conted(CL),{ atom_codes(Name, CL)},!.
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
/*
tokenize_cg_list(L,`[`,[]).
tokenize_cg_list(L,`[Begin]-        -obj->[Session],
        -srce->[Proposition = [Press] -
       -obj -> [Key : enter]-partOf->[Keyboard],
       -agnt -> [Person : John] ],
        -agnt->[Person : John]`,O).
*/
:- begin_cg.

cg_reader_text("
[Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
	       -obj -> [Key : enter]-partOf->[Keyboard],
	       -agnt -> [Person : John] ],
        -agnt->[Person : John]").


cg_reader_text("[Cat: ?x]-(On)->[Mat].").
cg_reader_text("[Mat]<-(On)-[Cat: ?x].").
%cg_reader_text("[Cat: ?x]-(On)->[Mat]-(equal)->M1.").

cg_reader_text("M1<-(eq)-[Mat]<-(On)-[Cat: ?x].").

%cg_reader_text("[Cat: ?x]-(equal)->M1-(On)->[Mat].").
%cg_reader_text("[Man:karim]<-agnt-[Drink]-obj->[Water]").

%cg_reader_text("[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table").

%cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).



%cg([Cat: @every]->(On)->[Mat]).

%cg([Man:karim]<-agnt-[Drink]-obj->[Water]).


/*
cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).
cg([Man:imad]<-agnt-[Drive]-obj->[Car]).
cg([Cat: ?x]-(On)->[Mat]).


cg_reader_text("
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]].
").

cg_reader_text("[Cat: @every]->(On)->[Mat]").
cg_reader_text("

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus].

").

cg([Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]).

cg([Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]).

cg(
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) -

   [Go2]).


cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]).

*/
:- fixup_exports.

:- pop_operators.

end_of_file.



left_from(Left,W,Sent):- reparse(cg_sentence(Sent), [Left, w(`<-`),w(`[`),concept(W),w(`]`)]).
right_from(W, Right,Sent):- reparse(cg_sentence(Sent), [w(`[`),concept(W),w(`]`),w(`->`),Right]).


cg_sentence(Sent) --> left_from(Left,W,Sent1), w(`<-`),w(`[`),concept(W),w(`]`),w(`->`),right_from(W, Right,Sent2),
   {flatten([Sent1,Sent2],Sent)}.
cg_sentence(Sent) --> concept(S), w(`-`),relation(R),w(`->`),concept(O).
cg_sentence(Sent) --> concept(S), w(`-`),relation(R),w(`->`),concept(O).

?- cg_reader_tests.

Outputs:

['Person':'Tom']<-'Expr'<-['Believe']->'Thme'-['Proposition':['Person':'Mary'*x]<-'Expr'<-['Want']->'Thme'-['Situation':[?x]<-'Agnt'<-['Marry']->'Thme'->['Sailor']]].

['Cat': @every]->'On'->['Mat'].

['Go']-'Agnt'->['Person':'John']-'Dest'->['City':'Boston']-'Inst'->['Bus'].

['Cat': ?x]-'On'->['Mat'].



end_of_file.

%:- expects_dialect(cg).
:- begin_cg.

cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).

cg([Man:imad]<-agnt-[Drive]-obj->[Car]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).

/* ?- cg([Man:karim]<-agnt-[x]).

{x = Eat};

{x = Drink};

 no

?-
*/

