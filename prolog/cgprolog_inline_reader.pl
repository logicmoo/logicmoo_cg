
:- multifile_data(cg/1).

inline_reader_ops(OPS),
:- current_op(X,Y,'->'),current_op(X1,Y1,'+'),
   OPS=([op(X,Y,'<-'), 
   op(X1,Y1,(*)),op(X1,Y1,(?)),op(X1,Y1,(@))
   op(900,xfy,'<-'),op(1000,yfx,'->'),op(1100,xfy,'-'),op(1110,xfx,'-'),op(1100,yfx,'-'),op(500,xfx,':'),
   op(300, fx,'?'),op(300, fx,'#'),op(300, fx,'*'),op(300, fx,'@'),
   op(300,yfx,'?'),op(300,yfx,'#'),op(300,yfx,'*'),op(300,yfx,'@'),
   op(1200,xfx,':'),op(1200,xfx,'=')]).

:- inline_reader_ops(OPS), push_operators(OPS).

/*
reader_df_to_term(In,Out):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['//'='%'],Str,Str0),
  inline_reader_ops(OPS),
  with_operators(OPS, 
     read_term_from_atom(Str0,Out,[variable_names(Vs)])), maplist(call,Vs),!.
*/

term_to_cg(In,Out):-
  format(chars(Chars),' ~q. ',[In]),
  any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['\r'='\n'],Str,Str0),
  atom_codes(Str0,Codes),
  must_or_rtrace(tokenize_cg(Toks,Codes,[])),
  parse_cg(Out,Toks,[]),!,
  ignore((fail,Out\=@=In, with_no_operators((nl,display(bf(In)),nl,display(af(Out)),nl)))),!.

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
   term_to_cg(Term,CG),
   current_why(UU),IS=OS,
   Out = (:- with_current_why(UU, assert_cg(cg(CG)))).


begin_cg:- style_check(-singleton), nb_setval(cg_term_expand,true),inline_reader_ops(OPS), push_operators(OPS).

:- fixup_exports.


cg([Cat: @every]-(On)->[Mat]).
cg([Mat #1]-(equal)->[Thingy #1]).
cg(['Man':imad]<-agnt-['Drive']-obj->['Car']).
cg([Cat#1]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Cat: ?x]-(equal)->M1-(On)->[Mat]).
cg([Cat: ?x]-(On)->[Mat]).
cg([Man:karim]<-agnt-[Drink]-obj->[Water]).
cg([Mat #1]<- (on)- [Cat #1]).
cg([Mat]<-(On)-[Cat: ?x]).
cg([Thingy #1]<-(equal)-[Mat #1]).
cg([Cat #1]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Man:karim]<-agnt-[Drink]-obj->[Water]).
cg([Thingy #1] <- (equal) -[Mat #1]<- (on)- [Cat#1]).
cg([Cat: @every]->(On)->[Mat]).
%cg([Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?z)).
cg(?x -(equal)-> [Thingy #1]).

cg(?x -(On)->[Mat #1]-(equal)->[Thingy #1]).
cg(?x -(On)->[Mat #1]).
cg([?x] -(equal)-> [Thingy #1]).
cg([?x]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Mat ?x]-(equal)->[Thingy #1]).
cg([Cat: ?x]-(On)->[Mat #1]-(equal)->[Thingy #2]).


:- begin_cg.
cg(

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]).


cg(
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]).

cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).

cg([Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
       -obj -> [Key : enter]-partOf->[Keyboard],
       -agnt -> [Person : John] ],
        -agnt->[Person : John]).

cg(
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2])).

cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]).

cg(
% ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -  
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]).


cg([Cat: ?x]-(equal)->M1-(On)->[Mat]).
cg([Cat: ?x]-(On)->[Mat].).
cg([Mat]<-(On)-[Cat: ?x].).




cg(

// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
      -obj->[Apple],
      -manr->[Fast],
      -agnt->[Man]

).


cg(
[Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
	       -obj -> [Key : enter]-partOf->[Keyboard],
	       -agnt -> [Person : John] ],
        -agnt->[Person : John]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).

cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).


:- begin_cg.

cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).

cg([Man:imad]<-agnt-[Drive]-obj->[Car]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).

%cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).



%cg([Cat: @every]->(On)->[Mat]).

%cg([Man:karim]<-agnt-[Drink]-obj->[Water]).


%cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).
%cg([Man:imad]<-agnt-[Drive]-obj->[Car]).
%cg([Cat: ?x]-(On)->[Mat]).


cg([Cat: @every]->(On)->[Mat]).
cg(

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus].

).


cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]].
).


 
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



:- pop_operators.

