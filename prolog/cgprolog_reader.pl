
                 
cginput:is_module.
:- current_op(X,Y,'->'),cginput:op(X,Y,'<-').
:- current_op(X,Y,(+)),cginput:op(X,Y,(*)).
:- cginput:current_op(X,Y,(*)),cginput:op(X,Y,(?)).
:- cginput:current_op(X,Y,(*)),cginput:op(X,Y,(@)).

cg_df_to_term(In,cg(Out)):- any_to_string(In,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['XXX'='XXX'],Str,Str0),
  read_term_from_atom(Str0,Out,[module(cginput),variable_names(Vars)]),
  maplist(call,Vars),!.
  

assert_cg(X):- \+ compound(X),cg_df_to_term(X,Y),!,assert_cg(Y).
assert_cg(X):- is_list(X),maplist(assert_cg,X).
assert_cg(X):- format("~N~p.~n",[X]),ain(X).


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
cg_reader_text("[Cat: ?x]-(On)->[Mat].").
cg_reader_tests :- forall(cg_reader_text(X),assert_cg(X)).

cg_demo:- !.
ground_variables_as_atoms([],_Vars):-!.
ground_variables_as_atoms(_,[]):-!.
ground_variables_as_atoms(Vs,[N=V|Vars]):-
  ground_variables_as_atoms(Vs,Vars),
  (member_eq0(V, Vs) -> V = N ; true).

term_expansion(cg(Stuff), Out):- nonvar(Stuff), nb_current(cg_term_expand,true),
   term_variables(Stuff,Vs),
   nb_current('$variable_names',Vars), ground_variables_as_atoms(Vs,Vars),
   current_why(UU),
   Out = (:- with_current_why(UU, assert_cg(Stuff))).


begin_cg:- style_check(-singleton), nb_setval(cg_term_expand,true).

:- begin_cg.

cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).

cg([Man:imad]<-agnt-[Drive]-obj->[Car]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).



end_of_file.


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

