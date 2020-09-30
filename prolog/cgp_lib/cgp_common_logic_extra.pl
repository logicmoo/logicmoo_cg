


do_varaibles(_Neg,_EoF,[],[]):-!.
do_varaibles(-, EoF,[?(X)],[- cg_quanz(EoF,?(X))]):-!.
do_varaibles(+, EoF,[?(X)],[cg_quanz(EoF,?(X))]):-!.
do_varaibles(Neg, EoF,[Pair|MOREVARZ],[Grok|Out]):-
  Pair = [?(X),Type],
  Grok = [cg_quanz(EoF,?(X)),cg_type(?(X),Type)],
  do_varaibles(Neg, EoF,MOREVARZ,Out).


chop_up(Stuff,Out):-
   chop_up(+,Stuff,Out).


chop_up(Neg,[ExistsOrForall,VarList,Stuff],Out):- 
   member(ExistsOrForall,[exists,forall]),
   do_varaibles(ExistsOrForall,VarList,Out1),
   chop_up(Neg,Stuff,Out2),
   unchop(Out1,Out2,Out).
chop_up(Neg,[and|Stuff],Out):-chop_up_list(Neg,Stuff,Out).

chop_up(+,[not,Stuff],Out):-chop_up(-,Stuff,Out).
chop_up(-,[not,Stuff],Out):-chop_up(+,Stuff,Out).
chop_up(-,[or|Stuff],Out):-chop_up_list(+,Stuff,Out).
chop_up(+,[or|Stuff],or(Out)):-chop_up_list(+,Stuff,Out).

chop_up(Neg,[Type,Arg],[cg_type(Arg,Type)]).
chop_up(Neg,[Pred,Arg1,Arg2],[cg_holds(Pred,Arg1,Arg2)]).



chop_up_list(Neg,[Stuff1|Stuff2],Out):-
   chop_up(Neg,Stuff1,Out1),
   chop_up_list(Neg,Stuff2,Out2),
   unchop(Out1,Out2,Out).
   









unchop(Out1,Out2,Out):- flatten([Out1,Out2],Out).
