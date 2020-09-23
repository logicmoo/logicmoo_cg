# logicmoo_cg
Conceptual Graph (CG) Libraries in Prolog


```
/pack/logicmoo_cg/prolog# cls ; swipl -l cgprolog.pl -t halt -g cg_reader_tests
```

init_why(before_boot,after(/.../(prolog,'cgprolog.pl')))
% init_why(before_boot, after(/.../(prolog, 'cgprolog.pl'))).



```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[PERSON: x] :- [CITIZEN : x].").
% ===========================================

[ lbl(frame1),
  frame_var('X',Person_X_Citizen),
  cg_type(Person_X_Citizen,'Person'),
  preconds(
     [ [ frame_var('X',Person_X_Citizen),
         cg_type(Person_X_Citizen,'Citizen'),
         lbl(frame2) ] ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

[ lbl(frame3),
  frame_var('X',Citizen_X_Person),
  cg_type(Oz_Country7,'Country'),
  cg_type(Citizen_X_Person,'Citizen'),
  cg_name(Oz_Country7,'Oz'),
  cg_holds(memberOf,Oz_Country7,Citizen_X_Person),
  preconds(
     [ [ cg_holds('Loc',Being_Born,Oz_Country7),
         cg_name(Oz_Country7,'Oz'),
         cg_type(Oz_Country7,'Country'),
         cg_holds('Agnt',Being_Born,Citizen_X_Person),
         cg_type(Being_Born,'Being_Born'),
         frame_var('X',Citizen_X_Person),
         cg_type(Citizen_X_Person,'Person'),
         lbl(frame4) ] ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: ?x]<-childOf-[PERSON: y],
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].").
% ===========================================

[ lbl(frame5),
  frame_var('X',X_Citizen),
  cg_type(Oz_Country10,'Country'),
  cg_type(X_Citizen,'Citizen'),
  cg_name(Oz_Country10,'Oz'),
  cg_holds(memberOf,Oz_Country10,X_Citizen),
  preconds(
     [ [ cg_holds(memberOf,Oz_Country10,Y_Citizen8),
         cg_name(Oz_Country10,'Oz'),
         cg_type(Oz_Country10,'Country'),
         frame_var('Y',Y_Citizen8),
         cg_type(Y_Citizen8,'Citizen'),
         cg_holds(childOf,Y_Person6,X),
         frame_var('Y',Y_Person6),
         cg_type(Y_Person6,'Person'),
         cg_quantz(e,X),
         cg_type(X,'Person'),
         lbl(frame6) ] ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].").
% ===========================================

[ lbl(frame7),
  frame_var('X',Citizen_X_Person),
  cg_type(Oz_Country7,'Country'),
  cg_type(Citizen_X_Person,'Citizen'),
  cg_name(Oz_Country7,'Oz'),
  cg_holds(memberOf,Oz_Country7,Citizen_X_Person),
  preconds(
     [ [ cg_holds('Loc',Naturalize,Oz_Country7),
         cg_name(Oz_Country7,'Oz'),
         cg_type(Oz_Country7,'Country'),
         cg_holds('Rcpt',Naturalize,Citizen_X_Person),
         cg_type(Naturalize,'Naturalize'),
         frame_var('X',Citizen_X_Person),
         cg_type(Citizen_X_Person,'Person'),
         lbl(frame8) ] ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[PERSON : Tinman]-
              -childOf->[GIRL : Dorothy],
              <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

[ lbl(frame9),
  cg_type(Oz_Country,'Country'),
  cg_type(Being_Born,'Being_Born'),
  cg_type(Dorothy_Girl,'Girl'),
  cg_type(Tinman_Person,'Person'),
  cg_name(Oz_Country,'Oz'),
  cg_name(Dorothy_Girl,'Dorothy'),
  cg_name(Tinman_Person,'Tinman'),
  cg_holds(childOf,Tinman_Person,Dorothy_Girl),
  cg_holds('Loc',Being_Born,Oz_Country),
  cg_holds('Agnt',Being_Born,Tinman_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color #1]").
% ===========================================

[ lbl(frame10),
  cg_type(Color,'Color'),
  cg_type(Mat,'Mat'),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Color,'Color#1'),
  cg_equal(Mat,'Mat#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color]2").
% ===========================================

[ lbl(frame11),
  cg_type(Color,'Color'),
  cg_type(Mat,'Mat'),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Color,'Color#2'),
  cg_equal(Mat,'Mat#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_QUANT:@every]-(On)->[Mat]").
% ===========================================

[ lbl(frame12),
  cg_type(Mat,'Mat'),
  cg_type(Every_Cat_Quant,'Cat_Quant'),
  cg_quantz(every,Every_Cat_Quant),
  cg_holds('On',Every_Cat_Quant,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[A_CAT]->(On)->[Mat]").
% ===========================================

[ lbl(frame13),
  cg_type(Mat,'Mat'),
  cg_type(A_Cat,'A_Cat'),
  cg_holds('On',A_Cat,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[THE_CAT:#666]->(On)->[Mat]").
% ===========================================

[ lbl(frame14),
  cg_type(Mat,'Mat'),
  cg_type(The_Cat,'The_Cat'),
  cg_holds('On',The_Cat,Mat),
  cg_equal(The_Cat,'The_Cat#666') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[NAMED_CAT:Moris]->(On)->[Mat]").
% ===========================================

[ lbl(frame15),
  cg_type(Mat,'Mat'),
  cg_type(Moris_Named_Cat,'Named_Cat'),
  cg_name(Moris_Named_Cat,'Moris'),
  cg_holds('On',Moris_Named_Cat,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[LENGTH:@5ft]<-(SizeOf)-[Mat]").
% ===========================================

[ lbl(frame16),
  frame_var('FT',Ft_Length_Num5_Num5),
  cg_type(Mat,'Mat'),
  cg_type(Ft_Length_Num5_Num5,'Length'),
  cg_quantz(5,Ft_Length_Num5_Num5),
  cg_holds('SizeOf',Mat,Ft_Length_Num5_Num5) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[LENGTH:@5ft.]<-(SizeOf)-[Mat]").
% ===========================================

[ lbl(frame17),
  frame_var('FT',Ft_Length_Num5_Num5),
  cg_type(Mat,'Mat'),
  cg_type(Ft_Length_Num5_Num5,'Length'),
  cg_quantz(5,Ft_Length_Num5_Num5),
  cg_holds('SizeOf',Mat,Ft_Length_Num5_Num5) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_NONE:{}]-(On)->[Mat]").
% ===========================================

[ lbl(frame18),
  cg_type(Mat,'Mat'),
  cg_count(Cat_Set_None,0,0),
  cg_type(Cat_Set_None,'Cat_Set_None'),
  cg_holds('On',Cat_Set_None,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]").
% ===========================================

[ lbl(frame19),
  cg_type(Mat,'Mat'),
  cg_type(Set_Cats_One_Or_More,'Cats_One_Or_More'),
  cg_quantz(set,Set_Cats_One_Or_More),
  cg_holds('On',Set_Cats_One_Or_More,Mat),
  cg_count(Set_Cats_One_Or_More,1,Cg_Count) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_FIVE:{*}@5]-(On)->[Mat]").
% ===========================================

[ lbl(frame20),
  cg_type(Mat,'Mat'),
  cg_quantz(set,Set_Cat_Five_Num5_Num5),
  cg_count(Set_Cat_Five_Num5_Num5,1,Cg_Count),
  cg_type(Set_Cat_Five_Num5_Num5,'Cat_Five'),
  cg_quantz(5,Set_Cat_Five_Num5_Num5),
  cg_holds('On',Set_Cat_Five_Num5_Num5,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_FM:{Felix,Moris}]-(On)->[Mat]").
% ===========================================

[ lbl(frame21),
  cg_values(
     FelixMoris_Set_Cat_Fm,
     [ 'Felix',
       'Moris' ]),
  cg_type(Mat,'Mat'),
  cg_quantz(set,FelixMoris_Set_Cat_Fm),
  cg_count(FelixMoris_Set_Cat_Fm,2,Cg_Count),
  cg_type(FelixMoris_Set_Cat_Fm,'Cat_Fm'),
  cg_holds('On',FelixMoris_Set_Cat_Fm,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]").
% ===========================================

[ lbl(frame22),
  cg_values(
     FelixMoris_Cat_Set_Min_Two,
     [ 'Felix',
       'Moris' ]),
  cg_type(Mat,'Mat'),
  cg_quantz(set,FelixMoris_Cat_Set_Min_Two),
  cg_count(FelixMoris_Cat_Set_Min_Two,2,Cg_Count),
  cg_type(FelixMoris_Cat_Set_Min_Two,'Cat_Set_Min_Two'),
  cg_holds('On',FelixMoris_Cat_Set_Min_Two,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]").
% ===========================================

[ lbl(frame23),
  cg_values(
     FelixMoris_Cat_Set_Five_Num5_Num5,
     [ 'Felix',
       'Moris' ]),
  cg_type(Mat,'Mat'),
  cg_quantz(set,FelixMoris_Cat_Set_Five_Num5_Num5),
  cg_count(FelixMoris_Cat_Set_Five_Num5_Num5,2,Cg_Count),
  cg_type(FelixMoris_Cat_Set_Five_Num5_Num5,'Cat_Set_Five'),
  cg_quantz(5,FelixMoris_Cat_Set_Five_Num5_Num5),
  cg_holds('On',FelixMoris_Cat_Set_Five_Num5_Num5,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"['Man':imad]<-agnt-['Drive']-obj->['Car']").
% ===========================================

[ lbl(frame24),
  frame_var('IMAD',Imad_Man),
  cg_type(Car,'Car'),
  cg_type(Drive,'Drive'),
  cg_type(Imad_Man,'Man'),
  cg_holds(obj,Drive,Car),
  cg_holds(agnt,Drive,Imad_Man) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat#1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

[ lbl(frame25),
  cg_type(Color,'Color'),
  cg_type(Mat,'Mat'),
  cg_type(Cat,'Cat'),
  cg_holds('On',Cat,Mat),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Color,'Color#1'),
  cg_equal(Mat,'Mat#1'),
  cg_equal(Cat,'Cat#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]").
% ===========================================

[ lbl(frame26),
  cg_type(Mat,'Mat'),
  cg_type(C1,'C1'),
  cg_quantz(e,X),
  cg_holds('On',C1,Mat),
  cg_holds('Attrib',X,C1),
  cg_type(X,'Cat') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat]").
% ===========================================

[ lbl(frame27),
  cg_type(Mat,'Mat'),
  cg_quantz(e,X),
  cg_holds('On',X,Mat),
  cg_type(X,'Cat') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[*MatC]").
% ===========================================

[ lbl(frame28),
  frame_var('MATC',Matc),
  cg_quantz(e,X),
  cg_holds('On',X,Matc),
  cg_type(X,'Cat') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat: *MatC]").
% ===========================================

[ lbl(frame29),
  frame_var('MATC',Mat),
  cg_type(Mat,'Mat'),
  cg_quantz(e,X),
  cg_holds('On',X,Mat),
  cg_type(X,'Cat') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

[ lbl(frame30),
  frame_var('KARIM',Karim_Man),
  cg_type(Water,'Water'),
  cg_type(Drink,'Drink'),
  cg_type(Karim_Man,'Man'),
  cg_holds(obj,Drink,Water),
  cg_holds(agnt,Drink,Karim_Man) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat #1]<- (on)- [Cat #1]").
% ===========================================

[ lbl(frame31),
  cg_type(Cat,'Cat'),
  cg_type(Mat,'Mat'),
  cg_holds(on,Cat,Mat),
  cg_equal(Cat,'Cat#1'),
  cg_equal(Mat,'Mat#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]<-(On)-[Cat: ?x]").
% ===========================================

[ lbl(frame32),
  cg_type(Mat,'Mat'),
  cg_quantz(e,X),
  cg_holds('On',X,Mat),
  cg_type(X,'Cat') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Color #1]<-(Attrib)-[Mat #1]").
% ===========================================

[ lbl(frame33),
  cg_type(Mat,'Mat'),
  cg_type(Color,'Color'),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Mat,'Mat#1'),
  cg_equal(Color,'Color#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

[ lbl(frame34),
  cg_type(Color,'Color'),
  cg_type(Mat,'Mat'),
  cg_type(Cat,'Cat'),
  cg_holds('On',Cat,Mat),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Color,'Color#1'),
  cg_equal(Mat,'Mat#1'),
  cg_equal(Cat,'Cat#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

[ lbl(frame35),
  frame_var('KARIM',Karim_Man),
  cg_type(Water,'Water'),
  cg_type(Drink,'Drink'),
  cg_type(Karim_Man,'Man'),
  cg_holds(obj,Drink,Water),
  cg_holds(agnt,Drink,Karim_Man) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]").
% ===========================================

[ lbl(frame36),
  cg_type(Cat,'Cat'),
  cg_type(Mat,'Mat'),
  cg_type(Color,'Color'),
  cg_holds(on,Cat,Mat),
  cg_holds('Attrib',Mat,Color),
  cg_equal(Cat,'Cat#1'),
  cg_equal(Mat,'Mat#1'),
  cg_equal(Color,'Color#1') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: @every]->(On)->[Mat]").
% ===========================================

[ lbl(frame37),
  cg_type(Mat,'Mat'),
  cg_type(Every_Cat,'Cat'),
  cg_quantz(every,Every_Cat),
  cg_holds('On',Every_Cat,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
% ===========================================

[ lbl(frame38),
  cg_type(Mat,'Mat'),
  cg_type(Sit,'Sit'),
  cg_type(Cat,'Cat'),
  cg_holds('Stat',Cat,Sit),
  cg_holds('Loc',Sit,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT]").
% ===========================================

[ lbl(frame39),
  cg_type(Mat,'Mat'),
  cg_type(Sit,'Sit'),
  cg_type(Cat,'Cat'),
  cg_holds('Stat',Cat,Sit),
  cg_holds('Loc',Sit,Mat) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [Drive *x] [Person: Bob] [City: "St. Louis"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x "St. Louis") (Thme ?x ?y) (Poss Bob ?y)").
% ===========================================

[ textOf(C34_St_C46_C32_Louis_C34_City,"St. Louis"),
  lbl(frame40),
  frame_var('Y',Y_Chevy),
  frame_var('X',X_Drive),
  cg_type(Y_Chevy,'Chevy'),
  cg_type(C34_St_C46_C32_Louis_C34_City,'City'),
  cg_type(Bob_Person,'Person'),
  cg_type(X_Drive,'Drive'),
  cg_name(Bob_Person,'Bob'),
  cg_holds('Thme',X_Drive,Y_Chevy),
  cg_holds('Poss',Bob_Person,Y_Chevy),
  cg_holds('Dest',X_Drive,"St. Louis"),
  cg_holds('Agnt',X_Drive,Bob_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) ->
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]").
% ===========================================

[ lbl(frame41),
  frame_var('FT',Ft_Length_Num5_Num5),
  cg_count(Moris_Set_Cats_One_Or_More,1,Cg_Count),
  cg_values(
     Moris_Set_Cats_One_Or_More,
     ['Moris']),
  cg_values(
     MorisFelix_Set_Cats_Two,
     [ 'Moris',
       'Felix' ]),
  cg_type(Moris_Set_Cats_One_Or_More,'Cats_One_Or_More'),
  cg_type(MorisFelix_Set_Cats_Two,'Cats_Two'),
  cg_type(Set_Cat5_Num5_Num5,'Cat5'),
  cg_type(Cat_Set,'Cat_Set'),
  cg_type(Ft_Length_Num5_Num5,'Length'),
  cg_type(Moris_Named_Cat,'Named_Cat'),
  cg_type(The_Cat,'The_Cat'),
  cg_type(A_Cat,'A_Cat'),
  cg_quantz(set,Moris_Set_Cats_One_Or_More),
  cg_quantz(set,MorisFelix_Set_Cats_Two),
  cg_quantz(set,Set_Cat5_Num5_Num5),
  cg_quantz(set,Cat_Set),
  cg_quantz(5,Set_Cat5_Num5_Num5),
  cg_quantz(5,Ft_Length_Num5_Num5),
  cg_name(Moris_Named_Cat,'Moris'),
  cg_holds('KnowsAbout',MorisFelix_Set_Cats_Two,Moris_Set_Cats_One_Or_More),
  cg_holds('KnowsAbout',Set_Cat5_Num5_Num5,MorisFelix_Set_Cats_Two),
  cg_holds('KnowsAbout',Cat_Set,Set_Cat5_Num5_Num5),
  cg_holds('KnowsAbout',Ft_Length_Num5_Num5,Cat_Set),
  cg_holds('KnowsAbout',Moris_Named_Cat,Ft_Length_Num5_Num5),
  cg_holds('KnowsAbout',The_Cat,Moris_Named_Cat),
  cg_holds('KnowsAbout',A_Cat,The_Cat),
  cg_equal(The_Cat,'The_Cat#666'),
  cg_count(MorisFelix_Set_Cats_Two,2,Cg_Count18),
  cg_count(Set_Cat5_Num5_Num5,1,Cg_Count19),
  cg_count(Cat_Set,1,Cg_Count20) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Relation: *r] (Familial ?r) (#?r Bob Sue)").
% ===========================================

[ lbl(frame42),
  frame_var('R',Relation),
  cg_type(Relation,'Relation'),
  cg_holds('Familial',Relation),
  cg_holds(R,'Bob','Sue'),
  R=Relation ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").
% ===========================================

[ lbl(frame43),
  cg_type(Mat,'Mat'),
  cg_type(Cat,'Cat'),
  cg_type(Sit,'Sit'),
  cg_holds('Stat',Cat,Sit),
  cg_holds('Loc',Cat,Mat) ].
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(Attrib)-> [Color #1]").
% ===========================================

cg_holds('Attrib',X,Color).
cg_equal(Color,'Color#1').
cg_type(Color,'Color').
cg_quantz(e,X).
lbl(frame44).
% pred_cg(call_cg_real, xtext("?x -(Attrib)-> [Color #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

cg_holds('Attrib',Mat,Color).
cg_equal(Color,'Color#1').
cg_type(Color,'Color').
cg_holds('On',X,Mat).
cg_equal(Mat,'Mat#1').
cg_type(Mat,'Mat').
cg_quantz(e,X).
lbl(frame45).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]").
% ===========================================

cg_holds('On',X,Mat).
cg_equal(Mat,'Mat#1').
cg_type(Mat,'Mat').
cg_quantz(e,X).
lbl(frame46).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"[?x] -(Attrib)-> [Color #1]").
% ===========================================

cg_holds('Attrib',X,Color).
cg_equal(Color,'Color#1').
cg_type(Color,'Color').
cg_type(
   X,
   ?('X')).
lbl(frame47).
% pred_cg(call_cg_real, xtext("[?x] -(Attrib)-> [Color #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

cg_holds('Attrib',Mat,Color).
cg_equal(Color,'Color#1').
cg_type(Color,'Color').
cg_holds('On',X,Mat).
cg_equal(Mat,'Mat#1').
cg_type(Mat,'Mat').
cg_type(
   X,
   ?('X')).
lbl(frame48).
% pred_cg(call_cg_real, xtext("[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"[Mat ?x]-(Attrib)->[Color #1]").
% ===========================================

cg_holds('Attrib',X,Color).
cg_equal(Color,'Color#1').
cg_type(Color,'Color').
cg_quantz(e,X).
cg_type(X,'Mat').
lbl(frame49).
% pred_cg(call_cg_real, xtext("[Mat ?x]-(Attrib)->[Color #1]")).


```
% ===========================================
% ?- pred_cg(call_cg_real,"[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]").
% ===========================================

cg_holds('Attrib',Mat,Color).
cg_equal(Color,'Color#2').
cg_type(Color,'Color').
cg_holds('On',X,Mat).
cg_equal(Mat,'Mat#1').
cg_type(Mat,'Mat').
cg_quantz(e,X).
cg_type(X,'Cat').
lbl(frame50).
% pred_cg(call_cg_real, xtext("[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]")).


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
% ===========================================

[ lbl(frame51),
  cg_type(Statement,statement),
  cg_type(A,a),
  cg_holds(belives,A,Statement),
  'GRAPH'(
     Statement,
     [ cg_holds('On',Every_Cat,Mat),
       cg_type(Mat,'Mat'),
       cg_quantz(every,Every_Cat),
       cg_type(Every_Cat,'Cat'),
       lbl(frame52) ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").
% ===========================================

[ lbl(frame53),
  cg_type(Statement2,statement2),
  cg_type(A,a),
  cg_holds(belives,A,Statement2),
  '='(
     Statement2,
     'GRAPH'(
        [ cg_holds('On',Every_Cat,Mat),
          cg_type(Mat,'Mat'),
          cg_quantz(every,Every_Cat),
          cg_type(Every_Cat,'Cat'),
          lbl(frame54) ])) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"

[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").
% ===========================================

[ lbl(frame55),
  cg_type(Bus,'Bus'),
  cg_type(Boston_City,'City'),
  cg_type(John_Person,'Person'),
  cg_type(Go,'Go'),
  cg_name(Boston_City,'Boston'),
  cg_name(John_Person,'John'),
  cg_holds('Inst',Boston_City,Bus),
  cg_holds('Dest',John_Person,Boston_City),
  cg_holds('Agnt',Go,John_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [Person: John2] <- (Agnt) -
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").
% ===========================================

[ lbl(frame56),
  cg_type(Go2,'Go2'),
  cg_type(Bus2,'Bus2'),
  cg_type(Boston2_City,'City'),
  cg_type(John2_Person,'Person'),
  cg_name(Boston2_City,'Boston2'),
  cg_name(John2_Person,'John2'),
  cg_holds('Inst',Go2,Bus2),
  cg_holds('Dest',Bus2,Boston2_City),
  cg_holds('Agnt',Boston2_City,John2_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Begin]-
        -obj->[Session],
        -srce->[Proposition =
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John]
               ],
        -agnt->[Person : John]").
% ===========================================

[ lbl(frame57),
  cg_type(John_Person9,'Person'),
  cg_type(Proposition,'Proposition'),
  cg_type(Session,'Session'),
  cg_type(Begin,'Begin'),
  cg_name(John_Person9,'John'),
  cg_holds(srce,Begin,Proposition),
  cg_holds(obj,Begin,Session),
  cg_holds(agnt,Begin,John_Person9),
  '='(
     Proposition,
     'GRAPH'(
        [ cg_holds(agnt,Press,John_Person),
          cg_name(John_Person,'John'),
          cg_type(John_Person,'Person'),
          cg_holds(partOf,Enter_Key,Keyboard),
          cg_type(Keyboard,'Keyboard'),
          cg_holds(obj,Press,Enter_Key),
          frame_var('ENTER',Enter_Key),
          cg_type(Enter_Key,'Key'),
          cg_type(Press,'Press'),
          lbl(frame58) ])) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
 [a] - (belives) ->
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").
% ===========================================

[ lbl(frame59),
  cg_type(Statement,statement),
  cg_type(A,a),
  cg_holds(belives,A,Statement),
  '='(
     Statement,
     'GRAPH'(
        [ cg_holds('Inst',Boston2_City,Bus2),
          cg_type(Bus2,'Bus2'),
          cg_holds('Dest',John2_Person,Boston2_City),
          cg_name(Boston2_City,'Boston2'),
          cg_type(Boston2_City,'City'),
          cg_holds('Agnt',Go2,John2_Person),
          cg_name(John2_Person,'John2'),
          cg_type(John2_Person,'Person'),
          cg_type(Go2,'Go2'),
          lbl(frame60) ])) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)").
% ===========================================

[ lbl(frame61),
  frame_var('Z',Z_Boston_City),
  frame_var('Y',Y_John_Person),
  frame_var('X',X_Go),
  frame_var('W',W_Bus),
  cg_type(W_Bus,'Bus'),
  cg_type(Z_Boston_City,'City'),
  cg_type(Y_John_Person,'Person'),
  cg_type(X_Go,'Go'),
  cg_name(Z_Boston_City,'Boston'),
  cg_name(Y_John_Person,'John'),
  cg_holds('Inst',X_Go,W_Bus),
  cg_holds('Dest',X_Go,Z_Boston_City),
  cg_holds('Agnt',X_Go,Y_John_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]").
% ===========================================

[ lbl(frame62),
  frame_var('RED',Red_Woman),
  frame_var('KARIM',Karim_Man),
  cg_type(Table,table),
  cg_type(Apple,'Apple'),
  cg_type(Eat,'Eat'),
  cg_type(Karim_Man,'Man'),
  cg_type(Red_Woman,'Woman'),
  cg_holds(on,Apple,Table),
  cg_holds(obj,Eat,Apple),
  cg_holds(knows,Karim_Man,Red_Woman),
  cg_holds(agnt,Eat,Karim_Man) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]").
% ===========================================

[ lbl(frame63),
  cg_type(Sailor,'Sailor'),
  cg_type(Marry,'Marry'),
  cg_type(
     X,
     ?('X')),
  cg_holds('Thme',Marry,Sailor),
  cg_holds('Agnt',Marry,X) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]").
% ===========================================

[ lbl(frame64),
  frame_var('X',X_Mary_Person),
  cg_type(Situation,'Situation'),
  cg_type(Want,'Want'),
  cg_type(X_Mary_Person,'Person'),
  cg_name(X_Mary_Person,'Mary'),
  cg_holds('Thme',Want,Situation),
  cg_holds('Expr',Want,X_Mary_Person),
  'GRAPH'(
     Situation,
     [ cg_holds('Thme',Marry,Sailor),
       cg_type(Sailor,'Sailor'),
       cg_holds('Agnt',Marry,X),
       cg_type(Marry,'Marry'),
       cg_type(X,X_Mary_Person),
       lbl(frame65) ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

[ lbl(frame66),
  cg_type(Proposition,'Proposition'),
  'GRAPH'(
     Proposition,
     [ cg_holds('Thme',Want,Situation),
       'GRAPH'(
          Situation,
          [ cg_holds('Thme',Marry,Sailor),
            cg_type(Sailor,'Sailor'),
            cg_holds('Agnt',Marry,X),
            cg_type(Marry,'Marry'),
            cg_type(X,X_Mary_Person),
            lbl(frame68) ]),
       cg_type(Situation,'Situation'),
       cg_holds('Expr',Want,X_Mary_Person),
       cg_type(Want,'Want'),
       frame_var('X',X_Mary_Person),
       cg_name(X_Mary_Person,'Mary'),
       cg_type(X_Mary_Person,'Person'),
       lbl(frame67) ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

[ lbl(frame69),
  cg_type(Proposition,'Proposition'),
  cg_type(Believe,'Believe'),
  cg_type(Tom_Person,'Person'),
  cg_name(Tom_Person,'Tom'),
  cg_holds('Thme',Believe,Proposition),
  cg_holds('Expr',Believe,Tom_Person),
  'GRAPH'(
     Proposition,
     [ cg_holds('Thme',Want,Situation),
       'GRAPH'(
          Situation,
          [ cg_holds('Thme',Marry,Sailor),
            cg_type(Sailor,'Sailor'),
            cg_holds('Agnt',Marry,X),
            cg_type(Marry,'Marry'),
            cg_type(X,X_Mary_Person3),
            lbl(frame71) ]),
       cg_type(Situation,'Situation'),
       cg_holds('Expr',Want,X_Mary_Person3),
       cg_type(Want,'Want'),
       frame_var('X',X_Mary_Person3),
       cg_name(X_Mary_Person3,'Mary'),
       cg_type(X_Mary_Person3,'Person'),
       lbl(frame70) ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").
% ===========================================

[ lbl(frame72),
  cg_type(Proposition,'Proposition'),
  cg_type(Believe,'Believe'),
  cg_type(Tom_Person,'Person'),
  cg_name(Tom_Person,'Tom'),
  cg_holds('Thme',Believe,Proposition),
  cg_holds('Expr',Believe,Tom_Person),
  'GRAPH'(
     Proposition,
     [ cg_holds('Thme',Want,Situation),
       'GRAPH'(
          Situation,
          [ cg_holds('Thme',Marry,Sailor),
            cg_type(Sailor,'Sailor'),
            cg_holds('Agnt',Marry,X),
            cg_type(Marry,'Marry'),
            cg_type(X,X_Mary_Person3),
            lbl(frame74) ]),
       cg_type(Situation,'Situation'),
       cg_holds('Expr',Want,X_Mary_Person3),
       cg_type(Want,'Want'),
       frame_var('X',X_Mary_Person3),
       cg_name(X_Mary_Person3,'Mary'),
       cg_type(X_Mary_Person3,'Person'),
       lbl(frame73) ]) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ARRIVAL-DATE(a) IS [UNIV:*a].").
% ===========================================

[ lbl(frame75),
  frame_var('A',A_Univ),
  cg_type(A_Univ,'Univ'),
  cg_holds(A_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c].").
% ===========================================

[ lbl(frame76),
  frame_var('C',C_Univ),
  cg_type(C_Univ,'Univ'),
  cg_holds(C_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c]").
% ===========================================

[ lbl(frame77),
  frame_var('C',C_Univ),
  cg_type(C_Univ,'Univ'),
  cg_holds(C_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]").
% ===========================================

[ lbl(frame78),
  frame_var('C',C_Elephant),
  cg_type(Circus,'Circus'),
  cg_type(Perform,'Perform'),
  cg_type(C_Elephant,'Elephant'),
  cg_holds('Loc',Perform,Circus),
  cg_holds('Agnt',Perform,C_Elephant) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus])
").
% ===========================================

[ lbl(frame79),
  frame_var('X',X_Go),
  cg_type(Bus,'Bus'),
  cg_type(Boston_City,'City'),
  cg_type(John_Person,'Person'),
  cg_type(X_Go,'Go'),
  cg_name(Boston_City,'Boston'),
  cg_name(John_Person,'John'),
  cg_holds('Inst',X_Go,Bus),
  cg_holds('Dest',X_Go,Boston_City),
  cg_holds('Agnt',X_Go,John_Person) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

[ lbl(frame80),
  frame_var('C',C_Elephant),
  cg_type(Circus,'Circus'),
  cg_type(Perform,'Perform'),
  cg_type(C_Elephant,'Elephant'),
  cg_holds('Loc',Perform,Circus),
  cg_holds('Agnt',Perform,C_Elephant),
  cg_holds(C_Elephant) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

[ lbl(frame81),
  frame_var('C',C_Elephant),
  cg_type(Circus,'Circus'),
  cg_type(Perform,'Perform'),
  cg_type(C_Elephant,'Elephant'),
  cg_holds('Loc',Perform,Circus),
  cg_holds('Agnt',Perform,C_Elephant),
  cg_holds(C_Elephant) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE DEPARTURE-DATE(d) IS [UNIV:*d].").
% ===========================================

[ lbl(frame82),
  frame_var('D',D_Univ),
  cg_type(D_Univ,'Univ'),
  cg_holds(D_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ELEPHANT(e) IS [UNIV:*e].").
% ===========================================

[ lbl(frame83),
  frame_var('E',E_Univ),
  cg_type(E_Univ,'Univ'),
  cg_holds(E_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE HOTEL(h) IS [UNIV:*h].").
% ===========================================

[ lbl(frame84),
  frame_var('H',H_Univ),
  cg_type(H_Univ,'Univ'),
  cg_holds(H_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.").
% ===========================================

[ lbl(frame85),
  frame_var('RESERVATION_NO',Reservation),
  cg_type(Departure_Date,'Departure_Date'),
  cg_type(Arrival_Date,'Arrival_Date'),
  cg_type(Time_Period,'Time_Period'),
  cg_type(Hotel,'Hotel'),
  cg_type(Room,'Room'),
  cg_type(Person,'Person'),
  cg_type(Reservation,'Reservation'),
  cg_holds('Untl',Arrival_Date,Departure_Date),
  cg_holds('Strt',Reservation,Arrival_Date),
  cg_holds('Reservation_No'),
  cg_holds('Rcpt',Reservation,Person),
  cg_holds('Obj',Person,Room),
  cg_holds('Loc',Room,Hotel),
  cg_holds('Dur',Hotel,Time_Period) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PERFORM(p) IS [UNIV:*p].").
% ===========================================

[ lbl(frame86),
  frame_var('P',P_Univ),
  cg_type(P_Univ,'Univ'),
  cg_holds(P_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PERSON(p) IS [UNIV:*p].").
% ===========================================

[ lbl(frame87),
  frame_var('P',P_Univ),
  cg_type(P_Univ,'Univ'),
  cg_holds(P_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PROPOSITION(p) IS [UNIV:*p].").
% ===========================================

[ lbl(frame88),
  frame_var('P',P_Univ),
  cg_type(P_Univ,'Univ'),
  cg_holds(P_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE RESERVATION(r) IS [UNIV:*r].").
% ===========================================

[ lbl(frame89),
  frame_var('R',R_Univ),
  cg_type(R_Univ,'Univ'),
  cg_holds(R_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ROOM(r) IS [UNIV:*r].").
% ===========================================

[ lbl(frame90),
  frame_var('R',R_Univ),
  cg_type(R_Univ,'Univ'),
  cg_holds(R_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE TIME-PERIOD(t) IS [UNIV:*t].").
% ===========================================

[ lbl(frame91),
  frame_var('T',T_Univ),
  cg_type(T_Univ,'Univ'),
  cg_holds(T_Univ) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

[ lbl(frame92),
  cg_type(March_18_1983_Departure_Date,'Departure_Date'),
  cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
  cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
  cg_type(Shelburne_Hotel,'Hotel'),
  cg_type(Q2_Room,'Room'),
  cg_type(John_Sowa_Person,'Person'),
  cg_type(Reservation,'Reservation'),
  cg_quantz(4,Nights_Time_Period_Num4_Num4),
  cg_name(March_18_1983_Departure_Date,'March_18_1983'),
  cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
  cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
  cg_name(Shelburne_Hotel,'Shelburne'),
  cg_name(Q2_Room,'Q2'),
  cg_name(John_Sowa_Person,'John_Sowa'),
  cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
  cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
  cg_holds('Rcpt',Reservation,John_Sowa_Person),
  cg_holds('Obj',John_Sowa_Person,Q2_Room),
  cg_holds('Loc',Q2_Room,Shelburne_Hotel),
  cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
  cg_equal(Reservation,'Reservation#316209') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

[ lbl(frame93),
  cg_type(March_18_1983_Departure_Date,'Departure_Date'),
  cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
  cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
  cg_type(Shelburne_Hotel,'Hotel'),
  cg_type(Q2_Room,'Room'),
  cg_type(John_Sowa_Person,'Person'),
  cg_type(Reservation,'Reservation'),
  cg_quantz(4,Nights_Time_Period_Num4_Num4),
  cg_name(March_18_1983_Departure_Date,'March_18_1983'),
  cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
  cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
  cg_name(Shelburne_Hotel,'Shelburne'),
  cg_name(Q2_Room,'Q2'),
  cg_name(John_Sowa_Person,'John_Sowa'),
  cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
  cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
  cg_holds('Rcpt',Reservation,John_Sowa_Person),
  cg_holds('Obj',John_Sowa_Person,Q2_Room),
  cg_holds('Loc',Q2_Room,Shelburne_Hotel),
  cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
  cg_equal(Reservation,'Reservation#316209') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.").
% ===========================================

[ lbl(frame94),
  cg_type(March_18_1983_Departure_Date,'Departure_Date'),
  cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
  cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
  cg_type(Shelburne_Hotel,'Hotel'),
  cg_type(Q2_Room,'Room'),
  cg_type(John_Sowa_Person,'Person'),
  cg_type(Reservation,'Reservation'),
  cg_quantz(4,Nights_Time_Period_Num4_Num4),
  cg_name(March_18_1983_Departure_Date,'March_18_1983'),
  cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
  cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
  cg_name(Shelburne_Hotel,'Shelburne'),
  cg_name(Q2_Room,'Q2'),
  cg_name(John_Sowa_Person,'John_Sowa'),
  cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
  cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
  cg_holds('Rcpt',Reservation,John_Sowa_Person),
  cg_holds('Obj',John_Sowa_Person,Q2_Room),
  cg_holds('Loc',Q2_Room,Shelburne_Hotel),
  cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
  Cg_Holds#316209,
  cg_holds(Cg_Holds),
  cg_equal(Reservation,'Reservation#316209') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.").
% ===========================================

[ lbl(frame95),
  cg_type(March_19_1983_Departure_Date,'Departure_Date'),
  cg_type(March_12_1983_Arrival_Date,'Arrival_Date'),
  cg_type(Nights_Time_Period_Num7_Num7,'Time_Period'),
  cg_type(Sidney_Hotel,'Hotel'),
  cg_type(Q3_Room,'Room'),
  cg_type(John_Esch_Person,'Person'),
  cg_type(Reservation,'Reservation'),
  cg_quantz(7,Nights_Time_Period_Num7_Num7),
  cg_name(March_19_1983_Departure_Date,'March_19_1983'),
  cg_name(March_12_1983_Arrival_Date,'March_12_1983'),
  cg_name(Nights_Time_Period_Num7_Num7,'Nights'),
  cg_name(Sidney_Hotel,'Sidney'),
  cg_name(Q3_Room,'Q3'),
  cg_name(John_Esch_Person,'John_Esch'),
  cg_holds('Untl',March_12_1983_Arrival_Date,March_19_1983_Departure_Date),
  cg_holds('Strt',Reservation,March_12_1983_Arrival_Date),
  cg_holds('Rcpt',Reservation,John_Esch_Person),
  cg_holds('Obj',John_Esch_Person,Q3_Room),
  cg_holds('Loc',Q3_Room,Sidney_Hotel),
  cg_holds('Dur',Sidney_Hotel,Nights_Time_Period_Num7_Num7),
  Cg_Holds#316210,
  cg_holds(Cg_Holds),
  cg_equal(Reservation,'Reservation#316210') ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].").
% ===========================================

[ lbl(frame96),
  cg_type(Flying_Tigers_Circus,'Circus'),
  cg_type(Set_Perform,'Perform'),
  cg_type(Elephant,'Elephant'),
  cg_quantz(set,Set_Perform),
  cg_name(Flying_Tigers_Circus,'Flying_Tigers'),
  cg_holds('Loc',Set_Perform,Flying_Tigers_Circus),
  cg_holds('Agnt',Set_Perform,Elephant),
  Bumbo#'Bumbo',
  cg_holds(Bumbo),
  cg_equal(Elephant,'Elephant#Bumbo'),
  cg_count(Set_Perform,1,Cg_Count) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL CIRCUS-ELEPHANT(#JUMBO) IS
[ELEPHANT:#JUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Barnum & Bailey].").
% ===========================================

[ lbl(frame97),
  cg_type(Barnum_C38_Bailey_Circus,'Circus'),
  cg_type(Set_Perform,'Perform'),
  cg_type(Elephant,'Elephant'),
  cg_quantz(set,Set_Perform),
  cg_name(Barnum_C38_Bailey_Circus,'Barnum_&_Bailey'),
  cg_holds('Loc',Set_Perform,Barnum_C38_Bailey_Circus),
  cg_holds('Agnt',Set_Perform,Elephant),
  Jumbo#'Jumbo',
  cg_holds(Jumbo),
  cg_equal(Elephant,'Elephant#Jumbo'),
  cg_count(Set_Perform,1,Cg_Count) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Leopard::[Animal : x]-isA->[Leopard] :-
   Mammal::[Animal : x]-isA->[Mammal],
   Carnivorous::[Animal : x]-isA->[Carnivorous],
   Fact::[Animal : x]-colorOf->[Color]-attr->[Wild],
   Fact::[Animal : x]-partOf->[Component]-attr->[Dark]. ").
% ===========================================

[ named_graph(
     'Leopard',
     [ preconds(
          [ [ values_from('Mammal'),
              named_graph(
                 'Mammal',
                 [ values_from('Carnivorous'),
                   named_graph(
                      'Carnivorous',
                      [ values_from('Fact'),
                        named_graph(
                           'Fact',
                           [ values_from('Fact'),
                             named_graph(
                                'Fact',
                                [ cg_holds(attr,Component,Dark),
                                  cg_type(Dark,'Dark'),
                                  cg_holds(partOf,X_Animal13,Component),
                                  cg_type(Component,'Component'),
                                  frame_var('X',X_Animal13),
                                  cg_type(X_Animal13,'Animal'),
                                  lbl(frame104) ]),
                             cg_holds(attr,Color,Wild),
                             cg_type(Wild,'Wild'),
                             cg_holds(colorOf,X_Animal9,Color),
                             cg_type(Color,'Color'),
                             frame_var('X',X_Animal9),
                             cg_type(X_Animal9,'Animal'),
                             lbl(frame103) ]),
                        cg_holds(isA,X_Animal6,Carnivorous),
                        cg_type(Carnivorous,'Carnivorous'),
                        frame_var('X',X_Animal6),
                        cg_type(X_Animal6,'Animal'),
                        lbl(frame102) ]),
                   cg_holds(isA,X_Animal3,Mammal),
                   cg_type(Mammal,'Mammal'),
                   frame_var('X',X_Animal3),
                   cg_type(X_Animal3,'Animal'),
                   lbl(frame101) ]),
              lbl(frame100) ] ]),
       cg_holds(isA,X_Animal13,Leopard),
       cg_type(Leopard,'Leopard'),
       frame_var('X',X_Animal13),
       cg_type(X_Animal13,'Animal'),
       lbl(frame99) ]),
  values_from('Leopard'),
  lbl(frame98) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Mammal::[Animal : x]-isA->[Mammal] :-
        Fact::[Animal : x]-poss->[Hair].
").
% ===========================================

[ named_graph(
     'Mammal',
     [ preconds(
          [ [ values_from('Fact'),
              named_graph(
                 'Fact',
                 [ cg_holds(poss,X_Animal3,Hair),
                   cg_type(Hair,'Hair'),
                   frame_var('X',X_Animal3),
                   cg_type(X_Animal3,'Animal'),
                   lbl(frame108) ]),
              lbl(frame107) ] ]),
       cg_holds(isA,X_Animal3,Mammal),
       cg_type(Mammal,'Mammal'),
       frame_var('X',X_Animal3),
       cg_type(X_Animal3,'Animal'),
       lbl(frame106) ]),
  values_from('Mammal'),
  lbl(frame105) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]<-agnt-[Eat]-obj->[Meat].
").
% ===========================================

[ named_graph(
     'Carnivorous',
     [ preconds(
          [ [ values_from('Fact'),
              named_graph(
                 'Fact',
                 [ cg_holds(obj,Eat,Meat),
                   cg_type(Meat,'Meat'),
                   cg_holds(agnt,Eat,X_Animal3),
                   cg_type(Eat,'Eat'),
                   frame_var('X',X_Animal3),
                   cg_type(X_Animal3,'Animal'),
                   lbl(frame112) ]),
              lbl(frame111) ] ]),
       cg_holds(isA,X_Animal3,Carnivorous),
       cg_type(Carnivorous,'Carnivorous'),
       frame_var('X',X_Animal3),
       cg_type(X_Animal3,'Animal'),
       lbl(frame110) ]),
  values_from('Carnivorous'),
  lbl(frame109) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]-poss->[Teeth]-attr->[Sharp],
   Fact::[Animal : x]-poss->[Claw],
   Fact::[Animal : x]-has->[Eyes]-attr->[Forward].
").
% ===========================================

[ named_graph(
     'Carnivorous',
     [ preconds(
          [ [ values_from('Fact'),
              named_graph(
                 'Fact',
                 [ values_from('Fact'),
                   named_graph(
                      'Fact',
                      [ values_from('Fact'),
                        named_graph(
                           'Fact',
                           [ cg_holds(attr,Eyes,Forward),
                             cg_type(Forward,'Forward'),
                             cg_holds(has,X_Animal10,Eyes),
                             cg_type(Eyes,'Eyes'),
                             frame_var('X',X_Animal10),
                             cg_type(X_Animal10,'Animal'),
                             lbl(frame118) ]),
                        cg_holds(poss,X_Animal7,Claw),
                        cg_type(Claw,'Claw'),
                        frame_var('X',X_Animal7),
                        cg_type(X_Animal7,'Animal'),
                        lbl(frame117) ]),
                   cg_holds(attr,Teeth,Sharp),
                   cg_type(Sharp,'Sharp'),
                   cg_holds(poss,X_Animal3,Teeth),
                   cg_type(Teeth,'Teeth'),
                   frame_var('X',X_Animal3),
                   cg_type(X_Animal3,'Animal'),
                   lbl(frame116) ]),
              lbl(frame115) ] ]),
       cg_holds(isA,X_Animal10,Carnivorous),
       cg_type(Carnivorous,'Carnivorous'),
       frame_var('X',X_Animal10),
       cg_type(X_Animal10,'Animal'),
       lbl(frame114) ]),
  values_from('Carnivorous'),
  lbl(frame113) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Fact::[Animal : Yala]-
            <-pat-[BelongTo]-bnfcre->[Man : Robert],
            -colorOf->[Color]-attr->[Wild],
            -poss->[Teeth]-attr->[Sharp],
            -has->[Eyes]-attr->[Forward].
").
% ===========================================

[ named_graph(
     'Fact',
     [ cg_holds(attr,Eyes,Forward),
       cg_type(Forward,'Forward'),
       cg_holds(has,Yala_Animal,Eyes),
       cg_type(Eyes,'Eyes'),
       cg_holds(attr,Teeth,Sharp),
       cg_type(Sharp,'Sharp'),
       cg_holds(poss,Yala_Animal,Teeth),
       cg_type(Teeth,'Teeth'),
       cg_holds(attr,Color,Wild),
       cg_type(Wild,'Wild'),
       cg_holds(colorOf,Yala_Animal,Color),
       cg_type(Color,'Color'),
       cg_holds(bnfcre,Belongto,Robert_Man),
       cg_name(Robert_Man,'Robert'),
       cg_type(Robert_Man,'Man'),
       cg_holds(pat,Belongto,Yala_Animal),
       cg_type(Belongto,'BelongTo'),
       cg_name(Yala_Animal,'Yala'),
       cg_type(Yala_Animal,'Animal'),
       lbl(frame120) ]),
  values_from('Fact'),
  lbl(frame119) ].
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"Fact::[Animal : Yala]-poss->[Claw].").
% ===========================================

[ named_graph(
     'Fact',
     [ cg_holds(poss,Yala_Animal,Claw),
       cg_type(Claw,'Claw'),
       cg_name(Yala_Animal,'Yala'),
       cg_type(Yala_Animal,'Animal'),
       lbl(frame122) ]),
  values_from('Fact'),
  lbl(frame121) ].
```

```
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.28)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

cgpro:  ?-
```

