# logicmoo_cg
Conceptual Graph (CG) Libraries in Prolog


```
/pack/logicmoo_cg/prolog# cls ; swipl -l cgprolog.pl -t halt -g cg_reader_tests
```


```
% ===========================================

[PERSON: x] :- [CITIZEN : x].
% ===========================================

% preconds([frame_var('X', X_Citizen), cg_type(X_Citizen, 'Citizen'), lbl(frame2)]),
% frame_var('X', X_Citizen),
% cg_type(X_Citizen, 'Person'),
% lbl(frame1).
```


```
% ===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].
% ===========================================

% preconds([gc_pred('Loc', Being_Born, Oz_Country7), cg_name(Oz_Country7, 'Oz'), cg_type(Oz_Country7, 'Country'), gc_pred('Agnt', Being_Born, X_Person), cg_type(Being_Born, 'Being_Born'), frame_var('X', X_Person), cg_type(X_Person, 'Person'), lbl(frame4)]),
% gc_pred(memberOf, Oz_Country7, X_Person),
% cg_name(Oz_Country7, 'Oz'),
% cg_type(Oz_Country7, 'Country'),
% frame_var('X', X_Person),
% cg_type(X_Person, 'Citizen'),
% lbl(frame3).
```


```
% ===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: ?x]<-childOf-[PERSON: y],
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].
% ===========================================

% preconds([gc_pred(memberOf, Oz_Country10, Y_Citizen8), cg_name(Oz_Country10, 'Oz'), cg_type(Oz_Country10, 'Country'), frame_var('Y', Y_Citizen8), cg_type(Y_Citizen8, 'Citizen'), gc_pred(childOf, Y_Citizen8, X_), frame_var('Y', Y_Citizen8), cg_type(Y_Citizen8, 'Person'), cg_quantz(e, X_), cg_type(X_, 'Person'), lbl(frame6)]),
% gc_pred(memberOf, Oz_Country10, X_Citizen),
% cg_name(Oz_Country10, 'Oz'),
% cg_type(Oz_Country10, 'Country'),
% frame_var('X', X_Citizen),
% cg_type(X_Citizen, 'Citizen'),
% lbl(frame5).
```


```
% ===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].
% ===========================================

% preconds([gc_pred('Loc', Naturalize, Oz_Country7), cg_name(Oz_Country7, 'Oz'), cg_type(Oz_Country7, 'Country'), gc_pred('Rcpt', Naturalize, X_Person), cg_type(Naturalize, 'Naturalize'), frame_var('X', X_Person), cg_type(X_Person, 'Person'), lbl(frame8)]),
% gc_pred(memberOf, Oz_Country7, X_Person),
% cg_name(Oz_Country7, 'Oz'),
% cg_type(Oz_Country7, 'Country'),
% frame_var('X', X_Person),
% cg_type(X_Person, 'Citizen'),
% lbl(frame7).
```


```
% ===========================================

[PERSON : Tinman]-
              -childOf->[GIRL : Dorothy],
              <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].
% ===========================================

% gc_pred('Loc', Being_Born, Oz_Country),
% cg_name(Oz_Country, 'Oz'),
% cg_type(Oz_Country, 'Country'),
% gc_pred('Agnt', Being_Born, Tinman_Person),
% cg_type(Being_Born, 'Being_Born'),
% gc_pred(childOf, Tinman_Person, Dorothy_Girl),
% cg_name(Dorothy_Girl, 'Dorothy'),
% cg_type(Dorothy_Girl, 'Girl'),
% cg_name(Tinman_Person, 'Tinman'),
% cg_type(Tinman_Person, 'Person'),
% lbl(frame9).
```


```
% ===========================================
[Mat]1-(Attrib)->[Color #1]
% ===========================================

% gc_pred('Attrib', Mat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame10).
```


```
% ===========================================
[Mat]1-(Attrib)->[Color]2
% ===========================================

% gc_pred('Attrib', Mat, Color),
% cg_equal(Color, 'Color#2'),
% cg_type(Color, 'Color'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame11).
```


```
% ===========================================
[CAT_QUANT:@every]-(On)->[Mat]
% ===========================================

% gc_pred('On', Every_Cat_Quant, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(every, Every_Cat_Quant),
% cg_type(Every_Cat_Quant, 'Cat_Quant'),
% lbl(frame12).
```


```
% ===========================================
[A_CAT]->(On)->[Mat]
% ===========================================

% gc_pred('On', A_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_type(A_Cat, 'A_Cat'),
% lbl(frame13).
```


```
% ===========================================
[THE_CAT:#666]->(On)->[Mat]
% ===========================================

% gc_pred('On', The_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_equal(The_Cat, 'The_Cat#666'),
% cg_type(The_Cat, 'The_Cat'),
% lbl(frame14).
```


```
% ===========================================
[NAMED_CAT:Moris]->(On)->[Mat]
% ===========================================

% gc_pred('On', Moris_Named_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_name(Moris_Named_Cat, 'Moris'),
% cg_type(Moris_Named_Cat, 'Named_Cat'),
% lbl(frame15).
```


```
% ===========================================
[LENGTH:@5ft]<-(SizeOf)-[Mat]
% ===========================================

% gc_pred('SizeOf', Mat, Ft_Length),
% cg_type(Mat, 'Mat'),
% frame_var('FT', Ft_Length),
% cg_quantz(5, Ft_Length),
% cg_type(Ft_Length, 'Length'),
% lbl(frame16).
```


```
% ===========================================
[LENGTH:@5ft.]<-(SizeOf)-[Mat]
% ===========================================

% gc_pred('SizeOf', Mat, Ft_Length),
% cg_type(Mat, 'Mat'),
% frame_var('FT', Ft_Length),
% cg_quantz(5, Ft_Length),
% cg_type(Ft_Length, 'Length'),
% lbl(frame17).
```


```
% ===========================================
[CAT_SET_NONE:{}]-(On)->[Mat]
% ===========================================

% gc_pred('On', Cat_Set_None, Mat),
% cg_type(Mat, 'Mat'),
% cg_count(Cat_Set_None, 0, 0),
% cg_type(Cat_Set_None, 'Cat_Set_None'),
% lbl(frame18).
```


```
% ===========================================
[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]
% ===========================================

% gc_pred('On', Set_Cats_One_Or_More, Mat),
% cg_type(Mat, 'Mat'),
% cg_count(Set_Cats_One_Or_More, 1, _1258),
% cg_quantz(set, Set_Cats_One_Or_More),
% cg_type(Set_Cats_One_Or_More, 'Cats_One_Or_More'),
% lbl(frame19).
```


```
% ===========================================
[CAT_FIVE:{*}@5]-(On)->[Mat]
% ===========================================

% gc_pred('On', Set_Cat_Five, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(5, Set_Cat_Five),
% cg_count(Set_Cat_Five, 1, _11100),
% cg_quantz(set, Set_Cat_Five),
% cg_type(Set_Cat_Five, 'Cat_Five'),
% lbl(frame20).
```


```
% ===========================================
[CAT_FM:{Felix,Moris}]-(On)->[Mat]
% ===========================================

% gc_pred('On', FelixMoris_Set_Cat_Fm, Mat),
% cg_type(Mat, 'Mat'),
% each_of(FelixMoris_Set_Cat_Fm, ['Felix', 'Moris']),
% cg_quantz(set, FelixMoris_Set_Cat_Fm),
% cg_type(FelixMoris_Set_Cat_Fm, 'Cat_Fm'),
% lbl(frame21).
```


```
% ===========================================
[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]
% ===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Cat_Set_Min_Two',:,'{','Felix',',','Moris',',',*,'}',']',-,'(','On',')',->,'[','Mat',']']))
   ).
```


```
% ===========================================
[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]
% ===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Cat_Set_Five',:,'{','Felix',',','Moris',',',*,'}',@,5,']',-,'(','On',')',->,'[','Mat',']']))
   ).
```


```
% ===========================================
['Man':imad]<-agnt-['Drive']-obj->['Car']
% ===========================================

% gc_pred(obj, Drive, Car),
% cg_type(Car, 'Car'),
% gc_pred(agnt, Drive, Imad_Man),
% cg_type(Drive, 'Drive'),
% frame_var('IMAD', Imad_Man),
% cg_type(Imad_Man, 'Man'),
% lbl(frame24).
```


```
% ===========================================
[Cat#1]-(On)->[Mat #1]-(Attrib)->[Color #1]
% ===========================================

% gc_pred('Attrib', Cat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% gc_pred('On', Cat, Mat),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% lbl(frame25).
```


```
% ===========================================
[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]
% ===========================================

% gc_pred('On', X_, Mat),
% cg_type(Mat, 'Mat'),
% gc_pred('Attrib', X_, C1),
% cg_type(C1, 'C1'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame26).
```


```
% ===========================================
[Cat: ?x]-(On)->[Mat]
% ===========================================

% gc_pred('On', X_, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame27).
```


```
% ===========================================
[Cat: ?x]-(On)->[*MatC]
% ===========================================

% gc_pred('On', X_, MatC),
% frame_var('MATC', MatC),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame28).
```


```
% ===========================================
[Cat: ?x]-(On)->[Mat: *MatC]
% ===========================================

% gc_pred('On', X_, Mat),
% frame_var('MATC', Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame29).
```


```
% ===========================================
[Man:karim]<-agnt-[Drink]-obj->[Water]
% ===========================================

% gc_pred(obj, Drink, Water),
% cg_type(Water, 'Water'),
% gc_pred(agnt, Drink, Karim_Man),
% cg_type(Drink, 'Drink'),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% lbl(frame30).
```


```
% ===========================================
[Mat #1]<- (on)- [Cat #1]
% ===========================================

% gc_pred(on, Cat, Mat),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame31).
```


```
% ===========================================
[Mat]<-(On)-[Cat: ?x]
% ===========================================

% gc_pred('On', X_, Mat),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% cg_type(Mat, 'Mat'),
% lbl(frame32).
```


```
% ===========================================
[Color #1]<-(Attrib)-[Mat #1]
% ===========================================

% gc_pred('Attrib', Mat, Color),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% lbl(frame33).
```


```
% ===========================================
[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]
% ===========================================

% gc_pred('Attrib', Cat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% gc_pred('On', Cat, Mat),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% lbl(frame34).
```


```
% ===========================================
[Man:karim]<-agnt-[Drink]-obj->[Water]
% ===========================================

% gc_pred(obj, Drink, Water),
% cg_type(Water, 'Water'),
% gc_pred(agnt, Drink, Karim_Man),
% cg_type(Drink, 'Drink'),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% lbl(frame35).
```


```
% ===========================================
[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]
% ===========================================

% gc_pred(on, Cat, Mat),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% gc_pred('Attrib', Mat, Color),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% lbl(frame36).
```


```
% ===========================================
[Cat: @every]->(On)->[Mat]
% ===========================================

% gc_pred('On', Every_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(every, Every_Cat),
% cg_type(Every_Cat, 'Cat'),
% lbl(frame37).
```


```
% ===========================================
[CAT]->(STAT)->[SIT]->(LOC)->[MAT].
% ===========================================

% gc_pred('Loc', Sit, Mat),
% cg_type(Mat, 'Mat'),
% gc_pred('Stat', Cat, Sit),
% cg_type(Sit, 'Sit'),
% cg_type(Cat, 'Cat'),
% lbl(frame38).
```


```
% ===========================================
[CAT]->(STAT)->[SIT]->(LOC)->[MAT]
% ===========================================

% gc_pred('Loc', Sit, Mat),
% cg_type(Mat, 'Mat'),
% gc_pred('Stat', Cat, Sit),
% cg_type(Sit, 'Sit'),
% cg_type(Cat, 'Cat'),
% lbl(frame39).
```


```
% ===========================================

   [Drive *x] [Person: Bob] [City: "St. Louis"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x "St. Louis") (Thme ?x ?y) (Poss Bob ?y)
% ===========================================

% cg_holds(Poss_Bob_c63_c40_Y_c41),
% cg_name(Poss_Bob_c63_c40_Y_c41, 'Poss_Bob_?(\'Y\')'),
% cg_holds(Thme_c63_c40_X_c41_c63_c40_Y_c41),
% cg_name(Thme_c63_c40_X_c41_c63_c40_Y_c41, 'Thme_?(\'X\')_?(\'Y\')'),
% cg_holds(Dest_c63_c40_X_c41_c34_St_c46_c32_Louis_c34),
% cg_name(Dest_c63_c40_X_c41_c34_St_c46_c32_Louis_c34, 'Dest_?(\'X\')_"St. Louis"'),
% cg_holds(Agnt_c63_c40_X_c41_Bob),
% cg_name(Agnt_c63_c40_X_c41_Bob, 'Agnt_?(\'X\')_Bob'),
% frame_var('Y', Y_Chevy),
% cg_type(Y_Chevy, 'Chevy'),
% textOf(St_c46_c32_Louis_City, "St. Louis"),
% cg_type(St_c46_c32_Louis_City, 'City'),
% cg_name(Bob_Person, 'Bob'),
% cg_type(Bob_Person, 'Person'),
% frame_var('X', X_Drive),
% cg_type(X_Drive, 'Drive'),
% lbl(frame40).
```


```
% ===========================================

   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) ->
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]
% ===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','A_Cat',']',->,'(','KnowsAbout',')',->,'[','The_Cat',:,#,666,']',->,'(','KnowsAbout',')',->,'[','Named_Cat',:,'Moris',']',->,'(','KnowsAbout',')',->,'[','Length',:,@,5,ft,']',->,'(','KnowsAbout',')',->,'[','Cat_Set',:,'{',*,'}',']',->,'(','KnowsAbout',')',->,'[','Cat5',:,'{',*,'}',@,5,']',->,'(','KnowsAbout',')',->,'[','Cats_Two',:,'{','Moris',',','Felix','}',']',->,'(','KnowsAbout',')',->,'[','Cats_One_Or_More',:,'{','Moris',',',*,'}',']']))
   ).
```


```
% ===========================================
[Relation: *r] (Familial ?r) (#?r Bob Sue)
% ===========================================

% cg_holds(R, Bob_Sue),
% cg_name(Bob_Sue, 'Bob_Sue'),
% R=Relation,
% cg_holds(Familial_c63_c40_R_c41),
% cg_name(Familial_c63_c40_R_c41, 'Familial_?(\'R\')'),
% frame_var('R', Relation),
% cg_type(Relation, 'Relation'),
% lbl(frame42).
```


```
% ===========================================

[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.
% ===========================================

% gc_pred('Loc', Cat, Mat),
% cg_type(Mat, 'Mat'),
% gc_pred('Stat', Cat, Sit),
% cg_type(Cat, 'Cat'),
% cg_type(Sit, 'Sit'),
% lbl(frame43).
```


```
% ===========================================
?x -(Attrib)-> [Color #1]
% ===========================================

  gc_pred('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  lbl(frame44).
% pred_cg(call_cg_real, xtext("?x -(Attrib)-> [Color #1]")).


```
% ===========================================
?x -(On)->[Mat #1]-(Attrib)->[Color #1]
% ===========================================

  gc_pred('On', X, Mat).
  gc_pred('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_type(Mat, 'Mat').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  cg_equal(Mat, 'Mat#1').
  lbl(frame45).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
?x -(On)->[Mat #1]
% ===========================================

  gc_pred('On', X, Mat).
  cg_type(Mat, 'Mat').
  cg_quantz(e, X).
  cg_equal(Mat, 'Mat#1').
  lbl(frame46).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]")).


```
% ===========================================
[?x] -(Attrib)-> [Color #1]
% ===========================================

  gc_pred('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_type(X, ?('X')).
  cg_equal(Color, 'Color#1').
  lbl(frame47).
% pred_cg(call_cg_real, xtext("[?x] -(Attrib)-> [Color #1]")).


```
% ===========================================
[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]
% ===========================================

  gc_pred('On', X, Mat).
  gc_pred('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_type(Mat, 'Mat').
  cg_type(X, ?('X')).
  cg_equal(Color, 'Color#1').
  cg_equal(Mat, 'Mat#1').
  lbl(frame48).
% pred_cg(call_cg_real, xtext("[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
[Mat ?x]-(Attrib)->[Color #1]
% ===========================================

  gc_pred('Attrib', X, Color).
  cg_type(X, 'Mat').
  cg_type(Color, 'Color').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  lbl(frame49).
% pred_cg(call_cg_real, xtext("[Mat ?x]-(Attrib)->[Color #1]")).


```
% ===========================================
[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]
% ===========================================

  gc_pred('On', X, Mat).
  gc_pred('Attrib', X, Color).
  cg_type(X, 'Cat').
  cg_type(Color, 'Color').
  cg_type(Mat, 'Mat').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#2').
  cg_equal(Mat, 'Mat#1').
  lbl(frame50).
% pred_cg(call_cg_real, xtext("[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]")).


```
% ===========================================
[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]
% ===========================================

% gc_pred(belives, A, Statement),
% 'GRAPH'(Statement, [gc_pred('On', Every_Cat, Mat), cg_type(Mat, 'Mat'), cg_quantz(every, Every_Cat), cg_type(Every_Cat, 'Cat'), lbl(frame52)]),
% cg_type(Statement, statement),
% cg_type(A, a),
% lbl(frame51).
```


```
% ===========================================
[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]
% ===========================================

% gc_pred(belives, A, Statement2),
% Statement2='GRAPH'([gc_pred('On', Every_Cat, Mat), cg_type(Mat, 'Mat'), cg_quantz(every, Every_Cat), cg_type(Every_Cat, 'Cat'), lbl(frame54)]),
% cg_type(Statement2, statement2),
% cg_type(A, a),
% lbl(frame53).
```


```
% ===========================================


[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]
% ===========================================

% gc_pred('Inst', Go, Bus),
% cg_type(Bus, 'Bus'),
% gc_pred('Dest', Go, Boston_City),
% cg_name(Boston_City, 'Boston'),
% cg_type(Boston_City, 'City'),
% gc_pred('Agnt', Go, John_Person),
% cg_name(John_Person, 'John'),
% cg_type(John_Person, 'Person'),
% cg_type(Go, 'Go'),
% lbl(frame55).
```


```
% ===========================================

   [Person: John2] <- (Agnt) -
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]
% ===========================================

% gc_pred('Inst', Go2, Bus2),
% cg_type(Go2, 'Go2'),
% gc_pred('Dest', Bus2, Boston2_City),
% cg_type(Bus2, 'Bus2'),
% gc_pred('Agnt', Boston2_City, John2_Person),
% cg_name(Boston2_City, 'Boston2'),
% cg_type(Boston2_City, 'City'),
% cg_name(John2_Person, 'John2'),
% cg_type(John2_Person, 'Person'),
% lbl(frame56).
```


```
% ===========================================

[Begin]-
        -obj->[Session],
        -srce->[Proposition =
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John]
               ],
        -agnt->[Person : John]
% ===========================================

% gc_pred(agnt, Begin, John_Person9),
% cg_name(John_Person9, 'John'),
% cg_type(John_Person9, 'Person'),
% gc_pred(srce, Begin, Proposition),
% Proposition='GRAPH'([gc_pred(agnt, Press, John_Person), cg_name(John_Person, 'John'), cg_type(John_Person, 'Person'), gc_pred(partOf, Press, Keyboard), cg_type(Keyboard, 'Keyboard'), gc_pred(obj, Press, Enter_Key), frame_var('ENTER', Enter_Key), cg_type(Enter_Key, 'Key'), cg_type(Press, 'Press'), lbl(frame58)]),
% cg_type(Proposition, 'Proposition'),
% gc_pred(obj, Begin, Session),
% cg_type(Session, 'Session'),
% cg_type(Begin, 'Begin'),
% lbl(frame57).
```


```
% ===========================================

 [a] - (belives) ->
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]
% ===========================================

% gc_pred(belives, A, Statement),
% Statement='GRAPH'([gc_pred('Inst', Go2, Bus2), cg_type(Bus2, 'Bus2'), gc_pred('Dest', Go2, Boston2_City), cg_name(Boston2_City, 'Boston2'), cg_type(Boston2_City, 'City'), gc_pred('Agnt', Go2, John2_Person), cg_name(John2_Person, 'John2'), cg_type(John2_Person, 'Person'), cg_type(Go2, 'Go2'), lbl(frame60)]),
% cg_type(Statement, statement),
% cg_type(A, a),
% lbl(frame59).
```


```
% ===========================================
[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)
% ===========================================

% cg_holds(Inst_c63_c40_X_c41_c63_c40_W_c41),
% cg_name(Inst_c63_c40_X_c41_c63_c40_W_c41, 'Inst_?(\'X\')_?(\'W\')'),
% cg_holds(Dest_c63_c40_X_c41_c63_c40_Z_c41),
% cg_name(Dest_c63_c40_X_c41_c63_c40_Z_c41, 'Dest_?(\'X\')_?(\'Z\')'),
% cg_holds(Agnt_c63_c40_X_c41_c63_c40_Y_c41),
% cg_name(Agnt_c63_c40_X_c41_c63_c40_Y_c41, 'Agnt_?(\'X\')_?(\'Y\')'),
% frame_var('W', W_Bus),
% cg_type(W_Bus, 'Bus'),
% frame_var('Z', Z_Boston_City),
% cg_name(Z_Boston_City, 'Boston'),
% cg_type(Z_Boston_City, 'City'),
% frame_var('Y', Y_John_Person),
% cg_name(Y_John_Person, 'John'),
% cg_type(Y_John_Person, 'Person'),
% frame_var('X', X_Go),
% cg_type(X_Go, 'Go'),
% lbl(frame61).
```


```
% ===========================================
[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]
% ===========================================

% gc_pred(on, Red_Woman, Table),
% cg_type(Table, table),
% gc_pred(obj, Eat, Apple),
% cg_type(Apple, 'Apple'),
% gc_pred(agnt, Eat, Karim_Man),
% cg_type(Eat, 'Eat'),
% gc_pred(knows, Karim_Man, Red_Woman),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% frame_var('RED', Red_Woman),
% cg_type(Red_Woman, 'Woman'),
% lbl(frame62).
```


```
% ===========================================
[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]
% ===========================================

% gc_pred('Thme', Marry, Sailor),
% cg_type(Sailor, 'Sailor'),
% gc_pred('Agnt', Marry, X),
% cg_type(Marry, 'Marry'),
% cg_type(X, ?('X')),
% lbl(frame63).
```


```
% ===========================================

[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]
% ===========================================

% gc_pred('Thme', Want, Situation),
% 'GRAPH'(Situation, [gc_pred('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), gc_pred('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person), lbl(frame65)]),
% cg_type(Situation, 'Situation'),
% gc_pred('Expr', Want, X_Mary_Person),
% cg_type(Want, 'Want'),
% frame_var('X', X_Mary_Person),
% cg_name(X_Mary_Person, 'Mary'),
% cg_type(X_Mary_Person, 'Person'),
% lbl(frame64).
```


```
% ===========================================

[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]
% ===========================================

% 'GRAPH'(Proposition, [gc_pred('Thme', Want, Situation), 'GRAPH'(Situation, [gc_pred('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), gc_pred('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person), lbl(frame68)]), cg_type(Situation, 'Situation'), gc_pred('Expr', Want, X_Mary_Person), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person), cg_name(X_Mary_Person, 'Mary'), cg_type(X_Mary_Person, 'Person'), lbl(frame67)]),
% cg_type(Proposition, 'Proposition'),
% lbl(frame66).
```


```
% ===========================================

[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]
% ===========================================

% gc_pred('Thme', Believe, Proposition),
% 'GRAPH'(Proposition, [gc_pred('Thme', Want, Situation), 'GRAPH'(Situation, [gc_pred('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), gc_pred('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person3), lbl(frame71)]), cg_type(Situation, 'Situation'), gc_pred('Expr', Want, X_Mary_Person3), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person3), cg_name(X_Mary_Person3, 'Mary'), cg_type(X_Mary_Person3, 'Person'), lbl(frame70)]),
% cg_type(Proposition, 'Proposition'),
% gc_pred('Expr', Believe, Tom_Person),
% cg_type(Believe, 'Believe'),
% cg_name(Tom_Person, 'Tom'),
% cg_type(Tom_Person, 'Person'),
% lbl(frame69).
```


```
% ===========================================

[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]
% ===========================================

% gc_pred('Thme', Believe, Proposition),
% 'GRAPH'(Proposition, [gc_pred('Thme', Want, Situation), 'GRAPH'(Situation, [gc_pred('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), gc_pred('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person3), lbl(frame74)]), cg_type(Situation, 'Situation'), gc_pred('Expr', Want, X_Mary_Person3), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person3), cg_name(X_Mary_Person3, 'Mary'), cg_type(X_Mary_Person3, 'Person'), lbl(frame73)]),
% cg_type(Proposition, 'Proposition'),
% gc_pred('Expr', Believe, Tom_Person),
% cg_type(Believe, 'Believe'),
% cg_name(Tom_Person, 'Tom'),
% cg_type(Tom_Person, 'Person'),
% lbl(frame72).
```


```
% ===========================================
TYPE ARRIVAL-DATE(a) IS [UNIV:*a].
% ===========================================

% cg_quantz(type, A_Univ),
% cg_type(A_Univ, 'Arrival_Date'),
% lbl(frame76),
% cg_type(A_Univ, 'Univ'),
% frame_var('A', A_Univ),
% frame_var('A', A_Univ),
% lbl(frame75).
```


```
% ===========================================
TYPE CIRCUS(c) IS [UNIV:*c].
% ===========================================

% cg_quantz(type, C_Univ),
% cg_type(C_Univ, 'Circus'),
% lbl(frame78),
% cg_type(C_Univ, 'Univ'),
% frame_var('C', C_Univ),
% frame_var('C', C_Univ),
% lbl(frame77).
```


```
% ===========================================
TYPE CIRCUS(c) IS [UNIV:*c]
% ===========================================

% cg_quantz(type, C_Univ),
% cg_type(C_Univ, 'Circus'),
% lbl(frame80),
% cg_type(C_Univ, 'Univ'),
% frame_var('C', C_Univ),
% frame_var('C', C_Univ),
% lbl(frame79).
```


```
% ===========================================
[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]
% ===========================================

% gc_pred('Loc', Perform, Circus),
% cg_type(Circus, 'Circus'),
% gc_pred('Agnt', Perform, C_Elephant),
% cg_type(Perform, 'Perform'),
% frame_var('C', C_Elephant),
% cg_type(C_Elephant, 'Elephant'),
% lbl(frame81).
```


```
% ===========================================

[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus])

% ===========================================

% cg_holds(Inst_c63_c40_X_c41, Bus),
% cg_type(Bus, 'Bus'),
% cg_name(Inst_c63_c40_X_c41, 'Inst_?(\'X\')'),
% cg_holds(Dest_c63_c40_X_c41, Boston_City),
% cg_name(Boston_City, 'Boston'),
% cg_type(Boston_City, 'City'),
% cg_name(Dest_c63_c40_X_c41, 'Dest_?(\'X\')'),
% cg_holds(Agnt_c63_c40_X_c41, John_Person),
% cg_name(John_Person, 'John'),
% cg_type(John_Person, 'Person'),
% cg_name(Agnt_c63_c40_X_c41, 'Agnt_?(\'X\')'),
% frame_var('X', X_Go),
% cg_type(X_Go, 'Go'),
% lbl(frame82).
```


```
% ===========================================
TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].
% ===========================================

% cg_quantz(type, C_Elephant),
% cg_type(C_Elephant, 'Circus_Elephant'),
% lbl(frame84),
% cg_type(C_Elephant, 'Elephant'),
% frame_var('C', C_Elephant),
% cg_type(Perform, 'Perform'),
% gc_pred('Agnt', Perform, C_Elephant),
% cg_type(Circus, 'Circus'),
% gc_pred('Loc', Perform, Circus),
% cg_name(C_Elephant, 'C'),
% lbl(frame83).
```


```
% ===========================================
TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].
% ===========================================

% cg_quantz(type, C_Elephant),
% cg_type(C_Elephant, 'Circus_Elephant'),
% lbl(frame86),
% cg_type(C_Elephant, 'Elephant'),
% frame_var('C', C_Elephant),
% cg_type(Perform, 'Perform'),
% gc_pred('Agnt', Perform, C_Elephant),
% cg_type(Circus, 'Circus'),
% gc_pred('Loc', Perform, Circus),
% cg_name(C_Elephant, 'C'),
% lbl(frame85).
```


```
% ===========================================
TYPE DEPARTURE-DATE(d) IS [UNIV:*d].
% ===========================================

% cg_quantz(type, D_Univ),
% cg_type(D_Univ, 'Departure_Date'),
% lbl(frame88),
% cg_type(D_Univ, 'Univ'),
% frame_var('D', D_Univ),
% frame_var('D', D_Univ),
% lbl(frame87).
```


```
% ===========================================
TYPE ELEPHANT(e) IS [UNIV:*e].
% ===========================================

% cg_quantz(type, E_Univ),
% cg_type(E_Univ, 'Elephant'),
% lbl(frame90),
% cg_type(E_Univ, 'Univ'),
% frame_var('E', E_Univ),
% frame_var('E', E_Univ),
% lbl(frame89).
```


```
% ===========================================
TYPE HOTEL(h) IS [UNIV:*h].
% ===========================================

% cg_quantz(type, H_Univ),
% cg_type(H_Univ, 'Hotel'),
% lbl(frame92),
% cg_type(H_Univ, 'Univ'),
% frame_var('H', H_Univ),
% frame_var('H', H_Univ),
% lbl(frame91).
```


```
% ===========================================
TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.
% ===========================================

% cg_quantz(type, Reservation_No),
% cg_type(Reservation_No, 'Hotel_Reservation'),
% lbl(frame94),
% cg_type(Reservation, 'Reservation'),
% frame_var('RESERVATION_NO', Reservation),
% cg_type(Person, 'Person'),
% gc_pred('Rcpt', Reservation, Person),
% cg_type(Room, 'Room'),
% gc_pred('Obj', Person, Room),
% cg_type(Hotel, 'Hotel'),
% gc_pred('Loc', Room, Hotel),
% cg_type(Time_Period, 'Time_Period'),
% gc_pred('Dur', Hotel, Time_Period),
% cg_type(Arrival_Date, 'Arrival_Date'),
% gc_pred('Strt', Reservation, Arrival_Date),
% cg_type(Departure_Date, 'Departure_Date'),
% gc_pred('Untl', Arrival_Date, Departure_Date),
% cg_name(Reservation_No, 'Reservation_No'),
% lbl(frame93).
```


```
% ===========================================
TYPE PERFORM(p) IS [UNIV:*p].
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Perform'),
% lbl(frame96),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% frame_var('P', P_Univ),
% lbl(frame95).
```


```
% ===========================================
TYPE PERSON(p) IS [UNIV:*p].
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Person'),
% lbl(frame98),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% frame_var('P', P_Univ),
% lbl(frame97).
```


```
% ===========================================
TYPE PROPOSITION(p) IS [UNIV:*p].
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Proposition'),
% lbl(frame100),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% frame_var('P', P_Univ),
% lbl(frame99).
```


```
% ===========================================
TYPE RESERVATION(r) IS [UNIV:*r].
% ===========================================

% cg_quantz(type, R_Univ),
% cg_type(R_Univ, 'Reservation'),
% lbl(frame102),
% cg_type(R_Univ, 'Univ'),
% frame_var('R', R_Univ),
% frame_var('R', R_Univ),
% lbl(frame101).
```


```
% ===========================================
TYPE ROOM(r) IS [UNIV:*r].
% ===========================================

% cg_quantz(type, R_Univ),
% cg_type(R_Univ, 'Room'),
% lbl(frame104),
% cg_type(R_Univ, 'Univ'),
% frame_var('R', R_Univ),
% frame_var('R', R_Univ),
% lbl(frame103).
```


```
% ===========================================
TYPE TIME-PERIOD(t) IS [UNIV:*t].
% ===========================================

% cg_quantz(type, T_Univ),
% cg_type(T_Univ, 'Time_Period'),
% lbl(frame106),
% cg_type(T_Univ, 'Univ'),
% frame_var('T', T_Univ),
% frame_var('T', T_Univ),
% lbl(frame105).
```


```
% ===========================================

[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]
% ===========================================

% gc_pred('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% gc_pred('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% gc_pred('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_quantz(4, Nights_Time_Period),
% cg_type(Nights_Time_Period, 'Time_Period'),
% gc_pred('Loc', Q2_Room, Shelburne_Hotel),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% cg_type(Shelburne_Hotel, 'Hotel'),
% gc_pred('Obj', John_Sowa_Person, Q2_Room),
% cg_name(Q2_Room, 'Q2'),
% cg_type(Q2_Room, 'Room'),
% gc_pred('Rcpt', Reservation, John_Sowa_Person),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(Reservation, 'Reservation'),
% lbl(frame107).
```


```
% ===========================================

[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]
% ===========================================

% gc_pred('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% gc_pred('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% gc_pred('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_quantz(4, Nights_Time_Period),
% cg_type(Nights_Time_Period, 'Time_Period'),
% gc_pred('Loc', Q2_Room, Shelburne_Hotel),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% cg_type(Shelburne_Hotel, 'Hotel'),
% gc_pred('Obj', John_Sowa_Person, Q2_Room),
% cg_name(Q2_Room, 'Q2'),
% cg_type(Q2_Room, 'Room'),
% gc_pred('Rcpt', Reservation, John_Sowa_Person),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(Reservation, 'Reservation'),
% lbl(frame108).
```


```
% ===========================================

INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.
% ===========================================

% cg_quantz(individual, _1298),
% cg_type(_1298, 'Hotel_Reservation'),
% lbl(frame110),
% cg_type(Reservation, 'Reservation'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% gc_pred('Rcpt', Reservation, John_Sowa_Person),
% cg_type(Q2_Room, 'Room'),
% cg_name(Q2_Room, 'Q2'),
% gc_pred('Obj', John_Sowa_Person, Q2_Room),
% cg_type(Shelburne_Hotel, 'Hotel'),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% gc_pred('Loc', Q2_Room, Shelburne_Hotel),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_quantz(4, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% gc_pred('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% gc_pred('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% gc_pred('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% _1298#316209,
% lbl(frame109).
```


```
% ===========================================

INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.
% ===========================================

% cg_quantz(individual, _1296),
% cg_type(_1296, 'Hotel_Reservation'),
% lbl(frame112),
% cg_type(Reservation, 'Reservation'),
% cg_equal(Reservation, 'Reservation#316210'),
% cg_type(John_Esch_Person, 'Person'),
% cg_name(John_Esch_Person, 'John_Esch'),
% gc_pred('Rcpt', Reservation, John_Esch_Person),
% cg_type(Q3_Room, 'Room'),
% cg_name(Q3_Room, 'Q3'),
% gc_pred('Obj', John_Esch_Person, Q3_Room),
% cg_type(Sidney_Hotel, 'Hotel'),
% cg_name(Sidney_Hotel, 'Sidney'),
% gc_pred('Loc', Q3_Room, Sidney_Hotel),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_quantz(7, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% gc_pred('Dur', Sidney_Hotel, Nights_Time_Period),
% cg_type(March_12_1983_Arrival_Date, 'Arrival_Date'),
% cg_name(March_12_1983_Arrival_Date, 'March_12_1983'),
% gc_pred('Strt', Reservation, March_12_1983_Arrival_Date),
% cg_type(March_19_1983_Departure_Date, 'Departure_Date'),
% cg_name(March_19_1983_Departure_Date, 'March_19_1983'),
% gc_pred('Untl', March_12_1983_Arrival_Date, March_19_1983_Departure_Date),
% _1296#316210,
% lbl(frame111).
```


```
% ===========================================

INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].
% ===========================================

% cg_quantz(individual, Bumbo),
% cg_type(Bumbo, 'Circus_Elephant'),
% lbl(frame114),
% cg_type(Elephant, 'Elephant'),
% cg_equal(Elephant, 'Elephant#Bumbo'),
% cg_type(Set_Perform, 'Perform'),
% cg_quantz(set, Set_Perform),
% cg_count(Set_Perform, 1, _14570),
% gc_pred('Agnt', Set_Perform, Elephant),
% cg_type(Flying_Tigers_Circus, 'Circus'),
% cg_name(Flying_Tigers_Circus, 'Flying_Tigers'),
% gc_pred('Loc', Set_Perform, Flying_Tigers_Circus),
% Bumbo#'Bumbo',
% lbl(frame113).
```

