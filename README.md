# logicmoo_cg
Conceptual Graph (CG) Libraries in Prolog


```
/pack/logicmoo_cg/prolog# cls ; swipl -l cgprolog.pl -t halt -g cg_reader_tests
```

init_why(before_boot,after(/.../(prolog,'cgprolog.pl')))
% init_why(before_boot, after(/.../(prolog, 'cgprolog.pl'))).


```
% ===========================================
?- pred_cg(assert_cg_real,"
[PERSON: x] :- [CITIZEN : x].").
% ===========================================

% preconds([frame_var('X', X_Citizen), cg_type(X_Citizen, 'Citizen'), lbl(frame2)]),
% frame_var('X', X_Citizen),
% cg_type(X_Citizen, 'Person'),
% lbl(frame1).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

% preconds([cg_holds('Loc', Being_Born, Oz_Country7), cg_name(Oz_Country7, 'Oz'), cg_type(Oz_Country7, 'Country'), cg_holds('Agnt', Being_Born, X_Person), cg_type(Being_Born, 'Being_Born'), frame_var('X', X_Person), cg_type(X_Person, 'Person'), lbl(frame4)]),
% cg_holds(memberOf, Oz_Country7, X_Person),
% cg_name(Oz_Country7, 'Oz'),
% cg_type(Oz_Country7, 'Country'),
% frame_var('X', X_Person),
% cg_type(X_Person, 'Citizen'),
% lbl(frame3).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: ?x]<-childOf-[PERSON: y],
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].").
% ===========================================

% preconds([cg_holds(memberOf, Oz_Country10, Y_Citizen8), cg_name(Oz_Country10, 'Oz'), cg_type(Oz_Country10, 'Country'), frame_var('Y', Y_Citizen8), cg_type(Y_Citizen8, 'Citizen'), cg_holds(childOf, Y_Citizen8, X_), frame_var('Y', Y_Citizen8), cg_type(Y_Citizen8, 'Person'), cg_quantz(e, X_), cg_type(X_, 'Person'), lbl(frame6)]),
% cg_holds(memberOf, Oz_Country10, X_Citizen),
% cg_name(Oz_Country10, 'Oz'),
% cg_type(Oz_Country10, 'Country'),
% frame_var('X', X_Citizen),
% cg_type(X_Citizen, 'Citizen'),
% lbl(frame5).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].").
% ===========================================

% preconds([cg_holds('Loc', Naturalize, Oz_Country7), cg_name(Oz_Country7, 'Oz'), cg_type(Oz_Country7, 'Country'), cg_holds('Rcpt', Naturalize, X_Person), cg_type(Naturalize, 'Naturalize'), frame_var('X', X_Person), cg_type(X_Person, 'Person'), lbl(frame8)]),
% cg_holds(memberOf, Oz_Country7, X_Person),
% cg_name(Oz_Country7, 'Oz'),
% cg_type(Oz_Country7, 'Country'),
% frame_var('X', X_Person),
% cg_type(X_Person, 'Citizen'),
% lbl(frame7).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[PERSON : Tinman]-
              -childOf->[GIRL : Dorothy],
              <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

% cg_holds('Loc', Being_Born, Oz_Country),
% cg_name(Oz_Country, 'Oz'),
% cg_type(Oz_Country, 'Country'),
% cg_holds('Agnt', Being_Born, Tinman_Person),
% cg_type(Being_Born, 'Being_Born'),
% cg_holds(childOf, Tinman_Person, Dorothy_Girl),
% cg_name(Dorothy_Girl, 'Dorothy'),
% cg_type(Dorothy_Girl, 'Girl'),
% cg_name(Tinman_Person, 'Tinman'),
% cg_type(Tinman_Person, 'Person'),
% lbl(frame9).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color #1]").
% ===========================================

% cg_holds('Attrib', Mat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame10).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color]2").
% ===========================================

% cg_holds('Attrib', Mat, Color),
% cg_equal(Color, 'Color#2'),
% cg_type(Color, 'Color'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame11).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_QUANT:@every]-(On)->[Mat]").
% ===========================================

% cg_holds('On', Every_Cat_Quant, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(every, Every_Cat_Quant),
% cg_type(Every_Cat_Quant, 'Cat_Quant'),
% lbl(frame12).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[A_CAT]->(On)->[Mat]").
% ===========================================

% cg_holds('On', A_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_type(A_Cat, 'A_Cat'),
% lbl(frame13).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[THE_CAT:#666]->(On)->[Mat]").
% ===========================================

% cg_holds('On', The_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_equal(The_Cat, 'The_Cat#666'),
% cg_type(The_Cat, 'The_Cat'),
% lbl(frame14).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[NAMED_CAT:Moris]->(On)->[Mat]").
% ===========================================

% cg_holds('On', Moris_Named_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_name(Moris_Named_Cat, 'Moris'),
% cg_type(Moris_Named_Cat, 'Named_Cat'),
% lbl(frame15).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[LENGTH:@5ft]<-(SizeOf)-[Mat]").
% ===========================================

% cg_holds('SizeOf', Mat, Ft_Length),
% cg_type(Mat, 'Mat'),
% frame_var('FT', Ft_Length),
% cg_quantz(5, Ft_Length),
% cg_type(Ft_Length, 'Length'),
% lbl(frame16).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[LENGTH:@5ft.]<-(SizeOf)-[Mat]").
% ===========================================

% cg_holds('SizeOf', Mat, Ft_Length),
% cg_type(Mat, 'Mat'),
% frame_var('FT', Ft_Length),
% cg_quantz(5, Ft_Length),
% cg_type(Ft_Length, 'Length'),
% lbl(frame17).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_SET_NONE:{}]-(On)->[Mat]").
% ===========================================

% cg_holds('On', Cat_Set_None, Mat),
% cg_type(Mat, 'Mat'),
% cg_count(Cat_Set_None, 0, 0),
% cg_type(Cat_Set_None, 'Cat_Set_None'),
% lbl(frame18).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]").
% ===========================================

% cg_holds('On', Set_Cats_One_Or_More, Mat),
% cg_type(Mat, 'Mat'),
% cg_count(Set_Cats_One_Or_More, 1, _1076),
% cg_quantz(set, Set_Cats_One_Or_More),
% cg_type(Set_Cats_One_Or_More, 'Cats_One_Or_More'),
% lbl(frame19).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_FIVE:{*}@5]-(On)->[Mat]").
% ===========================================

% cg_holds('On', Set_Cat_Five, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(5, Set_Cat_Five),
% cg_count(Set_Cat_Five, 1, _432),
% cg_quantz(set, Set_Cat_Five),
% cg_type(Set_Cat_Five, 'Cat_Five'),
% lbl(frame20).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_FM:{Felix,Moris}]-(On)->[Mat]").
% ===========================================

% cg_holds('On', FELIXMORIS_Set_Cat_Fm, Mat),
% cg_type(Mat, 'Mat'),
% cg_values(FELIXMORIS_Set_Cat_Fm, ['FELIX', 'MORIS']),
% cg_count(FELIXMORIS_Set_Cat_Fm, 2, _9682),
% cg_quantz(set, FELIXMORIS_Set_Cat_Fm),
% cg_type(FELIXMORIS_Set_Cat_Fm, 'Cat_Fm'),
% lbl(frame21).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]").
% ===========================================

% cg_holds('On', FELIXMORIS_Cat_Set_Min_Two, Mat),
% cg_type(Mat, 'Mat'),
% cg_values(FELIXMORIS_Cat_Set_Min_Two, ['FELIX', 'MORIS']),
% cg_count(FELIXMORIS_Cat_Set_Min_Two, 2, _4506),
% cg_quantz(set, FELIXMORIS_Cat_Set_Min_Two),
% cg_type(FELIXMORIS_Cat_Set_Min_Two, 'Cat_Set_Min_Two'),
% lbl(frame22).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]").
% ===========================================

% cg_holds('On', FELIXMORIS_Cat_Set_Five, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(5, FELIXMORIS_Cat_Set_Five),
% cg_values(FELIXMORIS_Cat_Set_Five, ['FELIX', 'MORIS']),
% cg_count(FELIXMORIS_Cat_Set_Five, 2, _4048),
% cg_quantz(set, FELIXMORIS_Cat_Set_Five),
% cg_type(FELIXMORIS_Cat_Set_Five, 'Cat_Set_Five'),
% lbl(frame23).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"['Man':imad]<-agnt-['Drive']-obj->['Car']").
% ===========================================

% cg_holds(obj, Drive, Car),
% cg_type(Car, 'Car'),
% cg_holds(agnt, Drive, Imad_Man),
% cg_type(Drive, 'Drive'),
% frame_var('IMAD', Imad_Man),
% cg_type(Imad_Man, 'Man'),
% lbl(frame24).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat#1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

% cg_holds('Attrib', Mat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% cg_holds('On', Cat, Mat),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% lbl(frame25).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]").
% ===========================================

% cg_holds('On', C1, Mat),
% cg_type(Mat, 'Mat'),
% cg_holds('Attrib', X_, C1),
% cg_type(C1, 'C1'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame26).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat]").
% ===========================================

% cg_holds('On', X_, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame27).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[*MatC]").
% ===========================================

% cg_holds('On', X_, MatC),
% frame_var('MATC', MatC),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame28).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat: *MatC]").
% ===========================================

% cg_holds('On', X_, Mat),
% frame_var('MATC', Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% lbl(frame29).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

% cg_holds(obj, Drink, Water),
% cg_type(Water, 'Water'),
% cg_holds(agnt, Drink, Karim_Man),
% cg_type(Drink, 'Drink'),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% lbl(frame30).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Mat #1]<- (on)- [Cat #1]").
% ===========================================

% cg_holds(on, Cat, Mat),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% lbl(frame31).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Mat]<-(On)-[Cat: ?x]").
% ===========================================

% cg_holds('On', X_, Mat),
% cg_quantz(e, X_),
% cg_type(X_, 'Cat'),
% cg_type(Mat, 'Mat'),
% lbl(frame32).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Color #1]<-(Attrib)-[Mat #1]").
% ===========================================

% cg_holds('Attrib', Mat, Color),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% lbl(frame33).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

% cg_holds('Attrib', Mat, Color),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% cg_holds('On', Cat, Mat),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% lbl(frame34).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

% cg_holds(obj, Drink, Water),
% cg_type(Water, 'Water'),
% cg_holds(agnt, Drink, Karim_Man),
% cg_type(Drink, 'Drink'),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% lbl(frame35).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]").
% ===========================================

% cg_holds(on, Cat, Mat),
% cg_equal(Cat, 'Cat#1'),
% cg_type(Cat, 'Cat'),
% cg_holds('Attrib', Mat, Color),
% cg_equal(Mat, 'Mat#1'),
% cg_type(Mat, 'Mat'),
% cg_equal(Color, 'Color#1'),
% cg_type(Color, 'Color'),
% lbl(frame36).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Cat: @every]->(On)->[Mat]").
% ===========================================

% cg_holds('On', Every_Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_quantz(every, Every_Cat),
% cg_type(Every_Cat, 'Cat'),
% lbl(frame37).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
% ===========================================

% cg_holds('Loc', Sit, Mat),
% cg_type(Mat, 'Mat'),
% cg_holds('Stat', Cat, Sit),
% cg_type(Sit, 'Sit'),
% cg_type(Cat, 'Cat'),
% lbl(frame38).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT]").
% ===========================================

% cg_holds('Loc', Sit, Mat),
% cg_type(Mat, 'Mat'),
% cg_holds('Stat', Cat, Sit),
% cg_type(Sit, 'Sit'),
% cg_type(Cat, 'Cat'),
% lbl(frame39).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
   [Drive *x] [Person: Bob] [City: "St. Louis"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x "St. Louis") (Thme ?x ?y) (Poss Bob ?y)").
% ===========================================

% cg_holds('Poss', Bob_Person, Y_Chevy),
% cg_holds('Thme', X_Drive, Y_Chevy),
% cg_holds('Dest', X_Drive, "St. Louis"),
% cg_holds('Agnt', X_Drive, Bob_Person),
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
?- pred_cg(assert_cg_real,"
   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) ->
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]").
% ===========================================

% cg_holds('KnowsAbout', Set_Cat5, MORISFELIXc125c93c62c40KNOWSABOUTc41c62c91CATS_ONE_OR_MOREkw_c123MORIS_Set_Cats_Two),
% cg_values(MORISFELIXc125c93c62c40KNOWSABOUTc41c62c91CATS_ONE_OR_MOREkw_c123MORIS_Set_Cats_Two, [Moris_Named_Cat, 'FELIX', '}', ']', ->, '(', 'KNOWSABOUT', ')', ->, '[', 'CATS_ONE_OR_MORE', :, '{', Moris_Named_Cat]),
% cg_count(MORISFELIXc125c93c62c40KNOWSABOUTc41c62c91CATS_ONE_OR_MOREkw_c123MORIS_Set_Cats_Two, 14, _6444),
% cg_quantz(set, MORISFELIXc125c93c62c40KNOWSABOUTc41c62c91CATS_ONE_OR_MOREkw_c123MORIS_Set_Cats_Two),
% cg_type(MORISFELIXc125c93c62c40KNOWSABOUTc41c62c91CATS_ONE_OR_MOREkw_c123MORIS_Set_Cats_Two, 'Cats_Two'),
% cg_holds('KnowsAbout', Cat_Set, Set_Cat5),
% cg_quantz(5, Set_Cat5),
% cg_count(Set_Cat5, 1, _3320),
% cg_quantz(set, Set_Cat5),
% cg_type(Set_Cat5, 'Cat5'),
% cg_holds('KnowsAbout', Ft_Length, Cat_Set),
% cg_count(Cat_Set, 1, _1754),
% cg_quantz(set, Cat_Set),
% cg_type(Cat_Set, 'Cat_Set'),
% cg_holds('KnowsAbout', Moris_Named_Cat, Ft_Length),
% frame_var('FT', Ft_Length),
% cg_quantz(5, Ft_Length),
% cg_type(Ft_Length, 'Length'),
% cg_holds('KnowsAbout', The_Cat, Moris_Named_Cat),
% cg_name(Moris_Named_Cat, 'Moris'),
% cg_type(Moris_Named_Cat, 'Named_Cat'),
% cg_holds('KnowsAbout', A_Cat, The_Cat),
% cg_equal(The_Cat, 'The_Cat#666'),
% cg_type(The_Cat, 'The_Cat'),
% cg_type(A_Cat, 'A_Cat'),
% lbl(frame41).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Relation: *r] (Familial ?r) (#?r Bob Sue)").
% ===========================================

% cg_holds(R, 'Bob', 'Sue'),
% R=Relation,
% cg_holds('Familial', Relation),
% frame_var('R', Relation),
% cg_type(Relation, 'Relation'),
% lbl(frame42).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").
% ===========================================

% cg_holds('Loc', Cat, Mat),
% cg_type(Mat, 'Mat'),
% cg_holds('Stat', Cat, Sit),
% cg_type(Cat, 'Cat'),
% cg_type(Sit, 'Sit'),
% lbl(frame43).
```


```
% ===========================================
?- pred_cg(call_cg_real,"?x -(Attrib)-> [Color #1]").
% ===========================================

  cg_holds('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  lbl(frame44).
% pred_cg(call_cg_real, xtext("?x -(Attrib)-> [Color #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

  cg_holds('On', X, Mat).
  cg_holds('Attrib', Mat, Color).
  cg_type(Color, 'Color').
  cg_type(Mat, 'Mat').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  cg_equal(Mat, 'Mat#1').
  lbl(frame45).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]").
% ===========================================

  cg_holds('On', X, Mat).
  cg_type(Mat, 'Mat').
  cg_quantz(e, X).
  cg_equal(Mat, 'Mat#1').
  lbl(frame46).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"[?x] -(Attrib)-> [Color #1]").
% ===========================================

  cg_holds('Attrib', X, Color).
  cg_type(Color, 'Color').
  cg_type(X, ?('X')).
  cg_equal(Color, 'Color#1').
  lbl(frame47).
% pred_cg(call_cg_real, xtext("[?x] -(Attrib)-> [Color #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

  cg_holds('On', X, Mat).
  cg_holds('Attrib', Mat, Color).
  cg_type(Color, 'Color').
  cg_type(Mat, 'Mat').
  cg_type(X, ?('X')).
  cg_equal(Color, 'Color#1').
  cg_equal(Mat, 'Mat#1').
  lbl(frame48).
% pred_cg(call_cg_real, xtext("[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"[Mat ?x]-(Attrib)->[Color #1]").
% ===========================================

  cg_holds('Attrib', X, Color).
  cg_type(X, 'Mat').
  cg_type(Color, 'Color').
  cg_quantz(e, X).
  cg_equal(Color, 'Color#1').
  lbl(frame49).
% pred_cg(call_cg_real, xtext("[Mat ?x]-(Attrib)->[Color #1]")).


```
% ===========================================
?- pred_cg(call_cg_real,"[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]").
% ===========================================

  cg_holds('On', X, Mat).
  cg_holds('Attrib', Mat, Color).
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
?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
% ===========================================

% cg_holds(belives, A, Statement),
% 'GRAPH'(Statement, [cg_holds('On', Every_Cat, Mat), cg_type(Mat, 'Mat'), cg_quantz(every, Every_Cat), cg_type(Every_Cat, 'Cat'), lbl(frame52)]),
% cg_type(Statement, statement),
% cg_type(A, a),
% lbl(frame51).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").
% ===========================================

% cg_holds(belives, A, Statement2),
% Statement2='GRAPH'([cg_holds('On', Every_Cat, Mat), cg_type(Mat, 'Mat'), cg_quantz(every, Every_Cat), cg_type(Every_Cat, 'Cat'), lbl(frame54)]),
% cg_type(Statement2, statement2),
% cg_type(A, a),
% lbl(frame53).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"

[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").
% ===========================================

% cg_holds('Inst', Boston_City, Bus),
% cg_type(Bus, 'Bus'),
% cg_holds('Dest', John_Person, Boston_City),
% cg_name(Boston_City, 'Boston'),
% cg_type(Boston_City, 'City'),
% cg_holds('Agnt', Go, John_Person),
% cg_name(John_Person, 'John'),
% cg_type(John_Person, 'Person'),
% cg_type(Go, 'Go'),
% lbl(frame55).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
   [Person: John2] <- (Agnt) -
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").
% ===========================================

% cg_holds('Inst', Go2, Bus2),
% cg_type(Go2, 'Go2'),
% cg_holds('Dest', Bus2, Boston2_City),
% cg_type(Bus2, 'Bus2'),
% cg_holds('Agnt', Boston2_City, John2_Person),
% cg_name(Boston2_City, 'Boston2'),
% cg_type(Boston2_City, 'City'),
% cg_name(John2_Person, 'John2'),
% cg_type(John2_Person, 'Person'),
% lbl(frame56).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Begin]-
        -obj->[Session],
        -srce->[Proposition =
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John]
               ],
        -agnt->[Person : John]").
% ===========================================

% cg_holds(agnt, Begin, John_Person9),
% cg_name(John_Person9, 'John'),
% cg_type(John_Person9, 'Person'),
% cg_holds(srce, Begin, Proposition),
% Proposition='GRAPH'([cg_holds(agnt, Press, John_Person), cg_name(John_Person, 'John'), cg_type(John_Person, 'Person'), cg_holds(partOf, Enter_Key, Keyboard), cg_type(Keyboard, 'Keyboard'), cg_holds(obj, Press, Enter_Key), frame_var('ENTER', Enter_Key), cg_type(Enter_Key, 'Key'), cg_type(Press, 'Press'), lbl(frame58)]),
% cg_type(Proposition, 'Proposition'),
% cg_holds(obj, Begin, Session),
% cg_type(Session, 'Session'),
% cg_type(Begin, 'Begin'),
% lbl(frame57).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
 [a] - (belives) ->
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").
% ===========================================

% cg_holds(belives, A, Statement),
% Statement='GRAPH'([cg_holds('Inst', Boston2_City, Bus2), cg_type(Bus2, 'Bus2'), cg_holds('Dest', John2_Person, Boston2_City), cg_name(Boston2_City, 'Boston2'), cg_type(Boston2_City, 'City'), cg_holds('Agnt', Go2, John2_Person), cg_name(John2_Person, 'John2'), cg_type(John2_Person, 'Person'), cg_type(Go2, 'Go2'), lbl(frame60)]),
% cg_type(Statement, statement),
% cg_type(A, a),
% lbl(frame59).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)").
% ===========================================

% cg_holds('Inst', X_Go, W_Bus),
% cg_holds('Dest', X_Go, Z_Boston_City),
% cg_holds('Agnt', X_Go, Y_John_Person),
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
?- pred_cg(assert_cg_real,"[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]").
% ===========================================

% cg_holds(on, Apple, Table),
% cg_type(Table, table),
% cg_holds(obj, Eat, Apple),
% cg_type(Apple, 'Apple'),
% cg_holds(agnt, Eat, Karim_Man),
% cg_type(Eat, 'Eat'),
% cg_holds(knows, Karim_Man, Red_Woman),
% frame_var('KARIM', Karim_Man),
% cg_type(Karim_Man, 'Man'),
% frame_var('RED', Red_Woman),
% cg_type(Red_Woman, 'Woman'),
% lbl(frame62).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]").
% ===========================================

% cg_holds('Thme', Marry, Sailor),
% cg_type(Sailor, 'Sailor'),
% cg_holds('Agnt', Marry, X),
% cg_type(Marry, 'Marry'),
% cg_type(X, ?('X')),
% lbl(frame63).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]").
% ===========================================

% cg_holds('Thme', Want, Situation),
% 'GRAPH'(Situation, [cg_holds('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), cg_holds('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person), lbl(frame65)]),
% cg_type(Situation, 'Situation'),
% cg_holds('Expr', Want, X_Mary_Person),
% cg_type(Want, 'Want'),
% frame_var('X', X_Mary_Person),
% cg_name(X_Mary_Person, 'Mary'),
% cg_type(X_Mary_Person, 'Person'),
% lbl(frame64).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

% 'GRAPH'(Proposition, [cg_holds('Thme', Want, Situation), 'GRAPH'(Situation, [cg_holds('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), cg_holds('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person), lbl(frame68)]), cg_type(Situation, 'Situation'), cg_holds('Expr', Want, X_Mary_Person), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person), cg_name(X_Mary_Person, 'Mary'), cg_type(X_Mary_Person, 'Person'), lbl(frame67)]),
% cg_type(Proposition, 'Proposition'),
% lbl(frame66).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

% cg_holds('Thme', Believe, Proposition),
% 'GRAPH'(Proposition, [cg_holds('Thme', Want, Situation), 'GRAPH'(Situation, [cg_holds('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), cg_holds('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person3), lbl(frame71)]), cg_type(Situation, 'Situation'), cg_holds('Expr', Want, X_Mary_Person3), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person3), cg_name(X_Mary_Person3, 'Mary'), cg_type(X_Mary_Person3, 'Person'), lbl(frame70)]),
% cg_type(Proposition, 'Proposition'),
% cg_holds('Expr', Believe, Tom_Person),
% cg_type(Believe, 'Believe'),
% cg_name(Tom_Person, 'Tom'),
% cg_type(Tom_Person, 'Person'),
% lbl(frame69).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").
% ===========================================

% cg_holds('Thme', Believe, Proposition),
% 'GRAPH'(Proposition, [cg_holds('Thme', Want, Situation), 'GRAPH'(Situation, [cg_holds('Thme', Marry, Sailor), cg_type(Sailor, 'Sailor'), cg_holds('Agnt', Marry, X), cg_type(Marry, 'Marry'), cg_type(X, X_Mary_Person3), lbl(frame74)]), cg_type(Situation, 'Situation'), cg_holds('Expr', Want, X_Mary_Person3), cg_type(Want, 'Want'), frame_var('X', X_Mary_Person3), cg_name(X_Mary_Person3, 'Mary'), cg_type(X_Mary_Person3, 'Person'), lbl(frame73)]),
% cg_type(Proposition, 'Proposition'),
% cg_holds('Expr', Believe, Tom_Person),
% cg_type(Believe, 'Believe'),
% cg_name(Tom_Person, 'Tom'),
% cg_type(Tom_Person, 'Person'),
% lbl(frame72).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE ARRIVAL-DATE(a) IS [UNIV:*a].").
% ===========================================

% cg_quantz(type, A_Univ),
% cg_type(A_Univ, 'Arrival_Date'),
% lbl(frame76),
% cg_type(A_Univ, 'Univ'),
% frame_var('A', A_Univ),
% lbl(frame75).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c].").
% ===========================================

% cg_quantz(type, C_Univ),
% cg_type(C_Univ, 'Circus'),
% lbl(frame78),
% cg_type(C_Univ, 'Univ'),
% frame_var('C', C_Univ),
% lbl(frame77).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c]").
% ===========================================

% cg_quantz(type, C_Univ),
% cg_type(C_Univ, 'Circus'),
% lbl(frame80),
% cg_type(C_Univ, 'Univ'),
% frame_var('C', C_Univ),
% lbl(frame79).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]").
% ===========================================

% cg_holds('Loc', Perform, Circus),
% cg_type(Circus, 'Circus'),
% cg_holds('Agnt', Perform, C_Elephant),
% cg_type(Perform, 'Perform'),
% frame_var('C', C_Elephant),
% cg_type(C_Elephant, 'Elephant'),
% lbl(frame81).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus])
").
% ===========================================

% cg_holds('Inst', X_Go, Bus),
% cg_type(Bus, 'Bus'),
% cg_holds('Dest', X_Go, Boston_City),
% cg_name(Boston_City, 'Boston'),
% cg_type(Boston_City, 'City'),
% cg_holds('Agnt', X_Go, John_Person),
% cg_name(John_Person, 'John'),
% cg_type(John_Person, 'Person'),
% frame_var('X', X_Go),
% cg_type(X_Go, 'Go'),
% lbl(frame82).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

% cg_quantz(type, C_Elephant),
% cg_type(C_Elephant, 'Circus_Elephant'),
% lbl(frame84),
% cg_type(C_Elephant, 'Elephant'),
% frame_var('C', C_Elephant),
% cg_type(Perform, 'Perform'),
% cg_holds('Agnt', Perform, C_Elephant),
% cg_type(Circus, 'Circus'),
% cg_holds('Loc', Perform, Circus),
% lbl(frame83).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

% cg_quantz(type, C_Elephant),
% cg_type(C_Elephant, 'Circus_Elephant'),
% lbl(frame86),
% cg_type(C_Elephant, 'Elephant'),
% frame_var('C', C_Elephant),
% cg_type(Perform, 'Perform'),
% cg_holds('Agnt', Perform, C_Elephant),
% cg_type(Circus, 'Circus'),
% cg_holds('Loc', Perform, Circus),
% lbl(frame85).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE DEPARTURE-DATE(d) IS [UNIV:*d].").
% ===========================================

% cg_quantz(type, D_Univ),
% cg_type(D_Univ, 'Departure_Date'),
% lbl(frame88),
% cg_type(D_Univ, 'Univ'),
% frame_var('D', D_Univ),
% lbl(frame87).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE ELEPHANT(e) IS [UNIV:*e].").
% ===========================================

% cg_quantz(type, E_Univ),
% cg_type(E_Univ, 'Elephant'),
% lbl(frame90),
% cg_type(E_Univ, 'Univ'),
% frame_var('E', E_Univ),
% lbl(frame89).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE HOTEL(h) IS [UNIV:*h].").
% ===========================================

% cg_quantz(type, H_Univ),
% cg_type(H_Univ, 'Hotel'),
% lbl(frame92),
% cg_type(H_Univ, 'Univ'),
% frame_var('H', H_Univ),
% lbl(frame91).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.").
% ===========================================

% cg_quantz(type, 'Reservation_No'),
% cg_type('Reservation_No', 'Hotel_Reservation'),
% lbl(frame94),
% cg_type(Reservation, 'Reservation'),
% frame_var('RESERVATION_NO', Reservation),
% cg_type(Person, 'Person'),
% cg_holds('Rcpt', Reservation, Person),
% cg_type(Room, 'Room'),
% cg_holds('Obj', Person, Room),
% cg_type(Hotel, 'Hotel'),
% cg_holds('Loc', Room, Hotel),
% cg_type(Time_Period, 'Time_Period'),
% cg_holds('Dur', Hotel, Time_Period),
% cg_type(Arrival_Date, 'Arrival_Date'),
% cg_holds('Strt', Reservation, Arrival_Date),
% cg_type(Departure_Date, 'Departure_Date'),
% cg_holds('Untl', Arrival_Date, Departure_Date),
% lbl(frame93).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE PERFORM(p) IS [UNIV:*p].").
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Perform'),
% lbl(frame96),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% lbl(frame95).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE PERSON(p) IS [UNIV:*p].").
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Person'),
% lbl(frame98),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% lbl(frame97).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE PROPOSITION(p) IS [UNIV:*p].").
% ===========================================

% cg_quantz(type, P_Univ),
% cg_type(P_Univ, 'Proposition'),
% lbl(frame100),
% cg_type(P_Univ, 'Univ'),
% frame_var('P', P_Univ),
% lbl(frame99).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE RESERVATION(r) IS [UNIV:*r].").
% ===========================================

% cg_quantz(type, R_Univ),
% cg_type(R_Univ, 'Reservation'),
% lbl(frame102),
% cg_type(R_Univ, 'Univ'),
% frame_var('R', R_Univ),
% lbl(frame101).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE ROOM(r) IS [UNIV:*r].").
% ===========================================

% cg_quantz(type, R_Univ),
% cg_type(R_Univ, 'Room'),
% lbl(frame104),
% cg_type(R_Univ, 'Univ'),
% frame_var('R', R_Univ),
% lbl(frame103).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"TYPE TIME-PERIOD(t) IS [UNIV:*t].").
% ===========================================

% cg_quantz(type, T_Univ),
% cg_type(T_Univ, 'Time_Period'),
% lbl(frame106),
% cg_type(T_Univ, 'Univ'),
% frame_var('T', T_Univ),
% lbl(frame105).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

% cg_holds('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% cg_holds('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% cg_holds('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_quantz(4, Nights_Time_Period),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_holds('Loc', Q2_Room, Shelburne_Hotel),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% cg_type(Shelburne_Hotel, 'Hotel'),
% cg_holds('Obj', John_Sowa_Person, Q2_Room),
% cg_name(Q2_Room, 'Q2'),
% cg_type(Q2_Room, 'Room'),
% cg_holds('Rcpt', Reservation, John_Sowa_Person),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(Reservation, 'Reservation'),
% lbl(frame107).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

% cg_holds('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% cg_holds('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% cg_holds('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_quantz(4, Nights_Time_Period),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_holds('Loc', Q2_Room, Shelburne_Hotel),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% cg_type(Shelburne_Hotel, 'Hotel'),
% cg_holds('Obj', John_Sowa_Person, Q2_Room),
% cg_name(Q2_Room, 'Q2'),
% cg_type(Q2_Room, 'Room'),
% cg_holds('Rcpt', Reservation, John_Sowa_Person),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(Reservation, 'Reservation'),
% lbl(frame108).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.").
% ===========================================

% cg_quantz(individual, _920),
% cg_type(_920, 'Hotel_Reservation'),
% lbl(frame110),
% cg_type(Reservation, 'Reservation'),
% cg_equal(Reservation, 'Reservation#316209'),
% cg_type(John_Sowa_Person, 'Person'),
% cg_name(John_Sowa_Person, 'John_Sowa'),
% cg_holds('Rcpt', Reservation, John_Sowa_Person),
% cg_type(Q2_Room, 'Room'),
% cg_name(Q2_Room, 'Q2'),
% cg_holds('Obj', John_Sowa_Person, Q2_Room),
% cg_type(Shelburne_Hotel, 'Hotel'),
% cg_name(Shelburne_Hotel, 'Shelburne'),
% cg_holds('Loc', Q2_Room, Shelburne_Hotel),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_quantz(4, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_holds('Dur', Shelburne_Hotel, Nights_Time_Period),
% cg_type(March_14_1983_Arrival_Date, 'Arrival_Date'),
% cg_name(March_14_1983_Arrival_Date, 'March_14_1983'),
% cg_holds('Strt', Reservation, March_14_1983_Arrival_Date),
% cg_type(March_18_1983_Departure_Date, 'Departure_Date'),
% cg_name(March_18_1983_Departure_Date, 'March_18_1983'),
% cg_holds('Untl', March_14_1983_Arrival_Date, March_18_1983_Departure_Date),
% _920#316209,
% lbl(frame109).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.").
% ===========================================

% cg_quantz(individual, _918),
% cg_type(_918, 'Hotel_Reservation'),
% lbl(frame112),
% cg_type(Reservation, 'Reservation'),
% cg_equal(Reservation, 'Reservation#316210'),
% cg_type(John_Esch_Person, 'Person'),
% cg_name(John_Esch_Person, 'John_Esch'),
% cg_holds('Rcpt', Reservation, John_Esch_Person),
% cg_type(Q3_Room, 'Room'),
% cg_name(Q3_Room, 'Q3'),
% cg_holds('Obj', John_Esch_Person, Q3_Room),
% cg_type(Sidney_Hotel, 'Hotel'),
% cg_name(Sidney_Hotel, 'Sidney'),
% cg_holds('Loc', Q3_Room, Sidney_Hotel),
% cg_type(Nights_Time_Period, 'Time_Period'),
% cg_quantz(7, Nights_Time_Period),
% cg_name(Nights_Time_Period, 'Nights'),
% cg_holds('Dur', Sidney_Hotel, Nights_Time_Period),
% cg_type(March_12_1983_Arrival_Date, 'Arrival_Date'),
% cg_name(March_12_1983_Arrival_Date, 'March_12_1983'),
% cg_holds('Strt', Reservation, March_12_1983_Arrival_Date),
% cg_type(March_19_1983_Departure_Date, 'Departure_Date'),
% cg_name(March_19_1983_Departure_Date, 'March_19_1983'),
% cg_holds('Untl', March_12_1983_Arrival_Date, March_19_1983_Departure_Date),
% _918#316210,
% lbl(frame111).
```


```
% ===========================================
?- pred_cg(assert_cg_real,"
INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].").
% ===========================================

% cg_quantz(individual, Bumbo),
% cg_type(Bumbo, 'Circus_Elephant'),
% lbl(frame114),
% cg_type(Elephant, 'Elephant'),
% cg_equal(Elephant, 'Elephant#Bumbo'),
% cg_type(Set_Perform, 'Perform'),
% cg_quantz(set, Set_Perform),
% cg_count(Set_Perform, 1, _14388),
% cg_holds('Agnt', Set_Perform, Elephant),
% cg_type(Flying_Tigers_Circus, 'Circus'),
% cg_name(Flying_Tigers_Circus, 'Flying_Tigers'),
% cg_holds('Loc', Set_Perform, Flying_Tigers_Circus),
% Bumbo#'Bumbo',
% lbl(frame113).
```

```
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.28)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

cgpro:  ?-
```

