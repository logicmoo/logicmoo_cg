# logicmoo_cg
Conceptual Graph (CG) Libraries in Prolog




===========================================
[Mat]1-(Attrib)->[Color #1]
===========================================

% attrib(Mat, Color),
% Color='Color#1',
% isa(Color, 'Color'),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% lbl(frame317).



===========================================
[Mat]1-(Attrib)->[Color]2
===========================================

% attrib(Mat, Color),
% Color='Color#2',
% isa(Color, 'Color'),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% lbl(frame318).



===========================================
[CAT_QUANT:@every]-(On)->[Mat]
===========================================

% on(Every_Cat_quant, Mat),
% isa(Mat, 'Mat'),
% quantz(every, Every_Cat_quant),
% isa(Every_Cat_quant, 'Cat_quant'),
% lbl(frame319).



===========================================
[A_CAT]->(On)->[Mat]
===========================================

% on(A_cat, Mat),
% isa(Mat, 'Mat'),
% isa(A_cat, 'A_cat'),
% lbl(frame320).



===========================================
[THE_CAT:#666]->(On)->[Mat]
===========================================

% on(The_cat, Mat),
% isa(Mat, 'Mat'),
% The_cat='The_cat#666',
% isa(The_cat, 'The_cat'),
% lbl(frame321).



===========================================
[NAMED_CAT:Moris]->(On)->[Mat]
===========================================

% on(Moris_Named_cat, Mat),
% isa(Mat, 'Mat'),
% named(Moris_Named_cat, ['Moris']),
% isa(Moris_Named_cat, 'Named_cat'),
% lbl(frame322).



===========================================
[LENGTH:@5ft]<-(SizeOf)-[Mat]
===========================================

% sizeof(Mat, Ft_Length),
% isa(Mat, 'Mat'),
% named(Ft_Length, [ft]),
% quantz(5, Ft_Length),
% isa(Ft_Length, 'Length'),
% lbl(frame323).



===========================================
[LENGTH:@5ft.]<-(SizeOf)-[Mat]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Length',:,@,5,ft,'.',']',<-,'(','SizeOf',')',-,'[','Mat',']']))
   ).



===========================================
[CAT_SET_NONE:{}]-(On)->[Mat]
===========================================

% on(Cat_set_none, Mat),
% isa(Mat, 'Mat'),
% countof(Cat_set_none, 0, 0),
% isa(Cat_set_none, 'Cat_set_none'),
% lbl(frame325).



===========================================
[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]
===========================================

% on(Set_Cats_one_or_more, Mat),
% isa(Mat, 'Mat'),
% countof(Set_Cats_one_or_more, 1, _7404),
% quantz(set, Set_Cats_one_or_more),
% isa(Set_Cats_one_or_more, 'Cats_one_or_more'),
% lbl(frame326).



===========================================
[CAT_FIVE:{*}@5]-(On)->[Mat]
===========================================

% on(Set_Cat_five, Mat),
% isa(Mat, 'Mat'),
% quantz(5, Set_Cat_five),
% countof(Set_Cat_five, 1, _708),
% quantz(set, Set_Cat_five),
% isa(Set_Cat_five, 'Cat_five'),
% lbl(frame327).



===========================================
[CAT_M:{Moris}]-(On)->[Mat]
===========================================

% on(Moris_Set_Cat_m, Mat),
% isa(Mat, 'Mat'),
% each_of(Moris_Set_Cat_m, ['Moris']),
% quantz(set, Moris_Set_Cat_m),
% isa(Moris_Set_Cat_m, 'Cat_m'),
% lbl(frame328).



===========================================
[CAT_FM:{Felix,Moris}]-(On)->[Mat]
===========================================

% on(FelixMoris_Set_Cat_fm, Mat),
% isa(Mat, 'Mat'),
% each_of(FelixMoris_Set_Cat_fm, ['Felix', 'Moris']),
% quantz(set, FelixMoris_Set_Cat_fm),
% isa(FelixMoris_Set_Cat_fm, 'Cat_fm'),
% lbl(frame329).



===========================================
[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Cat_set_min_two',:,'{','Felix',',','Moris',',',*,'}',']',-,'(','On',')',->,'[','Mat',']']))
   ).



===========================================
[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Cat_set_five',:,'{','Felix',',','Moris',',',*,'}',@,5,']',-,'(','On',')',->,'[','Mat',']']))
   ).



===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Citizen',:,x,']',<-,memberOf,-,'[','Country',:,'Oz',']',:-,'[','Person',:,x,']',<-,'Agnt',-,'[','Being_Born',']',-,'Loc',->,'[','Country',:,'Oz',']','.']))
   ).



===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-childOf-[PERSON: y],
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Citizen',:,x,']',<-,memberOf,-,'[','Country',:,'Oz',']',:-,'[','Person',:,x,']',<-,childOf,-,'[','Person',:,y,']',',','[','Citizen',:,y,']',<-,memberOf,-,'[','Country',:,'Oz',']','.']))
   ).



===========================================

[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Citizen',:,x,']',<-,memberOf,-,'[','Country',:,'Oz',']',:-,'[','Person',:,x,']',<-,'Rcpt',-,'[','Naturalize',']',-,'Loc',->,'[','Country',:,'Oz',']','.']))
   ).



===========================================

[PERSON : Tinman]-
              -childOf->[GIRL : Dorothy],
              <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].
===========================================

% loc(Being_Born_Event, Oz_Country),
% named(Oz_Country, ['Oz']),
% isa(Oz_Country, 'Country'),
% agnt(Being_Born_Event, Tinman_Person),
% isa(Being_Born_Event, 'Being_Born'),
% childof(Tinman_Person, Dorothy_Girl),
% named(Dorothy_Girl, ['Dorothy']),
% isa(Dorothy_Girl, 'Girl'),
% named(Tinman_Person, ['Tinman']),
% isa(Tinman_Person, 'Person'),
% lbl(frame335).



===========================================
['Man':imad]<-agnt-['Drive']-obj->['Car']
===========================================

% obj(Drive_Event, Car),
% isa(Car, 'Car'),
% agnt(Drive_Event, Imad_Man),
% isa(Drive_Event, 'Drive'),
% named(Imad_Man, [imad]),
% isa(Imad_Man, 'Man'),
% lbl(frame336).



===========================================
[Cat#1]-(On)->[Mat #1]-(Attrib)->[Color #1]
===========================================

% attrib(Cat, Color),
% Color='Color#1',
% isa(Color, 'Color'),
% on(Cat, Mat),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% Cat='Cat#1',
% isa(Cat, 'Cat'),
% lbl(frame337).



===========================================
[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]
===========================================

% on(X_, Mat),
% isa(Mat, 'Mat'),
% attrib(X_, C1),
% isa(C1, 'C1'),
% isa(X_, 'Cat'),
% lbl(frame338).



===========================================
[Cat: ?x]-(On)->[Mat]
===========================================

% on(X_, Mat),
% isa(Mat, 'Mat'),
% isa(X_, 'Cat'),
% lbl(frame339).



===========================================
[Cat: ?x]-(On)->[*MatC]
===========================================

% on(X_, MatC),
% isa(X_, 'Cat'),
% lbl(frame340).



===========================================
[Cat: ?x]-(On)->[Mat: *MatC]
===========================================

% on(X_, Mat),
% isa(Mat, 'Mat'),
% isa(X_, 'Cat'),
% lbl(frame341).



===========================================
[Man:karim]<-agnt-[Drink]-obj->[Water]
===========================================

% obj(Drink_Event, Water),
% isa(Water, 'Water'),
% agnt(Drink_Event, Karim_Man),
% isa(Drink_Event, 'Drink'),
% named(Karim_Man, [karim]),
% isa(Karim_Man, 'Man'),
% lbl(frame342).



===========================================
[Mat #1]<- (on)- [Cat #1]
===========================================

% on(Cat, Mat),
% Cat='Cat#1',
% isa(Cat, 'Cat'),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% lbl(frame343).



===========================================
[Mat]<-(On)-[Cat: ?x]
===========================================

% on(X_, Mat),
% isa(X_, 'Cat'),
% isa(Mat, 'Mat'),
% lbl(frame344).



===========================================
[Color #1]<-(Attrib)-[Mat #1]
===========================================

% attrib(Mat, Color),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% Color='Color#1',
% isa(Color, 'Color'),
% lbl(frame345).



===========================================
[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]
===========================================

% attrib(Cat, Color),
% Color='Color#1',
% isa(Color, 'Color'),
% on(Cat, Mat),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% Cat='Cat#1',
% isa(Cat, 'Cat'),
% lbl(frame346).



===========================================
[Man:karim]<-agnt-[Drink]-obj->[Water]
===========================================

% obj(Drink_Event, Water),
% isa(Water, 'Water'),
% agnt(Drink_Event, Karim_Man),
% isa(Drink_Event, 'Drink'),
% named(Karim_Man, [karim]),
% isa(Karim_Man, 'Man'),
% lbl(frame347).



===========================================
[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]
===========================================

% on(Cat, Mat),
% Cat='Cat#1',
% isa(Cat, 'Cat'),
% attrib(Mat, Color),
% Mat='Mat#1',
% isa(Mat, 'Mat'),
% Color='Color#1',
% isa(Color, 'Color'),
% lbl(frame348).



===========================================
[Cat: @every]->(On)->[Mat]
===========================================

% on(Every_Cat, Mat),
% isa(Mat, 'Mat'),
% quantz(every, Every_Cat),
% isa(Every_Cat, 'Cat'),
% lbl(frame349).



===========================================
[CAT]->(STAT)->[SIT]->(LOC)->[MAT].
===========================================

% loc(Sit, Mat),
% isa(Mat, 'Mat'),
% stat(Cat, Sit),
% isa(Sit, 'Sit'),
% isa(Cat, 'Cat'),
% lbl(frame350).



===========================================
[CAT]->(STAT)->[SIT]->(LOC)->[MAT]
===========================================

% loc(Sit, Mat),
% isa(Mat, 'Mat'),
% stat(Cat, Sit),
% isa(Sit, 'Sit'),
% isa(Cat, 'Cat'),
% lbl(frame351).



===========================================

   [Drive *x] [Person: Bob] [City: "St. Louis"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x "St. Louis") (Thme ?x ?y) (Poss Bob ?y)
===========================================

% rel('Poss', ['Bob', Y_Chevy]),
% rel('Thme', [X_Drive, Y_Chevy]),
% rel('Dest', [X_Drive, "St. Louis"]),
% rel('Agnt', [X_Drive, 'Bob']),
% isa(Y_Chevy, 'Chevy'),
% textof(St_c46_c32_Louis_City, "St. Louis"),
% isa(St_c46_c32_Louis_City, 'City'),
% named(Bob_Person, ['Bob']),
% isa(Bob_Person, 'Person'),
% isa(X_Drive, 'Drive'),
% lbl(frame352).



===========================================

   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) ->
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','A_cat',']',->,'(','KnowsAbout',')',->,'[','The_cat',:,#,666,']',->,'(','KnowsAbout',')',->,'[','Named_cat',:,'Moris',']',->,'(','KnowsAbout',')',->,'[','Length',:,@,5,ft,']',->,'(','KnowsAbout',')',->,'[','Cat_set',:,'{',*,'}',']',->,'(','KnowsAbout',')',->,'[','Cat5',:,'{',*,'}',@,5,']',->,'(','KnowsAbout',')',->,'[','Cats_two',:,'{','Moris',',','Felix','}',']',->,'(','KnowsAbout',')',->,'[','Cats_one_or_more',:,'{','Moris',',',*,'}',']']))
   ).



===========================================
[Relation: *r] (Familial ?r) (#?r Bob Sue)
===========================================

% rel(R, ['Bob', 'Sue']),
% R=Relation,
% rel('Familial', [Relation]),
% isa(Relation, 'Relation'),
% lbl(frame354).



===========================================

[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.
===========================================

% loc(Cat, Mat),
% isa(Mat, 'Mat'),
% stat(Cat, Sit),
% isa(Cat, 'Cat'),
% isa(Sit, 'Sit'),
% lbl(frame355).



===========================================
?x -(Attrib)-> [Color #1]
===========================================

  isa(Color, 'Color').
  attrib(X, Color).
  Color='Color#1'.
  lbl(frame356).
% pred_cg(call_cg_real, xtext("?x -(Attrib)-> [Color #1]")).



===========================================
?x -(On)->[Mat #1]-(Attrib)->[Color #1]
===========================================

  on(X, Mat).
  isa(Color, 'Color').
  isa(Mat, 'Mat').
  attrib(X, Color).
  Color='Color#1'.
  Mat='Mat#1'.
  lbl(frame357).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]-(Attrib)->[Color #1]")).



===========================================
?x -(On)->[Mat #1]
===========================================

  on(X, Mat).
  isa(Mat, 'Mat').
  Mat='Mat#1'.
  lbl(frame358).
% pred_cg(call_cg_real, xtext("?x -(On)->[Mat #1]")).



===========================================
[?x] -(Attrib)-> [Color #1]
===========================================

  isa(Color, 'Color').
  isa(X, '?(X)').
  attrib(X, Color).
  Color='Color#1'.
  lbl(frame359).
% pred_cg(call_cg_real, xtext("[?x] -(Attrib)-> [Color #1]")).



===========================================
[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]
===========================================

  on(X, Mat).
  isa(Color, 'Color').
  isa(Mat, 'Mat').
  isa(X, '?(X)').
  attrib(X, Color).
  Color='Color#1'.
  Mat='Mat#1'.
  lbl(frame360).
% pred_cg(call_cg_real, xtext("[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]")).



===========================================
[Mat ?x]-(Attrib)->[Color #1]
===========================================

  isa(X, 'Mat').
  isa(Color, 'Color').
  attrib(X, Color).
  Color='Color#1'.
  lbl(frame361).
% pred_cg(call_cg_real, xtext("[Mat ?x]-(Attrib)->[Color #1]")).



===========================================
[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]
===========================================

  on(X, Mat).
  isa(X, 'Cat').
  isa(Color, 'Color').
  isa(Mat, 'Mat').
  attrib(X, Color).
  Color='Color#2'.
  Mat='Mat#1'.
  lbl(frame362).
% pred_cg(call_cg_real, xtext("[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]")).



===========================================
[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[',a,']',-,'(',belives,')',->,'[',statement,:,'[','Cat',:,@,every,']',->,'(','On',')',->,'[','Mat',']',']']))
   ).



===========================================
[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[',a,']',-,'(',belives,')',->,'[',statement2,=,'[','Cat',:,@,every,']',->,'(','On',')',->,'[','Mat',']',']']))
   ).



===========================================


[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]
===========================================

% inst(Go_Event, Bus),
% isa(Bus, 'Bus'),
% dest(Go_Event, Boston_City),
% named(Boston_City, ['Boston']),
% isa(Boston_City, 'City'),
% agnt(Go_Event, John_Person),
% named(John_Person, ['John']),
% isa(John_Person, 'Person'),
% isa(Go_Event, 'Go'),
% lbl(frame367).



===========================================

   [Person: John2] <- (Agnt) -
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]
===========================================

% inst(Go2_Event, Bus2),
% isa(Go2_Event, 'Go2'),
% dest(Bus2, Boston2_City_Event),
% isa(Bus2, 'Bus2'),
% agnt(Boston2_City_Event, John2_Person),
% named(Boston2_City_Event, ['Boston2']),
% isa(Boston2_City_Event, 'City'),
% named(John2_Person, ['John2']),
% isa(John2_Person, 'Person'),
% lbl(frame368).



===========================================

[Begin]-
        -obj->[Session],
        -srce->[Proposition =
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John]
               ],
        -agnt->[Person : John]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Begin',']',-,-,obj,->,'[','Session',']',',',-,srce,->,'[','Proposition',=,'[','Press',']',-,-,obj,->,'[','Key',:,enter,']',-,partOf,->,'[','Keyboard',']',',',-,agnt,->,'[','Person',:,'John',']',']',',',-,agnt,->,'[','Person',:,'John',']']))
   ).



===========================================

 [a] - (belives) ->
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[',a,']',-,'(',belives,')',->,'[',statement,=,'[','Go2',']',-,'(','Agnt',')',->,'[','Person',:,'John2',']',-,'(','Dest',')',->,'[','City',:,'Boston2',']',-,'(','Inst',')',->,'[','Bus2',']',']']))
   ).



===========================================
[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)
===========================================

% rel('Inst', [X_Go, W_Bus]),
% rel('Dest', [X_Go, Z_Boston_City]),
% rel('Agnt', [X_Go, Y_John_Person]),
% isa(W_Bus, 'Bus'),
% named(Z_Boston_City, ['Boston']),
% isa(Z_Boston_City, 'City'),
% named(Y_John_Person, ['John']),
% isa(Y_John_Person, 'Person'),
% isa(X_Go, 'Go'),
% lbl(frame374).



===========================================
[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]
===========================================

% on(Red_Woman, Table),
% isa(Table, table),
% obj(Eat_Event, Apple),
% isa(Apple, 'Apple'),
% agnt(Eat_Event, Karim_Man),
% isa(Eat_Event, 'Eat'),
% knows(Karim_Man, Red_Woman),
% named(Karim_Man, [karim]),
% isa(Karim_Man, 'Man'),
% named(Red_Woman, [red]),
% isa(Red_Woman, 'Woman'),
% lbl(frame375).



===========================================
[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]
===========================================

% thme(Marry_Event, Sailor),
% isa(Sailor, 'Sailor'),
% agnt(Marry_Event, X),
% isa(Marry_Event, 'Marry'),
% isa(X, '?(X)'),
% lbl(frame376).



===========================================

[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Person',:,'Mary',*,x,']',<-,'(','Expr',')',-,'[','Want',']',-,'(','Thme',')',->,'[','Situation',:,'[',?('X'),']',<-,'(','Agnt',')',-,'[','Marry',']',-,'(','Thme',')',->,'[','Sailor',']',']']))
   ).



===========================================

[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Proposition',:,'[','Person',:,'Mary',*,x,']',<-,'(','Expr',')',-,'[','Want',']',-,'(','Thme',')',->,'[','Situation',:,'[',?('X'),']',<-,'(','Agnt',')',-,'[','Marry',']',-,'(','Thme',')',->,'[','Sailor',']',']',']']))
   ).



===========================================

[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Person',:,'Tom',']',<-,'(','Expr',')',-,'[','Believe',']',-,'(','Thme',')',->,'[','Proposition',:,'[','Person',:,'Mary',*,x,']',<-,'(','Expr',')',-,'[','Want',']',-,'(','Thme',')',->,'[','Situation',:,'[',?('X'),']',<-,'(','Agnt',')',-,'[','Marry',']',-,'(','Thme',')',->,'[','Sailor',']',']',']']))
   ).



===========================================

[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['[','Person',:,'Tom',']',<-,'(','Expr',')',<-,'[','Believe',']',->,'(','Thme',')',-,'[','Proposition',:,'[','Person',:,'Mary',*,x,']',<-,'(','Expr',')',<-,'[','Want',']',->,'(','Thme',')',-,'[','Situation',:,'[',?('X'),']',<-,'(','Agnt',')',<-,'[','Marry',']',->,'(','Thme',')',->,'[','Sailor',']',']',']']))
   ).



===========================================
TYPE ARRIVAL-DATE(a) IS [UNIV:*a].
===========================================

% isa(A_Univ, 'Univ'),
% decl(type, rEL('Arrival-date', [A_Univ])),
% lbl(frame398).



===========================================
TYPE CIRCUS(c) IS [UNIV:*c].
===========================================

% isa(C_Univ, 'Univ'),
% decl(type, rEL('Circus', [C_Univ])),
% lbl(frame399).



===========================================
TYPE CIRCUS(c) IS [UNIV:*c]
===========================================

% isa(C_Univ, 'Univ'),
% decl(type, rEL('Circus', [C_Univ])),
% lbl(frame400).



===========================================
[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]
===========================================

% loc(Perform_Event, Circus),
% isa(Circus, 'Circus'),
% agnt(Perform_Event, C_Elephant),
% isa(Perform_Event, 'Perform'),
% isa(C_Elephant, 'Elephant'),
% lbl(frame401).



===========================================

[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus])

===========================================

% rel('Inst', [X_Go, Bus]),
% isa(Bus, 'Bus'),
% rel('Dest', [X_Go, Boston_City]),
% named(Boston_City, ['Boston']),
% isa(Boston_City, 'City'),
% rel('Agnt', [X_Go, John_Person]),
% named(John_Person, ['John']),
% isa(John_Person, 'Person'),
% isa(X_Go, 'Go'),
% lbl(frame402).



===========================================
TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['Type','Circus-elephant','(','C',')','Is','[','Elephant',:,*,'C',']',<-,'(','Agnt',')',<-,'[','Perform',']',->,'(','Loc',')',->,'[','Circus',']','.']))
   ).



===========================================
TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['Type','Circus-elephant','(','C',')','Is','[','Elephant',:,*,'C',']',<-,'(','Agnt',')',<-,'[','Perform',']',->,'(','Loc',')',->,'[','Circus',']','.']))
   ).



===========================================
TYPE DEPARTURE-DATE(d) IS [UNIV:*d].
===========================================

% isa(D_Univ, 'Univ'),
% decl(type, rEL('Departure-date', [D_Univ])),
% lbl(frame405).



===========================================
TYPE ELEPHANT(e) IS [UNIV:*e].
===========================================

% isa(E_Univ, 'Univ'),
% decl(type, rEL('Elephant', [E_Univ])),
% lbl(frame406).



===========================================
TYPE HOTEL(h) IS [UNIV:*h].
===========================================

% isa(H_Univ, 'Univ'),
% decl(type, rEL('Hotel', [H_Univ])),
% lbl(frame407).



===========================================
TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.
===========================================


% Failed Parse
?- rtrace(
    pred_cg(assert_cg_real,tOkS(['Type','Hotel-reservation','(','Reservation-no',')','Is','[','Reservation',:,*,'Reservation-no',']',-,->,'(','Rcpt',')',->,'[','Person',']',->,'(','Obj',')',->,'[','Room',']',->,'(','Loc',')',->,'[','Hotel',']',->,'(','Dur',')',->,'[','Time-period',']',-,->,'(','Strt',')',->,'[','Arrival-date',']',->,'(','Untl',')',->,'[','Departure-date',']',',',',','.']))
   ).



===========================================
TYPE PERFORM(p) IS [UNIV:*p].
===========================================

% isa(P_Univ, 'Univ'),
% decl(type, rEL('Perform', [P_Univ])),
% lbl(frame409).



===========================================
TYPE PERSON(p) IS [UNIV:*p].
===========================================

% isa(P_Univ, 'Univ'),
% decl(type, rEL('Person', [P_Univ])),
% lbl(frame410).



===========================================
TYPE PROPOSITION(p) IS [UNIV:*p].
===========================================

% isa(P_Univ, 'Univ'),
% decl(type, rEL('Proposition', [P_Univ])),
% lbl(frame411).



===========================================
TYPE RESERVATION(r) IS [UNIV:*r].
===========================================

% isa(R_Univ, 'Univ'),
% decl(type, rEL('Reservation', [R_Univ])),
% lbl(frame412).



===========================================
TYPE ROOM(r) IS [UNIV:*r].
===========================================

% isa(R_Univ, 'Univ'),
% decl(type, rEL('Room', [R_Univ])),
% lbl(frame413).



===========================================
TYPE TIME-PERIOD(t) IS [UNIV:*t].
===========================================

% isa(T_Univ, 'Univ'),
% decl(type, rEL('Time-period', [T_Univ])),
% lbl(frame414).



===========================================

[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]
===========================================

% untl(Marchd14c1983_Arrival_date, Marchd18c1983_Departure_date),
% named(Marchd18c1983_Departure_date, ['March', 18, named([1983])]),
% isa(Marchd18c1983_Departure_date, 'Departure-date'),
% strt(Reservation, Marchd14c1983_Arrival_date),
% named(Marchd14c1983_Arrival_date, ['March', 14, named([1983])]),
% isa(Marchd14c1983_Arrival_date, 'Arrival-date'),
% dur(Shelburne_Hotel, Nights_Time_period),
% named(Nights_Time_period, ['Nights']),
% quantz(4, Nights_Time_period),
% isa(Nights_Time_period, 'Time-period'),
% loc(Q2_Room, Shelburne_Hotel),
% named(Shelburne_Hotel, ['Shelburne']),
% isa(Shelburne_Hotel, 'Hotel'),
% obj(JohnSowa_Person, Q2_Room),
% named(Q2_Room, ['Q2']),
% isa(Q2_Room, 'Room'),
% rcpt(Reservation, JohnSowa_Person),
% named(JohnSowa_Person, ['John', 'Sowa']),
% isa(JohnSowa_Person, 'Person'),
% Reservation='Reservation#316209',
% isa(Reservation, 'Reservation'),
% lbl(frame415).



===========================================

[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]
===========================================

% untl(Marchd14c1983_Arrival_date, Marchd18c1983_Departure_date),
% named(Marchd18c1983_Departure_date, ['March', 18, named([1983])]),
% isa(Marchd18c1983_Departure_date, 'Departure-date'),
% strt(Reservation, Marchd14c1983_Arrival_date),
% named(Marchd14c1983_Arrival_date, ['March', 14, named([1983])]),
% isa(Marchd14c1983_Arrival_date, 'Arrival-date'),
% dur(Shelburne_Hotel, Nights_Time_period),
% named(Nights_Time_period, ['Nights']),
% quantz(4, Nights_Time_period),
% isa(Nights_Time_period, 'Time-period'),
% loc(Q2_Room, Shelburne_Hotel),
% named(Shelburne_Hotel, ['Shelburne']),
% isa(Shelburne_Hotel, 'Hotel'),
% obj(JohnSowa_Person, Q2_Room),
% named(Q2_Room, ['Q2']),
% isa(Q2_Room, 'Room'),
% rcpt(Reservation, JohnSowa_Person),
% named(JohnSowa_Person, ['John', 'Sowa']),
% isa(JohnSowa_Person, 'Person'),
% Reservation='Reservation#316209',
% isa(Reservation, 'Reservation'),
% lbl(frame416).



===========================================

INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.
===========================================

% untl(Marchd14c1983_Arrival_date, Marchd18c1983_Departure_date),
% named(Marchd18c1983_Departure_date, ['March', 18, named([1983])]),
% isa(Marchd18c1983_Departure_date, 'Departure-date'),
% strt(Reservation, Marchd14c1983_Arrival_date),
% named(Marchd14c1983_Arrival_date, ['March', 14, named([1983])]),
% isa(Marchd14c1983_Arrival_date, 'Arrival-date'),
% dur(Shelburne_Hotel, Nights_Time_period),
% named(Nights_Time_period, ['Nights']),
% quantz(4, Nights_Time_period),
% isa(Nights_Time_period, 'Time-period'),
% loc(Q2_Room, Shelburne_Hotel),
% named(Shelburne_Hotel, ['Shelburne']),
% isa(Shelburne_Hotel, 'Hotel'),
% obj(JohnSowa_Person, Q2_Room),
% named(Q2_Room, ['Q2']),
% isa(Q2_Room, 'Room'),
% rcpt(Reservation, JohnSowa_Person),
% named(JohnSowa_Person, ['John', 'Sowa']),
% isa(JohnSowa_Person, 'Person'),
% Reservation='Reservation#316209',
% isa(Reservation, 'Reservation'),
% rel(_3314, []),
% _3314#316209,
% lbl(frame417).



===========================================

INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.
===========================================

% untl(Marchd12c1983_Arrival_date, Marchd19c1983_Departure_date),
% named(Marchd19c1983_Departure_date, ['March', 19, named([1983])]),
% isa(Marchd19c1983_Departure_date, 'Departure-date'),
% strt(Reservation, Marchd12c1983_Arrival_date),
% named(Marchd12c1983_Arrival_date, ['March', 12, named([1983])]),
% isa(Marchd12c1983_Arrival_date, 'Arrival-date'),
% dur(Sidney_Hotel, Nights_Time_period),
% named(Nights_Time_period, ['Nights']),
% quantz(7, Nights_Time_period),
% isa(Nights_Time_period, 'Time-period'),
% loc(Q3_Room, Sidney_Hotel),
% named(Sidney_Hotel, ['Sidney']),
% isa(Sidney_Hotel, 'Hotel'),
% obj(JohnEsch_Person, Q3_Room),
% named(Q3_Room, ['Q3']),
% isa(Q3_Room, 'Room'),
% rcpt(Reservation, JohnEsch_Person),
% named(JohnEsch_Person, ['John', 'Esch']),
% isa(JohnEsch_Person, 'Person'),
% Reservation='Reservation#316210',
% isa(Reservation, 'Reservation'),
% rel(_3312, []),
% _3312#316210,
% lbl(frame418).



===========================================

INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].
===========================================

% loc(Set_Perform_Event, FlyingTigers_Circus),
% named(FlyingTigers_Circus, ['Flying', 'Tigers']),
% isa(FlyingTigers_Circus, 'Circus'),
% agnt(Set_Perform_Event, Elephant),
% countof(Set_Perform_Event, 1, _5686),
% quantz(set, Set_Perform_Event),
% isa(Set_Perform_Event, 'Perform'),
% Elephant='Elephant#Bumbo',
% isa(Elephant, 'Elephant'),
% rel(Bumbo, []),
% Bumbo#'Bumbo',
% lbl(frame419).
true.

cgpro:  ?-


