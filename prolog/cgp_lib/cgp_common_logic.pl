

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/util_bb_frame)).
:- use_module(library(cgp_lib/cgp_swipl)).
:- use_module(library(logicmoo_clif)).

kif_to_term(InS,Wff,[]):- 
  locally(t_l:sreader_options(logicmoo_read_kif,true),
      parse_sexpr(string(InS), Wff)),!.
  
kif_to_term_lvars(InS,Out,VarsO):- 
  locally(t_l:sreader_options(logicmoo_read_kif,true),
      must_det_l(( input_to_forms(string(InS), Wff, Vs),               
                copy_lvars(Wff,Vs,Out,VarsO)))),!.
  
run_1_test(String):- 
   wdmsg("================================================="),
   kif_to_term(String,Wff,_Vs),
   wdmsg(testing_string(String)),
   wdmsg(testing_wff(Wff)),
   convert_clif_to_cg(Wff,CG),
   wdmsg(result_cg(CG)),
   wdmsg("================================================="),!.

run_tests:- make, forall(cl_example(String),run_1_test(String)).

convert_clif_to_cg(In,Out):- In=Out.

/*
(documentation Hajj EnglishLanguage "The Pilgrimage to Mecca in Islam.  It is 
the fifth obligatory Pillar of the Five Pillars of Islam for those who are
ablebodied and can afford to do pilgrimage to Mecca at least once in their
lifetime.  It takes place every year in the Islamic month of Dhu al-Hijjah.")
*/

cl_example("
(=>
  (and
    (attribute ?P Muslim)
    (capability Hajj agent ?P))

  (modalAttribute 
    (exists (?H)
      (and
        (instance ?H Hajj)
        (agent ?H ?P)))
    Obligation))  ").
cl_example("
(exists (x y) (and (Red x) (not (Ball x)) (On x y) (not (and (Table y) (not (Blue y))))))").

cl_example("
(exists ((x Drive) (y Chevy) (z Old))
  (and (Person Bob) (City \"St. Louis\")
   (Agnt x Bob)(Dest x \"St. Louis\") (Thme x y) (Poss Bob y) (Attr y z) ))").

% If a cat is on a mat, then the cat is a happy pet.
cl_example("(not (exists ((x Cat) (y Mat)) (and (On x y)(not (exists z) (and (Pet x) (Happy z) (Attr x z))))))").

% For every cat x and every mat y and x is on y, then x is a happy pet.
cl_example("(forall ((x Cat) (y Mat))(if (On x y) (and (Pet x) (exists ((z Happy)) (Attr x z)))))").

cl_example("(exists ((r Relation)) (and (Familial r) (r Bob Sue)))").

cl_example("(exists ( ?y ) (implies (isa ?y Mat)  (Pred ?y ?z)))").

% a cat on a mat
cl_example("(exists ((?x Cat) (?y Mat)) (On ?x ?y))").

cl_example("(not (exists ((?x Cat)) (not (exists ((?y Mat)) (On ?x ?y)))))").

%all cats are on a mat
cl_example("(forall ((?x Cat)) (exists ((?y Mat)) (On ?x ?y)))").

%there are two cats on a mat.
cl_example("(exists ((?y Mat)(?x Cat)(?z Cat)) (and (On ?x ?y)(On ?z ?y)(different ?x ?z)))").

%john goes to Boston by bus... or something like that
cl_example("
(exists ((x Go) (y Bus))
      (and (Person John) (city Boston)
           (Agnt x John) (Dest x Boston) (Inst x y)))").

cl_example("
(exists ((?x Go) (?y Person) (?z City) (?w Bus))
        (and (Name ?y John) (Name ?z Boston)
             (Agnt ?x ?y) (Dest ?x ?z) (Inst ?x ?w)))").


%he believes that mary wants to marry a sailor
cl_example("
(exists ((?x1 person) (?x2 believe))
   (and (expr ?x2 ?x1)
        (thme ?x2
           (exists ((?x3 person) (?x4 want) (?x8 situation))
              (and (name ?x3 'Mary) (expr ?x4 ?x3) (thme ?x4 ?x8)
                   (dscr ?x8 (exists ((?x5 marry) (?x6 sailor))
                                (and (Agnt ?x5 ?x3) (Thme ?x5 ?x6)))))))))").
% cg_reader_tests


skip_cl_example("
(exists ((?x person) (?y rock) (?z place) (?w hard))
        (and (betw ?y ?z ?x) (attr ?z ?w)))").

skip_cl_example(  "
(For a number x, a number y is ((x+7) / sqrt(7)))").


