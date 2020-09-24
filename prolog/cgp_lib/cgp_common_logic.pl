

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
   kif_to_term(String,Wff,Vs),
   wdmsg(testing_string(String)),
   wdmsg(testing_wff(Wff-Vs)),
   convert_clif_to_cg(Wff,CG),
   wdmsg(result_cg(CG)),
   wdmsg("================================================="),!.

run_tests:- make, forall(cl_example(String),run_1_test(String)).

convert_clif_to_cg(In,Out):- In=Out.




cl_example("(exists ((?x Cat) (?y Mat)) (On ?x ?y))").
cl_example("(not (exists ((?x Cat)) (not (exists ((?y Mat)) (On ?x ?y)))))").
cl_example("(forall ((?x Cat)) (exists ((?y Mat)) (On ?x ?y)))").
cl_example("
(exists ((x Go) (y Bus))
      (and (Person John) (city Boston)
           (Agnt x John) (Dest x Boston) (Inst x y) )) ").
cl_example("
(exists ((?x Go) (?y Person) (?z City) (?w Bus))
        (and (Name ?y John) (Name ?z Boston)
             (Agnt ?x ?y) (Dest ?x ?z) (Inst ?x ?w)))").



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
        (and (betw ?y ?z ?x) (attr ?z ?w)))

").

