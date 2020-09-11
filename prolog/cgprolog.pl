:- module(cgpro,[cg_reader_tests/0,cg_demo/0]).
:- set_module(class(library)).
:- use_module(library(logicmoo_common)).
%:- abolish(cgpro:cg_isa/2).
%:- multifile(baseKB:cg_isa/2).
%:- dynamic(baseKB:cg_isa/2).
%:- kb_global(baseKB:cg_isa/2).
%:- baseKB:export(baseKB:cg_isa/2).
%:- import(baseKB:cg_isa/2).



%:- break.
% :- kb_shared(cg_isa/2).

:- expects_dialect(sicstus).
:- ensure_loaded(cgprolog_translator).
:- ensure_loaded(cgprolog_reader).
:- ensure_loaded(cgprolog_api).
:- ensure_loaded(cgprolog_operations).
:- ensure_loaded('cgprolog_fwd.pfc').


