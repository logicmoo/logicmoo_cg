:- module(cgpro,[cg_reader_tests/0,cg_demo/0]).
:- use_module(library(logicmoo_common)).

:- kb_global(isa/2).

:- expects_dialect(sicstus).
:- ensure_loaded(cgprolog_reader).
:- ensure_loaded(cgprolog_operations).
:- ensure_loaded(cgprolog_translator).
:- ensure_loaded('cgprolog_fwd.pfc').





