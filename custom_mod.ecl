%%% Mod Operator
:- module(custom_mod).
:- export cmod/3.

:-lib(ic).

%%% mod/3
%%% mod(X,Y,Mod)
%%% Mod = X mod Y
cmod(X, Y, Mod) :-
  sanity_check(X),
  sanity_check(Y),
  integers([X]),

  Mod #< abs(Y),
  Mod #= X - N * Y,
  Mod #> 0.

sanity_check(X):-
  integer(X),!.

sanity_check(X):-
  is_solver_var(X).
