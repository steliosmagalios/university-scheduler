:- module(utilities).

:- lib(ic).
:- lib(ic_global).
:- lib(ic_edge_finder).

% Exports
:- export split_list/3.
:- export unpack_at/3.
:- export apply_disjunctive/2.
:- export remove_duplicates/2.
:- export custom_mod/3.
:- export custom_div/3.
:- export alternative/5. 





%%% split_list/3.
%%% split_list(+List, -Starts, -Durations).
%%%
%%% This predicate splits a list of tuples into two lists,
%%% one containing the starts and the other containing the durations.
%%% The tuples are assumed to be of the form (Start, Duration).

%%% The list is empty
split_list([], [], []).

%%% At least one element
split_list([(Var1, Var2) | RestVars], [Var1 | RestList1], [Var2 | RestList2]) :-
  split_list(RestVars, RestList1, RestList2).





%%% unpack_at/2.
%%% unpack_at(+AtFacts, -Starts, -Durations).
%%%
%%% This predicate accepts a list of at facts and splits them into
%%% a list of start variables and a list of durations.

%%% The list is empty
unpack_at([], [], []).

%%% At least one element
unpack_at([at(_Id, Start, Dur) | RestAts], [Start | Starts], [Dur | Durations]) :-
  unpack_at(RestAts, Starts, Durations).





%%% apply_disjunctive/2.
%%% apply_disjunctive(+Starts, +Durations).
%%%
%%% This predicate is a wrapper for the disjunctive constraint
%%% of the ic_edge_finder library. The sole purpose of this predicate
%%% is to not fail if the list of variables is empty.

%%% If the lists are empty, don't do anything.
apply_disjunctive([], []) :- !.

%%% Else, apply disjunctive
apply_disjunctive(Starts, Durations) :-
  % Ensure the lists are not empty
  Starts \= [], Durations \= [], 

  % Apply disjunctive 
  disjunctive(Starts, Durations).





%%% remove_duplicates/2.
%%% remove_duplicates(+List, -UniqueList).
%%%
%%% This predicate accepts a list and return the unique elements of the list.

%%% Base case
remove_duplicates([], []).

%%% The current element does not appear in the rest of 
%%% the list, so add it to the unique list.
remove_duplicates([Id | RestList], [Id | RestUnique]) :-
  remove_duplicates(RestList, RestUnique),
  not(member(Id, RestList)), !.

%%% The search continues.
remove_duplicates([_Id | RestList], RestUnique) :-
  remove_duplicates(RestList, RestUnique).





custom_division(X, Y, Div, Mod) :-
  % Checks for sanity
  sanity_check(X),
  sanity_check(Y),
  integers([Div]),

  Mod #< abs(Y),
  Mod #= X - Div * Y,
  Mod #>= 0.

custom_mod(X, Y, Mod) :- custom_division(X, Y, _, Mod).

custom_div(X, Y, Div) :- custom_division(X, Y, Div, _).

sanity_check(X):-
  integer(X), !.

sanity_check(X):-
  is_solver_var(X).





%%% alternative/5
%%% The constraint maps each S,D of a class to N Start Duration vars, where each
%%% corresponds to one of N rooms available. RoomsInfo, is a list of available rooms with availabilities.
alternative(S, D, Room, RoomsInfo, RoomAt) :-
  findall(R, member(room(R, _Type, _Cap, _), RoomsInfo), RoomsDomain),
  Room #:: RoomsDomain,
  post_room_cons(S, D, Room, RoomsInfo, RoomAt),
  collectVars(RoomAt, SumStart, DurStart),
  S #= SumStart,
  D #= DurStart.

%%% Post cons
post_room_cons(_, _, _, [], []).

post_room_cons(Start, Duration, Room, [room(RoomID, _Type, _Cap, Available)|RestRooms], [at(RoomID,StR,DurR)|Rest]):-
  % Constraint on duration same everywhare
  DurR #:: [0,Duration],

  % constraint on Start Times (General)
  ic:get_domain_as_list(Start, StartDomain),
  StR #:: [0 | StartDomain],

  % Alternative
  (Room #= RoomID => Start #= StR and DurR #= Duration and ::(Start, Available)),
  (neg ::(Start,Available) => Room #\= RoomID and StR #= 0 and DurR #= 0),
  post_room_cons(Start, Duration, Room, RestRooms, Rest).

%%% Idea is to collect all vars and enforce all but one to be 0.
collectVars([], 0, 0).
collectVars([at(_,St,Dur) | RestAt], SumStart, DurStart):-
	collectVars(RestAt, S, D),
	SumStart #= St + S,
	DurStart #= Dur + D.

%%labelingForTesting Purposes
labelAt([]).
labelAt([at(Room, St, D) | Rest]):-
	labeling([Room, St, D]),
	labelAt(Rest).
