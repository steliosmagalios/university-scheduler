:- module(scheduler_utilities).

:- lib(ic).
:- lib(ic_global).

:- export custom_mod/3.
:- export alternative/5. 





%%% custom_mod/3
%%% custom_mod(X,Y,Mod)
%%% Mod = X mod Y
custom_mod(X, Y, Mod) :-
  % Checks for sanity
  sanity_check(X),
  sanity_check(Y),
  integers([N]),

  Mod #< abs(Y),
  Mod #= X - N * Y,
  Mod #>= 0.

sanity_check(X):-
  integer(X), !.

sanity_check(X):-
  is_solver_var(X).





%%% alternative/5
%%% The constraint maps each S,D of a class to N Start Duration vars, where each
%%% corresponds to one of N rooms available. RoomsInfo, is a list of available rooms with availabilities.
alternative(S, D, Room, RoomsInfo, RoomAt) :-
  findall(R,member(infoRoom(R,_),RoomsInfo),RoomsDomain),
  Room #:: RoomsDomain,
  post_room_cons(S,D,Room,RoomsInfo,RoomAt),
  collectVars(RoomAt,SumStart,DurStart),
  S #= SumStart,
  D #= DurStart.

%%% Post cons
post_room_cons(_,_,_,[],[]).

post_room_cons(Start,Duration,Room,[infoRoom(RoomID,Available)|RestRooms],[at(RoomID,StR,DurR)|Rest]):-
  % Constraint on duration same everywhare
  DurR #:: [0,Duration],

  % constraint on Start Times (General)
  ic:get_domain_as_list(Start, StartDomain),
  StR #:: [0 | StartDomain],

  % Alternative
  (Room #= RoomID => Start #= StR and DurR #= Duration and ::(Start, Available)),
  (neg ::(Start,Available) => Room #\= RoomID and StR #= 0 and DurR #= 0),
  post_room_cons(Start,Duration,Room,RestRooms,Rest).

%%% Idea is to collect all vars and enforce all but one to be 0.
collectVars([],0,0).
collectVars([at(_,St,Dur)|RestAt],SumStart,DurStart):-
	collectVars(RestAt,S,D),
	SumStart #= St + S,
	DurStart #= Dur + D.

%%labelingForTesting Purposes
labelAt([]).
labelAt([at(Room,St,D)|Rest]):-
	labeling([Room,St,D]),
	labelAt(Rest).
