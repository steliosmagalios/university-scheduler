:- module(map_filter_reduce).

:- export map/4.
:- export filter/4.
:- export reduce/4.





map([], _Predicate, _Arguments, []) :- !.

map([Item | RestItems], Predicate, Arguments, [MappedItem | RestMapped]) :-
  C =.. [Predicate, Item, MappedItem | Arguments],
  call(C), !,

  map(RestItems, Predicate, Arguments, RestMapped).





filter([], _Predicate, _Arguments, []) :- !.

filter([Item | RestItems], Predicate, Arguments, [Item | RestResult]) :-
  C =.. [Predicate, Item | Arguments],
  call(C), !,

  filter(RestItems, Predicate, Arguments, RestResult).

filter([Item | RestItems], Predicate, Arguments, Result) :-
  C =.. [Predicate, Item | Arguments],
  not(call(C)), !,

  filter(RestItems, Predicate, Arguments, Result).





% If the list is empty, the predicate fails
reduce([], _Predicate, _Arguments, _Result) :- fail.

reduce([Item], _Predicate, _Arguments, Item) :- !.

reduce([Item1, Item2 | RestItems], Predicate, Arguments, Result) :-
  C =.. [Predicate, Item1, Item2, Accumulator | Arguments],
  call(C), !,

  reduce([Accumulator | RestItems], Predicate, Arguments, Result).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

correct_id(Item, IdList) :-
  Item =.. [_PredName, Id | _Rest],
  member(Id, IdList).



correct_type_and_capacity(room(_Id, Type, Capacity, _Times), Type, MinCapacity) :-
  Capacity >= MinCapacity.



get_when(task(_Id, _Where, When), When).



extract_professor_time(professor(Id, Times), Times).


intersect_domains(Domain1, Domain2, Domain) :-
  intersection(Domain1, Domain2, Domain).


extract_group_size(group(Id, Size, _Overlapping), Size).