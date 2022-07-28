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

id_in_list(Item, List) :-
  Item =.. [_Pred, Id | _Rest],
  member(Id, List).

map_domain(professor(_Id, Domain), Domain).

get_professor_domain(D1, D2, Intersected) :-
  intersection(D1, D2, Intersected).

map_number_of_students(group(_Id, Students, _Overlapping), Students).

is_room_eligible(room(_Id, Type, Capacity, _Availability), Type, MinCapacity) :-
  Capacity >= MinCapacity.

map_lecture_to_vars(lecture(Id, Duration, _Type, _Profs, _Groups), (When, Duration), TaskList) :-
  member(task(Id, _Where, When), TaskList), !.

is_professor_in_lecture(lecture(_Id, _Duration, _Type, Profs, _Groups), ProfId) :-
  member(ProfId, Profs).

get_id(Item, Id) :-
  Item =.. [_Pred, Id | _Rest].

is_group_in_lecture(lecture(_Id, _Duration, _Type, _Profs, Groups), GroupId) :-
  member(GroupId, Groups).

get_when(task(_Id, _Where, When), When).


list_from_ids(Id, Item, ListOfItems) :-
  first_occurrence(Id, Item, ListOfItems).

first_occurrence(Id, _Item, []) :- fail.

first_occurrence(Id, Item, [Item | Rest]) :-
  Item =.. [_Pred, Id | _Rest], !.

first_occurrence(Id, Item, [_Item | Rest]) :-
  first_occurrence(Id, Item, Rest).
