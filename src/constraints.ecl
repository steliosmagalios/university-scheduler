:- module(constraints).

:- use_module("./utilities").
:- use_module('./map_filter_reduce').

:- lib(ic).
:- lib(ic_global).
:- lib(ic_edge_finder).

% Exports
:- export lecture_constraints/6.
:- export professor_constraints/3.
:- export group_constraints/3.
:- export room_constraints/2.





%%% lecture_constraints/6
%%% lecture_constraints(+Lectures, -Tasks, +AllProfessors, +AllGroups, +AllRooms)
lecture_constraints([], [], [], _Professors, _Groups, _Rooms).

lecture_constraints(
  [lecture(LecId, Duration, Type, LecProfs, LecGroups) | Lectures],
  [task(LecId, Where, When) | Tasks], AtList,
  Professors, Groups, Rooms
) :-
  % Get the domain of all the professors teaching the lecture
  filter(Professors, correct_id, [LecProfs], FilteredProfessors),
  map(FilteredProfessors, extract_professor_time, [], MappedTimes),
  reduce(MappedTimes, intersect_domains, [], ProfessorDomain),

  % Calculate the number of students in the lecture
  filter(Groups, correct_id, [LecGroups], FilterGroups),
  map(FilterGroups, extract_group_size, [], GroupSizes),
  ic_global:sumlist(GroupSizes, NumbeOfStudents),

  % Filter the eligible rooms for the lecture
  filter(Rooms, correct_type_and_capacity, [Type, NumbeOfStudents], FilteredRooms), !,

  % Apply time constrains
  [When] #:: ProfessorDomain,

  EndTime #= When + Duration,

  custom_mod(When, 14, StartTimeMod),
  custom_mod(EndTime, 14, EndTimeMod),
  EndTimeMod #> StartTimeMod,

  % Apply room constraints
  alternative(When, Duration, Where, FilteredRooms, RoomsAt),  


  lecture_constraints(Lectures, Tasks, RestAts, Professors, Groups, Rooms),
  append(RoomsAt, RestAts, AtList).





%%% professor_constraints/3
%%% professor_constraints(+Professors, +Lectures, +Tasks)
professor_constraints([], _Lectures, _Tasks).

professor_constraints([professor(Id, _Times) | RestProfessors], Lectures, Tasks) :-
  findall((When, Duration), (
    member(lecture(LecId, Duration, _Type, Profs, _Groups), Lectures),
    member(Id, Profs),
    member(task(LecId, _Where, When), Tasks)
  ), VarList),

  split_list(VarList, WhenList, DurationList), !,
  disjunctive(WhenList, DurationList),

  professor_constraints(RestProfessors, Lectures, Tasks).





%%% group_constraints/3
%%% group_constraints(+Groups, +Lectures, +Tasks)
group_constraints([], _Lectures, _Tasks).

group_constraints([group(Id, _Members, Overlapping) | RestGroups], Lectures, Tasks) :-
  % Get the When and Duration of all lectures that have this group or any other overlapping
  get_vars_of_groups([Id | Overlapping], Lectures, Tasks, WhenList, DurationList), !,

  disjunctive(WhenList, DurationList),

  group_constraints(RestGroups, Lectures, Tasks).





%%% room_constraits/3
%%% room_constraints(+Rooms, +Lectures, +Tasks)
room_constraints([], _RoomAts).

room_constraints([room(Id, _Type, _Capacity, _Times) | RestRooms], RoomAts) :-
  findall((Start, Dur), member(at(Id, Start, Dur), RoomAts), VarList),
  split_list(VarList, StartList, DurList), !,
  disjunctive(StartList, DurList),

  room_constraints(RestRooms, RoomAts).






get_vars_of_groups([], _Lectures, _Tasks, [], []).

get_vars_of_groups([GId | RestIds], Lectures, Tasks, WhenList, DurationList) :-
  findall((When, Duration), (
    member(lecture(LecId, Duration, _Type, _Profs, Groups), Lectures),
    member(GId, Groups),
    member(task(LecId, _Where, When), Tasks)
  ), VarList),

  split_list(VarList, CurrWhenList, CurrDurationList), !,
  get_vars_of_groups(RestIds, Lectures, Tasks, RestWhenList, RestDurationList),
  append(CurrWhenList, RestWhenList, WhenList),
  append(CurrDurationList, RestDurationList, DurationList).





split_list([], [], []).

split_list([(Item1, Item2) | RestItems], [Item1 | RestList1], [Item2 | RestList2]) :-
  split_list(RestItems, RestList1, RestList2).
