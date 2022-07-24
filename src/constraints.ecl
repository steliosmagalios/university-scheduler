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





%%% lecture_constraints/6.
%%% lecture_constraints(+Lectures, -Tasks, -AtList, +Professors, +Groups, +Rooms).
lecture_constraints([], [], [], _, _, _).

lecture_constraints(
  [lecture(LecId, Duration, RoomType, LecProfs, LecGroups) | Lectures],
  [task(LecId, Where, When) | Tasks], AtList, Professors, Groups, Rooms
) :-
  % Continue with the rest of the lectures
  lecture_constraints(Lectures, Tasks, RestAtList, Professors, Groups, Rooms),

  % Get the intersected domain of all professors teaching this lecture
  get_all_professors_domain(LecProfs, Professors, WhenDomain),

  % Get the total number of students in the lecture
  get_total_number_of_students(LecGroups, Groups, TotalStudents),

  % Get all eligible rooms for this lecture
  filter(Rooms, is_room_eligible, [RoomType, TotalStudents], EligibleRooms),

  % Create the When variable and apply time constraints
  [When] #:: WhenDomain,
  EndTime #= When + Duration,

  % Lecture must start and end in the same day
  custom_mod(When, 14, StartMod),
  custom_mod(EndTime, 14, EndMod),
  StartMod #< EndMod,

  % Apply alternative constraint for all eligible rooms
  alternative(When, Duration, Where, EligibleRooms, CurrAtList),


  % Add the current AtList to the final AtList
  append(RestAtList, CurrAtList, AtList).





professor_constraints([], _Lectures, _Tasks) :- !.

professor_constraints([professor(Id, _Availability) | RestProfs], Lectures, Tasks) :-
  professor_constraints(RestProfs, Lectures, Tasks),

  % Get the lectures taught by this professor
  filter(Lectures, is_professor_in_lecture, [Id], FilteredLectures),
  map(FilteredLectures, get_id, [], LectureIds),

  % Get the tasks for these lectures
  filter(Tasks, id_in_list, [LectureIds], FilteredTasks),

  % Get the variables
  map(FilteredLectures, map_lecture_to_vars, [FilteredTasks], VarList),

  split_list(VarList, StartList, DurList),
  apply_disjunctive(StartList, DurList).





group_constraints([], _Lectures, _Tasks).

group_constraints([group(Id, _Members, Overlapping) | Groups], Lectures, Tasks) :-
  group_constraints(Groups, Lectures, Tasks),

  % Get all lectures that this group and 
  % all the overlapping ones are in.
  get_lectures_of_groups([Id | Overlapping], Lectures, FilteredLectures),
  % filter(Lectures, is_group_in_lecture, [[Id | Overlapping]], FilteredLectures),

  % Get the tasks for these lectures and remove duplicate tasks
  map(FilteredLectures, get_id, [], LectureIds),
  filter(Tasks, id_in_list, [LectureIds], FilteredTasks),
  remove_duplicates(FilteredTasks, UniqueTasks),

  % Get the variables
  map(FilteredLectures, map_lecture_to_vars, [UniqueTasks], VarList),

  split_list(VarList, StartList, DurList),
  apply_disjunctive(StartList, DurList).





room_constraints([], _RoomAts).

room_constraints([room(Id, _Type, _Capacity, _Availability) | RestRooms], RoomAts) :-
  room_constraints(RestRooms, RoomAts),

  % Get all ats for this room
  filter(RoomAts, id_in_list, [[Id]], FilteredAtRooms),
  unpack_at(FilteredAtRooms, WhenList, DurList),

  apply_disjunctive(WhenList, DurList).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_lectures_of_groups([], _Lectures, []).

get_lectures_of_groups([Id | GroupIds], Lectures, FilteredLectures) :-
  get_lectures_of_groups(GroupIds, Lectures, RestFilteredLectures),

  filter(Lectures, is_group_in_lecture, [Id], CurrFilteredLectures),
  append(CurrFilteredLectures, RestFilteredLectures, FilteredLectures).




split_list([], [], []).

split_list([(Var1, Var2) | RestVars], [Var1 | RestList1], [Var2 | RestList2]) :-
  split_list(RestVars, RestList1, RestList2).



unpack_at([], [], []) :- !.

unpack_at([at(_Id, Start, Dur) | RestAts], [Start | WhenList], [Dur | DurList]) :-
  unpack_at(RestAts, WhenList, DurList).


apply_disjunctive([], []) :- !.

apply_disjunctive(WhenList, DurList) :-
  WhenList \= [], DurList \= [],
  disjunctive(WhenList, DurList).




get_all_professors_domain(LecProfs, Professors, WhenDomain) :-
  filter(Professors, id_in_list, [LecProfs], FilteredProfessors),
  map(FilteredProfessors, map_domain, [], ProfessorDomains),
  reduce(ProfessorDomains, get_professor_domain, [], WhenDomain), !.





get_total_number_of_students(LecGroups, Groups, TotalStudents) :-
  filter(Groups, id_in_list, [LecGroups], FilteredGroups),
  map(FilteredGroups, map_number_of_students, [], GroupNumbers),
  ic_global:sumlist(GroupNumbers, TotalStudents), !.



% remove_duplicates(List, Result).
remove_duplicates([], []).

remove_duplicates([Head | RestTasks], [Head | RestResult]) :-
  Head = task(_Id, _Where, _When),
  not(member(Head, RestTasks)), !,

  remove_duplicates(RestTasks, RestResult).

remove_duplicates([Head | RestTasks], RestResult) :-
  Head = task(Id, _Where, _When),
  member(task(Id, _, _), RestTasks), !,
  
  remove_duplicates(RestTasks, RestResult).




















% %%% lecture_constraints/6
% %%% lecture_constraints(+Lectures, -Tasks, +AllProfessors, +AllGroups, +AllRooms)
% lecture_constraints([], [], [], _Professors, _Groups, _Rooms).

% lecture_constraints(
%   [lecture(LecId, Duration, Type, LecProfs, LecGroups) | Lectures],
%   [task(LecId, Where, When) | Tasks], AtList,
%   Professors, Groups, Rooms
% ) :-
%   % Get the domain of all the professors teaching the lecture
%   filter(Professors, correct_id, [LecProfs], FilteredProfessors),
%   map(FilteredProfessors, extract_professor_time, [], MappedTimes),
%   reduce(MappedTimes, intersect_domains, [], ProfessorDomain),

%   % Calculate the number of students in the lecture
%   filter(Groups, correct_id, [LecGroups], FilterGroups),
%   map(FilterGroups, extract_group_size, [], GroupSizes),
%   ic_global:sumlist(GroupSizes, NumbeOfStudents),

%   % Filter the eligible rooms for the lecture
%   filter(Rooms, correct_type_and_capacity, [Type, NumbeOfStudents], FilteredRooms), !,

%   % Apply time constrains
%   [When] #:: ProfessorDomain,

%   EndTime #= When + Duration,

%   custom_mod(When, 14, StartTimeMod),
%   custom_mod(EndTime, 14, EndTimeMod),
%   EndTimeMod #> StartTimeMod,

%   % Apply room constraints
%   alternative(When, Duration, Where, FilteredRooms, RoomsAt),  


%   lecture_constraints(Lectures, Tasks, RestAts, Professors, Groups, Rooms),
%   append(RoomsAt, RestAts, AtList).





% %%% professor_constraints/3
% %%% professor_constraints(+Professors, +Lectures, +Tasks)
% professor_constraints([], _Lectures, _Tasks).

% professor_constraints([professor(Id, _Times) | RestProfessors], Lectures, Tasks) :-
%   findall((When, Duration), (
%     member(lecture(LecId, Duration, _Type, Profs, _Groups), Lectures),
%     member(Id, Profs),
%     member(task(LecId, _Where, When), Tasks)
%   ), VarList),

%   split_list(VarList, WhenList, DurationList), !,
%   apply_disjunctive(WhenList, DurationList),

%   professor_constraints(RestProfessors, Lectures, Tasks).





% %%% group_constraints/3
% %%% group_constraints(+Groups, +Lectures, +Tasks)
% group_constraints([], _Lectures, _Tasks).

% group_constraints([group(Id, _Members, Overlapping) | RestGroups], Lectures, Tasks) :-
%   % Get the When and Duration of all lectures that have this group or any other overlapping
%   get_vars_of_groups([Id | Overlapping], Lectures, Tasks, WhenList, DurationList), !,

%   apply_disjunctive(WhenList, DurationList),

%   group_constraints(RestGroups, Lectures, Tasks).





% %%% room_constraits/3
% %%% room_constraints(+Rooms, +Lectures, +Tasks)
% room_constraints([], _RoomAts).

% room_constraints([room(Id, _Type, _Capacity, _Times) | RestRooms], RoomAts) :-
%   findall((Start, Dur), member(at(Id, Start, Dur), RoomAts), VarList),
%   split_list(VarList, StartList, DurList), !,
%   apply_disjunctive(StartList, DurList),

%   room_constraints(RestRooms, RoomAts).



% apply_disjunctive([], []).

% apply_disjunctive(WhenList, DurationList) :-
%   WhenList \= [], DurationList \= [],
%   disjunctive(WhenList, DurationList).




% get_vars_of_groups([], _Lectures, _Tasks, [], []).

% get_vars_of_groups([GId | RestIds], Lectures, Tasks, WhenList, DurationList) :-
%   findall((When, Duration), (
%     member(lecture(LecId, Duration, _Type, _Profs, Groups), Lectures),
%     member(GId, Groups),
%     member(task(LecId, _Where, When), Tasks)
%   ), VarList),

%   split_list(VarList, CurrWhenList, CurrDurationList), !,
%   get_vars_of_groups(RestIds, Lectures, Tasks, RestWhenList, RestDurationList),
%   append(CurrWhenList, RestWhenList, WhenList),
%   append(CurrDurationList, RestDurationList, DurationList).





% split_list([], [], []).

% split_list([(Item1, Item2) | RestItems], [Item1 | RestList1], [Item2 | RestList2]) :-
%   split_list(RestItems, RestList1, RestList2).
