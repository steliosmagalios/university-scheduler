:- module(constraints).

:- use_module("./utilities").
:- use_module('./map_filter_reduce').

:- lib(ic).
:- lib(ic_global).

% Exports
:- export lecture_constraints/6.
:- export professor_constraints/3.
:- export group_constraints/3.
:- export room_constraints/2.





%%% lecture_constraints/6.
%%% lecture_constraints(+Lectures, -Tasks, -AtList, +Professors, +Groups, +Rooms).
%%%
%%% This predicate accepts the list of lectures and creates the decision variables When and Where.
%%% The decision variables are calculated as such:
%%%   - When: The when variable is the time slot that the lecture will begin, and has a domain
%%%           of the intersection of the domains of the professors teaching the lecture.
%%%
%%%   - Where: The where variable is the room that the lecture will be held, and the value
%%%            of the variable is provided by the alternative constraint. The domain of the
%%%            variable is the ids of the rooms that are fit to host the lecture. A room
%%%            is fit to host a lecture if it is large enough to host the lecture, and if it
%%%            is of the correct type.

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





%%% professor_constraints/3.
%%% professor_constraints(+Professors, +Lectures, +Tasks).
%%% 
%%% This predicate accepts all professors provided and applies a constraint
%%% to each of them. The constraint is that the professor can only teach one
%%% lecture at a time.

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

  % Apply disjuctive
  split_list(VarList, StartList, DurList),
  apply_disjunctive(StartList, DurList).





%%% group_constraints/3.
%%% group_constraints(+Groups, +Lectures, +Tasks).
%%% 
%%% This predicate accepts all groups provided in the input
%%% and applied the necessary constraints for every group.
%%% The constraint is that all groups that have overlapping members
%%% (groups with overlapping members are in the Overlapping list) cannot
%%% be scheduled in the same time.

group_constraints([], _Lectures, _Tasks).

group_constraints([group(Id, _MemberCount, Overlapping) | RestGroups], Lectures, Tasks) :-
  group_constraints(RestGroups, Lectures, Tasks),

  % Get all lectures that this group and the
  % overlapping groups are in, and remove the duplicates
  get_lectures_of_groups([Id | Overlapping], Lectures, FilteredLectures),
  remove_duplicate_lectures(FilteredLectures, UniqueLectures),

  % Get the tasks for these lectures and extract the variables from them
  map(UniqueLectures, get_id, [], UniqueLectureIds),
  filter(Tasks, id_in_list, [UniqueLectureIds], FilteredTasks),
  map(UniqueLectures, map_lecture_to_vars, [FilteredTasks], GroupVars), 

  % Split the variables and apply disjunctive constraints
  split_list(GroupVars, StartList, DurList),
  apply_disjunctive(StartList, DurList).





%%% room_constraints/2.
%%% room_constraints(+Rooms, +RoomAts).
%%%
%%% This predicate accepts a list of rooms and a list of 
%%% at facts and applies the room constraints.
%%% The constraint applied is that a room can host only
%%% one lecture at a time. This is achieved by appliying the
%%% disjunctive constraint on the variables of the at facts for every room.

room_constraints([], _RoomAts).

room_constraints([room(Id, _Type, _Capacity, _Availability) | RestRooms], RoomAts) :-
  room_constraints(RestRooms, RoomAts),

  % Get all ats for this room
  filter(RoomAts, id_in_list, [[Id]], FilteredAtRooms),

  % Unpack the at facts and apply disjuctive  
  unpack_at(FilteredAtRooms, WhenList, DurList),
  apply_disjunctive(WhenList, DurList).



% ============= Lecture constraints helper predicates =============



%%% get_all_professors_domain/3.
%%% get_all_professors_domain(+LectureProfs, +Professors, -Domain).
%%%
%%% This predicate accepts a list of professor ids and a list of professors
%%% and calculated the time slots where all professors are available.
%%% The calculation is the intersection of all domains.
get_all_professors_domain(LecProfs, Professors, WhenDomain) :-
  % Get all professors in the LecProfs list
  filter(Professors, id_in_list, [LecProfs], FilteredProfessors),
  
  % Get the domains of all professors
  map(FilteredProfessors, map_domain, [], ProfessorDomains),
  
  % Intersect the domains
  reduce(ProfessorDomains, get_professor_domain, [], WhenDomain), !.





%%% get_total_number_of_students/3.
%%% get_total_number_of_students(+LecGroups, +Groups, -TotalStudents).
%%%
%%% This predicates accepts a list of group ids and returns the total
%%% number of students in those groups.
get_total_number_of_students(LecGroups, Groups, TotalStudents) :-
  % Filter all the groups that are in the LecGroup list
  filter(Groups, id_in_list, [LecGroups], FilteredGroups),

  % Get the number of students in each group
  map(FilteredGroups, map_number_of_students, [], GroupNumbers),

  % Calculate the total number of students and return it
  ic_global:sumlist(GroupNumbers, TotalStudents), !.



% ============= Group constraints helper predicates =============



%%% get_lectures_of_groups/3.
%%% get_lectures_of_groups(+GroupIds, +Lectures, -FilteredLectures).
%%%
%%% This predicate accepts a list of group ids and returns all the 
%%% lectures that contain at least one of these ids in the groups list.

%%% No id left
get_lectures_of_groups([], _Lectures, []).

%%% Get the lectures of a group
get_lectures_of_groups([Id | GroupIds], Lectures, FilteredLectures) :-
  get_lectures_of_groups(GroupIds, Lectures, RestFilteredLectures),

  % Get the lectures of this group
  filter(Lectures, is_group_in_lecture, [Id], CurrFilteredLectures),
  
  % Append the current filtered lectures to the rest
  append(CurrFilteredLectures, RestFilteredLectures, FilteredLectures).





%%% remove_duplicate_lectures/2.
%%% remove_duplicate_lectures(+Lectures, -UniqueLectures).
%%%
%%% This predicate removes duplicate lectures from a list of lectures.

remove_duplicate_lectures(Lectures, UniqueLectures) :-
  % Get the ids of the lectures
  map(Lectures, get_id, [], LectureIds), 
  
  % Filter out the duplicates
  remove_duplicates(LectureIds, UniqueLectureIds), 

  % Reconstruct the lecture list with only unique ones
  ids_to_lectures(UniqueLectureIds, Lectures, UniqueLectures). 





%%% ids_to_lectures/3.
%%% ids_to_lectures(+Ids, +Lectures, -UniqueLectures).
%%% 
%%% This predicate takes a list of ids and a list of lectures and
%%% returns a list of lectures with only the ids that are in the list.
%%% The purpose of this predicate is to reconstruct a list of lectures
%%% in the remove_duplicate_lectures predicate.

%%% Base case
ids_to_lectures([], _Lectures, []).

%%% Recursive case
ids_to_lectures([CurrId | RestIds], Lectures, [CurrLecture | RestLectures]) :-
  ids_to_lectures(RestIds, Lectures, RestLectures),
  get_lecture_by_id(CurrId, Lectures, CurrLecture).





%%% get_lecture_by_id/3.
%%% get_lecture_by_id(+Id, +Lectures, -Lecture).
%%%
%%% This predicates accepts and id and a list of lectures
%%% and returns the first instance of the lecture with the given id.
%%% If no lecture is found, the predicate fails.

%%% If we reach the end of the lecture list, the predicate fails.
get_lecture_by_id(_Id, [], _) :- !, fail.

%%% If the provided id matches the id of the current lecture, return it.
get_lecture_by_id(Id, [CurrLec | _RestLectures], CurrLec) :-
  CurrLec =.. [_Pred, Id | _RestArgs], !.

%%% Otherwise, continue searching.
get_lecture_by_id(Id, [_CurrLec | RestLectures], CurrLec) :-
  get_lecture_by_id(Id, RestLectures, CurrLec).
