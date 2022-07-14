:- module(constraints).

:- use_module("./utilities").

:- lib(ic).
:- lib(ic_global).
:- lib(ic_edge_finder).

% Exports
:- export lecture_constraints/5.
:- export professor_constraints/3.






%%% lecture_constraints/5
%%% lecture_constraints(+Lectures, -Tasks, +AllProfessors, +AllGroups, +AllRooms)
lecture_constraints([], [], _AllProfessors, _AllGroups, _AllRooms).

lecture_constraints(
  [lecture(Id, Duration, Type, Professors, Groups) | Lectures], 
  [task(Id, Where, When) | Tasks], 
  AllProfessors, AllGroups, AllRooms
) :-
  % Get the lectures's professors intersected availability schedule
  professors_intersected_availability(Professors, AllProfessors, ProfessorsDomain),

  % Get the sum of all participating groups' sizes
  sum_of_groups_sizes(Groups, AllGroups, GroupsSize),

  % Set the domain of the start and end times
  [When] #:: ProfessorsDomain,

  % Apply time constraints to When variable
  EndTime #= When + Duration,
  
  % Lecture should start and end the same day
  custom_mod(When, 14, StartMod), % 14 -> 08:00 - 21:00
  custom_mod(EndTime, 14, EndMod), % 14 -> 08:00 - 21:00
  EndMod #> StartMod,

  % Get the Where variable through the alternative constraint
  filter(AllRooms, correct_type_and_capacity, [Type, GroupsSize], FilteredRoomInfo),
  alternative(When, Duration, Where, FilteredRoomInfo, _RoomsAt),

  % Continue with the rest of the lectures
  lecture_constraints(Lectures, Tasks, AllProfessors, AllGroups, AllRooms).





%%% professor_constraints/3
%%% professor_constraints(+Professors, +Lectures, +Tasks)
professor_constraints([], _Lectures, _Tasks).

professor_constraints([professor(PId, _Times) | Professors], Lectures, Tasks) :-
  % Find all lectures the professor teaches and fetch the Duration and When variables
  findall((When, Dur), (
    member(lecture(Id, Dur, _Type, Profs, _Groups), Lectures),
    member(PId, Profs),
    member(task(Id, _, When), Tasks)
  ), Vars),

  % Split variables to two lists
  split_list(Vars, WhenList, DurationsList),

  % Apply disjunctive to When because a professor
  % can only teach one lecture at a time
  disjunctive(WhenList, DurationsList),

  % Continue with the rest of the professors
  professor_constraints(Professors, Lectures, Tasks).











%%% split_list/3
%%% split_list(+List, -First, -Second).
%%% This predicate accepts a list where all 
%%% elements are pairs and splits it into two lists.
split_list([], [], []).

split_list([(Var1, Var2) | Rest], [Var1 | RestList1], [Var2 | RestList2]) :-
  split_list(Rest, RestList1, RestList2).





%%% sum_of_groups_sizes/3
%%% sum_of_groups_sizes(+Groups, +AllGroups, -TotalSize)
sum_of_groups_sizes([], _AllGroups, 0).

sum_of_groups_sizes([Id | Groups], AllGroups, TotalSize) :-
  member(group(Id, Size, _Overlapping), AllGroups),

  sum_of_groups_sizes(Groups, AllGroups, RestTotalSize),
  TotalSize is Size + RestTotalSize.





%%% professors_intersected_availability/3
%%% professors_intersected_availability(+Professors, +AllProfessors, -Result)
%%% This predicate calculates the time slots that all professors teaching a lecture
%%% are available. The time slots for each professor are provided in the AllProfessors
%%% list. The result is a list of time slots.

% The list of Ids is empty
professors_intersected_availability([], _AllProfessors, []).

% One professor remains
professors_intersected_availability([Id], AllProfessors, Times) :-
  % Find the professors' availability
  member(professor(Id, Times), AllProfessors).

% More that one professor remains
professors_intersected_availability([Id | Professors], AllProfessors, Result) :-
  % Find the professors' availability
  member(professor(Id, Times), AllProfessors),

  % Get the rest of the professors' availability
  professors_intersected_availability(Professors, AllProfessors, RestTimes),

  % Intersect the times
  intersection(Times, RestTimes, Result).



