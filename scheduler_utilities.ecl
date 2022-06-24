:- module(scheduler_utilities).

:- lib(ic).

:- export custom_mod/3.
:- export intersectLists/2.

:- export getRoomDomain/5.
:- export calculateMakespan/2.
:- export getProfessorDomains/3.

:- export getVarsFromTasks/2.
:- export createDecisionVars/5.

%%% calculateMakespan/2
%%% calculateMakespan(Tasks, Makespan)
%%% Calculates the makespan for the schedule/5 predicate.

% TODO 2022-Jun-24: Placeholder makespan calculator
calculateMakespan(_, 0). 



%%% getVarsFromTasks/2
%%% getVarsFromTasks(Tasks, TaskVars).
getVarsFromTasks([], []).

getVarsFromTasks([task(_, Where, When) | Tasks], [Where, When | Rest]) :-
  getVarsFromTasks(T, Rest).



%%% intersectLists/2
%%% intersectLists(Lists, Intersected).
%%% A predicate that applies the intersection/3 predicate to a list
%%% containing other lists and returns the result. 

%%% List is empty
intersectLists([], []).

%%% Only one list remains
intersectLists([List], List).

%%% For >= 2 lists, intersect the first 2 lists and put the result back in
%%% the starting list (reduce style).
intersectLists([List1, List2 | Rest], Intersected) :-
  % intersect the 2 lists
  intersection(List1, List2, Temp),

  % Add the temporary result to the list and continue (reduce style)
  intersectLists([Temp | Rest], Intersected).



%%% mod/3
%%% mod(X,Y,Mod)
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



%%% calculateGroupMembers/3
%%% calculateGroupMembers(GroupIds, AllGroups, Total).
%%% A predicate that calculates the total number of members in
%%% the groups who's groupIds are in the GroupIds list.
calculateGroupMembers(GroupIds, AllGroups, Total) :-
  % Find the Count for every groups
  findall(Count, (
    member(GId, GroupIds),
    member(group(GId, Count, _), AllGroups)
  ), AllCounts),

  % Get the sum of the members
  sum(AllCounts, Total).



% lecture(Id, Duration, Semester, Professors, Groups).

%%% getProfessorDomains/3
%%% getProfessorDomains(Lecture, AllProfessors, Domain).
%%% A predicate that given a lecture, returns the availability  
%%% intersection of all professors that teach the specified lecture.
getProfessorDomains(lecture(Id, _Dur, _Sem, _Type, Profs, _Grp), AllProfs, Domain) :-
  % Find availability domain for all professors in the lecture
  findall(Time, (member(Id, Profs), member(professor(Id, Time), AllProfs)), ProfTimes),
  
  % Intersect the domains the return the Final Domain
  intersectLists(ProfTimes, Domain).



%%% getRoomDomain/5
%%% getRoomDomain(Lecture, AllRooms, AllGroups, SelectedRoom, SelectedDomain).
%%% A predicate that given a lecture, returns the id and the availability
%%% of a room that has the capacity to seat every student in the lecture and 
%%% the id of that specific room.
getRoomDomain(lecture(Id, _Dur, _Sem, Type, _Profs, Groups), AllRooms, AllGroups, SelectedRoom, SelectedDomain) :-
  % Calculate total member count for groups in the lecture
  calculateGroupMembers(Groups, AllGroups, TotalMembers),

  % Find all rooms that fulfill the requirements 
  FilteredRooms = [], fail, % TODO 2022-Jun-24: 

  % Select a single room
  element(_Idx, FilteredRooms, room(SelectedRoom, _Type, _Cap, SelectedDomain)).



%%% createDecisionVars/5
%%% createDecisionVars(Lecture, ProfDomains, RoomId, RoomDomain, Task).
%%% A predicate that for a given lecture, creates the StartTime decision variable
%%% from the supplied domain forthe professors and the room and applies the necessary
%%% constraints to it.
createDecisionVars(lecture(Id, Dur, _S, _T, _P, _G), ProfDomains, RoomId, RoomDomain, task(LecId, RoomId, StartTime)) :-
  % Intersect the two domains
  intersection(ProfDomains, RoomDomain, LectureDomain),

  % Create the StartTime variable
  StartTime #:: LectureDomain,
  EndTime #= StartTime + Dur,

  % A Lecture must end the same day it starts
  custom_mod(StartTime, 14, StartMod), % 14 -> Arbitrary
  custom_mod(EndTime, 14, EndMod), % 14 -> Arbitrary
  EndMod #> StartMod.
