:- module(scheduler_utilities).

:- lib(ic).

:- export custom_mod/3.
:- export intersect_lists/2.

:- export get_room_domain/5.
:- export calculate_makespan/2.
:- export get_professor_domains/3.

:- export get_vars_from_tasks/2.
:- export create_decision_vars/5.

%%% calculate_makespan/2
%%% calculate_makespan(Tasks, Makespan)
%%% Calculates the makespan for the schedule/5 predicate.

% TODO 2022-Jun-24: Placeholder makespan calculator
calculate_makespan(_, 0).



%%% get_vars_from_tasks/2
%%% get_vars_from_tasks(Tasks, TaskVars).
get_vars_from_tasks([], []).

get_vars_from_tasks([task(_, Where, When) | Tasks], [Where, When | Rest]) :-
  get_vars_from_tasks(Tasks, Rest).



%%% intersect_lists/2
%%% intersect_lists(Lists, Intersected).
%%% A predicate that applies the intersection/3 predicate to a list
%%% containing other lists and returns the result.

%%% List is empty
intersect_lists([], []).

%%% Only one list remains
intersect_lists([List], List).

%%% For >= 2 lists, intersect the first 2 lists and put the result back in
%%% the starting list (reduce style).
intersect_lists([List1, List2 | Rest], Intersected) :-
  % intersect the 2 lists
  intersection(List1, List2, Temp),

  % Add the temporary result to the list and continue (reduce style)
  intersect_lists([Temp | Rest], Intersected).



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



%%% calculate_group_members/3
%%% calculate_group_members(GroupIds, AllGroups, Total).
%%% A predicate that calculates the total number of members in
%%% the groups who's groupIds are in the GroupIds list.
calculate_group_members(GroupIds, AllGroups, Total) :-
  % Find the Count for every groups
  findall(Count, (
    member(GId, GroupIds),
    member(group(GId, Count, _), AllGroups)
  ), AllCounts),

  % Get the sum of the members
  sum(AllCounts, Total).



% lecture(Id, Duration, Semester, Professors, Groups).

%%% get_professor_domains/3
%%% get_professor_domains(Lecture, AllProfessors, Domain).
%%% A predicate that given a lecture, returns the availability
%%% intersection of all professors that teach the specified lecture.
get_professor_domains(lecture(Id, _Dur, _Sem, _Type, Profs, _Grp), AllProfs, Domain) :-
  % Find availability domain for all professors in the lecture
  findall(Time, (member(Id, Profs), member(professor(Id, Time), AllProfs)), ProfTimes),

  % Intersect the domains the return the Final Domain
  intersect_lists(ProfTimes, Domain).



%%% get_room_domain/5
%%% get_room_domain(Lecture, AllRooms, AllGroups, SelectedRoom, SelectedDomain).
%%% A predicate that given a lecture, returns the id and the availability
%%% of a room that has the capacity to seat every student in the lecture and
%%% the id of that specific room.
get_room_domain(lecture(Id, _Dur, _Sem, Type, _Profs, Groups), AllRooms, AllGroups, SelectedRoom, SelectedDomain) :-
  % Calculate total member count for groups in the lecture
  calculate_group_members(Groups, AllGroups, TotalMembers),

  % Find all rooms that fulfill the requirements
  FilteredRooms = [], fail, % TODO 2022-Jun-24:

  % Select a single room
  element(_Idx, FilteredRooms, room(SelectedRoom, _Type, _Cap, SelectedDomain)).



%%% create_decision_vars/5
%%% create_decision_vars(Lecture, ProfDomains, RoomId, RoomDomain, Task).
%%% A predicate that for a given lecture, creates the StartTime decision variable
%%% from the supplied domain forthe professors and the room and applies the necessary
%%% constraints to it.
create_decision_vars(lecture(Id, Dur, _S, _T, _P, _G), ProfDomains, RoomId, RoomDomain, task(LecId, RoomId, StartTime)) :-
  % Intersect the two domains
  intersection(ProfDomains, RoomDomain, LectureDomain),

  % Create the StartTime variable
  StartTime #:: LectureDomain,
  EndTime #= StartTime + Dur,

  % A Lecture must end the same day it starts
  custom_mod(StartTime, 14, StartMod), % 14 -> Arbitrary
  custom_mod(EndTime, 14, EndMod), % 14 -> Arbitrary
  EndMod #> StartMod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Alternative Constraint

%%% Alternative
%%% The constraint maps each S,D of a class to N Start Duration vars, where each
%%% corresponds to one of N rooms available. RoomsInfo, is a list of available rooms with availabilities.
alternative(S,D,Room,RoomsInfo,RoomAt):-
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
        get_domain_as_list(Start,StartDomain),
        StR #:: [0|StartDomain],
        % Alternative
	     (Room #= RoomID => Start #= StR and DurR #= Duration and ::(Start,Available)),
	     (neg ::(Start,Available) => Room#\= RoomID and StR #=0 and DurR #=0),
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
