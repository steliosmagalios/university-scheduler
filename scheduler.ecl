% Library imports
:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

% === Data structure ===

% lecture(id, semester, duration, groups, professors, roomType).
% professors(id, availablility).
% room(id, type, availablility).
% group(id, count, overlaps).
% task(lectureId, roomId, startTime).

% daySettings(dayHours, Days)
daySettings(14, 5).

% === DUMMY DATA ===

% lecture(id, semester, duration, groups, professors, roomType).
lecture(1, 1, 2, ["g1", "g2", "g3"], ["p1", "p2"], "Aud").
lecture(2, 1, 1, ["g2"], ["p1"], "Lab").
lecture(3, 2, 1, ["g3"], ["p3"], "Aud").

% professor(id, availablility).
professor("p1", []).
professor("p2", []).
professor("p3", []).

% room(id, type, availablility).
room("r1", "Lab", 50, []).
room("r2", "Lab", 50, []).
room("r3", "Aud", 100, []).
room("r4", "Aud", 150, []).

% group(id, count, overlaps).
group("g1", 50, ["g2", "g3"]).
group("g2", 25, ["g1"]).
group("g3", 50, ["g1"]).

% =============================================================

makeProfessorDomains(Professors, ProfessorDomains) :-
  findall(professor(Id, Availability), professor(Id, Availability), Professors),
  createProfessorDomains(Professors, ProfessorDomains).

createProfessorDomains([], []).

createProfessorDomains([professor(Id, Availability) | RestProfs], [(Id, CurrDom) | RestDoms]) :-
  CurrDom #:: Availability,
  createProfessorDomains(RestProfs, RestDoms).

% =============================================================

makeRoomDomains(Rooms, RoomDomains) :-
  findall(room(Id, Type, Capacity, Availability), room(Id, Type, Capacity, Availability), Rooms),
  createRoomDomains(Rooms, RoomDomains).

createRoomDomains([], []).

createRoomDomains([room(Id, _, _, Availability) | RestRooms], [(Id, CurrDom) | RestDoms]) :-
  CurrDom #:: Availability,
  createRoomDomains(RestRooms, RestDoms).

% =============================================================

makeGroupDomains(Groups, GroupDomains) :-
  findall(group(Id, Count, Overlaps), group(Id, Count, Overlaps), Groups),

  % Calculate total week duration
  daySettings(DayLength, WeekDays),
  TotalDuration is DayLength * WeekDays - 1, % Starting from 0

  createGroupDomains(Groups, GroupDomains, TotalDuration).

createGroupDomains([], [], _).

createGroupDomains([group(Id, _, _) | RestGroups], [(Id, CurrDom) | RestDoms], TotalDuration) :-
  CurrDom #:: 0..TotalDuration,
  createGroupDomains(RestGroups, RestDoms).

% =============================================================

intersectDomains([], _). % TODO: Replace _

intersectDomains([CurrDom | Domains], Intersected) :-
  % TODO: Intersect domains
  fail,
  intersectDomains(Domains, Intersected).


% findDomainFromId/3
% findDomainFromId(Id, Domains, Result).
findDomainFromId(_, [], _) :- fail.

findDomainFromId(Id, [(Id, Domain) | _], Domain).

findDomainFromId(Id, [(AnId, _) | Domains], Domain) :-
  AnId \= Id,
  findDomainFromId(Id, Domains, Domain).


% intersectResourceDomains/3
% intersectResourceDomains(Ids, Domains, Intersected).
intersectResourceDomains(Ids, Domains, Intersected) :-
  fail.


% findRoomsOfType/3
% findRoomsOfType(Rooms, Type, FoundRoomIds).
findRoomsOfType([], _, []).

findRoomsOfType([room(Id, Type, _, _) | Rooms], Type, [Id | RoomIds]) :-
  findRoomsOfType(Rooms, Type, RoomIds).

findRoomsOfType([room(Id, RType, _, _) | Rooms], Type, RoomIds) :-
  RType \= Type,
  findRoomsOfType(Rooms, Type, RoomIds).


% applyLectureConstraints(
%  Lectures, Tasks,
%  ProfDomains, RoomDomains, GroupDomains,
%  Professors,  Rooms,       Groups
% ).

applyLectureConstraints([], [], _, _, _, _, _, _).

applyLectureConstraints(
  [lecture(Id, Semester, Duration, GroupIds, ProfessorIds, RoomType) | Lectures],
  [task(Id, RoomId, StartTime) | Tasks],
  ProfDomains, RoomDomains, GroupDomains, Professors, Rooms, Groups
) :-
  % Get professor domains and get their intersection
  intersectResourceDomains(ProfessorIds, ProfDomains, IntersectedProfessors),

  % Get group domains and get their intersection
  intersectResourceDomains(GroupIds, GroupDomains, IntersectedGroups),

  % Intersect the two domains from before
  intersectDomains([IntersectedProfessors, IntersectedGroups], ProfessorGroupDomain),

  % Get all rooms of the correct type and their domain
  findRoomsOfType(Rooms, RoomType, RelevantRoomIds),

  % Make reified constraint for room
  % TODO: RoomId find
  fail,

  % Constraint: the room must be available at the same time as the other domain ???
  % intersectDomains([ProfessorGroupDomain, RoomDomain], LastDomain),
  % - RoomDomain is given by the reified constraint
  fail,

  % Calculate lecture start and end time
  EndTime #= StartTime + Duration,

  % Check that the range [StartTime .. EndTime] is in both domains (LastDomain ???)
  % indomain(???)???
  % ::(Var, Domain, Bool) ??? Reified περιορισμοί πεδίων
  fail,

  % Lecture ends in same day
  fail,
  % StartTime mod 14 #< EndTime mod 14, % 14: dayLength

  applyLectureConstraints(Lectures, Tasks, ProfDomains, RoomDomains, GroupDomains, Professors, Rooms, Groups).

% applyGroupConstraints(Groups, GroupDomains).
applyGroupConstraints([], []).

applyGroupConstraints([Group | Groups], [Domain | Domains]) :-
  % For every group, make all groups with overlaps
  % not available at the same time
  fail,

  applyGroupConstraints(Groups, Domains).

% task(lectureId, roomId, startTime).
getDecisionVars([], []).

getDecisionVars([task(_, RoomId, StartTime) | Tasks], [RoomId, StartTime|Vars]) :-
  getDecisionVars(Tasks, Vars).

% =============================================================

schedule(Tasks) :-
  % Create domains for resources
  makeProfessorDomains(Professors, ProfDomains),
  makeRoomDomains(Rooms, RoomDomains),
  makeGroupDomains(Groups, GroupDomains),

  % fetch lectures
  findall(lecture(Id, Semester, Duration, Groups, Professors, RoomType),
   lecture(Id, Semester, Duration, Groups, Professors, RoomType), Lectures),

  % Apply constraints for every lecture
  applyLectureConstraints(Lectures, Tasks,
   ProfDomains, RoomDomains, GroupDomains,
   Professors,  Rooms,       Groups
  ),

  % Apply constraint for all groups containing same members
  fail,

  % Makespan
  fail,

  % Run branch and bound
  getDecisionVars(Tasks, DesicionVars),
  bb_min(labeling(DesicionVars), 0, _).
