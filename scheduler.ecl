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

lecture(1, 1, 2, ["g1", "g2", "g3"], ["p1", "p2"], "Aud").
lecture(2, 1, 1, ["g2"], ["p1"], "Lab").
lecture(3, 2, 1, ["g3"], ["p3"], "Aud").

professor("p1", []).
professor("p2", []).
professor("p3", []).

room("r1", "Lab", 50, []).
room("r2", "Lab", 50, []).
room("r3", "Aud", 100, []).
room("r4", "Aud", 150, []).

group("g1", 50, ["g2", "g3"]).
group("g2", 25, ["g1"]).
group("g3", 50, ["g1", "g2"]).

% =============================================================

makeProfessorDomains(Professors, ProfessorDomains) :-
  findall(P, P = professor(_, _), Professors),
  createProfessorDomains(Professors, ProfessorDomains).

createProfessorDomains([], []).

createProfessorDomains([professor(Id, Availability) | RestProfs], [(Id, CurrDom) | RestDoms]) :-
  CurrDom #:: Availability,
  createProfessorDomains(RestProfs, RestDoms).

% =============================================================

makeRoomDomains(Rooms, RoomDomains) :-
  findall(R, R = room(_, _, _, _), Rooms),
  createRoomDomains(Rooms, RoomDomains).

createRoomDomains([], []).

createRoomDomains([room(Id, _, _, Availability) | RestRooms], [(Id, CurrDom) | RestDoms]) :-
  CurrDom #:: Availability,
  createRoomDomains(RestRooms, RestDoms).

% =============================================================

makeGroupDomains(Groups, GroupDomains) :-
  findall(P, P = group(_, _, _), Groups),
  createGroupDomains(Groups, GroupDomains).

createGroupDomains([], []).

createGroupDomains([group(Id, _, _, _) | RestGroups], [(Id, CurrDom) | RestDoms]) :-
  CurrDom #:: 69, % 5 days * 14 hours per day
  createGroupDomains(RestGroups, RestDoms).

% =============================================================

applyLectureConstraints([]).

applyLectureConstraints([Curr|Rest]) :-
  fail.


schedule(Tasks) :-
  % Create domains for resources
  makeProfessorDomains(Professors, ProfDomains),
  makeRoomDomains(Rooms, RoomDomains),
  makeGroupDomains(Groups, GroupDomains),

  % fetch lectures
  findall(L, L = lecture(_, _, _, _, _, _), Lectures),

  % Apply constraints for every lecture
  applyLectureConstraints(Lectures, Tasks,
   ProfDomains, RoomDomains, GroupDomains,
   Professors,  Rooms,       Groups
  ),

  % Run branch and bound
  fail.
