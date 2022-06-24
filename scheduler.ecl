% Library imports
:- use_module("./scheduler_utilities").

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

% === DATA MODELS ===

% professor(Id, Availability).
% group(Id, MemberCount, Availability).
% room(Id, Type, Capacity, Availability).
% lecture(Id, Duration, Semester, Type, Professors, Groups).

% === DUMMY DATA ===

% lecture(Id, Duration, Semester, Professors, Groups).
lecture(1, 1, 2, "Aud", ["p1", "p2"], ["g1", "g2", "g3"]).
lecture(2, 1, 1, "Lab", ["p1"], ["g2"]).
lecture(3, 2, 1, "Aud", ["p3"], ["g3"]).

% professor(Id, Availability).
professor("p1", []).
professor("p2", []).
professor("p3", []).

% room(Id, Type, Capacity, Availability).
room("r1", "Lab", 50, []).
room("r2", "Lab", 50, []).
room("r3", "Aud", 100, []).
room("r4", "Aud", 150, []).

% group(Id, MemberCount, Availability).
group("g1", 50, ["g2", "g3"]).
group("g2", 25, ["g1"]).
group("g3", 50, ["g1"]).


applyLectureConstraints([], _Professors, _Groups, _Rooms, []).

applyLectureConstraints([CurrLec | Lectures], Professors, Groups, Rooms, [CurrTask | Tasks]) :-
  % Get the lecture's professors intersected domain
  getProfessorDomains(CurrLec, Professors, ProfessorDomains),

  % Get room's with correct type and enough capacity
  getRoomDomain(CurrLec, Rooms, Groups, RoomId, RoomDomain),

  % Create the lectures decision variables and create the task
  createDecisionVars(CurrLec, ProfessorDomains, RoomId, RoomDomain, CurrTask),

  % Continue the next lecture
  applyLectureConstraints(Lectures, Professors, Groups, Rooms, Tasks).



%%% schedule/1.
%%% schedule/5.
%%% The schedule predicate. This predicate usese CLP with the branch 
%%% and bound algorithm to create a timetable for the given lectures.

%%% schedule/1.
%%% schedule(Tasks).
%%% In this version, the necessary data are fetched from prolog facts and then the
%%% the schedule/5 predicate is called.
schedule(Tasks) :-
  % Fetch data from prolog facts.
  % TODO 2022-Jun-24: Fetch data (findall???)
  Rooms = [], Groups = [], Lectures = [], Professors = [],
  fail,

  % Call the other schedule predicate
  schedule(Lectures, Professors, Groups, Rooms, Tasks).



%%% schedule/5.
%%% schedule(Lectures, Professors, Groups, Rooms, Tasks).
%%% This version is the main schedule/? predicate.
schedule(Lectures, Professors, Groups, Rooms, Tasks) :-
  % Apply constraints for every lecture
  applyLectureConstraints(Lectures, Professors, Groups, Rooms, Tasks),

  % Constraints for professors
  % TODO 2022-Jun-24: Every lecture a professor teaches must not occur at the same time
  fail,

  % Constraints for groups
  % TODO 2022-Jun-24: Every lecture a group has must not occur at the same time
  fail,

  % Constraints for rooms
  % TODO 2022-Jun-24: Every room must have only one lecture at the same time
  fail,

  % Calculate makespan from tasks and call bb_min
  calculateMakespan(Tasks, Goal),
  getVarsFromTasks(Tasks, TaskVars), % Get the variables from the tasks
  bb_min(labeling(TaskVars), Goal, _). % Test other bb strategies
