:- use_module("./utilities").
:- use_module("./constraints").

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

:- compile("../utils/data/data.ecl").





test_schedule(Tasks) :-
  findall(lecture(Id, Duration, Profs, Groups, Type), lecture(Id, Duration, Profs, Groups, Type), Lectures),
  findall(group(Id, Count, Overlapping), group(Id, Count, Overlapping), Groups),
  findall(room(Id, Type, Capacity, Times), room(Id, Type, Capacity, Times), Rooms),
  findall(professor(Id, Times), professor(Id, Times), Professors),

  schedule(Lectures, Professors, Groups, Rooms, Tasks).





%%% schedule/5
schedule(Lectures, Professors, Groups, Rooms, Tasks) :-
  % parse data
  % parse_info_room(Rooms, InfoRooms),

  % apply constraints
  lecture_constraints(Lectures, Tasks, Professors, Groups, Rooms),

  professor_constraints(Professors, Lectures, Tasks),
  
  group_constraints(Groups, Lectures, Tasks),

  room_constraints(Rooms, Lectures, Tasks),

  true.

  % solve problem
  % calculate_goal(Tasks, Goal),
  % get_vars_from_tasks(Tasks, Vars),
  % bb_min(labeling(Vars), Goal, _).





%%% calculate_goal/2
%%% calculate_goal(+Tasks, -Goal).
%%% This predicate acts as the "objective function" of the problem.
calculate_goal(_Tasks, 0).





%%% get_vars_from_tasks/2
%%% get_vars_from_tasks(+Tasks, -Vars)
%%% This predicate accepts a list of task facts and returns the decision variables.
get_vars_from_tasks([], []).

get_vars_from_tasks([task(_Id, Where, When) | Tasks], [Where, When | Vars]) :-
  get_vars_from_tasks(Tasks, Vars).
