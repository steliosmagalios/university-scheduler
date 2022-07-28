:- use_module("./utilities").
:- use_module("./constraints").
:- use_module('./map_filter_reduce').

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

% dummy data used for testing
:- compile("../utils/data/data.ecl").





%%% test predicate used for testing the program %%%
test_schedule(Tasks) :-
  findall(lecture(Id, Duration, Profs, Groups, Type), lecture(Id, Duration, Profs, Groups, Type), Lectures),
  findall(group(Id, Count, Overlapping), group(Id, Count, Overlapping), Groups),
  findall(room(Id, Type, Capacity, Times), room(Id, Type, Capacity, Times), Rooms),
  findall(professor(Id, Times), professor(Id, Times), Professors),

  schedule(Lectures, Professors, Groups, Rooms, Tasks).





%%% schedule/5
%%% schedule(+Lectures, +Professors, +Groups, +Rooms, -Tasks)
%%% 
%%% The core predicate of the program. This predicate accepts a list
%%% of lectures, a list of professors, a list of groups, a list of rooms,
%%% applies the constraints and runs the branch and bound algorithm. After
%%% execution, a list of task/3 facts is returned to the user.

schedule(Lectures, Professors, Groups, Rooms, Tasks) :-
  % Apply constraints
  lecture_constraints(Lectures, Tasks, RoomAts, Professors, Groups, Rooms),
  professor_constraints(Professors, Lectures, Tasks),
  group_constraints(Groups, Lectures, Tasks),
  room_constraints(Rooms, RoomAts),

  % Run branch and bound
  calculate_optimization_value(Tasks, Goal),
  get_vars_from_tasks(Tasks, Vars),
  bb_min(labeling(Vars), Goal, bb_options{ strategy: dichotomic, timeout: 10 }).





%%% calculate_optimization_value/2
%%% calculate_optimization_value(+Tasks, -Goal).
%%% This predicate acts as the "objective function" for optimization.
calculate_optimization_value(Tasks, Goal) :-
  % FIXME: Makespan here is for placeholder purposes.
  map(Tasks, get_when, [], Whens),
  ic_global:maxlist(Whens, Goal).





%%% get_vars_from_tasks/2
%%% get_vars_from_tasks(+Tasks, -Vars)
%%% This predicate accepts a list of task facts and returns the decision variables.
get_vars_from_tasks([], []).

get_vars_from_tasks([task(_Id, Where, When) | Tasks], [Where, When | Vars]) :-
  get_vars_from_tasks(Tasks, Vars).
