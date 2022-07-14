:- use_module("./scheduler_utilities").

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).





test_schedule(Tasks) :-
  findall(lecture(Id, Duration, Profs, Groups, Type), lecture(Id, Duration, Profs, Groups, Type), Lectures),
  findall(group(Id, Count, Overlapping), group(Id, Count, Overlapping), Groups),
  findall(room(Id, Type, Capacity, Times), room(Id, Type, Capacity, Times), Rooms),
  findall(professor(Id, Times), professor(Id, Times), Professors),

  schedule(Lectures, Professors, Groups, Rooms, Tasks).





% calculate_makespan(Tasks, Makespan).
calculate_makespan(_Tasks, 0).





%%% schedule/5
schedule(Lectures, Professors, Groups, Rooms, Tasks) :-
  fail.


