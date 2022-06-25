# University Scheduler

An prolog application to create timetables for university courses. 

# How does it work

The scheduler creates two decision variables for every lecture/6 item provided. Those variables are named **Where** and **When**. The schedule/5 predicate accepts the following lists of resources:

- Rooms: A list of room/4 facts,
- Groups: A list of group/3 facts,
- Lectures: A list of lecture/6 facts,
- Professors: A list of professor/2 facts.

The provided lists are processed by the predicate and after applying the necessary constraints, a list of **task/3** facts is returned.

- **Tasks**: A list of task/3 facts. The predicate has the following variables task(Id, Where, When). 

  - Id: The id of the lecture the tasks points into,
  - Where: The id of the room the lecture will take place in,
  - When: The time slot (integer) that the lecture will start.

# How to use it

You can use this scheduler standalone (with ECLiPSe installed in the machine) or by attaching it to a web server (see [this](https://github.com/steliosmagalios/spring-prolog-server) for an example of using prolog in the web).
