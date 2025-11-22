# Clocking utilities

I want to implement a custom and reliable clocking system for my daily work based on org-mode.
The functionality of the package is a bit confusing and error prone, and I want to make sure
that the clocking is done in a robust way without overlaps, unintended empty slots, etc.

Crucially, at any point I want to be able to see what is the logging for a given day
(clealy visualising all the clocked tasks and any empty space between them) and fix
thing if necessary.

A typical day would start with running a command that marks the start of the day.
This would clock-in in a given default task (e.g. "Organisation"). There normally
I'd organise my day, check what is left from previous days, shuffle and re-prioritise
tasks, etc.

Then I'll chose a given task and clock-into it. When it's done or I need to work in
another one, I'd select it and continue. This could be easily done with standard org-clock
functionality, but my problem is that interruptions happen all the time and I spend some
time separated from my computer. I need to be able to handle those cases. Typical cases are:

I create ad hoc tasks that come to me during the day in a special location: a first level
heading called "Tasks" under the current journal file (I'm using org-roam)

I'm using Doom Emacs, so I can use the doom-modeline to manage the modeline. I want
to keep an indication of the status of the clock there, specially in some transition
states where I need to find a task or do something in any free way (using Emacs freely)
before the task can be completed


## Interruption
Somebody enter the office, stops me in the corridor or calls me without me having the time
to create a new task and clocking into it. It's possible that during that time I need to
use Emacs, so relaying on idle time to find out when the interruption happened would not work.

In this case, I'd need to be able to encode interruptions by a dedicated command that:

- Asks me when the interruption started (I can specify a time or a difference like -10 to indicate 10 minutes ago)
- Asks me whether this is a new task
  - If yes: create the task (under the 'Tasks' heading in the current journal file)
  - If no (quite unusual): ask me to go to the task and run some command to select it. In the meantime a
    message in the modeline with a cleaer visual indicator (some flashing red icon or similar) is whoen
  
## Tasks away from computer
Sometimes I need to do things away from the computer (meetings)

