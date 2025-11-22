# Org Clock Documentation

## 1. Global State & Context Variables
**Crucial for determining the current state of the clocking engine.**

### Active Clock State (The "Hot" Variables)
* **`org-clock-marker`** (Variable)
    * A marker pointing to the headline of the currently clocked-in task.
    * *Dev Note:* If `(marker-buffer org-clock-marker)` is `nil`, no clock is running. Do not modify this manually; use `org-clock-in`/`out`.
* **`org-clock-hd-marker`** (Variable)
    * A marker pointing to the headline of the currently clocked task. Usually matches `org-clock-marker`, but used specifically for headline positioning.
* **`org-clock-start-time`** (Variable)
    * The time (Emacs time format) when the current clock was started.
* **`org-clock-total-time`** (Variable)
    * Holds the total time (in minutes) spent on the currently clocked item *before* the current session started.
* **`org-clock-current-task`** (Variable)
    * The string title of the task currently clocked in.
* **`org-clock-effort`** (Variable)
    * The effort estimate string (e.g., "2:00") of the currently clocking task.
* **`org-clock-has-been-used`** (Variable)
    * `t` if the clock has been used during the current Emacs session.

### Internal State Flags (Concurrency & Logic Control)
* **`org-clock-clocking-in`** (Variable)
    * Set to `t` while `org-clock-in` is executing. Prevents recursive calls to clock-in logic.
* **`org-clock-resolving-clocks`** (Variable)
    * Set to `t` while the user is resolving dangling or idle clocks.
    * *Important:* If you write custom idle handlers, check this variable. If `t`, do not trigger another resolution or you will cause a stack overflow/infinite loop.
* **`org-clock-resolving-clocks-due-to-idleness`** (Variable)
    * Specific flag indicating the resolution was triggered by the idle timer, not a dangling clock check on startup.
* **`org-clock-leftover-time`** (Variable)
    * If a user cancels a clock resolution, this variable stores the time that was "left over." Used to offer to resume that specific time later.
* **`org-clock-task-overrun`** (Variable)
    * Internal flag `t` if the current clock has exceeded its effort estimate.

### History & Navigation
* **`org-clock-history`** (Variable)
    * List of markers pointing to recently clocked tasks.
* **`org-clock-default-task`** (Variable)
    * Marker pointing to the "default" task (set via `C-u C-u C-u M-x org-clock-in`).
* **`org-clock-interrupted-task`** (Variable)
    * Marker pointing to the task that was interrupted by the current clock (allows switching back).

### Timers
* **`org-clock-mode-line-timer`** (Variable)
    * The timer object updating the mode line clock string.
* **`org-clock-idle-timer`** (Variable)
    * The timer object checking for user idleness.
* **`org-clock--auto-clockout-timer-obj`** (Variable)
    * Internal timer object for the auto-clockout feature.

---

## 2. Core Clocking Functions
**The primary API for manipulating the clock.**

### Clocking In
* **`org-clock-in`** `(&optional select start-time)`
    * Starts the clock on the current item. If a clock is open elsewhere, it closes it first.
    * *Arguments:*
        * `select`: Prefix arg.
            * `(4)`: Select from history.
            * `(16)`: Mark current task as default.
            * `(64)`: Clock in continuously (using last clock-out time as start time).
        * `start-time`: Force a specific start time.
* **`org-clock-in-last`** `(&optional arg)`
    * Clocks into the last closed clocked item.
* **`org-clock-mark-default-task`** `()`
    * Sets `org-clock-default-task` to the current entry.

### Clocking Out
* **`org-clock-out`** `(&optional switch-to-state fail-quietly at-time)`
    * Stops the current clock.
    * *Effects:* Calculates duration, inserts end timestamp `...--[end] => H:MM`, clears markers, handles state switching.
* **`org-clock-out-if-current`** `()`
    * Clocks out only if the current entry contains the running clock. Often used in `org-after-todo-state-change-hook`.

### Canceling
* **`org-clock-cancel`** `()`
    * Cancels the running clock by deleting the start timestamp line entirely.

### Location & Insertion Logic
* **`org-clock-find-position`** `(find-unclosed)`
    * *Critical Internal Function:* Determines *where* in the current entry the CLOCK line should go.
    * *Logic:* Handles creating the `LOGBOOK` drawer (or `org-clock-into-drawer` target) if it doesn't exist. It places point at the insertion target.
* **`org-clock-get-sum-start`** `()`
    * Returns the time from which clock times should be counted for the mode line display. Depends on `org-clock-mode-line-total`.

### Macros & Helpers
* **`org-clocking-p`** `()`
    * Returns `t` if a task is currently being clocked.
* **`org-with-clock`** `(clock &rest forms)`
    * *Macro:* Evaluates `forms` with `clock` (marker) as the active clock context.
* **`org-is-active-clock`** `(clock)`
    * Checks if `clock` (cons of marker and time) is the currently active global clock.
* **`org-clock-save-markers-for-cut-and-paste`** `(beg end)`
    * Saves relative positions of global clock markers within a region before a cut/move operation, ensuring markers aren't lost during text manipulation.

---

## 3. Idle Time & Resolution Logic
**Logic for handling scenarios where the user leaves the computer while the clock is running.**

* **`org-resolve-clocks`** `(&optional only-dangling-p prompt-fn last-valid)`
    * Scans all agenda files for open clocks. Used on startup or when idleness is detected.
* **`org-clock-resolve`** `(clock &optional prompt-fn last-valid fail-quietly)`
    * The interactive UI logic prompting the user (Keep, Subtract, Cancel, Split) when a clock needs resolution.
* **`org-resolve-clocks-if-idle`** `()`
    * Called by `org-clock-idle-timer`. Checks `org-user-idle-seconds` vs `org-clock-idle-time`.
* **`org-user-idle-seconds`** `()`
    * Returns idle seconds as a float. Dispatches to OS-specific logic (`org-mac-idle-seconds`, `org-x11-idle-seconds`, `org-logind-user-idle-seconds`).
* **`org-find-open-clocks`** `(file)`
    * Searches `file` for lines matching the open clock regex. Returns a list of `(marker . start-time)`.

---

## 4. Reporting & Clock Tables
**Functions responsible for generating reports (`#+BEGIN: clocktable`).**

### Clock Table Generation
* **`org-clock-report`** `(&optional arg)`
    * Interactive command to insert or update a clock table at point.
* **`org-dblock-write:clocktable`** `(params)`
    * The dynamic block writer. Parses parameters and calls the formatter.
* **`org-clock-get-table-data`** `(file params)`
    * Scans `file` for clock sums. Returns a structure of `(file total-minutes entries-list)`.
* **`org-clocktable-write-default`** `(ipos tables params)`
    * The default formatter. Renders the data structure into an Org table.
* **`org-clocktable-steps`** `(params)`
    * Generates multiple clock tables (daily, weekly, etc.) if `:step` is provided.
* **`org-clocktable-shift`** `(dir n)`
    * Shifts the time block of the clocktable at point (e.g., "today" -> "yesterday").

### Time Calculation & Display
* **`org-clock-sum`** `(&optional tstart tend headline-filter propname)`
    * Sums times for each subtree in the buffer.
    * *Side Effect:* Puts the `:org-clock-minutes` text property on headlines.
* **`org-clock-sum-current-item`** `(&optional tstart)`
    * Returns the clocked time of the current item.
* **`org-clock-get-clocked-time`** `()`
    * Returns the clocked time for the current item in minutes. Includes time from previous intervals + currently running clock.
* **`org-clock-special-range`** `(key &optional time as-strings wstart mstart)`
    * Returns start/end times for ranges like `today`, `lastweek`, `thismonth`.

---

## 5. Persistence
**Saving clock history across Emacs sessions.**

* **`org-clock-save`** `()`
    * Writes `org-clock-history` and the current running clock state to `org-clock-persist-file`.
* **`org-clock-load`** `()`
    * Loads the persistence file and restores history/resumes clock if configured.
* **`org-clock-stored-history`** (Variable)
    * List of history items loaded from disk.
* **`org-clock-stored-resume-clock`** (Variable)
    * The clock that was active when Emacs was last closed, loaded from disk.

---

## 6. Mode Line & Display
* **`org-clock-update-mode-line`** `(&optional refresh)`
    * Updates `org-mode-line-string` with current clock status.
* **`org-clock-get-clock-string`** `()`
    * Constructs the string (e.g., `[1:25] (Task Name)`) for the mode line.
* **`org-clock-notify-once-if-expired`** `()`
    * Checks if current time exceeds `org-clock-effort`. Triggers notification/sound.

---

## 7. Key Configuration Options (Defcustom)
**Variables that alter control flow.**

* **`org-clock-into-drawer`**: (Default: `t`) Wraps clocks in `LOGBOOK` (or custom) drawer. Logic handled in `org-clock-find-position`.
* **`org-clock-out-remove-zero-time-clocks`**: (Default: `nil`) If `t`, removes the clock line if the duration is 0 minutes upon clocking out.
* **`org-clock-mode-line-total`**: (Default: `auto`) Determines what "total" time is shown in the mode line (current instance, today's total, all time, etc.).
* **`org-clock-persist`**: (Default: `nil`) If `t` or `clock` or `history`, enables persistence.
* **`org-clock-in-resume`**: (Default: `nil`) If `t`, attempts to resume an open clock line rather than creating a new one.
* **`org-clock-continuously`**: (Default: `nil`) If `t`, `org-clock-in` defaults to starting from the last clock-out time.
* **`org-clock-rounding-minutes`**: (Default: `0`) Round timestamps to nearest N minutes.
* **`org-clock-idle-time`**: Minutes of idleness before asking to resolve clock.

---

## 8. Hooks
* **`org-clock-in-prepare-hook`**: Run before clock-in mechanics start.
* **`org-clock-in-hook`**: Run after the clock is started.
* **`org-clock-out-hook`**: Run after the clock is stopped.
    * *Check:* `org-clock-out-removed-last-clock` to see if the line was deleted.
* **`org-clock-cancel-hook`**: Run after a clock is canceled.
* **`org-clock-goto-hook`**: Run after jumping to the active clock.

---

## 9. Utility Functions
* **`org-clock-modify-effort-estimate`** `(&optional value)`: Updates effort property.
* **`org-clock-goto`** `(&optional select)`: Jumps buffer to the active clock.
* **`org-clock-timestamps-change`** `(updown &optional n)`: Shift timestamps in the clock line (S-up/S-down).
* **`org-clock-update-time-maybe`** `()`: Re-calculates duration `=> H:MM` if the user manually edited the timestamps on a clock line.

