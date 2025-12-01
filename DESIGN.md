# Span State Machine & Interaction Design

## 1. System States (Global)

The system state is determined by the *last logged event* of the current day.

### A. State: **PRE-START** (Empty Day)
- **Condition:** No events exist in the log for the selected date.
- **Visual Indicator:** "Status: Ready to Start"
- **Allowed Actions:**
  - `[s]` **Start Day**: Logs `DAY_START` (Default context: "Organization").
  - `[n]` / `[p]` **Navigate**: Go to next/prev day.
  - `[q]` **Quit**.

### B. State: **ACTIVE** (Working)
- **Condition:** Last event was `DAY_START` or `CTX_SWITCH`.
- **Visual Indicator:** "Status: Active: [Task Title]" (Green/Normal).
- **Allowed Actions:**
  - `[c]` **Clock In**: Switch to new task (`CTX_SWITCH`).
  - `[o]` **Clock Out**: Stop working (`STOP`).
  - `[i]` **Interrupt**: Log interruption (`INTERRUPTION`).
  - `[t]` **Tick**: Log bookmark (`TICK`).
  - `[R]` **Report**: Generate text report.

### C. State: **INTERRUPTED** (Gap/Idle)
- **Condition:** Last event was `INTERRUPTION`.
- **Visual Indicator:** "Status: Interrupted / Paused" (Red/Warning).
- **Allowed Actions:**
  - `[c]` **Resume/Clock In**: Switch to task (`CTX_SWITCH`).
  - `[o]` **Finish Day**: Log stop (`STOP`).

### D. State: **FINISHED** (Day Ended)
- **Condition:** Last event was `STOP`.
- **Visual Indicator:** "Status: Day Ended".
- **Allowed Actions:**
  - `[c]` **Resume**: Clock in again (`CTX_SWITCH`).
  - `[R]` **Report**: Generate text report.

---

## 2. Contextual Actions (Timeline Selection)

These actions depend on where the cursor is located within the `[ TIMELINE ]` section.

### Context: **On an Interval Block**
- **`[d]` Delete**: Removes the event that *started* this interval.
  - *Logic:* Merges this time into the *previous* interval.
- **`[S]` Split**: Divide the interval into two.
  - *Input:* Split time, New Task (or keep same).
  - *Logic:* Inserts a `CTX_SWITCH` event at the calculated split time.
- **`[e]` Edit Time**: Change the start time of this interval.
  - *Logic:* Modifies the timestamp of the event initiating this block.
- **`[M]` Merge Up**: Merge with previous block (effectively delete this event).

### Context: **On a Gap (Red Block)**
- **`[f]` Fill Gap**: Retroactively assign this time.
  - *Logic:* Inserts a `CTX_SWITCH` at the start of the gap.

---

## 3. Transitions & Event Logic

| Current State | Action | New Event | New State |
| :--- | :--- | :--- | :--- |
| **PRE-START** | Start Day | `DAY_START` | **ACTIVE** |
| **ACTIVE** | Clock In | `CTX_SWITCH` | **ACTIVE** |
| **ACTIVE** | Interrupt | `INTERRUPTION` | **INTERRUPTED** |
| **ACTIVE** | Clock Out | `STOP` | **FINISHED** |
| **INTERRUPTED** | Clock In | `CTX_SWITCH` | **ACTIVE** |
| **INTERRUPTED** | Clock Out | `STOP` | **FINISHED** |
| **FINISHED** | Clock In | `CTX_SWITCH` | **ACTIVE** |

---

## 4. UI Layout Updates

The Action Strip should dynamically render based on the Global State.

**Scenario: PRE-START**
```
[ ACTIONS ]
   [s] Start Day   [n] Next Day   [p] Prev Day   [q] Quit
```

**Scenario: ACTIVE**
```
[ ACTIONS ]
   [c] Clock In   [o] Clock Out   [i] Interrupt   [t] Tick
   [r] Refresh    [R] Report      [q] Quit
```

**Scenario: FINISHED**
```
[ ACTIONS ]
   [c] Resume     [R] Report      [q] Quit
```
