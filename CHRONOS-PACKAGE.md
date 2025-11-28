# Org-Chronos System Specification

## 1. Executive Summary

**Org-Chronos** is a time-tracking system designed to replace standard Org-mode clocking with an **Event Sourcing** architecture. Instead of recording isolated time ranges (`[Start]--[End]`) scattered across various files, Org-Chronos maintains a linear, immutable **Event Log**.

This design ensures:
1.  **No Overlaps:** Time is treated as a continuum; switching contexts automatically closes the previous one.
2.  **Gap Visibility:** Unaccounted time is mathematically derived and visually highlighted.
3.  **Robustness:** The log is the Single Source of Truth (SSOT). The UI is a projection of this log.

## 2. Core Architecture: Unidirectional Data Flow

The system follows a strict "Redux/Elm" style architecture to manage complexity and state.

### 2.1 The Cycle
1.  **State:** A single immutable struct holds the entire application state (Current Date, Event List, View Model, Selection).
2.  **Render:** A pure function takes the **State** and draws the UI buffer.
3.  **Input:** User triggers a command (Keypress).
4.  **Update:** A Reducer function takes `(Current State, Input)` and returns `(New State)`.
5.  **Effect:** If the state changes, the Event List is persisted to disk, and the cycle restarts at **Render**.

### 2.2 Layer Breakdown
1.  **Persistence Layer:** Pure I/O. Reads/Writes S-expressions to daily log files.
2.  **Domain Layer:** Pure business logic. Transforms raw Events into Intervals (Time Blocks).
3.  **State Layer:** Manages the transient application state (navigation, selection).
4.  **UI Layer:** Renders the State to the buffer.
5.  **Controller Layer:** Maps user inputs to State transitions.

## 3. Data Model

### 3.1 The Event Log
Events are stored in daily files (e.g., `chronos-logs/2023-11-28.log`). Each line is a valid Lisp S-expression.

**Event Structure:**
```lisp
(:time <float-epoch> :type <keyword> :payload <plist>)
```

**Event Types:**
| Type | Description | Priority (Sorting) |
| :--- | :--- | :--- |
| `:day-start` | Initializes the timeline. | 0 |
| `:stop` | Ends the current task. Stops the clock. | 10 |
| `:interruption` | Stops current task; marks subsequent time as "Interrupted". | 20 |
| `:ctx-switch` | Stops current task; starts a new task immediately. | 30 |
| `:tick` | A bookmark in time. Does not alter flow. | 40 |

*Note: Sorting Priority is critical. If a `:stop` and a `:ctx-switch` happen at the exact same second, the `:stop` must be processed first to ensure the interval is closed before the new one opens, preventing zero-duration artifacts.*

### 3.2 The Reducer (Domain Logic)
The Reducer transforms a list of Events into a **View Model**.

**Inputs:** List of Events (sorted by time + priority).
**Outputs:**
*   **Intervals:** List of closed time blocks `{start, end, title, id}`.
*   **Active:** The currently running interval (if any) `{start, nil, title, id}`.
*   **Gaps:** Computed periods where no task was active.
*   **State:** The global status of the day (see Section 4).

**Logic:**
*   Iterate events chronologically.
*   If `CTX_SWITCH` or `STOP`: Close the current open interval (if exists).
*   If `CTX_SWITCH`: Open a new interval.
*   If `STOP`: Do not open a new interval.
*   If time elapses between a `STOP` and the next `CTX_SWITCH`: Generate a **Gap Interval**.

### 3.3 Identification (`CHRONOS_ID`)
To link log entries to Org headings without polluting the database with standard IDs:
*   Use a custom property `:CHRONOS_ID:`.
*   Maintain a lightweight cache mapping `UUID -> (File, Marker)`.
*   Use `git grep` (or recursive grep) for fast lookups if the cache misses.

## 4. State Machine

The application's available actions depend on the **Global State** of the day, determined by the last processed event.

| State | Condition | Visual Indicator | Allowed Actions |
| :--- | :--- | :--- | :--- |
| **PRE-START** | No events logged. | "Ready to Start" | Start Day, Navigate Date, Quit |
| **ACTIVE** | Last event was `DAY_START` or `CTX_SWITCH`. | Green / Active Task | Clock In, Clock Out, Interrupt, Tick, Refresh |
| **INTERRUPTED** | Last event was `INTERRUPTION`. | Red / "Interrupted" | Resume (Clock In), Finish Day |
| **FINISHED** | Last event was `STOP`. | "Day Ended" | Resume, Report, Quit |

## 5. User Interface (The Dashboard)

The UI is a read-only buffer (`*Org-Chronos*`) that refreshes on every action.

### 5.1 Layout
1.  **Header:** Date and System Name.
2.  **Status Block:** Shows the Active Task and Elapsed Time (if Active).
3.  **Action Strip:** Dynamic menu of keys `[c] Clock In`, `[o] Clock Out`, etc. Changes based on Global State.
4.  **Contextual Actions:** An overlay appearing when the cursor is on a specific timeline row (e.g., `[f] Fill Gap` only appears on gaps).
5.  **Timeline:** Chronological list of intervals.
    *   **Format:** `HH:MM - HH:MM   Task Title`
    *   **Styling:**
        *   **Tasks:** Bold font.
        *   **Gaps:** Red/Error face.
        *   **Selection:** The currently selected row has a distinct background face extending to the window edge.

### 5.2 Interaction
*   **Navigation:** `j` / `k` moves the selection (highlight) up and down the timeline rows.
*   **Date Nav:** `n` / `p` switches the view to Next/Previous day.
*   **Selection:** `RET` jumps to the actual Org heading in its file.

## 6. Functional Workflows

### 6.1 Clocking In (`c`)
1.  User presses `c`.
2.  System prompts for a Task Title or Context.
    *   *Implementation:* Can use `completing-read` over Org headings.
    *   *Manual Find:* Supports a recursive edit flow where the user navigates to a file and selects a heading manually.
3.  System logs `:ctx-switch` event.
4.  UI Refreshes.

### 6.2 Handling Gaps (`f`)
1.  User navigates to a **Gap** (red block) in the timeline.
2.  Contextual action `[f] Fill Gap` appears.
3.  User presses `f`.
4.  System uses the Gap's start time as the timestamp.
5.  System logs `:ctx-switch` at that timestamp.
6.  **Result:** The Gap is replaced (or shortened) by the new task starting at that time.

### 6.3 Visual Editing
*   **Delete (`d`):** Removes the event that started the selected interval. Effectively merges the interval into the previous one.
*   **Split (`S`):** Inserts a new `:ctx-switch` event at a user-specified time within the selected interval.
*   **Edit Time (`e`):** Modifies the timestamp of the event initiating the selected block.

## 7. Implementation Details

### 7.1 Persistence
*   **Read:** `(chronos-fs-read date)` -> Returns list of events. Handles malformed lines gracefully.
*   **Write:** `(chronos-fs-write date events)` -> Overwrites the daily log file.

### 7.2 Testing Strategy
*   **Domain Tests:** Verify the Reducer logic (Events -> Intervals) using mock data. Ensure sorting stability.
*   **State Tests:** Verify navigation logic (`next-row`, `prev-row`) and state transitions.
*   **UI Tests:** Render state to a temp buffer and assert string content and text properties (faces).
