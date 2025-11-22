# Package Specification: Org-Chronos

## 1. Executive Summary

**Org-Chronos** is a robust time-tracking package for Emacs designed to layer over `org-mode`. Unlike standard Org clocking—which records isolated time ranges scattered across files—Org-Chronos utilizes a **linear event log (Event Sourcing)**. This ensures time is treated as a continuum, mathematically preventing overlaps and highlighting gaps.

The package features a centralized, Magit-style dashboard for visualization, correction, and retroactive time entry. It supports tracking time against **any Org heading** (not just TODO items) without polluting the user's Org-roam database.

## 2. Core Architecture: Event Sourcing

The fundamental design principle is that the **Event Log** is the Single Source of Truth (SSOT). The standard Org-mode `CLOCK:` lines in headers are *derivative* views (projections) generated from this log.

### 2.1 The Event Log

- The package maintains a dedicated daily log file (or a specific datetree entry).
- Instead of `[Start Time]--[End Time]`, the log records **State Changes** (Events).
- **Logic:** A context continues until the next event occurs.
- **Event Types:**
    1.  `DAY_START`: Begin tracking (defaults to "Organization" context).
    2.  `CTX_SWITCH`: Switch active clock to Heading X (implies Heading Y ended).
    3.  `INTERRUPTION`: Stop current context; enter "Unresolved" state.
    4.  `STOP`: End of working day.
    5.  `TICK`: A timestamp bookmark (no state change, just a marker for reference).

### 2.2 Identification & Linking (The `CHRONOS_ID`)

To avoid interfering with `org-roam` or creating unwanted nodes in the user's knowledge graph, the package must **not** use the standard `ID` property.

-   **Unique Identifier:** The package uses a custom property `CHRONOS_ID`.
-   **Generation:** When a heading is first clocked into, a unique UUID is generated and stored in the `CHRONOS_ID` property of that heading.
-   **Lookup Performance:** Since `org-id-find` does not track custom properties, Org-Chronos maintains a lightweight persistence file (hash map) mapping `CHRONOS_ID` $\to$ `(file-path, point/marker)`. This cache updates whenever a heading is selected or the system syncs. `(file-path, point)`. If a lookup fails (file moved/heading deleted), the system triggers a fallback scan of `org-agenda-files` to rebuild the cache transparently.

### 2.3 Synchronization

- **Log -> Org:** A command `org-chronos-sync` parses the Event Log. It uses the `CHRONOS_ID` to locate the correct heading (using the cache) and rewrites the `CLOCK:` lines to match the log exactly.
- **Org -> Log:** (Optional/Advanced) If a user manually edits a CLOCK line, the system flags a discrepancy in the Dashboard.

## 3. User Interface (The Dashboard)

The entry point is a transient buffer (Magit-style) invoked by `org-chronos-status`.

### 3.1 Visualization Pane

A visual timeline of the **currently selected day** (defaulting to today, but allowing navigation to past dates).

- **Time Blocks:** Colored blocks representing active contexts (headings).
- **Gaps:** Clearly highlighted in red (unaccounted time).
- **Overlaps:** Impossible by design (switching context $B$ closes context $A$).
- **Ticks:** Small horizontal markers on the timeline showing where "Ticks" were recorded.
- **Cursor Interaction:** Moving the cursor over a block highlights the corresponding Org heading.
- **Date Display:** The header must clearly identify which date is being visualized.

### 3.2 Action Menu (Transients)

- **`c` (Clock/Change):** Switch to a new context/heading immediately.
- **`i` (Interruption):** Log an interruption retroactive to $X$ minutes ago.
- **`t` (Tick):** Drop a marker at current time (or custom time).
- **`v` (Visualize/Fix):** Enter "Edit Mode" to manipulate the timeline.
- **`n` / `p` (Date Navigation):** Switch the dashboard view to the Next or Previous day to review or correct historical logs.
- **`J` (Jump to Date):** Select a specific date to visualize from a calendar.

## 4. Functional Workflows

### 4.1 Daily Routine

1.  **Start Day:** User runs `org-chronos-start-day`.
    -   System logs `DAY_START`.
    -   System clocks into default heading (configured as "Organization").
2.  **Normal Work:** User works. To switch contexts, user invokes the Dashboard and selects a new heading.

### 4.2 Handling Interruptions

**Scenario:** User is stopped in the hallway. They return to Emacs 15 minutes later.

1.  User invokes `org-chronos-interruption`.
2.  **Input 1 (Time):** User enters `-15` (minutes) or absolute time.
3.  **Input 2 (Context):** User selects:
    -   *New Ad-Hoc Heading:* Prompts for text; creates Heading under `Current Journal > Tasks`.
    -   *Existing Heading:* Uses fuzzy selector (scanning all agenda files).
    -   *Unresolved:* Logs the time as "Interruption" (flashing red modeline) to be categorized later.

### 4.3 The "Away" Workflow (Bulk Entry)

**Scenario:** User returns from a 2-hour meeting block involving 3 different topics.

1.  User invokes `org-chronos-fill-gap` (or selects the empty space in Dashboard).
2.  UI prompts for the start time of the gap.
3.  UI loops: "What did you do first?" -> User selects Heading A -> "How long?" -> User inputs duration.
4.  Repeat until current time is reached.

### 4.4 "Ticks" (Quick Markers)

- **Purpose:** User is busy but knows a context switch just happened. They don't have time to search for the heading name.
- **Action:** User hits a global hotkey.
- **Result:** A `TICK` event is logged at `NOW`.
- **Usage:** Later, in the Dashboard, the user can "Split block at Tick" to easily assign the heading starting from that exact moment.

## 5. Heading Selection & Navigation

To solve the "loss of context" problem during selection:

1.  **Scope:** The selector must allow picking **any** heading in `org-agenda-files` (or specific scopes), not just those marked TODO.
2.  **Primary Selector:** `completing-read` (compatible with Vertico/Helm/Ivy).
3.  **Quick Create:** Always offer "Create new capture heading" as the first option.
4.  **Visual Selection (The "Go To" Problem):**
    -   If the user must visually find a heading in a file, they select "Manual Find."
    -   Org-Chronos enters a **Recursive Edit** or saves window configuration.
    -   User navigates freely to the target heading.
    -   User invokes `org-chronos-select-here`.
    -   System generates/retrieves `CHRONOS_ID` for current heading, logs event, exits recursive edit, and restores previous window configuration.

## 6. Visual Editing (Fixing the Day)

The Dashboard allows direct manipulation of the Event Log:

-   **Split:** Select a block, split it at time $T$. Assign the new chunk to a different heading.
-   **Merge:** Join current block with previous.
-   **Insert:** Inject a context into the middle of another (shuffles times automatically).
-   **Move Boundary:** Drag the start/end time of an event up or down. Adjacent events adjust automatically to maintain continuity.

## 7. Integrations

### 7.1 Doom Modeline

-   **Standard:** Show current clock icon and heading title.
-   **Interruption Mode:** If the last event was `INTERRUPTION` (unresolved), the icon flashes red or turns distinctively high-contrast.
-   **Gap Warning:** If the computer has been idle > $X$ minutes but no event logged, show a "Gap?" warning.

### 7.2 Org-Roam

-   Integration to ensure "New Ad-Hoc Headings" are created specifically in the `org-roam-dailies` file for the current day under the "Tasks" headline.
-   **Isolation:** Strict adherence to using `CHRONOS_ID` to prevent polluting the Org-roam node database.
