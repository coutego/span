# Technical Implementation & Architecture: Org-Chronos (v2)

## 1\. Architecture Overview

The system follows a **Unidirectional Data Flow** pattern inspired by Redux/Elm but adapted for Emacs Lisp. It functions as a **Standalone Time Machine** that references Org files but does not write to them (except for ID generation).

1.  **Event Stream (SSOT):** All actions (start task, interrupt, stop) are appended to a human-readable daily log file.
2.  **Aggregator:** A pure function reads the log and reduces it into a list of "Intervals" (Time Blocks).
3.  **The Control Panel (UI):** A unified application buffer that acts as both the **Renderer** (visualizing the day) and the **Controller** (dispatching new events).

*(Note: The "Projector" phase—syncing back to Org CLOCK drawers—is currently out of scope to reduce complexity and data corruption risks.)*

## 2\. Data Structures & Persistence

### 2.1 Event Log Storage

**Location:** Logs are stored within your Org-roam directory to ensure they are version-controlled alongside your notes.
Path: `(expand-file-name "chronos-logs/YYYY-MM-DD.log" org-roam-directory)`

**Format: Human-Readable S-Expressions**
To satisfy the requirement of being editable by hand, we use valid Emacs Lisp s-expressions, strictly formatted as **one event per line**.

**Schema (Example):**

```lisp
;; Org-Chronos Log: 2023-11-26
(:time 1698386400.0 :type :day-start :payload nil)
(:time 1698386405.0 :type :ctx-switch :payload (:chronos-id "uuid-1" :title "Organization"))
(:time 1698388200.0 :type :interruption :payload (:reason "Phone call"))
;; User can manually delete a line or change the timestamp here easily.
```

### 2.2 ID Lookup Strategy (Git Grep)

**Purpose:** To link a log entry to a real Org heading without needing to maintain a database.

1.  **Primary Lookup:** `git grep -n ":CHRONOS_ID:\s*uuid-1234"` (Instant).
2.  **Fallback Lookup:** `grep -rn ...` (Optimized for non-git folders).

## 3\. Library Selection

| Component | Library | Reason |
| :--- | :--- | :--- |
| **Dashboard** | `magit-section` | Handling the collapsible timeline rendering. |
| **Search** | `vc` / `grep` | File lookup mechanics. |
| **Time** | `ts.el` | Ergonomic time manipulation. |
| **Lists/Files** | `dash.el` / `f.el` | Data manipulation. |
| **Compat** | `evil-mode` | **Critical:** Custom keymaps must be bound to Evil states. |
| **Menus** | `transient` | (Optional) Used for complex sub-menus if needed. |

## 4\. Functional Modules

### 4.1 Core Engine (`org-chronos-core.el`)

  * **`org-chronos-log-event`**: Appends immutable events to the daily log.
  * **`org-chronos-compute-day`**: The "Reducer." Reads the log and calculates:
      * Closed Intervals (Start -\> End).
      * Active Interval (Start -\> NOW).
      * Gaps/Interruptions.

### 4.2 The Locator (`org-chronos-lookup.el`)

  * **`org-chronos-find-id`**: Returns `(file . point)` for a given UUID using `git grep`.

### 4.3 The Synchronizer (`org-chronos-sync.el`)

  * **Status:** **OUT OF SCOPE / FUTURE**.
  * *Design Intent:* In the future, this module would read computed intervals and rewrite `CLOCK` drawers in Org files. For now, it is omitted to ensure a robust standalone experience.

## 5\. UI Design: The "Control Panel"

Instead of separating the "Dashboard" (View) from the "Menu" (Action), Org-Chronos uses a unified **Control Panel** pattern (similar to `magit-status`).

### 5.1 The Interface (`*Org-Chronos*`)

The buffer is interactive. Keybindings (`c`, `i`, `t`) trigger actions that immediately append to the log and refresh the buffer.

**Layout Mockup:**

```text
================================================================================
ORG-CHRONOS  ::  Sunday, Nov 26, 2023  ::  [<] Previous  [>] Next  [J] Jump
================================================================================
[ STATUS ]
   Active:  Writing Code (Deep Work)
   Elapsed: 45m
   Total:   3h 10m logged today

[ ACTIONS ]
   [c] Clock In    [i] Interrupt    [t] Tick    [v] Edit Log    [q] Quit
   [r] Refresh     [s] Search

--------------------------------------------------------------------------------
[ TIMELINE ]
   09:00 - 10:00   Organization (Daily Review)              ID: uuid-1
   10:00 - 10:15   [ INTERRUPTION: Co-worker Request ]      (15m Gap)
   10:15 - NOW     Writing Code                             ID: uuid-2
--------------------------------------------------------------------------------
```

### 5.2 Interaction & Evil Integration

The buffer runs in a special mode (`org-chronos-dashboard-mode`).

| Key | Action | Description |
| :--- | :--- | :--- |
| **Navigation** | | |
| `j` / `k` | `magit-section-...` | Navigate up/down the timeline blocks. |
| `RET` | `visit-entry` | Jump to the Org heading (using `git grep`). |
| **Write Actions** | | |
| `c` | `clock-in` | Trigger fuzzy find or "Manual Find" to switch context. |
| `i` | `interruption` | Log an interruption (gap). |
| `t` | `tick` | Log a bookmark/tick at current time. |
| **Correction** | | |
| `v` | `visual-edit` | Open options to Split/Merge the selected block. |
| `E` | `raw-edit` | Visit the `.log` file manually at the line of the event. |

## 6\. Integration Points

### 6.1 "Manual Find" Workflow (Recursive Edit)

When the user wants to "Clock In" to a specific file:

1.  User presses `c` -\> Selects "Manual Find".
2.  Org-Chronos enters `recursive-edit`.
3.  User navigates normally to any file/heading.
4.  User runs `M-x org-chronos-select-here`.
5.  System grabs/creates `:CHRONOS_ID:`, exits recursion, logs event, and refreshes Dashboard.

### 6.2 Doom Modeline

  * Custom segment showing current active task.
  * Visual alert (red/flashing) if the current state is "Interruption" or if the gap exceeds a threshold.

## 7\. Development Phases (Revised)

1.  **Phase 1: The Logger (Data Layer) [DONE]**
      * Implement `log-event` and `compute-day`.
      * Ensure robust time calculation logic.
2.  **Phase 2: The Locator (Read-Only) [DONE]**
      * Implement `git grep` lookup.
      * Implement basic `magit-section` visualization.
3.  **Phase 3: The Unified Control Panel (Interactivity)**
      * **Refactor UI:** Merge "Menu" concepts into the Dashboard.
      * Implement the "Action Strip" rendering.
      * Bind `c`, `i`, `t` directly to the buffer map.
      * Implement the Recursive Edit flow for "Clock In".
4.  **Phase 4: Visual Correction**
      * Implement the `v` key to Split/Merge blocks (editing the log file programmatically).
5.  **Future Phase: Synchronization (Out of Scope)**
      * Writing back to Org `CLOCK` drawers.
