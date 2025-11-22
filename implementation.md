# Technical Implementation & Architecture: Org-Chronos

## 1. Architecture Overview

The system follows a **Unidirectional Data Flow** pattern inspired by Redux/Elm but adapted for Emacs Lisp.

1.  **Event Stream (SSOT):** All actions (start task, interrupt, stop) are appended to a human-readable daily log file.
2.  **Aggregator:** A pure function reads the log and reduces it into a list of "Intervals" (Time Blocks).
3.  **Renderer:** The UI reads the Intervals to draw the Dashboard.
4.  **Projector:** The Synchronization engine reads the Intervals to update `CLOCK` lines in Org files.

## 2. Data Structures & Persistence

### 2.1 Event Log Storage

**Location:** Logs are stored within your Org-roam directory to ensure they are version-controlled alongside your notes.
Path: `(expand-file-name "chronos-logs/YYYY-MM-DD.log" org-roam-directory)`

**Format: Human-Readable S-Expressions**
To satisfy the requirement of being editable by hand, we will use valid Emacs Lisp s-expressions, but strictly formatted as **one event per line**. This allows for easy `diff`ing and manual editing.

**Schema (Example File Content):**

```lisp
;; Org-Chronos Log: 2023-10-27
(:time 1698386400.0 :type :day-start :payload nil)
(:time 1698386405.0 :type :ctx-switch :payload (:chronos-id "uuid-1" :title "Organization"))
(:time 1698388200.0 :type :ctx-switch :payload (:chronos-id "uuid-2" :title "Deep Work"))
(:time 1698390000.0 :type :interruption :payload (:reason "Phone call"))
;; User can manually delete a line or change the timestamp here easily.
````

### 2.2 ID Lookup Strategy (Git Grep / Grep)

**Decision:** We will use **`git grep`** as the primary search mechanism.
**Reason:** You explicitly stated your files are under version control. `git grep` is extremely fast (comparable to `rg`) because it searches the git index.

**Logic:**

1.  **Primary Lookup:** `git grep -n ":CHRONOS_ID:\s*uuid-1234"`
      - Run from `org-roam-directory`.
      - This is instant for text repositories of moderate to large size.
2.  **Fallback Lookup:** `grep -rn --include="*.org" ":CHRONOS_ID:\s*uuid-1234" .`
      - Used if the directory is not a git repo.
      - Optimized to only look at `.org` files to improve speed.

## 3\. Library Selection & Dependencies

| Component | Library | Reason |
| :--- | :--- | :--- |
| **UI Menus** | `transient` | Standard for Magit-like menus. |
| **Dashboard** | `magit-section` | Collapsible sections and clean UI rendering. |
| **Search** | `vc` / built-in | We will call `git grep` or `grep` via `shell-command` or `process-lines`. |
| **Time** | `ts.el` | Ergonomic time manipulation. |
| **Lists** | `dash.el` | Functional list processing. |
| **Files** | `f.el` | File path handling. |
| **Compat** | `evil-mode` | **Critical:** Custom keymaps must be bound to Evil states. |

## 4\. Functional Modules

### 4.1 Core Engine (`org-chronos-core.el`)

  - **`org-chronos-log-event (type payload)`**:
      - Opens the daily log file.
      - Moves to end of file.
      - Pretty-prints the event on a *single new line*.
      - Saves.
  - **`org-chronos-compute-day (date)`**:
      - Reads the file as a Lisp list (wrapping content in `(...)`).
      - Reduces list into `Interval` structs.
      - *Error Check:* If the user manually edited the file and broke the syntax, catch the error and display a "Log Syntax Error" link in the dashboard to jump to the broken line.

### 4.2 The Locator (`org-chronos-lookup.el`)

  - **`org-chronos-find-id (uuid)`**:
      - checks if `(vc-backend org-roam-directory)` is `Git`.
      - If yes: calls `git grep`.
      - If no: calls `grep -r`.
      - Returns `(file . point)`.

### 4.3 The Synchronizer (`org-chronos-sync.el`)

  - **`org-chronos-sync-day`**:
      - Calculates intervals.
      - Uses `org-chronos-find-id` to visit files.
      - Updates `CLOCK` drawers.
      - **Optimization:** If possible, perform a *batch search* for all unique IDs in the day's log at once using `git grep -E "id1|id2|id3"` to avoid spawning a shell process for every single interval.

## 5\. UI Design & UX

### 5.1 The Dashboard (`*Org-Chronos*`) & Evil Integration

The dashboard is read-only. We must define a specific `evil-state` or bind keys in `evil-motion-state-map` for this buffer.

**Keybindings (Evil-Compatible):**

| Key | Action | Description |
| :--- | :--- | :--- |
| `j` / `k` | `magit-section-forward/backward` | Navigate up/down blocks. |
| `RET` | `org-chronos-visit-entry` | Jump to the Org heading. |
| `TAB` | `magit-section-toggle` | Toggle section visibility (if we nest details). |
| `c` | `org-chronos-clock-in` | Transient: Clock into new/existing task. |
| `i` | `org-chronos-interruption` | Transient: Log interruption. |
| `v` | `org-chronos-visual-edit` | Enter editing mode for selected block. |
| `q` | `org-chronos-quit` | Close dashboard. |
| `S` | `org-chronos-sync` | Force sync logs to Org files. |

**Visuals:**
The dashboard will look similar to the previous mockup but will explicitly indicate if a log entry has been manually modified (e.g., via checksum or just relying on the sync).

### 5.2 The "Visual Edit" Transient (`v`)

When selecting a block and hitting `v`, we offer specific fixes:

  - `s`: **Split** block at time... (Prompt for time, inserts `CTX_SWITCH` event in the log).
  - `m`: **Merge** with previous (Deletes the event that started this block).
  - `e`: **Edit** details (Manually open the log file at the line corresponding to this event).

### 5.3 Recursive Editing (The "Go To" Flow)

Unchanged from previous version, but ensures that when `recursive-edit` starts, the user is in their normal Evil state to navigate files comfortably.

## 6\. Integration Points

### 6.1 Doom Modeline

  - Custom segment `chronos`.
  - Hooks into `window-configuration-change-hook` or a timer (low frequency) to check current interval state against `(current-time)`.

### 6.2 Org-Roam

  - When `org-chronos-clock-in` creates a "New Task":
    1.  Determine daily file: `(org-roam-dailies-capture-templates)`.
    2.  Insert heading under `* Tasks`.
    3.  Generate UUID.
    4.  Set property `:CHRONOS_ID: uuid`.
    5.  Save file.
    6.  Log event using new ID.

## 7\. Development Phases

1.  **Phase 1: The Logger (Data Layer)**
      - Implement `log-event` (append-to-file) and `compute-day` (reduce logic).
      - Define the text-based storage format.
      - Unit tests for the reducer logic (detecting gaps/overlaps).
2.  **Phase 2: The Locator & Dashboard (Read-Only)**
      - Implement `git grep` / `grep` lookup.
      - Render the Dashboard using `magit-section`.
      - Bind `evil` keys.
3.  **Phase 3: Interactivity (Write UI)**
      - Implement Transients for Clocking/Interrupting.
      - Implement the Recursive Edit flow for manual selection.
4.  **Phase 4: Synchronization**
      - Implement the `sync` logic to rewrite CLOCK entries in source files.


