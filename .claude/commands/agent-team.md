Create an agent team for working on the OpenMainframe z/OS clone codebase.

This is a Rust monorepo with 28 crates under `crates/`, each implementing a z/OS subsystem (JES2, RACF, TSO, datasets, JCL, COBOL, REXX, etc.). The server binary is `open-mainframe-zosmf`.

## Team Templates

Choose or adapt one of these team structures based on the task:

### 1. Parallel Crate Development
Spawn teammates to implement features across multiple crates simultaneously:
```
Create an agent team with teammates for parallel crate work:
- Teammate 1: work on crates/open-mainframe-jes2 (job entry subsystem)
- Teammate 2: work on crates/open-mainframe-racf (security)
- Teammate 3: work on crates/open-mainframe-dataset (dataset I/O)
Each teammate should run `cargo check -p <crate>` and `cargo test -p <crate>` after changes.
```

### 2. Cross-Subsystem Code Review
Spawn reviewers for different layers of the system:
```
Create an agent team for code review:
- Reviewer 1: review the zosmf REST API layer (crates/open-mainframe-zosmf)
- Reviewer 2: review the core subsystem crates (jes2, racf, tso, dataset)
- Reviewer 3: review the language crates (cobol, rexx, hlasm, pli)
Each reviewer should document issues, suggest improvements, and check for consistency.
```

### 3. Build, Test & Implement
One teammate focuses on CI quality while others implement:
```
Create an agent team:
- Build Guardian: continuously run `cargo check --workspace`, `cargo clippy -- -D warnings`, and `cargo test` after significant changes. Report failures to the lead.
- Implementer 1: implement feature X in crate Y
- Implementer 2: implement feature A in crate B
```

### 4. Research & Architecture
Investigate different aspects of a problem in parallel:
```
Create a research team:
- Researcher 1: investigate how z/OS handles <topic A>, read relevant source in crates/
- Researcher 2: investigate how <topic B> integrates with existing subsystems
- Architect: wait for researchers, then propose a unified design
```

## Team Lifecycle (IMPORTANT — follow every time)

### Before Creating a Team
1. **Always clean up first.** Only one team can be active per session. Tell the lead:
   ```
   Clean up the team
   ```
2. If teammates are stuck as orphaned tmux sessions, kill them manually:
   ```bash
   tmux ls
   tmux kill-session -t <session-name>
   ```

### Creating a Team
3. **Spawn the new team** using one of the templates above, or your own prompt.
4. **Wait for teammates to finish** before starting the next phase:
   ```
   Wait for your teammates to complete their tasks before proceeding
   ```

### During Work
5. **Check on teammates** — use `Shift+Down` (in-process) or click into panes (tmux) to monitor progress.
6. **Reassign or replace** — if a teammate stops on an error, either give it new instructions or shut it down and spawn a replacement:
   ```
   Ask the <name> teammate to shut down
   ```

### After Work is Done
7. **Always clean up** before starting a different task or team:
   ```
   Clean up the team
   ```

> **Key constraints:**
> - One team per session — clean up before creating a new team
> - No nested teams — teammates cannot spawn their own teams
> - `/resume` and `/rewind` do NOT restore teammates — spawn new ones after resuming

## Tips
- Each crate can be built independently: `cargo check -p open-mainframe-<name>`
- Run all tests: `cargo test --workspace`
- Linting: `cargo clippy --workspace -- -D warnings`
- Formatting: `cargo fmt --check`
- The BMAD planning artifacts are in `_bmad-output/` — check implementation progress there
