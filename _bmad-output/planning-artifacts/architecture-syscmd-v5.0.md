---
version: 'v5.0'
planningGroup: 'PG-10'
technology: 'System Commands & Console'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-syscmd-v5.0.md'
---

# Architecture: System Commands & Console

## 1. Crate Strategy

**New crate:** `open-mainframe-console`

Rationale: Console and system command processing is a distinct subsystem with its own I/O model (operator messages, WTOR responses). SDSF is a major component. Separate from `open-mainframe-mvs` (which handles programmatic services) and `open-mainframe-tso` (which handles interactive sessions).

## 2. Module Layout

```
crates/open-mainframe-console/src/
├── lib.rs
├── command/
│   ├── mod.rs          # Command dispatcher, CommandRegistry
│   ├── parser.rs       # Command syntax parser
│   ├── display.rs      # DISPLAY subcommands (A, J, T, M, WLM, GRS)
│   ├── lifecycle.rs    # START, STOP, MODIFY, CANCEL, FORCE
│   ├── reply.rs        # REPLY command (WTOR responses)
│   └── jes2.rs         # JES2 command routing ($D, $S, $P, etc.)
├── sdsf/
│   ├── mod.rs          # SDSF engine and panel model
│   ├── da_panel.rs     # DA — active jobs
│   ├── st_panel.rs     # ST — job status
│   ├── output_panel.rs # O/H — output queues
│   ├── log_panel.rs    # LOG — system log
│   ├── line_cmd.rs     # SDSF line commands (S, SJ, SE, SP, ?)
│   └── rexx_api.rs     # ISFEXEC / ISFCALLS REXX integration
├── console.rs          # Console model (MCS, extended MCS)
└── message.rs          # Message routing, MPF rules
```

## 3. Key Types

```rust
/// System command trait
pub trait SystemCommand: Send + Sync {
    fn name(&self) -> &str;
    fn execute(&self, args: &str, ctx: &mut ConsoleContext) -> CommandResult;
}

/// Console context for command execution
pub struct ConsoleContext {
    pub console_id: String,
    pub authority: ConsoleAuthority,
    pub output: Vec<ConsoleMessage>,
}

/// SDSF panel data model
pub struct SdsfPanel {
    pub panel_type: SdsfPanelType,
    pub filters: Vec<SdsfFilter>,
    pub sort_order: Vec<SortColumn>,
    pub rows: Vec<SdsfRow>,
}

pub enum SdsfPanelType { DA, ST, Output, Held, Log }
```

## 4. Design Decisions

### DD-5.0-CMD-01: Command Registry Pattern
**Decision:** System commands registered in a CommandRegistry (same pattern as UtilityRegistry). Commands are dispatched by name.

### DD-5.0-CMD-02: SDSF as Data Model + Renderer
**Decision:** SDSF has a panel data model (job list, filters, sort) that is independent of rendering. Rendering targets TUI (via open-mainframe-tui) or REST API.
