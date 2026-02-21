---
version: 'v5.0-addendum'
planningGroup: 'PG-4'
technology: 'JES2 Extensions'
date: '2026-02-21'
status: 'complete'
parentDocument: 'epics-jes2-v4.0.md'
inputDocuments:
  - 'epics-jes2-v4.0.md'
  - 'zos-complete-inventory.md (AREA-5)'
  - 'gap-analysis/batch-11-jes2.md'
newEpics: 1
newStories: 6
---

# JES2 Extensions Addendum (v5.0)

This addendum extends the existing JES2 v4.0 planning (J100-J107) with SYS-108: JES2 Installation Exits Framework.

## Relationship to Existing Epics

| Existing v4.0 | New SYS Epic | Relationship |
|---------------|-------------|-------------|
| J100 (Job Queue & Scheduling) | SYS-108 | SYS-108 extends J100 with exit points during job lifecycle |
| J103 (Output Processing) | SYS-108 | SYS-108 adds output-processing exits (EXIT5) |
| J106 (JES2 Init & Config) | SYS-108 | SYS-108 exits are loaded during JES2 initialization |

**Dependency:** SYS-108 depends on J100, J102, J103, and J106 being at least partially complete.

---

## SYS-108: JES2 Installation Exits Framework

**Description:** Implement a framework for JES2 installation exits — numbered extension points (EXIT1 through EXIT255) that allow customization of job processing at every stage. Real z/OS shops use exits for security checks (EXIT7), output routing (EXIT5), JCL scanning (EXIT6), and job selection (EXIT1). The framework provides a trait-based exit dispatch system where custom exit routines can be registered.

**User Value:** OpenMainframe can be customized at the JES2 level to implement site-specific job processing rules, output routing, security checks, and JCL transformations — matching how real z/OS installations customize JES2 behavior.

**Size:** M | **Stories:** 6

### SYS-108.1: Exit Framework Infrastructure

**As a** system programmer, **I want** a trait-based exit dispatch framework, **so that** exit routines can be registered and invoked at defined points in JES2 processing.

**Acceptance Criteria:**
- Given the `Jes2Exit` trait with method `fn invoke(&self, context: &ExitContext) -> ExitAction`, when an exit point is reached in JES2 processing, then all registered exit routines for that exit number are invoked in priority order
- Given `ExitAction::Continue`, when returned from an exit, then normal JES2 processing continues
- Given `ExitAction::Fail(reason)`, when returned, then the current operation fails with the reason code
- Given `ExitAction::Bypass`, when returned, then the JES2 default processing for this point is skipped
- Given no exits registered for a point, when the exit point is reached, then processing continues normally (no-op)

### SYS-108.2: Job Selection Exits (EXIT1, EXIT2)

**As a** system programmer, **I want** EXIT1 (job select) and EXIT2 (job queue change) exits, **so that** I can customize which jobs are selected for execution and track queue transitions.

**Acceptance Criteria:**
- Given EXIT1 is registered, when JES2 selects a job for execution, then EXIT1 is invoked with the job's characteristics (class, priority, jobname, userid)
- Given EXIT1 returns `ExitAction::Bypass`, when evaluated, then the job is skipped for this selection cycle
- Given EXIT2 is registered, when a job transitions from INPUT to EXECUTION queue, then EXIT2 is invoked with old and new queue identifiers
- Given the exit context, when EXIT1 or EXIT2 receives it, then it contains: job_id, job_name, job_class, priority, userid, queue_from, queue_to

### SYS-108.3: JCL/Output Exits (EXIT5, EXIT6, EXIT7)

**As a** system programmer, **I want** EXIT5 (SYSOUT selection), EXIT6 (JCL pre-scan), and EXIT7 (JCL scan) exits, **so that** I can customize output routing and JCL validation.

**Acceptance Criteria:**
- Given EXIT5 is registered, when JES2 selects SYSOUT for output processing, then EXIT5 is invoked with spool file attributes (class, destination, forms)
- Given EXIT5 modifies the output destination, when processing continues, then the SYSOUT is routed to the modified destination
- Given EXIT6 is registered, when JCL is received before conversion, then EXIT6 is invoked with each JCL statement as a text string
- Given EXIT6 modifies a JCL statement, when conversion continues, then the modified statement is used
- Given EXIT7 is registered, when JCL conversion processes each statement, then EXIT7 is invoked with the parsed JCL control blocks (job card fields, DD parameters)

### SYS-108.4: Security and Spool Exits (EXIT15, EXIT44)

**As a** security administrator, **I want** EXIT15 (job termination) and EXIT44 (SPOOL data access) exits, **so that** I can implement custom security checks and audit trails.

**Acceptance Criteria:**
- Given EXIT15 is registered, when a job terminates, then EXIT15 is invoked with the job's completion information (return code, ABEND code, step results)
- Given EXIT44 is registered, when any access to SPOOL data occurs (read/write/delete), then EXIT44 is invoked with the requestor's userid and the spool file identifier
- Given EXIT44 returns `ExitAction::Fail`, when evaluated, then the spool access is denied (security enforcement)

### SYS-108.5: Exit Configuration and Loading

**As a** system programmer, **I want** exits configured via JES2 initialization parameters, **so that** exit routines are loaded and activated during JES2 startup.

**Acceptance Criteria:**
- Given `EXIT(nnn) ROUTINES=routname,STATUS=ENABLED` in JES2PARM, when JES2 initializes, then the named routine is registered for exit point nnn
- Given `EXIT(nnn) STATUS=DISABLED` in JES2PARM, when JES2 initializes, then exit point nnn is disabled even if routines are registered
- Given `$T EXIT(nnn),STATUS=ENABLED` operator command, when issued, then exit nnn is dynamically enabled
- Given `$D EXIT(nnn)` operator command, when issued, then the exit configuration (status, routine names, call count) is displayed

### SYS-108.6: Integration Tests — Exit Processing

**As a** developer, **I want** end-to-end tests proving exits integrate with JES2 job processing, **so that** exit invocation timing and ordering is correct.

**Acceptance Criteria:**
- Given EXIT1 + EXIT7 + EXIT5 + EXIT15 all registered, when a job is submitted and runs to completion, then exits are invoked in order: EXIT7 (JCL scan) → EXIT1 (job select) → EXIT5 (output) → EXIT15 (termination)
- Given an EXIT7 that rejects jobs with NOTIFY parameter, when such a job is submitted, then the job fails at JCL conversion with a JES2 error message
- Given `cargo test -p open-mainframe-jes2`, then all SYS-108 tests pass

---

## Updated Dependency Graph

```
J100 (Job Queue) ─────┐
J102 (Spool) ─────────┤
J103 (Output) ────────┼──► SYS-108 (Installation Exits)
J106 (JES2 Init) ─────┘
```

## Coverage Notes

**Inventory components now covered by v4.0 + SYS-108:**
- Job queuing → J100 ✓
- Spool file management → J102 ✓
- JES2 Initialization → J106 ✓
- JES2 Exits (EXIT1-EXIT255) → SYS-108 ✓ (key exits implemented, framework extensible)
- JES2 Operator Commands → J104 ✓
- JECL → J105 ✓
- Internal Reader → J107 ✓
- SDSF → J107.2 ✓

**Remaining deferred (low priority):**
- NJE (Network Job Entry) — multi-system feature
- MAS (Multi-Access Spool) — sysplex feature
- PSF/FSS — print services
- SPOOL Offloading — operational
- Checkpoint datasets — operational recovery
