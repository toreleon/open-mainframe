---
version: 'v5.0'
planningGroup: 'PG-22'
technology: 'System Initialization & PARMLIB Configuration'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: System Initialization & PARMLIB Configuration

## 1. Overview

z/OS system behavior is governed by SYS1.PARMLIB, a partitioned dataset containing 55+ configuration members that control every subsystem from IPL through steady-state operation. The IPL process (Nucleus Initialization Program) reads these members to establish system identity, library paths, subsystem parameters, and operational policies.

This PRD defines requirements for a PARMLIB-compatible configuration framework in OpenMainframe, enabling z/OS-style system configuration via member parsing, symbol substitution, and subsystem parameter injection.

## 2. Functional Requirements

### FR-CFG-001: PARMLIB Dataset Model
The system SHALL model SYS1.PARMLIB as a collection of named configuration members. Each member is identified by a base name (e.g., IEASYS) and a two-character suffix (xx), allowing multiple versions with runtime selection. The system SHALL support a PARMLIB concatenation (multiple PARMLIB datasets searched in order).

### FR-CFG-002: IEASYSxx — Master System Parameters
The system SHALL parse IEASYSxx members to extract master system parameters including: SYSPLEX name, system name, LNKLST suffix, LPA suffix, SMF suffix, CONSOLxx suffix, CLOCK, MAXUSER, RSVSTRT, PAGE datasets, SQA/CSA sizes, and subsystem suffix specifications. All parameters SHALL use the IBM-documented keyword=value syntax.

### FR-CFG-003: IEASYMxx — System Symbol Substitution
The system SHALL parse IEASYMxx members to define system symbols (e.g., `&SYSNAME.`, `&SYSPLEX.`). These symbols SHALL be substituted into all subsequently-read PARMLIB members, enabling single-source configuration across multiple systems in a sysplex.

### FR-CFG-004: LNKLSTxx — Linklist Configuration
The system SHALL parse LNKLSTxx members to define the linklist concatenation — an ordered list of load library datasets automatically searched for program loading without explicit STEPLIB/JOBLIB.

### FR-CFG-005: PROGxx — Program Management
The system SHALL parse PROGxx members to manage APF (Authorized Program Facility) authorized libraries, dynamic linklist updates (LNKLST ADD/DELETE), and LPA (Link Pack Area) updates. This replaces the legacy IEAAPFxx and IEALPAxx members.

### FR-CFG-006: CONSOLxx — Console Configuration
The system SHALL parse CONSOLxx members to define console attributes including: console name, unit address, authority level (MASTER/ALL/INFO/IO/SYS/CONS), routing codes, and hardcopy log settings.

### FR-CFG-007: COMMNDxx — IPL Commands
The system SHALL parse COMMNDxx members to define commands automatically issued during system initialization. Each entry specifies a z/OS operator command and when it should be issued (during or after NIP processing).

### FR-CFG-008: IKJTSOxx — TSO/E Parameters
The system SHALL parse IKJTSOxx members to define TSO/E configuration including: authorized commands, authorized programs, authorized CALL libraries, LOGON procedure defaults, SEND parameters, and HELP dataset concatenation.

### FR-CFG-009: ALLOCxx — Dynamic Allocation Defaults
The system SHALL parse ALLOCxx members to define default allocation parameters for datasets including: unit name, volume, SMS class defaults, and disposition settings.

### FR-CFG-010: SMFPRMxx/BPXPRMxx Delegation
The system SHALL delegate SMFPRMxx parsing to the SMF crate and BPXPRMxx parsing to the USS crate, providing a common member-reading interface. Each subsystem owns its parameter semantics; the PARMLIB framework provides discovery and raw member content.

### FR-CFG-011: System Initialization Sequence
The system SHALL implement a startup sequence that: (1) locates the PARMLIB concatenation, (2) reads IEASYMxx for symbol definitions, (3) reads IEASYSxx for master parameters with symbol substitution, (4) reads subsidiary members (LNKLSTxx, PROGxx, CONSOLxx, etc.) as directed by IEASYSxx suffix specifications, (5) issues COMMNDxx commands, and (6) signals subsystem initialization.

### FR-CFG-012: SET/SETPROG Operator Commands
The system SHALL support runtime reconfiguration via operator commands: SET for changing PARMLIB member suffixes (e.g., `SET SMF=xx` to switch SMF parameters), and SETPROG for modifying APF list, linklist, and LPA at runtime without IPL.

## 3. Non-Functional Requirements

### NFR-CFG-001: Test Coverage
All PARMLIB member parsers SHALL have unit tests achieving ≥90% code coverage. Integration tests SHALL validate the full initialization sequence.

### NFR-CFG-002: Extensibility
The PARMLIB framework SHALL support registration of new member parsers by other crates, enabling subsystems to define their own PARMLIB members without modifying the core framework.

### NFR-CFG-003: Backward Compatibility
The system SHALL accept real z/OS PARMLIB member syntax, allowing existing PARMLIB members to be used with minimal or no modification.
