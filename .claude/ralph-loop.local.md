---
active: true
iteration: 1
max_iterations: 50
completion_promise: "ZOSMF CONFORMANCE COMPLETE"
started_at: "2026-02-20T16:24:16Z"
---

Follow all instructions in RALPH-ZOSMF-CONFORMANCE.md exactly. Fix the open-mainframe-zosmf crate to achieve full wire-compatibility with the real IBM z/OSMF REST API. There are 4 tiers and 45 stories covering missing endpoints, missing fields, missing query parameters/headers, and behavioral fixes. Track progress in _bmad-output/conformance-progress.md. Read source files before modifying them. Run cargo check and cargo test after each story. Commit after each story passes. Signal completion with the promise tag containing ZOSMF CONFORMANCE COMPLETE when all 45 stories pass.
