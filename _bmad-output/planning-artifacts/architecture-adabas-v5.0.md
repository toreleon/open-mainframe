---
version: 'v5.0'
planningGroup: 'PG-28'
technology: 'ADABAS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-adabas-v5.0.md'
---

# Architecture: ADABAS

## 1. Crate Strategy

**New crate:** `open-mainframe-adabas`

## 2. Module Layout

```
crates/open-mainframe-adabas/src/
├── lib.rs              # Crate root
├── nucleus/
│   ├── mod.rs          # Multi-threaded nucleus
│   ├── buffer.rs       # Buffer pool management
│   ├── plog.rs         # Protection log
│   └── clog.rs         # Command log
├── storage/
│   ├── mod.rs          # Storage engine
│   ├── associator.rs   # Inverted lists, address converter
│   ├── data.rs         # Data Storage with compression
│   ├── isn.rs          # ISN management
│   └── compress.rs     # Null-suppression compression
├── fdt/
│   ├── mod.rs          # Field Definition Table
│   ├── types.rs        # 7 data types + field options
│   └── descriptors.rs  # 6 descriptor types
├── commands/
│   ├── mod.rs          # ACB dispatch
│   ├── search.rs       # S1/S2/S4/S8/S9
│   ├── read.rs         # L1-L6/L9/LF
│   ├── store.rs        # N1/N2
│   ├── update.rs       # A1
│   ├── delete.rs       # E1
│   └── session.rs      # OP/CL/ET/BT/HI/RI/RC
├── acb.rs              # 80-byte ACB interface
├── buffers.rs          # FB/RB/SB/VB/IB buffer handling
├── ddm.rs              # DDM definitions for Natural
├── utilities/
│   ├── mod.rs          # Utility framework
│   ├── adacmp.rs       # Compress/decompress
│   ├── adalod.rs       # Load
│   └── adauld.rs       # Unload
└── error.rs            # Response codes
```

## 3. Design Decisions

### DD-5.0-ADA-01: Inverted Lists as B-Tree Indexes
**Decision:** ADABAS inverted lists are implemented as B-tree indexes mapping descriptor values to ISN lists. The address converter maps ISNs to physical storage locations. This preserves the ADABAS access model while using efficient Rust data structures.

### DD-5.0-ADA-02: Compression via Null-Suppression
**Decision:** Data compression uses ADABAS-compatible null-suppression: trailing blanks/zeros are removed from fixed-length fields, with length indicators. Decompression restores original field lengths from FDT metadata.

### DD-5.0-ADA-03: ACB as Command Dispatcher
**Decision:** The 80-byte ACB (ADABAS Control Block) is the single entry point for all ADABAS operations. The command code byte dispatches to the appropriate handler. All buffer parameters (FB/RB/SB/VB/IB) are passed as byte slices, matching the real ADABAS interface.
