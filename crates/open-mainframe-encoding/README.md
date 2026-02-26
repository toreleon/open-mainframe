# open-mainframe-encoding

EBCDIC encoding and decimal arithmetic for OpenMainframe — provides data encoding and conversion facilities for processing mainframe data on Linux systems, covering 21 IBM code pages, six COBOL numeric storage formats, DBCS mixed encoding, EBCDIC collation, NATIONAL (UTF-16) fields, and PIC-clause-driven field conversion.

## Overview

Mainframe data interchange requires faithful conversion between IBM EBCDIC character encoding and modern Unicode/ASCII, plus correct handling of COBOL numeric storage formats (packed decimal, zoned decimal, binary integers, floating point). This crate is a foundational dependency used by nearly every other `open-mainframe-*` crate whenever raw mainframe bytes must be interpreted or produced.

The implementation provides:

- **21 EBCDIC code pages** — 3 base (CP037, CP500, CP1047), 8 national (German, Danish, Swedish, Italian, Spanish, UK, French, Icelandic), 10 Euro-enabled (CP1140–CP1149)
- **Packed decimal (COMP-3)** — BCD encoding with 18-digit precision and `rust_decimal` integration
- **Zoned decimal (DISPLAY)** — one-digit-per-byte format with sign zone nibble
- **Binary integer (COMP/COMP-4)** — big-endian two's complement at halfword/fullword/doubleword sizes
- **Native binary (COMP-5)** — full storage-range variant of COMP
- **IBM HFP floating point** — COMP-1 (4-byte short) and COMP-2 (8-byte long) with base-16 exponents
- **IEEE 754 floating point** — FLOAT-SHORT (f32) and FLOAT-LONG (f64) in big-endian byte order
- **NATIONAL (PIC N)** — UTF-16 big-endian encoding with EBCDIC-to-national conversion
- **DBCS mixed encoding** — SO/SI shift-state parsing for East Asian code pages
- **EBCDIC collation** — native byte-value sorting matching z/OS DFSORT behavior
- **PIC clause pipeline** — parses COBOL PIC strings and resolves the correct encoder automatically

## Architecture

```
open-mainframe-encoding
├── ebcdic/                   # EBCDIC character encoding
│   ├── mod.rs                # CodePage encode/decode, from_ccsid, by_name
│   ├── tables.rs             # CodePage struct + CP037/CP500/CP1047 tables
│   ├── extended_tables.rs    # 18 national + Euro code page tables
│   ├── registry.rs           # CodePageRegistry (CCSID + name lookup)
│   ├── collation.rs          # EBCDIC byte-value comparison and sorting
│   └── dbcs.rs               # SO/SI mixed SBCS/DBCS stream handling
├── decimal/                  # Numeric storage formats
│   ├── mod.rs                # Sign enum, module re-exports
│   ├── packed.rs             # COMP-3 packed decimal (BCD)
│   ├── zoned.rs              # DISPLAY zoned decimal
│   ├── binary.rs             # COMP/COMP-4 big-endian binary
│   ├── native_binary.rs      # COMP-5 native binary (full range)
│   ├── floating.rs           # IBM HFP short/long (base-16 float)
│   ├── ieee_float.rs         # IEEE 754 single/double precision
│   └── national.rs           # NATIONAL (UTF-16 BE) encoding
├── field_conversion.rs       # PIC clause parser + encoder resolver
├── error.rs                  # EncodingError with miette diagnostics
└── lib.rs                    # Crate root, re-exports
```

### Module Descriptions

| Module | Description |
|--------|-------------|
| `ebcdic::tables` | `CodePage` struct with bidirectional 256-byte translation tables and special-char overlay for multi-byte Unicode (e.g., Euro sign) |
| `ebcdic::extended_tables` | Static `CodePage` constants for 18 code pages: CP273 (DE), CP277 (DK/NO), CP278 (SE/FI), CP280 (IT), CP284 (ES), CP285 (UK), CP297 (FR), CP871 (IS), CP1140–CP1149 (Euro variants) |
| `ebcdic::registry` | `CodePageRegistry` with CCSID lookup, multi-format name matching (CP/IBM/IBM-/EBCDIC- prefixes, case-insensitive), and `all()` enumeration |
| `ebcdic::collation` | EBCDIC native collation: lowercase < uppercase < digits ordering, `sort_ebcdic`, `is_ebcdic_sorted`, `EbcdicCharClass` classification |
| `ebcdic::dbcs` | SO (0x0E) / SI (0x0F) state machine for mixed SBCS/DBCS streams; `parse_mixed_stream`, `encode_mixed_stream`, `mixed_char_count` |
| `decimal::packed` | `PackedDecimal` struct and `pack_decimal`/`unpack_decimal` with `rust_decimal::Decimal`; lightweight `pack_from_i64`/`unpack_to_i64` for SORT/IMS use |
| `decimal::zoned` | `ZonedDecimal` struct and `zone_decimal`/`unzone_decimal` with zone-nibble sign encoding; `zone_from_i64`/`unzone_to_i64` helpers |
| `decimal::binary` | `BinaryInteger` struct; COBOL storage-size rules (1–4 digits → 2B, 5–9 → 4B, 10–18 → 8B); big-endian encode/decode with range validation |
| `decimal::native_binary` | `NativeBinaryInteger` (COMP-5); uses full storage range rather than PIC-digit-limited range; `validate_native_range` |
| `decimal::floating` | `HfpFloat`/`HfpDouble` wrappers; base-16 exponent encode/decode; `ieee_to_hfp_*` / `hfp_*_to_ieee` cross-format conversion |
| `decimal::ieee_float` | `IeeeFloat`/`IeeeDouble` newtype wrappers; big-endian IEEE 754 encode/decode with NaN rejection; `encode_*_into` buffer variants |
| `decimal::national` | UTF-16 big-endian encode/decode with space padding and trimming; `ebcdic_to_national` cross-encoding conversion |
| `field_conversion` | `parse_pic` parser (handles S, 9, X, N, V, parenthesized repeats); `resolve_encoder` maps PIC + USAGE → `FieldEncoder`; `FieldEncoder::byte_length` |
| `error` | `EncodingError` enum with 6 variants: `InvalidCodePage`, `ConversionFailed`, `OutOfRange`, `InvalidDigit`, `InvalidSign`, `PrecisionLoss`; all with miette diagnostics |

## Key Types and Traits

### EBCDIC

- **`CodePage`** — Core struct holding bidirectional `[u8; 256]` translation tables plus a `special_chars` overlay for Unicode code points outside Latin-1 (e.g., Euro sign at 0x9F). Provides `encode`, `decode`, `ebcdic_to_char`, `ebcdic_to_ascii_byte`, `ascii_to_ebcdic_byte`, `from_ccsid`, and `by_name`.
- **`CodePageRegistry`** — Static registry of all 21 code pages with `from_ccsid(u16)`, `by_name(&str)` (supports CP/IBM/IBM-/EBCDIC- prefixes, case-insensitive), and `all()`.
- **`EbcdicCharClass`** — Enum classifying EBCDIC bytes: `Control`, `Space`, `Special`, `Lowercase`, `Uppercase`, `Digit`.
- **`MixedSegment`** — Enum for DBCS parsing: `Sbcs(Vec<u8>)` or `Dbcs(Vec<[u8; 2]>)`.
- **`ShiftState`** — SO/SI state machine state: `Sbcs` or `Dbcs`.

### Decimal / Numeric

- **`Sign`** — Positive/Negative/Unsigned with nibble conversion methods (`to_packed_nibble`, `from_packed_nibble`).
- **`PackedDecimal`** — Value + metadata (integer digits, decimal digits, signed); `encode()` / `decode()` / `storage_size()`.
- **`ZonedDecimal`** — Same shape as `PackedDecimal` but for DISPLAY format.
- **`BinaryInteger`** — COMP/COMP-4 with COBOL storage-size rules.
- **`NativeBinaryInteger`** — COMP-5 with full storage-range access and `min_value`/`max_value`.
- **`HfpFloat` / `HfpDouble`** — IBM HFP wrappers rejecting NaN/Infinity.
- **`IeeeFloat` / `IeeeDouble`** — Newtype wrappers `IeeeFloat(pub f32)`, `IeeeDouble(pub f64)`.

### Field Conversion

- **`CobolUsage`** — 9-variant enum: `Display`, `Comp`, `Comp3`, `Comp5`, `Comp1`, `Comp2`, `FloatShort`, `FloatLong`, `National`.
- **`FieldEncoder`** — 10-variant enum mapping to specific encoders with `byte_length()`.
- **`PicClause`** — Parsed PIC information: digits, decimal places, signed, is_alpha, is_national.

### Error

- **`EncodingError`** — 6 variants with miette diagnostic codes and help text.

## Implementation Details

### EBCDIC Code Page Design

Each code page is a `const CodePage` with two `[u8; 256]` lookup tables for O(1) byte-to-byte conversion. The `special_chars` slice handles Unicode characters above U+00FF (currently only the Euro sign € at 0x9F in CP1140–CP1149). Encoding scans `special_chars` first, then falls through to the main table. The 10 Euro code pages are derived from their base pages (CP1140 from CP037, CP1148 from CP500, etc.) with only position 0x9F changed.

National code pages (German, Danish, French, etc.) remap specific positions for accented characters while preserving the standard EBCDIC letter/digit positions (A–Z at 0xC1–0xE9, 0–9 at 0xF0–0xF9).

### EBCDIC Collation

Native EBCDIC collation uses raw byte-value ordering, which differs significantly from ASCII: lowercase (0x81–0xA9) sorts before uppercase (0xC1–0xE9), and letters sort before digits (0xF0–0xF9). This matches z/OS DFSORT `COLLATING SEQUENCE IS NATIVE` behavior.

### Packed Decimal (COMP-3)

Two digit nibbles per byte, sign nibble in the rightmost position of the last byte. Supported signs: 0xC/0xA/0xE (positive), 0xD/0xB (negative), 0xF (unsigned). The `pack_decimal`/`unpack_decimal` functions work with `rust_decimal::Decimal` for arbitrary precision. Lightweight `i64` helpers (`pack_from_i64`/`unpack_to_i64`) avoid `Decimal` overhead for SORT and IMS use cases.

### Zoned Decimal (DISPLAY)

One digit per byte with 0xF0 zone nibble for most positions. The last byte's zone nibble carries the sign (0xC0 positive, 0xD0 negative, 0xF0 unsigned). Same dual API pattern: `Decimal`-based and `i64`-based helpers.

### Binary Integers (COMP / COMP-5)

Both use big-endian two's complement. COMP limits values to PIC-digit range (e.g., PIC S9(4) → ±9999), while COMP-5 allows the full storage range (PIC S9(4) → ±32767). Storage sizes follow COBOL rules: 1–4 digits → halfword (2B), 5–9 → fullword (4B), 10–18 → doubleword (8B).

### IBM HFP Floating Point

Uses base-16 exponents with bias 64 (not base-2 like IEEE 754). No implicit leading bit, no NaN/Infinity/denormals. Short format: 1 sign + 7 exponent + 24 fraction bits. Long format: 1 sign + 7 exponent + 56 fraction bits. Cross-conversion functions handle IEEE ↔ HFP with appropriate precision loss.

### PIC Clause Pipeline

`parse_pic` handles data-storage PIC forms: `S9(5)V99`, `X(30)`, `N(10)`, `999`, etc. `resolve_encoder` combines the parsed PIC with a `CobolUsage` to produce the correct `FieldEncoder` variant, enabling automatic record-level conversion from copybook definitions.

## Feature Coverage

### EBCDIC Code Pages

| Code Page | Region | Status |
|-----------|--------|--------|
| CP037 | US/Canada | :white_check_mark: Complete |
| CP500 | International | :white_check_mark: Complete |
| CP1047 | Latin-1/Open Systems (USS) | :white_check_mark: Complete |
| CP273 | Germany/Austria | :white_check_mark: Complete |
| CP277 | Denmark/Norway | :white_check_mark: Complete |
| CP278 | Sweden/Finland | :white_check_mark: Complete |
| CP280 | Italy | :white_check_mark: Complete |
| CP284 | Spain/Latin America | :white_check_mark: Complete |
| CP285 | United Kingdom | :white_check_mark: Complete |
| CP297 | France | :white_check_mark: Complete |
| CP871 | Iceland | :white_check_mark: Complete |
| CP1140–CP1149 | Euro variants (10 pages) | :white_check_mark: Complete |

### Numeric Formats

| Format | COBOL Usage | Status |
|--------|-------------|--------|
| Packed Decimal | COMP-3 | :white_check_mark: 18-digit precision |
| Zoned Decimal | DISPLAY | :white_check_mark: Full sign support |
| Binary Integer | COMP / COMP-4 | :white_check_mark: Halfword/fullword/doubleword |
| Native Binary | COMP-5 | :white_check_mark: Full storage range |
| HFP Short | COMP-1 | :white_check_mark: Base-16 float |
| HFP Long | COMP-2 | :white_check_mark: Base-16 double |
| IEEE Short | FLOAT-SHORT | :white_check_mark: f32 big-endian |
| IEEE Long | FLOAT-LONG | :white_check_mark: f64 big-endian |
| NATIONAL | PIC N | :white_check_mark: UTF-16 BE |

### Additional Features

| Feature | Status |
|---------|--------|
| EBCDIC collation (native byte-value order) | :white_check_mark: Complete |
| DBCS mixed encoding (SO/SI) | :white_check_mark: Complete |
| PIC clause parsing | :white_check_mark: Data-storage forms |
| PIC → FieldEncoder resolution | :white_check_mark: All 9 USAGE variants |
| Code page registry (CCSID + name lookup) | :white_check_mark: 21 pages |
| miette diagnostic error reporting | :white_check_mark: 6 error variants |

## Usage Examples

### EBCDIC Conversion

```rust
use open_mainframe_encoding::ebcdic::{CP037, CP1140, CodePage};

// Basic encode/decode
let ebcdic = CP037.encode("HELLO WORLD").unwrap();
let ascii = CP037.decode(&ebcdic).unwrap();
assert_eq!(ascii, "HELLO WORLD");

// Euro sign with CP1140
let encoded = CP1140.encode("Price: 100€").unwrap();
let decoded = CP1140.decode(&encoded).unwrap();
assert_eq!(decoded, "Price: 100€");

// Runtime code page lookup
let cp = CodePage::from_ccsid(273).unwrap(); // German
let cp = CodePage::by_name("IBM-1047").unwrap(); // USS
```

### Packed Decimal

```rust
use open_mainframe_encoding::decimal::{pack_decimal, unpack_decimal};
use rust_decimal::Decimal;
use std::str::FromStr;

let value = Decimal::from_str("123.45").unwrap();
let packed = pack_decimal(&value, 3, 2, true).unwrap();
// Result: [0x12, 0x34, 0x5C] — three BCD digit pairs + positive sign

let (unpacked, sign) = unpack_decimal(&packed, 2).unwrap();
assert_eq!(unpacked, value);
```

### EBCDIC Collation

```rust
use open_mainframe_encoding::ebcdic::collation::{sort_ebcdic, ebcdic_compare};
use open_mainframe_encoding::ebcdic::CP037;

let mut data = vec!["123".into(), "ABC".into(), "abc".into(), " ".into()];
sort_ebcdic(&mut data, &CP037);
// EBCDIC order: space < lowercase < uppercase < digits
assert_eq!(data, vec![" ", "abc", "ABC", "123"]);
```

### PIC Clause Resolution

```rust
use open_mainframe_encoding::field_conversion::{parse_pic, resolve_encoder, CobolUsage, FieldEncoder};

let pic = parse_pic("S9(5)V99");
let encoder = resolve_encoder(&pic, &CobolUsage::Comp3);
// Returns: FieldEncoder::PackedDecimal { digits: 7, decimal: 2, signed: true }
assert_eq!(encoder.byte_length(), 4);
```

## Dependencies

| Crate | Purpose |
|-------|---------|
| `miette` | Diagnostic-rich error reporting |
| `thiserror` | Error derive macros |
| `rust_decimal` | Arbitrary-precision decimal arithmetic for packed/zoned conversions |

## Testing

The crate includes extensive tests across all modules (~200 test cases):

- **EBCDIC roundtrip** — all 256 byte values verified for every code page (21 × 256 = 5,376 roundtrips)
- **Euro sign** — encode/decode verified for all 10 CP1140–CP1149 pages at position 0x9F
- **National characters** — German (ä/ö/ü/ß/Ä/Ö/Ü), Danish (æ/ø/å/Æ/Ø/Å), French (é/è/à/ù/ç), UK (£)
- **Packed decimal** — positive, negative, unsigned, zero, single-digit, 18-digit precision, even/odd digit counts, `i64` helpers
- **Zoned decimal** — sign zone nibble encoding, decimal places, `i64` helpers
- **Binary integer** — halfword/fullword/doubleword, signed/unsigned, range validation
- **COMP-5 native binary** — full storage range vs PIC-digit range verification
- **IBM HFP** — known reference values (0x41100000 = 1.0), roundtrip for short and long, NaN/Infinity rejection
- **IEEE 754** — big-endian byte order verification, NaN rejection, infinity handling, buffer-write variants
- **NATIONAL** — UTF-16 BE roundtrip, space padding/trimming, truncation, EBCDIC-to-national conversion
- **DBCS mixed** — SO/SI parsing, unpaired shift detection, odd-byte DBCS error, empty regions, multiple DBCS regions
- **Collation** — lowercase < uppercase < digits ordering, sorted-check, character classification
- **PIC parsing** — all USAGE types, case insensitivity, repeated nines, byte_length calculation
- **Registry** — CCSID lookup, multi-format name lookup (CP/IBM/IBM-/EBCDIC-), unknown code page errors

```bash
cargo test -p open-mainframe-encoding
```

## Limitations and Future Work

- **DBCS code pages not shipped** — the SO/SI parsing infrastructure exists but no East Asian code page tables (CP930/CP933/CP935) are included; only the stream parser is implemented
- **PIC clause subset** — `parse_pic` handles data-storage forms (9, X, N, V, S) but not editing PIC characters (Z, *, $, B, 0, comma, period insertion)
- **No HFP extended precision** — only short (4-byte) and long (8-byte) HFP; no extended (16-byte) format
- **Special characters limited to Euro** — the `special_chars` overlay mechanism is in place but only the Euro sign (€) is mapped; other multi-byte EBCDIC characters (e.g., yen sign in some pages) are not handled
- **No streaming API** — all encode/decode operations work on complete buffers; no incremental/streaming conversion for very large records
- **Code page derivation is static** — each code page has its own full 256-byte tables; no shared-base-with-delta mechanism (compile-time only concern, not a runtime issue)
