//! DRDA response builders.
//!
//! Builds the binary DDM objects sent back to the client: SQLCARD, SQLDARD,
//! QRYDSC, QRYDTA, and various reply messages.

use bytes::{BufMut, BytesMut};

use crate::code_points::*;
use crate::ddm::{DdmBuilder, DdmObject};
use crate::dss::DssSegment;

/// Column metadata for query result descriptions.
#[derive(Debug, Clone)]
pub struct ColumnDesc {
    pub name: String,
    pub data_type: FdocaType,
    pub length: u16,
    pub precision: u16,
    pub scale: u16,
    pub nullable: bool,
}

/// FD:OCA data types used in DRDA query results.
#[derive(Debug, Clone, Copy)]
pub enum FdocaType {
    Varchar,
    FixedChar,
    SmallInt,
    Integer,
    BigInt,
    Decimal,
    Float,
    Date,
    Time,
    Timestamp,
}

impl FdocaType {
    /// Get the FD:OCA type code byte (not-nullable variant).
    pub fn type_code(self) -> u8 {
        match self {
            FdocaType::Varchar => FDOCA_TYPE_VARCHAR,
            FdocaType::FixedChar => FDOCA_TYPE_FIXEDCHAR,
            FdocaType::SmallInt => FDOCA_TYPE_SMALLINT,
            FdocaType::Integer => FDOCA_TYPE_INTEGER,
            FdocaType::BigInt => FDOCA_TYPE_BIGINT,
            FdocaType::Decimal => FDOCA_TYPE_DECIMAL,
            FdocaType::Float => FDOCA_TYPE_FLOAT8,
            FdocaType::Date => FDOCA_TYPE_DATE,
            FdocaType::Time => FDOCA_TYPE_TIME,
            FdocaType::Timestamp => FDOCA_TYPE_TIMESTAMP,
        }
    }

    /// Get the FD:OCA type code byte (nullable variant — odd code).
    pub fn nullable_type_code(self) -> u8 {
        match self {
            FdocaType::Varchar => FDOCA_TYPE_NVARCHAR,
            FdocaType::FixedChar => FDOCA_TYPE_NFIXEDCHAR,
            FdocaType::SmallInt => FDOCA_TYPE_NSMALLINT,
            FdocaType::Integer => FDOCA_TYPE_NINTEGER,
            FdocaType::BigInt => FDOCA_TYPE_NBIGINT,
            FdocaType::Decimal => FDOCA_TYPE_NDECIMAL,
            FdocaType::Float => FDOCA_TYPE_NFLOAT8,
            FdocaType::Date => FDOCA_TYPE_NDATE,
            FdocaType::Time => FDOCA_TYPE_NTIME,
            FdocaType::Timestamp => FDOCA_TYPE_NTIMESTAMP,
        }
    }
}

// ── SQLCARD (SQLCA Reply Data) ───────────────────────────

/// Build an SQLCARD DDM object representing a successful operation.
///
/// For a simple success with no row count, uses a null SQLCAGRP (1-byte 0xFF).
/// For success with rows_affected > 0, uses full SQLCAGRP layout.
pub fn build_sqlcard_success(rows_affected: i32) -> DdmObject {
    if rows_affected == 0 {
        // Null SQLCAGRP — shorthand for SQLCODE=0, SQLSTATE='00000'
        DdmObject::new(SQLCARD, vec![0xFF])
    } else {
        build_sqlcard(0, "00000", rows_affected, "")
    }
}

/// Build an SQLCARD for "not found" (SQLCODE=100).
pub fn build_sqlcard_not_found() -> DdmObject {
    build_sqlcard(100, "02000", 0, "")
}

/// Build an SQLCARD for an error.
pub fn build_sqlcard_error(sqlcode: i32, sqlstate: &str, message: &str) -> DdmObject {
    build_sqlcard(sqlcode, sqlstate, 0, message)
}

/// Build a raw SQLCARD DDM object.
///
/// Wire format (SQLAM >= 7, DRDA spec §5.6.4.6):
///
/// ```text
/// [0]       1   SQLCAGRP null_ind (0x00=present, 0xFF=null)
/// [1-4]     4   SQLCODE (i32 little-endian, QTDSQLX86)
/// [5-9]     5   SQLSTATE (FCS, 5 chars)
/// [10-17]   8   SQLERRPROC (FCS, 8 chars)
/// [18]      1   SQLCAXGRP null_ind (0x00=present, 0xFF=null)
/// [19-42]  24   SQLERRD (6 x i32)
/// [43-53]  11   SQLWARN (11 x 1-byte char)
/// [54-55]   2   SQLRDBNAME length + data
/// [..]      2   SQLERRMSG_m length + data
/// [..]      2   SQLERRMSG_s length + data
/// [..]      1   SQLDIAGGRP null_ind (0xFF=null)
/// ```
fn build_sqlcard(sqlcode: i32, sqlstate: &str, rows_affected: i32, message: &str) -> DdmObject {
    let mut payload = BytesMut::new();

    // ── SQLCAGRP (not null) ──
    payload.put_u8(0x00);

    // SQLCODE (4 bytes, little-endian for QTDSQLX86)
    payload.put_i32_le(sqlcode);

    // SQLSTATE (5 bytes, space-padded)
    let mut state_bytes = [0x20u8; 5];
    let state = sqlstate.as_bytes();
    let copy_len = state.len().min(5);
    state_bytes[..copy_len].copy_from_slice(&state[..copy_len]);
    payload.extend_from_slice(&state_bytes);

    // SQLERRPROC (8 bytes, space-padded — procedure/module name)
    payload.extend_from_slice(b"SQLDRDA ");

    // ── SQLCAXGRP (not null) ──
    payload.put_u8(0x00);

    // SQLERRD (6 x 4 bytes = 24 bytes, little-endian for QTDSQLX86)
    payload.put_i32_le(0);              // SQLERRD1
    payload.put_i32_le(0);              // SQLERRD2
    payload.put_i32_le(rows_affected);  // SQLERRD3 — row count
    payload.put_i32_le(0);              // SQLERRD4
    payload.put_i32_le(0);              // SQLERRD5
    payload.put_i32_le(0);              // SQLERRD6

    // SQLWARN (11 bytes: SQLWARN0..SQLWARNA, space = no warning)
    payload.extend_from_slice(&[0x20u8; 11]);

    // SQLRDBNAME (VCS: 2-byte length + data) — big-endian (pydrda hardcodes 'big')
    payload.put_u16(0);

    // SQLERRMSG_m (VCM: 2-byte length + data) — big-endian (pydrda hardcodes 'big')
    let msg_bytes = message.as_bytes();
    payload.put_u16(msg_bytes.len() as u16);
    payload.extend_from_slice(msg_bytes);

    // SQLERRMSG_s (VCS: 2-byte length + data) — big-endian (pydrda hardcodes 'big')
    payload.put_u16(0);

    // ── SQLDIAGGRP (null — no diagnostics) ──
    payload.put_u8(0xFF);

    DdmObject::new(SQLCARD, payload.to_vec())
}

// ── SQLDARD (SQL Descriptor Area Reply Data) ─────────────

/// Build a SQLDARD describing the columns of a query result.
///
/// SQLDARD structure at SQLAM level 7 (matching Apache Derby):
/// 1. SQLCAGRP — full SQLCA (SQLCODE=0 for success, not null indicator)
/// 2. SQLDHROW — holdability group with null indicator + fields
/// 3. SQLD — number of result columns (2 bytes)
/// 4. Per-column: basic fields + SQLDOPTGRP (includes SQLDXGRP)
pub fn build_sqldard(columns: &[ColumnDesc]) -> DdmObject {
    let mut payload = BytesMut::new();

    // ── SQLCAGRP (full SQLCA, matching Derby's writeSQLCAGRP) ──
    // Derby sends a full SQLCAGRP with SQLCODE=0, not a null indicator
    write_sqlcagrp_success(&mut payload);

    // ── SQLDHROW (holdability group) — DB2 format ──
    // pydrda expects: null_ind(1) + 12 bytes + VCM(sqlrdbnam) + parse_name(sqlschema)
    // The 12 bytes appear to be: SQLDHOLD(2) + SQLSENSITIVITY(2) + padding(8)
    // SQLDHOLD and padding are skipped by pydrda (rest = rest[13:])
    // VCM/VCS lengths use big-endian (parse_string hardcodes 'big')
    payload.put_u8(0x00);            // present
    payload.put_u16(1);              // SQLDHOLD = 1 (part of 12-byte block, skipped)
    payload.extend_from_slice(&[0x00; 10]); // remaining 10 bytes of the 12-byte block
    // SQLDRDBNAM (VCM: 2-byte length) — big-endian
    payload.put_u16(0);
    // SQLDSCHEMA (VCM + VCS) — big-endian
    payload.put_u16(0);              // VCM length = 0
    payload.put_u16(0);              // VCS length = 0

    // ── SQLD (number of columns, little-endian) ──
    payload.put_u16_le(columns.len() as u16);

    // ── Per-column SQLDAGRP entries (DB2 format, matching pydrda expectations) ──
    //
    // DB2 column descriptor layout (from pydrda _parse_column_db2):
    //   [0:2]   SQLPRECISION
    //   [2:4]   SQLSCALE
    //   [4:12]  SQLLENGTH (8 bytes, long form)
    //   [12:14] SQLTYPE
    //   [14:16] SQLCCSID
    //   [16:22] 6 bytes: DB2 extension fields (skipped by pydrda `b = b[6:]`)
    //   SQLDOPTGRP:
    //     [0]     null_ind (0x00 = present)
    //     [1:3]   SQLUNAMED (2 bytes)
    //     ...     SQLNAME (VCM + VCS)
    //     ...     SQLLABEL (VCM + VCS)
    //     ...     SQLCOMMENTS (VCM + VCS)
    //     [7 bytes] trailing (SQLUDTGRP + SQLDXGRP, skipped by pydrda `b = b[7:]`)
    for col in columns {
        // Basic column fields (16 bytes, all little-endian for QTDSQLX86)
        let precision = if col.precision == 0 {
            default_precision(col.data_type)
        } else {
            col.precision
        };
        payload.put_u16_le(precision);
        payload.put_u16_le(col.scale);
        payload.put_u64_le(col.length as u64);
        let type_code = drda_sql_type(col.data_type, col.nullable);
        payload.put_u16_le(type_code);
        let ccsid = match col.data_type {
            FdocaType::Varchar | FdocaType::FixedChar => 1208u16,
            _ => 0u16,
        };
        payload.put_u16(ccsid); // big-endian (pydrda hardcodes 'big' for sqlccsid)

        // ── DB2 extension (6 bytes, skipped by pydrda: `b = b[6:]`) ──
        // These appear to be SQLDAGRP extension fields present in DB2 format.
        payload.extend_from_slice(&[0x00; 6]);

        // ── SQLDOPTGRP ──
        payload.put_u8(0x00);       // not null
        payload.put_u16(0);         // SQLUNAMED = 0 (named)
        // SQLNAME (VCM + VCS) — VCM/VCS lengths are always big-endian
        let name_bytes = col.name.as_bytes();
        if name_bytes.is_empty() {
            payload.put_u16(0);     // VCM = empty
            payload.put_u16(0);     // VCS = empty
        } else {
            payload.put_u16(name_bytes.len() as u16); // VCM length (big-endian)
            payload.extend_from_slice(name_bytes);
            payload.put_u16(0);     // VCS = empty
        }
        // SQLLABEL (VCM + VCS) — empty
        payload.put_u16(0);
        payload.put_u16(0);
        // SQLCOMMENTS (VCM + VCS) — empty
        payload.put_u16(0);
        payload.put_u16(0);

        // ── Trailing 7 bytes (skipped by pydrda: `b = b[7:]`) ──
        // SQLUDTGRP null indicator + SQLDXGRP (null/compact form)
        payload.put_u8(0xFF);       // SQLUDTGRP null
        payload.put_u8(0xFF);       // SQLDXGRP null
        payload.extend_from_slice(&[0x00; 5]); // remaining 5 bytes of trailing block
    }

    DdmObject::new(SQLDARD, payload.to_vec())
}

/// Write a full SQLCAGRP for success (SQLCODE=0).
/// All numeric SQL data fields use little-endian (QTDSQLX86).
fn write_sqlcagrp_success(buf: &mut BytesMut) {
    // SQLCAGRP null indicator (0x00 = present)
    buf.put_u8(0x00);
    // SQLCODE (4 bytes, little-endian)
    buf.put_i32_le(0);
    // SQLSTATE (5 bytes, space-padded)
    buf.extend_from_slice(b"     ");
    // SQLERRPROC (8 bytes — "CSS10160" matches Derby)
    buf.extend_from_slice(b"CSS10160");
    // SQLCAXGRP null indicator (0x00 = present)
    buf.put_u8(0x00);
    // SQLERRD (6 x 4 bytes = 24 bytes, little-endian)
    for _ in 0..6 {
        buf.put_i32_le(0);
    }
    // SQLWARN (11 bytes, space = no warning)
    buf.extend_from_slice(&[0x20u8; 11]);
    // SQLRDBNAME (VCS: 2-byte length) — big-endian (pydrda hardcodes 'big')
    buf.put_u16(0);
    // SQLERRMSG_M (VCM: 2-byte length) — big-endian (pydrda hardcodes 'big')
    buf.put_u16(0);
    // SQLERRMSG_S (VCS: 2-byte length) — big-endian (pydrda hardcodes 'big')
    buf.put_u16(0);
    // SQLDIAGGRP null indicator (0xFF = null)
    buf.put_u8(0xFF);
}

/// Default precision for data types (used when precision is 0).
fn default_precision(typ: FdocaType) -> u16 {
    match typ {
        FdocaType::Integer => 10,
        FdocaType::SmallInt => 5,
        FdocaType::BigInt => 19,
        FdocaType::Decimal => 15,
        FdocaType::Float => 15,
        _ => 0,
    }
}

/// Map FdocaType + nullable to DRDA SQLTYPE code.
fn drda_sql_type(typ: FdocaType, nullable: bool) -> u16 {
    let base = match typ {
        FdocaType::FixedChar => 452,   // CHAR
        FdocaType::Varchar => 448,     // VARCHAR
        FdocaType::SmallInt => 500,    // SMALLINT
        FdocaType::Integer => 496,     // INTEGER
        FdocaType::BigInt => 492,      // BIGINT
        FdocaType::Decimal => 484,     // DECIMAL
        FdocaType::Float => 480,       // FLOAT/DOUBLE
        FdocaType::Date => 384,        // DATE
        FdocaType::Time => 388,        // TIME
        FdocaType::Timestamp => 392,   // TIMESTAMP
    };
    if nullable { base + 1 } else { base }
}

// ── QRYDSC (Query Answer Set Description) ────────────────

/// Build a QRYDSC describing the format of query result rows.
///
/// Uses pydrda-compatible compact FD:OCA format:
///
/// ```text
/// [total_len] [0x76 0xD0]          ← GDA header (repeating row group)
/// [type PS0 PS1] ...               ← 3-byte column descriptors
/// [0x06 0x71 0xE4 0xD0 0x00 0x01]  ← trailer (end of repeating group)
/// ```
///
/// Each column descriptor is 3 bytes:
///   - Byte 0: FD:OCA type code (even=NOT NULL, odd=NULLABLE)
///   - Bytes 1-2: parameter spec (length for most types, precision/scale for DECIMAL)
pub fn build_qrydsc(columns: &[ColumnDesc]) -> DdmObject {
    // Build the FD:OCA descriptor block
    let mut fdodsc = BytesMut::new();

    // Length byte = total bytes including itself: 1 + 2 (header) + 3*columns
    // pydrda reads: ln = obj[0]; b = obj[1:ln]; then splits b[2:] into 3-byte triplets
    // The trailer [06 71 E4 D0 00 01] goes AFTER the ln-counted region
    let data_len = 2 + (3 * columns.len()); // header + column descriptors
    let total_len = 1 + data_len;           // +1 for length byte itself
    fdodsc.put_u8(total_len as u8);

    // GDA header — repeating row group marker
    fdodsc.put_u8(0x76);
    fdodsc.put_u8(0xD0);

    // Per-column 3-byte descriptors
    for col in columns {
        let type_code = if col.nullable {
            col.data_type.nullable_type_code()
        } else {
            col.data_type.type_code()
        };
        fdodsc.put_u8(type_code);

        match col.data_type {
            FdocaType::Decimal => {
                // DECIMAL: PS = (precision, scale)
                fdodsc.put_u8(col.precision as u8);
                fdodsc.put_u8(col.scale as u8);
            }
            FdocaType::Varchar => {
                // VARCHAR: PS = 0xFFFF (length is in-stream with 2-byte prefix)
                fdodsc.put_u8(0xFF);
                fdodsc.put_u8(0xFF);
            }
            _ => {
                // Fixed-size types: PS = big-endian length (pydrda hardcodes 'big')
                fdodsc.put_u16(col.length);
            }
        }
    }

    // Trailer triplet — end of repeating group (outside ln-counted region)
    fdodsc.put_u8(0x06);
    fdodsc.put_u8(0x71);
    fdodsc.put_u8(0xE4);
    fdodsc.put_u8(0xD0);
    fdodsc.put_u8(0x00);
    fdodsc.put_u8(0x01);

    DdmObject::new(QRYDSC, fdodsc.to_vec())
}

// ── QRYDTA (Query Answer Set Data) ──────────────────────

/// Build a QRYDTA containing row data.
///
/// Wire format (matching pydrda/ibm_db expectations):
/// ```text
/// Per row: [0xFF] [0x00]  (2-byte row indicator — data row present)
///          For each column:
///            - Nullable types: [0xFF] = NULL, else [0x00] + data
///            - VARCHAR: [2-byte length] + [data]
///            - CHAR: [data] (fixed length from QRYDSC)
///            - INTEGER: [4 bytes little-endian]
///            - SMALLINT: [2 bytes little-endian]
///            - BIGINT: [8 bytes little-endian]
///            - FLOAT8: [8 bytes little-endian IEEE 754]
/// After all rows: embedded SQLCARD with SQLCODE=100 (end of data)
/// ```
pub fn build_qrydta(rows: &[Vec<ColumnValue>], columns: &[ColumnDesc]) -> DdmObject {
    let mut payload = BytesMut::new();

    for row in rows {
        // 2-byte row indicator: 0xFF 0x00 = data row follows
        payload.put_u8(0xFF);
        payload.put_u8(0x00);

        for (i, col_val) in row.iter().enumerate() {
            let nullable = columns.get(i).map_or(false, |c| c.nullable);
            match col_val {
                ColumnValue::Null => {
                    // Null indicator for nullable columns
                    payload.put_u8(0xFF);
                }
                ColumnValue::Varchar(s) => {
                    if nullable {
                        payload.put_u8(0x00); // not null indicator
                    }
                    let bytes = s.as_bytes();
                    payload.put_u16(bytes.len() as u16); // big-endian (pydrda hardcodes 'big')
                    payload.extend_from_slice(bytes);
                }
                ColumnValue::FixedChar(s, len) => {
                    if nullable {
                        payload.put_u8(0x00);
                    }
                    let mut padded = s.clone();
                    while padded.len() < *len as usize {
                        padded.push(' ');
                    }
                    let bytes = &padded.as_bytes()[..*len as usize];
                    payload.extend_from_slice(bytes);
                }
                ColumnValue::Integer(v) => {
                    if nullable {
                        payload.put_u8(0x00);
                    }
                    payload.put_i32_le(*v);
                }
                ColumnValue::BigInt(v) => {
                    if nullable {
                        payload.put_u8(0x00);
                    }
                    payload.put_i64_le(*v);
                }
                ColumnValue::SmallInt(v) => {
                    if nullable {
                        payload.put_u8(0x00);
                    }
                    payload.put_i16_le(*v);
                }
                ColumnValue::Float(v) => {
                    if nullable {
                        payload.put_u8(0x00);
                    }
                    payload.put_f64_le(*v);
                }
            }
        }
    }

    DdmObject::new(QRYDTA, payload.to_vec())
}

/// A typed column value for query result encoding.
#[derive(Debug, Clone)]
pub enum ColumnValue {
    Null,
    Varchar(String),
    FixedChar(String, u16),
    SmallInt(i16),
    Integer(i32),
    BigInt(i64),
    Float(f64),
}

// ── Reply Messages ───────────────────────────────────────

/// Build an EXCSATRD (Exchange Server Attributes Reply Data).
///
/// All string parameters are encoded in EBCDIC cp500 as required by the
/// DRDA protocol during the handshake phase (before encoding negotiation).
///
/// Parameter order and content matches Apache Derby for ibm_db compatibility:
/// EXTNAM, MGRLVLLS, SRVCLSNM, SRVNAM, SRVRLSLV.
/// Note: PRDID is NOT included in EXCSATRD (it goes in ACCRDBRM).
pub fn build_excsatrd(server_name: &str, _product_id: &str) -> DdmObject {
    DdmBuilder::new(EXCSATRD)
        .add_ebcdic_param(EXTNAM, "OpenMainframe DRDA Server")
        .add_param(MGRLVLLS, &build_mgrlvlls())
        .add_ebcdic_param(SRVCLSNM, "QDB2/LINUX")
        .add_ebcdic_param(SRVNAM, server_name)
        .add_ebcdic_param(SRVRLSLV, "CSS10160/10.16.1.1")
        .build()
}

/// Build manager level list for EXCSATRD.
///
/// Levels match Apache Derby (conservative, widely compatible):
/// AGENT=7, SQLAM=7, CMNTCPIP=5, RDB=7, SECMGR=7, UNICODEMGR=1208
fn build_mgrlvlls() -> Vec<u8> {
    let mut buf = BytesMut::new();
    // Manager code point (2 bytes) + level (2 bytes), repeated
    buf.put_u16(AGENT);
    buf.put_u16(7);
    buf.put_u16(SQLAM);
    buf.put_u16(7);
    buf.put_u16(CMNTCPIP);
    buf.put_u16(5);
    buf.put_u16(RDB);
    buf.put_u16(7);
    buf.put_u16(SECMGR);
    buf.put_u16(7);
    buf.put_u16(UNICODEMGR);
    buf.put_u16(1208);
    buf.to_vec()
}

/// Build an ACCSECRD (Access Security Reply Data).
pub fn build_accsecrd(secmec: u16) -> DdmObject {
    DdmBuilder::new(ACCSECRD)
        .add_u16_param(SECMEC, secmec)
        .build()
}

/// Build an ACCSECRD with a security token (for EUSRIDPWD).
pub fn build_accsecrd_with_token(secmec: u16, token: &[u8]) -> DdmObject {
    DdmBuilder::new(ACCSECRD)
        .add_u16_param(SECMEC, secmec)
        .add_param(SECTKN, token)
        .build()
}

/// Build a SECCHKRM (Security Check Reply Message).
/// secchkcd: 0x00 = success, 0x0E = invalid credentials.
pub fn build_secchkrm(secchkcd: u8) -> DdmObject {
    let svrcod = if secchkcd == 0x00 {
        SVRCOD_INFO
    } else {
        SVRCOD_ERROR
    };
    DdmBuilder::new(SECCHKRM)
        .add_u16_param(SVRCOD, svrcod)
        .add_param(SECCHKCD, &[secchkcd])
        .build()
}

/// Build an ACCRDBRM (Access RDB Reply Message).
///
/// Matches Apache Derby's ACCRDBRM format:
/// - SVRCOD, PRDID, TYPDEFNAM, TYPDEFOVR
/// - TYPDEFNAM is always QTDSQLASC (ASCII), matching Derby behavior
///   (Derby uses QTDSQLASC regardless of what the client sent)
/// - No RDBNAM (Derby doesn't include it)
///
/// After ACCRDBRM, the server should send PBSD (piggy-backed session data)
/// via `build_pbsd()`, NOT SQLCARD.
pub fn build_accrdbrm(_rdbnam: &str) -> DdmObject {
    DdmBuilder::new(ACCRDBRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .add_string_param(PRDID, "CSS10160")
        .add_string_param(TYPDEFNAM, "QTDSQLX86")
        .add_param(TYPDEFOVR, &build_typdefovr())
        .build()
}

/// Build PBSD (Piggy-Backed Session Data) — sent after ACCRDBRM.
///
/// Contains the initial session state: isolation level and schema name.
/// This matches Apache Derby's behavior (introduced in Derby 10.7).
pub fn build_pbsd(schema: &str) -> DdmObject {
    let mut builder = DdmBuilder::new(PBSD);
    // PBSD_ISO: isolation level (1 byte)
    // 0x02 = READ COMMITTED (CS, cursor stability — DB2 default)
    builder = builder.add_param(PBSD_ISO, &[0x02]);
    // PBSD_SCHEMA: schema name
    builder = builder.add_string_param(PBSD_SCHEMA, schema);
    builder.build()
}

/// Build TYPDEFOVR for ACCRDBRM — type definition overrides.
///
/// Matches Apache Derby: only CCSIDSBC and CCSIDMBC (no CCSIDDBC).
fn build_typdefovr() -> Vec<u8> {
    let mut buf = BytesMut::new();
    // CCSID single-byte: parameter 0x119C = 1208 (UTF-8)
    buf.put_u16(0x0006); // length
    buf.put_u16(0x119C); // CCSIDSBC
    buf.put_u16(1208);   // UTF-8
    // CCSID double-byte: parameter 0x119D = 1200 (UTF-16)
    buf.put_u16(0x0006);
    buf.put_u16(0x119D); // CCSIDDBC
    buf.put_u16(1200);   // UTF-16
    // CCSID mixed: parameter 0x119E = 1208
    buf.put_u16(0x0006);
    buf.put_u16(0x119E); // CCSIDMBC
    buf.put_u16(1208);
    buf.to_vec()
}

/// Build an ENDQRYRM (End of Query Reply Message).
///
/// Matches Derby: only SVRCOD (no RDBNAM).
pub fn build_endqryrm() -> DdmObject {
    DdmBuilder::new(ENDQRYRM)
        .add_u16_param(SVRCOD, SVRCOD_WARNING)
        .build()
}

/// Build an ENDUOWRM (End Unit of Work Reply Message) — for COMMIT/ROLLBACK.
pub fn build_enduowrm() -> DdmObject {
    DdmBuilder::new(ENDUOWRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .build()
}

/// Build an OPNQRYRM (Open Query Complete Reply Message).
///
/// Matches Derby/DB2 OPNQRYRM format:
///   SVRCOD=INFO, QRYPRCTYP=LMTBLKPRC, SQLCSRHLD=no,
///   QRYATTSCR=no, QRYATTSNS=insensitive, QRYATTUPD=read-only,
///   QRYINSID (8-byte instance), QRYCLSIMP=yes.
pub fn build_opnqryrm() -> DdmObject {
    DdmBuilder::new(OPNQRYRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .add_u16_param(QRYPRCTYP, 0x2417)        // LMTBLKPRC (limited block protocol)
        .add_param(SQLCSRHLD, &[0xF0])            // don't hold cursor (EBCDIC '0')
        .add_param(QRYATTSCR, &[0xF0])            // not scrollable (EBCDIC '0')
        .add_param(QRYATTSNS, &[0xF1])            // insensitive (EBCDIC '1')
        .add_param(QRYATTUPD, &[0xF0])            // read-only (EBCDIC '0')
        .add_param(QRYINSID, &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]) // query instance ID
        .add_param(QRYCLSIMP, &[0xF1])            // implicit close = yes (EBCDIC '1')
        .build()
}

/// Build a CMDCHKRM (Command Check — syntax error).
pub fn build_cmdchkrm(_message: &str) -> DdmObject {
    DdmBuilder::new(CMDCHKRM)
        .add_u16_param(SVRCOD, SVRCOD_ERROR)
        .build()
}

/// Build a RDBNACRM (RDB Not Accessed — database not found).
pub fn build_rdbnacrm(rdbnam: &str) -> DdmObject {
    DdmBuilder::new(RDBNACRM)
        .add_u16_param(SVRCOD, SVRCOD_ERROR)
        .add_string_param(RDBNAM, rdbnam)
        .build()
}

// ── DSS Segment Helpers ──────────────────────────────────

/// Wrap a DDM object in a reply DSS segment.
pub fn reply_dss(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_reply(correlation_id, obj.serialize())
}

/// Wrap a DDM object in a chained reply DSS segment.
pub fn reply_dss_chained(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_reply_chained(correlation_id, obj.serialize())
}

/// Wrap a DDM object in a chained reply DSS with same-correlator bit.
pub fn reply_dss_chained_same_corr(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_reply_chained_same_corr(correlation_id, obj.serialize())
}

/// Wrap a DDM object in an object DSS segment (for QRYDTA).
pub fn object_dss(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_object(correlation_id, obj.serialize())
}

/// Wrap a DDM object in a chained object DSS with same-correlator bit.
pub fn object_dss_chained_same_corr(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_object_chained_same_corr(correlation_id, obj.serialize())
}

/// Build multiple DDM objects into a single DSS reply payload.
pub fn multi_reply_dss(correlation_id: u16, objects: &[DdmObject]) -> DssSegment {
    let mut payload = BytesMut::new();
    for obj in objects {
        payload.extend_from_slice(&obj.serialize());
    }
    DssSegment::new_reply(correlation_id, payload)
}
