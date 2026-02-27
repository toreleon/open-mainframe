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
    /// Get the FD:OCA type code byte.
    pub fn type_code(self) -> u8 {
        match self {
            FdocaType::Varchar => FDOCA_TYPE_VARCHAR,
            FdocaType::FixedChar => FDOCA_TYPE_FIXEDCHAR,
            FdocaType::SmallInt => FDOCA_TYPE_SMALLINT,
            FdocaType::Integer => FDOCA_TYPE_INTEGER,
            FdocaType::BigInt => FDOCA_TYPE_BIGINT,
            FdocaType::Decimal => FDOCA_TYPE_DECIMAL,
            FdocaType::Float => FDOCA_TYPE_FLOAT,
            FdocaType::Date => FDOCA_TYPE_DATE,
            FdocaType::Time => FDOCA_TYPE_TIME,
            FdocaType::Timestamp => FDOCA_TYPE_TIMESTAMP,
        }
    }
}

// ── SQLCARD (SQLCA Reply Data) ───────────────────────────

/// Build an SQLCARD DDM object representing a successful operation.
pub fn build_sqlcard_success(rows_affected: i32) -> DdmObject {
    build_sqlcard(0, "00000", rows_affected, "")
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
/// The SQLCARD contains an embedded SQLCA structure:
/// - SQLCAGRP (null indicator + SQLCODE + SQLSTATE + SQLERRD + etc.)
fn build_sqlcard(sqlcode: i32, sqlstate: &str, rows_affected: i32, message: &str) -> DdmObject {
    let mut payload = BytesMut::new();

    // SQLCA null indicator: 0x00 = not null (SQLCA present)
    payload.put_u8(0x00);

    // SQLCODE (4 bytes, big-endian signed)
    payload.put_i32(sqlcode);

    // SQLSTATE (5 bytes, padded)
    let mut state_bytes = [0x20u8; 5]; // space-padded
    let state = sqlstate.as_bytes();
    let copy_len = state.len().min(5);
    state_bytes[..copy_len].copy_from_slice(&state[..copy_len]);
    payload.extend_from_slice(&state_bytes);

    // SQLERRD (6 x 4 bytes = 24 bytes)
    // SQLERRD[0]: not used
    payload.put_i32(0);
    // SQLERRD[1]: not used
    payload.put_i32(0);
    // SQLERRD[2]: rows affected
    payload.put_i32(rows_affected);
    // SQLERRD[3..5]: not used
    payload.put_i32(0);
    payload.put_i32(0);
    payload.put_i32(0);

    // SQLWARN (11 bytes: SQLWARN0..SQLWARNA)
    payload.extend_from_slice(&[0x20u8; 11]); // all spaces = no warnings

    // SQLRDBNAME length (2 bytes) + value — empty
    payload.put_u16(0);

    // SQLERRMSG_M length (2 bytes) + message
    let msg_bytes = message.as_bytes();
    payload.put_u16(msg_bytes.len() as u16);
    payload.extend_from_slice(msg_bytes);

    DdmObject::new(SQLCARD, payload.to_vec())
}

// ── SQLDARD (SQL Descriptor Area Reply Data) ─────────────

/// Build a SQLDARD describing the columns of a query result.
pub fn build_sqldard(columns: &[ColumnDesc]) -> DdmObject {
    let mut payload = BytesMut::new();

    // SQLDAGRP null indicator: 0x00 = not null
    payload.put_u8(0x00);

    // SQLD (number of columns, 2 bytes)
    payload.put_u16(columns.len() as u16);

    // For each column, write SQLVAR entries
    for col in columns {
        // SQLPRECISION (2 bytes)
        payload.put_u16(col.precision);
        // SQLSCALE (2 bytes)
        payload.put_u16(col.scale);
        // SQLLENGTH (8 bytes — long form)
        payload.put_u64(col.length as u64);
        // SQLTYPE (2 bytes)
        let type_code = drda_sql_type(col.data_type, col.nullable);
        payload.put_u16(type_code);
        // SQLCCSID (2 bytes — 1208 = UTF-8)
        payload.put_u16(1208);
        // SQLNAME_M length (2 bytes) + name
        let name_bytes = col.name.as_bytes();
        payload.put_u16(name_bytes.len() as u16);
        payload.extend_from_slice(name_bytes);
        // SQLLABEL_M length (2 bytes) + label (same as name)
        payload.put_u16(name_bytes.len() as u16);
        payload.extend_from_slice(name_bytes);
        // SQLCOMMENTS_M length (2 bytes) + empty
        payload.put_u16(0);
    }

    DdmObject::new(SQLDARD, payload.to_vec())
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
pub fn build_qrydsc(columns: &[ColumnDesc]) -> DdmObject {
    let mut payload = BytesMut::new();

    // For each column, emit a triplet descriptor
    for col in columns {
        // Triplet header: length(1) + type(1) + id(1)
        // Type 0x00 = FD:OCA Row Description
        let type_code = col.data_type.type_code();
        let nullable_flag: u8 = if col.nullable { 0x01 } else { 0x00 };

        // Simple triplet: type byte + length (2 bytes) + nullable flag
        payload.put_u8(type_code);
        if matches!(col.data_type, FdocaType::Varchar | FdocaType::FixedChar) {
            payload.put_u16(col.length);
        } else {
            payload.put_u16(col.length);
        }
        payload.put_u8(nullable_flag);
    }

    DdmObject::new(QRYDSC, payload.to_vec())
}

// ── QRYDTA (Query Answer Set Data) ──────────────────────

/// Build a QRYDTA containing row data.
///
/// Each row is: [null_indicator(1) + column_data]... for each column.
/// For VARCHAR: 2-byte length prefix + data.
/// For INTEGER: 4 bytes big-endian.
/// For other types: raw bytes.
pub fn build_qrydta(rows: &[Vec<ColumnValue>]) -> DdmObject {
    let mut payload = BytesMut::new();

    for row in rows {
        for col_val in row {
            match col_val {
                ColumnValue::Null => {
                    payload.put_u8(0xFF); // null indicator
                }
                ColumnValue::Varchar(s) => {
                    payload.put_u8(0x00); // not null
                    let bytes = s.as_bytes();
                    payload.put_u16(bytes.len() as u16);
                    payload.extend_from_slice(bytes);
                }
                ColumnValue::FixedChar(s, len) => {
                    payload.put_u8(0x00);
                    let mut padded = s.clone();
                    while padded.len() < *len as usize {
                        padded.push(' ');
                    }
                    let bytes = &padded.as_bytes()[..*len as usize];
                    payload.extend_from_slice(bytes);
                }
                ColumnValue::Integer(v) => {
                    payload.put_u8(0x00);
                    payload.put_i32(*v);
                }
                ColumnValue::BigInt(v) => {
                    payload.put_u8(0x00);
                    payload.put_i64(*v);
                }
                ColumnValue::SmallInt(v) => {
                    payload.put_u8(0x00);
                    payload.put_i16(*v);
                }
                ColumnValue::Float(v) => {
                    payload.put_u8(0x00);
                    payload.put_f64(*v);
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
pub fn build_excsatrd(server_name: &str, product_id: &str) -> DdmObject {
    DdmBuilder::new(EXCSATRD)
        .add_string_param(EXTNAM, "OpenMainframe DRDA Server")
        .add_string_param(SRVNAM, server_name)
        .add_string_param(SRVCLSNM, "QDB2/LINUX")
        .add_string_param(SRVRLSLV, "SQL110500")
        .add_string_param(PRDID, product_id)
        .add_param(MGRLVLLS, &build_mgrlvlls())
        .build()
}

/// Build manager level list for EXCSATRD.
fn build_mgrlvlls() -> Vec<u8> {
    let mut buf = BytesMut::new();
    // Manager code point (2 bytes) + level (2 bytes), repeated
    // AGENT level 7
    buf.put_u16(AGENT);
    buf.put_u16(0x0007);
    // SQLAM level 7
    buf.put_u16(SQLAM);
    buf.put_u16(0x0007);
    // RDB level 7
    buf.put_u16(RDB);
    buf.put_u16(0x0007);
    // SECMGR level 7
    buf.put_u16(SECMGR);
    buf.put_u16(0x0007);
    // UNICODEMGR level 0
    buf.put_u16(UNICODEMGR);
    buf.put_u16(0x0000);
    buf.to_vec()
}

/// Build an ACCSECRD (Access Security Reply Data).
pub fn build_accsecrd(secmec: u16) -> DdmObject {
    DdmBuilder::new(ACCSECRD)
        .add_u16_param(SECMEC, secmec)
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
pub fn build_accrdbrm(rdbnam: &str) -> DdmObject {
    DdmBuilder::new(ACCRDBRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .add_string_param(PRDID, "DSN11015")
        .add_string_param(TYPDEFNAM, "QTDSQLASC")
        .add_param(TYPDEFOVR, &build_typdefovr())
        .add_string_param(RDBNAM, rdbnam)
        .build()
}

/// Build TYPDEFOVR for ACCRDBRM — type definition overrides.
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
pub fn build_endqryrm() -> DdmObject {
    DdmBuilder::new(ENDQRYRM)
        .add_u16_param(SVRCOD, SVRCOD_WARNING)
        .add_param(RDBNAM, &[])
        .build()
}

/// Build an ENDUOWRM (End Unit of Work Reply Message) — for COMMIT/ROLLBACK.
pub fn build_enduowrm() -> DdmObject {
    DdmBuilder::new(ENDUOWRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .build()
}

/// Build an OPNQRYRM (Open Query Complete Reply Message).
pub fn build_opnqryrm() -> DdmObject {
    DdmBuilder::new(OPNQRYRM)
        .add_u16_param(SVRCOD, SVRCOD_INFO)
        .add_u16_param(QRYPRCTYP, 0x0002) // FIXROWPRC
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

/// Wrap a DDM object in an object DSS segment (for QRYDTA).
pub fn object_dss(correlation_id: u16, obj: &DdmObject) -> DssSegment {
    DssSegment::new_object(correlation_id, obj.serialize())
}

/// Build multiple DDM objects into a single DSS reply payload.
pub fn multi_reply_dss(correlation_id: u16, objects: &[DdmObject]) -> DssSegment {
    let mut payload = BytesMut::new();
    for obj in objects {
        payload.extend_from_slice(&obj.serialize());
    }
    DssSegment::new_reply(correlation_id, payload)
}
