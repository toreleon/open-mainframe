//! DRDA code point constants.
//!
//! These are the DDM code points used in the DRDA protocol for DB2 connectivity.
//! References: DRDA v5 specification, Apache Derby source, IBM DRDA documentation.

// ── DSS types ────────────────────────────────────────────
/// DSS magic byte — all DSS headers contain this at offset 2.
pub const DSS_MAGIC: u8 = 0xD0;

/// DSS type: Request.
pub const DSS_TYPE_REQUEST: u8 = 0x01;
/// DSS type: Reply.
pub const DSS_TYPE_REPLY: u8 = 0x02;
/// DSS type: Object (data).
pub const DSS_TYPE_OBJECT: u8 = 0x03;
/// DSS type: Communication (server info).
pub const DSS_TYPE_COMMUNICATION: u8 = 0x04;

/// DSS format bit: chained with same correlation ID.
pub const DSS_CHAIN_BIT: u8 = 0x40;
/// DSS format bit: continuation of previous DSS.
pub const DSS_CONTINUE_BIT: u8 = 0x20;

// ── Connection management ────────────────────────────────
/// Exchange Server Attributes (client→server).
pub const EXCSAT: u16 = 0x1041;
/// Exchange Server Attributes Reply Data (server→client).
pub const EXCSATRD: u16 = 0x1443;
/// Access Security (client→server).
pub const ACCSEC: u16 = 0x106D;
/// Access Security Reply Data (server→client).
pub const ACCSECRD: u16 = 0x14AC;
/// Security Check (client→server).
pub const SECCHK: u16 = 0x106E;
/// Security Check Reply Message (server→client).
pub const SECCHKRM: u16 = 0x1219;
/// Access RDB (client→server).
pub const ACCRDB: u16 = 0x2001;
/// Access RDB Reply Message (server→client).
pub const ACCRDBRM: u16 = 0x2201;

// ── SQL execution ────────────────────────────────────────
/// Execute Immediate SQL (client→server).
pub const EXCSQLIMM: u16 = 0x200A;
/// Open Query (client→server).
pub const OPNQRY: u16 = 0x200C;
/// Continue Query (client→server — fetch more rows).
pub const CNTQRY: u16 = 0x2005;
/// Close Query (client→server).
pub const CLSQRY: u16 = 0x2004;
/// Prepare SQL Statement (client→server).
pub const PRPSQLSTT: u16 = 0x200D;
/// Execute SQL Statement (client→server — execute prepared).
pub const EXCSQLSTT: u16 = 0x200B;
/// Describe SQL Statement (client→server).
pub const DSCSQLSTT: u16 = 0x2008;

// ── Transaction control ──────────────────────────────────
/// RDB Commit Unit of Work (client→server).
pub const RDBCMM: u16 = 0x200E;
/// RDB Rollback Unit of Work (client→server).
pub const RDBRLLBCK: u16 = 0x200F;

// ── Reply messages ───────────────────────────────────────
/// SQL Card (SQLCA reply data — server→client).
pub const SQLCARD: u16 = 0x2408;
/// SQL Descriptor Area Reply Data (server→client).
pub const SQLDARD: u16 = 0x2411;
/// Query Answer Set Description (server→client).
pub const QRYDSC: u16 = 0x241A;
/// Query Answer Set Data (server→client).
pub const QRYDTA: u16 = 0x241B;
/// End of Query Reply Message (server→client).
pub const ENDQRYRM: u16 = 0x220B;
/// End Unit of Work Reply Message (server→client).
pub const ENDUOWRM: u16 = 0x220C;
/// SQL Statement Reply Message (server→client — for EXCSQLIMM).
pub const SQLSTTVRM: u16 = 0x2213;
/// Command Check Reply Message (server→client — syntax error).
pub const CMDCHKRM: u16 = 0x1254;
/// RDB Not Found Reply Message.
pub const RDBNACRM: u16 = 0x2204;
/// RDB Access Failed Reply Message.
pub const RDBAFLRM: u16 = 0x221A;
/// Command Not Supported Reply Message.
pub const CMDNSPRM: u16 = 0x1250;
/// Object Not Supported Reply Message.
pub const OBJNSPRM: u16 = 0x1253;
/// Open Query Failure Reply Message.
pub const OPNQFLRM: u16 = 0x2212;
/// RDB Currently Accessed Reply Message.
pub const RDBACCRM: u16 = 0x2207;
/// Open Query Complete Reply Message.
pub const OPNQRYRM: u16 = 0x2205;

// ── DDM parameters ───────────────────────────────────────
/// External Name (server product name).
pub const EXTNAM: u16 = 0x115E;
/// Manager-Level List.
pub const MGRLVLLS: u16 = 0x1404;
/// Server Class Name.
pub const SRVCLSNM: u16 = 0x1147;
/// Server Name.
pub const SRVNAM: u16 = 0x116D;
/// Server Product Release Level.
pub const SRVRLSLV: u16 = 0x115A;
/// Security Mechanism.
pub const SECMEC: u16 = 0x11A2;
/// Security Check Code (result of SECCHK).
pub const SECCHKCD: u16 = 0x11A4;
/// RDB Name (database name).
pub const RDBNAM: u16 = 0x2110;
/// User ID.
pub const USRID: u16 = 0x11A0;
/// Password.
pub const PASSWORD: u16 = 0x11A1;
/// SQL Statement (text).
pub const SQLSTT: u16 = 0x2414;
/// Package Name and Consistency Token.
pub const PKGNAMCSN: u16 = 0x2113;
/// RDB Collection Identifier.
pub const RDBCOLID: u16 = 0x2108;
/// Package Identifier.
pub const PKGID: u16 = 0x2109;
/// Package Section Number.
pub const PKGSN: u16 = 0x210C;
/// Product-specific identifier (correlator).
pub const PRDID: u16 = 0x112E;
/// Code Point for Type Definition.
pub const TYPDEFNAM: u16 = 0x002F;
/// Type Definition Overrides.
pub const TYPDEFOVR: u16 = 0x0035;
/// Relational Database (RDB) — access RDB parameter.
pub const RDBACCCL: u16 = 0x210F;
/// CRRTKN — Correlation Token.
pub const CRRTKN: u16 = 0x2135;
/// SQL Attributes.
pub const SQLATTR: u16 = 0x2450;
/// Query Protocol Type.
pub const QRYPRCTYP: u16 = 0x2102;
/// Query Block Size.
pub const MAXBLKEXT: u16 = 0x2141;
/// Maximum result set count.
pub const MAXRSLCNT: u16 = 0x2140;
/// Query Instance Identifier.
pub const QRYINSID: u16 = 0x215B;
/// RDB Allow Updates.
pub const RDBALWUPD: u16 = 0x211A;
/// Severity Code.
pub const SVRCOD: u16 = 0x1149;

// ── Manager code points ──────────────────────────────────
/// Agent manager.
pub const AGENT: u16 = 0x1403;
/// SQL Application Manager.
pub const SQLAM: u16 = 0x2407;
/// RDB manager.
pub const RDB: u16 = 0x240F;
/// Security Manager.
pub const SECMGR: u16 = 0x1440;
/// Conversation/Communication Manager.
pub const CMNTCPIP: u16 = 0x1474;
/// Supervisor manager.
pub const SUPERVISOR: u16 = 0x143C;
/// UNICODE Manager.
pub const UNICODEMGR: u16 = 0x1C08;
/// XML support manager.
pub const XMLMGR: u16 = 0x1C00;

// ── Security mechanisms ──────────────────────────────────
/// User ID and password security (SECMEC value).
pub const SECMEC_USRIDPWD: u16 = 0x0003;
/// User ID only security.
pub const SECMEC_USRONLY: u16 = 0x0004;
/// Encrypted user ID and password (Kerberos).
pub const SECMEC_EUSRIDPWD: u16 = 0x0009;

// ── FD:OCA data type codes (for QRYDSC/QRYDTA) ─────────
/// CHAR / CHAR FOR BIT DATA.
pub const FDOCA_TYPE_FIXEDCHAR: u8 = 0x01;
/// VARCHAR.
pub const FDOCA_TYPE_VARCHAR: u8 = 0x30;
/// SMALLINT (2-byte integer).
pub const FDOCA_TYPE_SMALLINT: u8 = 0x05;
/// INTEGER (4-byte integer).
pub const FDOCA_TYPE_INTEGER: u8 = 0x03;
/// BIGINT (8-byte integer).
pub const FDOCA_TYPE_BIGINT: u8 = 0x17;
/// DECIMAL (packed decimal).
pub const FDOCA_TYPE_DECIMAL: u8 = 0x0F;
/// FLOAT (double).
pub const FDOCA_TYPE_FLOAT: u8 = 0x0B;
/// DATE.
pub const FDOCA_TYPE_DATE: u8 = 0x21;
/// TIME.
pub const FDOCA_TYPE_TIME: u8 = 0x23;
/// TIMESTAMP.
pub const FDOCA_TYPE_TIMESTAMP: u8 = 0x25;

// ── Severity codes ───────────────────────────────────────
pub const SVRCOD_INFO: u16 = 0x0000;
pub const SVRCOD_WARNING: u16 = 0x0004;
pub const SVRCOD_ERROR: u16 = 0x0008;
pub const SVRCOD_SEVERE: u16 = 0x0010;
pub const SVRCOD_ACCDMG: u16 = 0x0014;
pub const SVRCOD_PRMDMG: u16 = 0x0018;
