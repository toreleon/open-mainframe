//! Types 100-120 — Subsystem Records.
//!
//! Subsystem-specific SMF records for detailed monitoring and accounting:
//! - Types 100/101: DB2 accounting and performance
//! - Type 110: CICS transaction monitoring
//! - Types 115/116: MQ accounting and statistics
//! - Type 119: TCP/IP connection records

use crate::record::{extend_padded, push_u16, push_u32, push_u64, SmfRecord};

// ---------------------------------------------------------------------------
//  Subsystem type
// ---------------------------------------------------------------------------

/// Subsystem type identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SubsystemType {
    /// DB2 database.
    Db2,
    /// CICS transaction server.
    Cics,
    /// MQ message queue.
    Mq,
    /// TCP/IP networking.
    TcpIp,
}

impl SubsystemType {
    /// Display name for the subsystem.
    pub fn name(&self) -> &str {
        match self {
            SubsystemType::Db2 => "DB2",
            SubsystemType::Cics => "CICS",
            SubsystemType::Mq => "MQ",
            SubsystemType::TcpIp => "TCP/IP",
        }
    }
}

// ---------------------------------------------------------------------------
//  Type 100/101 — DB2 Records
// ---------------------------------------------------------------------------

/// Type 101 record — DB2 accounting (per-thread).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Db2AccountingRecord {
    /// DB2 subsystem name (4 chars).
    pub subsystem_name: String,
    /// Plan name (8 chars).
    pub plan_name: String,
    /// Connection type (e.g., "BATCH", "TSO", "CICS").
    pub connection_type: String,
    /// Authorization ID (8 chars).
    pub auth_id: String,
    /// SQL statement count.
    pub sql_count: u32,
    /// Elapsed time (microseconds).
    pub elapsed_time_us: u64,
    /// CPU time (microseconds).
    pub cpu_time_us: u64,
    /// Buffer pool getpages.
    pub getpages: u64,
    /// Buffer pool read I/O.
    pub read_io: u32,
    /// Buffer pool write I/O.
    pub write_io: u32,
    /// Number of commits.
    pub commits: u32,
    /// Number of rollbacks.
    pub rollbacks: u32,
}

impl Default for Db2AccountingRecord {
    fn default() -> Self {
        Self {
            subsystem_name: "DB2A".to_string(),
            plan_name: String::new(),
            connection_type: String::new(),
            auth_id: String::new(),
            sql_count: 0,
            elapsed_time_us: 0,
            cpu_time_us: 0,
            getpages: 0,
            read_io: 0,
            write_io: 0,
            commits: 0,
            rollbacks: 0,
        }
    }
}

impl Db2AccountingRecord {
    /// Convert to a generic SMF record (Type 101).
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.subsystem_name, 4);
        extend_padded(&mut data, &self.plan_name, 8);
        extend_padded(&mut data, &self.connection_type, 8);
        extend_padded(&mut data, &self.auth_id, 8);
        push_u32(&mut data, self.sql_count);
        push_u64(&mut data, self.elapsed_time_us);
        push_u64(&mut data, self.cpu_time_us);
        push_u64(&mut data, self.getpages);
        push_u32(&mut data, self.read_io);
        push_u32(&mut data, self.write_io);
        push_u32(&mut data, self.commits);
        push_u32(&mut data, self.rollbacks);

        SmfRecord::new(101, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 110 — CICS Records
// ---------------------------------------------------------------------------

/// Type 110 record — CICS transaction monitoring.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct CicsTransactionRecord {
    /// CICS region name (8 chars).
    pub region_name: String,
    /// Transaction ID (4 chars).
    pub transaction_id: String,
    /// Program name (8 chars).
    pub program_name: String,
    /// User ID (8 chars).
    pub user_id: String,
    /// Elapsed time (microseconds).
    pub elapsed_time_us: u64,
    /// CPU time (microseconds).
    pub cpu_time_us: u64,
    /// Response time (microseconds).
    pub response_time_us: u64,
    /// Number of file I/O operations.
    pub file_io_count: u32,
    /// Number of temporary storage operations.
    pub ts_io_count: u32,
    /// Abend code (empty if no abend).
    pub abend_code: String,
}

impl CicsTransactionRecord {
    /// Convert to a generic SMF record (Type 110).
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.region_name, 8);
        extend_padded(&mut data, &self.transaction_id, 4);
        extend_padded(&mut data, &self.program_name, 8);
        extend_padded(&mut data, &self.user_id, 8);
        push_u64(&mut data, self.elapsed_time_us);
        push_u64(&mut data, self.cpu_time_us);
        push_u64(&mut data, self.response_time_us);
        push_u32(&mut data, self.file_io_count);
        push_u32(&mut data, self.ts_io_count);
        extend_padded(&mut data, &self.abend_code, 4);

        SmfRecord::new(110, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 115/116 — MQ Records
// ---------------------------------------------------------------------------

/// Type 116 record — MQ statistics.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MqStatisticsRecord {
    /// Queue manager name (4 chars).
    pub queue_manager: String,
    /// Channel name (20 chars).
    pub channel_name: String,
    /// Connection name (8 chars).
    pub connection_name: String,
    /// Messages sent.
    pub messages_sent: u64,
    /// Messages received.
    pub messages_received: u64,
    /// Bytes transferred.
    pub bytes_transferred: u64,
    /// Channel status (e.g., "RUNNING", "STOPPED").
    pub channel_status: String,
    /// Elapsed time (microseconds).
    pub elapsed_time_us: u64,
}

impl Default for MqStatisticsRecord {
    fn default() -> Self {
        Self {
            queue_manager: String::new(),
            channel_name: String::new(),
            connection_name: String::new(),
            messages_sent: 0,
            messages_received: 0,
            bytes_transferred: 0,
            channel_status: "RUNNING".to_string(),
            elapsed_time_us: 0,
        }
    }
}

impl MqStatisticsRecord {
    /// Convert to a generic SMF record (Type 116).
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.queue_manager, 4);
        extend_padded(&mut data, &self.channel_name, 20);
        extend_padded(&mut data, &self.connection_name, 8);
        push_u64(&mut data, self.messages_sent);
        push_u64(&mut data, self.messages_received);
        push_u64(&mut data, self.bytes_transferred);
        extend_padded(&mut data, &self.channel_status, 8);
        push_u64(&mut data, self.elapsed_time_us);

        SmfRecord::new(116, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 119 — TCP/IP Records
// ---------------------------------------------------------------------------

/// Type 119 record — TCP/IP connection.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TcpIpConnectionRecord {
    /// Source IP address (as string).
    pub source_ip: String,
    /// Destination IP address (as string).
    pub dest_ip: String,
    /// Source port.
    pub source_port: u16,
    /// Destination port.
    pub dest_port: u16,
    /// Bytes sent.
    pub bytes_sent: u64,
    /// Bytes received.
    pub bytes_received: u64,
    /// Connection duration (microseconds).
    pub duration_us: u64,
    /// Protocol (e.g., "TCP", "UDP").
    pub protocol: String,
    /// Stack name (8 chars).
    pub stack_name: String,
}

impl Default for TcpIpConnectionRecord {
    fn default() -> Self {
        Self {
            source_ip: String::new(),
            dest_ip: String::new(),
            source_port: 0,
            dest_port: 0,
            bytes_sent: 0,
            bytes_received: 0,
            duration_us: 0,
            protocol: "TCP".to_string(),
            stack_name: String::new(),
        }
    }
}

impl TcpIpConnectionRecord {
    /// Convert to a generic SMF record (Type 119).
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.source_ip, 16);
        extend_padded(&mut data, &self.dest_ip, 16);
        push_u16(&mut data, self.source_port);
        push_u16(&mut data, self.dest_port);
        push_u64(&mut data, self.bytes_sent);
        push_u64(&mut data, self.bytes_received);
        push_u64(&mut data, self.duration_us);
        extend_padded(&mut data, &self.protocol, 4);
        extend_padded(&mut data, &self.stack_name, 8);

        SmfRecord::new(119, data)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- DB2 ---

    #[test]
    fn test_db2_accounting_record() {
        let rec = Db2AccountingRecord {
            subsystem_name: "DB2A".to_string(),
            plan_name: "MYPLAN".to_string(),
            connection_type: "BATCH".to_string(),
            auth_id: "USER01".to_string(),
            sql_count: 150,
            elapsed_time_us: 5_000_000,
            cpu_time_us: 1_000_000,
            getpages: 50000,
            read_io: 200,
            write_io: 50,
            commits: 10,
            rollbacks: 1,
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 101);
        assert!(!smf.data.is_empty());
    }

    #[test]
    fn test_db2_default() {
        let rec = Db2AccountingRecord::default();
        assert_eq!(rec.subsystem_name, "DB2A");
        assert_eq!(rec.sql_count, 0);
    }

    #[test]
    fn test_db2_subsystem_in_data() {
        let rec = Db2AccountingRecord {
            subsystem_name: "DB2B".to_string(),
            ..Default::default()
        };
        let smf = rec.to_record();
        let ssn = String::from_utf8_lossy(&smf.data[0..4])
            .trim_end()
            .to_string();
        assert_eq!(ssn, "DB2B");
    }

    // --- CICS ---

    #[test]
    fn test_cics_transaction_record() {
        let rec = CicsTransactionRecord {
            region_name: "CICSRGN1".to_string(),
            transaction_id: "PAY1".to_string(),
            program_name: "PAYPGM".to_string(),
            user_id: "USER01".to_string(),
            elapsed_time_us: 500_000,
            cpu_time_us: 100_000,
            response_time_us: 450_000,
            file_io_count: 25,
            ts_io_count: 5,
            abend_code: String::new(),
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 110);
    }

    #[test]
    fn test_cics_default() {
        let rec = CicsTransactionRecord::default();
        assert_eq!(rec.elapsed_time_us, 0);
        assert_eq!(rec.abend_code, "");
    }

    #[test]
    fn test_cics_with_abend() {
        let rec = CicsTransactionRecord {
            abend_code: "ASRA".to_string(),
            ..Default::default()
        };
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 110);
    }

    // --- MQ ---

    #[test]
    fn test_mq_statistics_record() {
        let rec = MqStatisticsRecord {
            queue_manager: "QM01".to_string(),
            channel_name: "TO.REMOTE.QM".to_string(),
            connection_name: "CONN01".to_string(),
            messages_sent: 10000,
            messages_received: 9500,
            bytes_transferred: 50_000_000,
            channel_status: "RUNNING".to_string(),
            elapsed_time_us: 3_600_000_000,
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 116);
    }

    #[test]
    fn test_mq_default() {
        let rec = MqStatisticsRecord::default();
        assert_eq!(rec.channel_status, "RUNNING");
        assert_eq!(rec.messages_sent, 0);
    }

    // --- TCP/IP ---

    #[test]
    fn test_tcpip_connection_record() {
        let rec = TcpIpConnectionRecord {
            source_ip: "10.0.0.1".to_string(),
            dest_ip: "10.0.0.2".to_string(),
            source_port: 12345,
            dest_port: 80,
            bytes_sent: 1024,
            bytes_received: 4096,
            duration_us: 1_000_000,
            protocol: "TCP".to_string(),
            stack_name: "TCPIP".to_string(),
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 119);
    }

    #[test]
    fn test_tcpip_default() {
        let rec = TcpIpConnectionRecord::default();
        assert_eq!(rec.protocol, "TCP");
        assert_eq!(rec.bytes_sent, 0);
    }

    #[test]
    fn test_tcpip_ip_in_data() {
        let rec = TcpIpConnectionRecord {
            source_ip: "192.168.1.1".to_string(),
            dest_ip: "10.0.0.1".to_string(),
            dest_port: 443,
            ..Default::default()
        };
        let smf = rec.to_record();
        // Source IP at 0..16.
        let src = String::from_utf8_lossy(&smf.data[0..16])
            .trim_end()
            .to_string();
        assert_eq!(src, "192.168.1.1");
        // Dest IP at 16..32.
        let dst = String::from_utf8_lossy(&smf.data[16..32])
            .trim_end()
            .to_string();
        assert_eq!(dst, "10.0.0.1");
    }

    // --- Subsystem type ---

    #[test]
    fn test_subsystem_type_names() {
        assert_eq!(SubsystemType::Db2.name(), "DB2");
        assert_eq!(SubsystemType::Cics.name(), "CICS");
        assert_eq!(SubsystemType::Mq.name(), "MQ");
        assert_eq!(SubsystemType::TcpIp.name(), "TCP/IP");
    }
}
