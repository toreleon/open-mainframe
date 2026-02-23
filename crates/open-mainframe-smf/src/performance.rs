//! Types 70-79 — Performance Records.
//!
//! Captures system performance metrics at configured intervals:
//! - Type 70: CPU activity (utilization, wait time, per-processor)
//! - Type 71: Paging activity (page-in/out, slot usage)
//! - Type 72: Workload activity (WLM service class metrics)
//! - Type 74: Device activity (DASD I/O statistics)

use crate::record::{extend_padded, push_u16, push_u32, push_u64, SmfRecord};

// ---------------------------------------------------------------------------
//  Type 70 — CPU Activity
// ---------------------------------------------------------------------------

/// Type 70 record — CPU activity.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Type70Record {
    /// Total CPU time used (microseconds) across all processors in the interval.
    pub total_cpu_us: u64,
    /// Total wait time (microseconds).
    pub total_wait_us: u64,
    /// Number of processors online.
    pub online_processors: u16,
    /// Per-processor CPU utilization (percentage * 100, i.e. 9500 = 95.00%).
    pub processor_utilization: Vec<u16>,
    /// MVS overhead time (microseconds).
    pub mvs_overhead_us: u64,
    /// Interval duration (microseconds).
    pub interval_us: u64,
}

impl Default for Type70Record {
    fn default() -> Self {
        Self {
            total_cpu_us: 0,
            total_wait_us: 0,
            online_processors: 1,
            processor_utilization: Vec::new(),
            mvs_overhead_us: 0,
            interval_us: 0,
        }
    }
}

impl Type70Record {
    /// Average CPU utilization as a percentage (0.0 - 100.0).
    pub fn average_utilization(&self) -> f64 {
        if self.interval_us == 0 || self.online_processors == 0 {
            return 0.0;
        }
        let total_capacity = self.interval_us * self.online_processors as u64;
        (self.total_cpu_us as f64 / total_capacity as f64) * 100.0
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        push_u64(&mut data, self.total_cpu_us);
        push_u64(&mut data, self.total_wait_us);
        push_u16(&mut data, self.online_processors);
        push_u16(&mut data, self.processor_utilization.len() as u16);
        for &util in &self.processor_utilization {
            push_u16(&mut data, util);
        }
        push_u64(&mut data, self.mvs_overhead_us);
        push_u64(&mut data, self.interval_us);

        SmfRecord::new(70, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 71 — Paging Activity
// ---------------------------------------------------------------------------

/// Type 71 record — paging activity.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Type71Record {
    /// Page-in rate (pages per second * 100).
    pub page_in_rate: u32,
    /// Page-out rate (pages per second * 100).
    pub page_out_rate: u32,
    /// Total page-ins in the interval.
    pub total_page_ins: u64,
    /// Total page-outs in the interval.
    pub total_page_outs: u64,
    /// Auxiliary storage slot usage (percentage * 100).
    pub aux_slot_usage: u16,
    /// Total auxiliary storage slots.
    pub total_aux_slots: u32,
    /// Used auxiliary storage slots.
    pub used_aux_slots: u32,
    /// Interval duration (microseconds).
    pub interval_us: u64,
}

impl Type71Record {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        push_u32(&mut data, self.page_in_rate);
        push_u32(&mut data, self.page_out_rate);
        push_u64(&mut data, self.total_page_ins);
        push_u64(&mut data, self.total_page_outs);
        push_u16(&mut data, self.aux_slot_usage);
        // 2 bytes padding for alignment.
        push_u16(&mut data, 0);
        push_u32(&mut data, self.total_aux_slots);
        push_u32(&mut data, self.used_aux_slots);
        push_u64(&mut data, self.interval_us);

        SmfRecord::new(71, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 72 — Workload Activity
// ---------------------------------------------------------------------------

/// Service class performance data within a Type 72 record.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ServiceClassMetrics {
    /// Service class name (8 chars).
    pub name: String,
    /// CPU time consumed (microseconds).
    pub cpu_time_us: u64,
    /// Average response time (microseconds).
    pub response_time_us: u64,
    /// Performance Index (PI) — actual/goal, scaled by 100 (100 = at goal).
    pub performance_index: u16,
    /// Transaction count in the interval.
    pub transaction_count: u32,
    /// Velocity (percentage * 100).
    pub velocity: u16,
}

impl Default for ServiceClassMetrics {
    fn default() -> Self {
        Self {
            name: String::new(),
            cpu_time_us: 0,
            response_time_us: 0,
            performance_index: 100,
            transaction_count: 0,
            velocity: 0,
        }
    }
}

/// Type 72 record — workload activity.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Type72Record {
    /// Service class metrics.
    pub service_classes: Vec<ServiceClassMetrics>,
    /// Interval duration (microseconds).
    pub interval_us: u64,
}

impl Type72Record {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        push_u16(&mut data, self.service_classes.len() as u16);
        // 2 bytes reserved.
        push_u16(&mut data, 0);
        push_u64(&mut data, self.interval_us);

        for sc in &self.service_classes {
            extend_padded(&mut data, &sc.name, 8);
            push_u64(&mut data, sc.cpu_time_us);
            push_u64(&mut data, sc.response_time_us);
            push_u16(&mut data, sc.performance_index);
            // 2 bytes padding.
            push_u16(&mut data, 0);
            push_u32(&mut data, sc.transaction_count);
            push_u16(&mut data, sc.velocity);
            // 2 bytes padding.
            push_u16(&mut data, 0);
        }

        SmfRecord::new(72, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 74 — Device Activity
// ---------------------------------------------------------------------------

/// Per-volume device statistics within a Type 74 record.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct VolumeDeviceMetrics {
    /// Volume serial (6 chars).
    pub volser: String,
    /// I/O count.
    pub io_count: u32,
    /// Average response time per I/O (microseconds).
    pub avg_response_us: u32,
    /// Average connect time per I/O (microseconds).
    pub avg_connect_us: u32,
    /// Average queue depth (scaled by 100).
    pub avg_queue_depth: u16,
    /// Device busy percentage (scaled by 100).
    pub busy_pct: u16,
}

/// Type 74 record — device (DASD) activity.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Type74Record {
    /// Per-volume metrics.
    pub volumes: Vec<VolumeDeviceMetrics>,
    /// Interval duration (microseconds).
    pub interval_us: u64,
}

impl Type74Record {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        push_u16(&mut data, self.volumes.len() as u16);
        // 2 bytes reserved.
        push_u16(&mut data, 0);
        push_u64(&mut data, self.interval_us);

        for vol in &self.volumes {
            extend_padded(&mut data, &vol.volser, 6);
            // 2 bytes padding.
            push_u16(&mut data, 0);
            push_u32(&mut data, vol.io_count);
            push_u32(&mut data, vol.avg_response_us);
            push_u32(&mut data, vol.avg_connect_us);
            push_u16(&mut data, vol.avg_queue_depth);
            push_u16(&mut data, vol.busy_pct);
        }

        SmfRecord::new(74, data)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Type 70 ---

    #[test]
    fn test_type70_default() {
        let rec = Type70Record::default();
        assert_eq!(rec.online_processors, 1);
        assert_eq!(rec.average_utilization(), 0.0);
    }

    #[test]
    fn test_type70_utilization() {
        let rec = Type70Record {
            total_cpu_us: 900_000,
            interval_us: 1_000_000,
            online_processors: 1,
            ..Default::default()
        };
        let util = rec.average_utilization();
        assert!((util - 90.0).abs() < 0.1);
    }

    #[test]
    fn test_type70_multi_processor() {
        let rec = Type70Record {
            total_cpu_us: 1_800_000,
            interval_us: 1_000_000,
            online_processors: 4,
            processor_utilization: vec![9500, 9500, 0, 0],
            ..Default::default()
        };
        let util = rec.average_utilization();
        assert!((util - 45.0).abs() < 0.1);
    }

    #[test]
    fn test_type70_to_record() {
        let rec = Type70Record {
            total_cpu_us: 500_000,
            total_wait_us: 100_000,
            online_processors: 2,
            processor_utilization: vec![5000, 5000],
            mvs_overhead_us: 10_000,
            interval_us: 1_000_000,
        };
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 70);
        assert!(!smf.data.is_empty());
    }

    // --- Type 71 ---

    #[test]
    fn test_type71_default() {
        let rec = Type71Record::default();
        assert_eq!(rec.page_in_rate, 0);
        assert_eq!(rec.total_page_ins, 0);
    }

    #[test]
    fn test_type71_to_record() {
        let rec = Type71Record {
            page_in_rate: 150,
            page_out_rate: 50,
            total_page_ins: 10000,
            total_page_outs: 3000,
            aux_slot_usage: 7500,
            total_aux_slots: 100000,
            used_aux_slots: 75000,
            interval_us: 1_000_000,
        };
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 71);
    }

    // --- Type 72 ---

    #[test]
    fn test_type72_service_classes() {
        let rec = Type72Record {
            service_classes: vec![
                ServiceClassMetrics {
                    name: "PRODBTCH".to_string(),
                    cpu_time_us: 500_000,
                    response_time_us: 100_000,
                    performance_index: 95,
                    transaction_count: 1000,
                    velocity: 8500,
                },
                ServiceClassMetrics {
                    name: "ONLINE".to_string(),
                    cpu_time_us: 200_000,
                    response_time_us: 50_000,
                    performance_index: 110,
                    transaction_count: 5000,
                    velocity: 9200,
                },
            ],
            interval_us: 1_000_000,
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 72);
        assert!(!smf.data.is_empty());
    }

    #[test]
    fn test_type72_empty() {
        let rec = Type72Record::default();
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 72);
    }

    // --- Type 74 ---

    #[test]
    fn test_type74_volumes() {
        let rec = Type74Record {
            volumes: vec![
                VolumeDeviceMetrics {
                    volser: "VOL001".to_string(),
                    io_count: 50000,
                    avg_response_us: 2000,
                    avg_connect_us: 500,
                    avg_queue_depth: 150,
                    busy_pct: 3500,
                },
                VolumeDeviceMetrics {
                    volser: "VOL002".to_string(),
                    io_count: 10000,
                    avg_response_us: 1000,
                    avg_connect_us: 300,
                    avg_queue_depth: 50,
                    busy_pct: 1000,
                },
            ],
            interval_us: 1_000_000,
        };

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 74);
    }

    #[test]
    fn test_type74_empty() {
        let rec = Type74Record::default();
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 74);
    }

    #[test]
    fn test_performance_index_at_goal() {
        let sc = ServiceClassMetrics {
            performance_index: 100,
            ..Default::default()
        };
        assert_eq!(sc.performance_index, 100);
    }
}
