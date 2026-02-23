//! SMFPRMxx Configuration Parser.
//!
//! Parses SMFPRMxx PARMLIB member parameters that control SMF behavior:
//! - TYPE/NOTYPE — record type filtering
//! - EXITS — exit program registration
//! - INTERVAL — recording interval
//! - RECORDING — recording mode (LOGSTREAM or DATASET)
//! - MAXBUFSIZE — maximum buffer size

#![allow(clippy::manual_range_contains)]

use std::collections::HashSet;
use std::time::Duration;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from SMFPRMxx configuration parsing.
#[derive(Debug, thiserror::Error)]
pub enum SmfConfigError {
    /// Unknown parameter keyword.
    #[error("unknown SMFPRMxx parameter: {0}")]
    UnknownParameter(String),

    /// Invalid type range specification.
    #[error("invalid type range: {0}")]
    InvalidTypeRange(String),

    /// Invalid interval specification.
    #[error("invalid interval: {0}")]
    InvalidInterval(String),

    /// Invalid buffer size.
    #[error("invalid buffer size: {0}")]
    InvalidBufferSize(String),
}

// ---------------------------------------------------------------------------
//  Recording mode
// ---------------------------------------------------------------------------

/// SMF recording destination mode.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum RecordingMode {
    /// Write to a sequential dataset (SYS1.MANx).
    #[default]
    Dataset,
    /// Write to a log stream.
    Logstream,
}

// ---------------------------------------------------------------------------
//  Exit entry
// ---------------------------------------------------------------------------

/// An SMF exit configuration entry.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfExitConfig {
    /// Exit name (e.g., "IEFU83", "IEFU84").
    pub name: String,
    /// Record types this exit applies to (empty = all types).
    pub types: HashSet<u8>,
}

// ---------------------------------------------------------------------------
//  SMFPRMxx Configuration
// ---------------------------------------------------------------------------

/// Parsed SMFPRMxx configuration.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfPrmConfig {
    /// Active record types (from TYPE parameter). Empty = all active.
    pub active_types: HashSet<u8>,
    /// Suppressed record types (from NOTYPE parameter).
    pub inactive_types: HashSet<u8>,
    /// Registered exits.
    pub exits: Vec<SmfExitConfig>,
    /// Recording interval in seconds.
    pub interval_secs: u64,
    /// Maximum buffer size in bytes.
    pub max_buffer_size: usize,
    /// Recording mode.
    pub recording_mode: RecordingMode,
}

impl Default for SmfPrmConfig {
    fn default() -> Self {
        Self {
            active_types: HashSet::new(),
            inactive_types: HashSet::new(),
            exits: Vec::new(),
            interval_secs: 1800, // 30 minutes
            max_buffer_size: 65536, // 64KB
            recording_mode: RecordingMode::Dataset,
        }
    }
}

impl SmfPrmConfig {
    /// Check if a given record type is active (not suppressed).
    pub fn is_type_active(&self, record_type: u8) -> bool {
        if self.inactive_types.contains(&record_type) {
            return false;
        }
        if self.active_types.is_empty() {
            return true;
        }
        self.active_types.contains(&record_type)
    }

    /// Get the recording interval as a Duration.
    pub fn interval(&self) -> Duration {
        Duration::from_secs(self.interval_secs)
    }

    /// Parse an SMFPRMxx configuration from text lines.
    pub fn parse(input: &str) -> Result<Self, SmfConfigError> {
        let mut config = SmfPrmConfig::default();

        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') || line.starts_with("/*") {
                continue;
            }
            let upper = line.to_uppercase();

            if upper.starts_with("TYPE(") {
                let types = parse_type_list(&upper, "TYPE(")?;
                config.active_types.extend(types);
            } else if upper.starts_with("NOTYPE(") {
                let types = parse_type_list(&upper, "NOTYPE(")?;
                config.inactive_types.extend(types);
            } else if upper.starts_with("EXITS(") {
                let exits = parse_exits(&upper)?;
                config.exits.extend(exits);
            } else if upper.starts_with("INTERVAL(") {
                config.interval_secs = parse_interval(&upper)?;
            } else if upper.starts_with("RECORDING(") {
                config.recording_mode = parse_recording_mode(&upper);
            } else if upper.starts_with("MAXBUFSIZE(") {
                config.max_buffer_size = parse_max_buf_size(&upper)?;
            }
            // Ignore unknown lines gracefully (some PARMLIB members have comments).
        }

        Ok(config)
    }
}

// ---------------------------------------------------------------------------
//  Parsing helpers
// ---------------------------------------------------------------------------

fn extract_parens(line: &str, prefix: &str) -> Option<String> {
    let start = line.find(prefix)? + prefix.len();
    let end = line[start..].find(')')? + start;
    Some(line[start..end].to_string())
}

fn parse_type_list(line: &str, prefix: &str) -> Result<HashSet<u8>, SmfConfigError> {
    let content = extract_parens(line, prefix)
        .ok_or_else(|| SmfConfigError::InvalidTypeRange(line.to_string()))?;
    let mut types = HashSet::new();

    for part in content.split(',') {
        let part = part.trim();
        if part.contains(':') {
            // Range: e.g., "70:79"
            let mut iter = part.split(':');
            let lo: u8 = iter
                .next()
                .and_then(|s| s.trim().parse().ok())
                .ok_or_else(|| SmfConfigError::InvalidTypeRange(part.to_string()))?;
            let hi: u8 = iter
                .next()
                .and_then(|s| s.trim().parse().ok())
                .ok_or_else(|| SmfConfigError::InvalidTypeRange(part.to_string()))?;
            for t in lo..=hi {
                types.insert(t);
            }
        } else {
            let t: u8 = part
                .parse()
                .map_err(|_| SmfConfigError::InvalidTypeRange(part.to_string()))?;
            types.insert(t);
        }
    }

    Ok(types)
}

fn parse_exits(line: &str) -> Result<Vec<SmfExitConfig>, SmfConfigError> {
    let content = extract_parens(line, "EXITS(")
        .ok_or_else(|| SmfConfigError::UnknownParameter(line.to_string()))?;
    let mut exits = Vec::new();
    for name in content.split(',') {
        let name = name.trim().to_string();
        if !name.is_empty() {
            exits.push(SmfExitConfig {
                name,
                types: HashSet::new(),
            });
        }
    }
    Ok(exits)
}

fn parse_interval(line: &str) -> Result<u64, SmfConfigError> {
    let content = extract_parens(line, "INTERVAL(")
        .ok_or_else(|| SmfConfigError::InvalidInterval(line.to_string()))?;
    // Format: INTERVAL(SMF,HHMM) or INTERVAL(HHMM)
    let time_part = if content.contains(',') {
        content.split(',').nth(1).unwrap_or("0030").trim()
    } else {
        content.trim()
    };

    if time_part.len() == 4 {
        let hh: u64 = time_part[..2]
            .parse()
            .map_err(|_| SmfConfigError::InvalidInterval(time_part.to_string()))?;
        let mm: u64 = time_part[2..]
            .parse()
            .map_err(|_| SmfConfigError::InvalidInterval(time_part.to_string()))?;
        Ok(hh * 3600 + mm * 60)
    } else {
        Err(SmfConfigError::InvalidInterval(time_part.to_string()))
    }
}

fn parse_recording_mode(line: &str) -> RecordingMode {
    let content = extract_parens(line, "RECORDING(").unwrap_or_default();
    if content.contains("LOGSTREAM") {
        RecordingMode::Logstream
    } else {
        RecordingMode::Dataset
    }
}

fn parse_max_buf_size(line: &str) -> Result<usize, SmfConfigError> {
    let content = extract_parens(line, "MAXBUFSIZE(")
        .ok_or_else(|| SmfConfigError::InvalidBufferSize(line.to_string()))?;
    let content = content.trim();
    // Support K suffix (kilobytes).
    let (num_str, multiplier) = if let Some(stripped) = content.strip_suffix('K') {
        (stripped, 1024)
    } else {
        (content, 1)
    };
    let val: usize = num_str
        .parse()
        .map_err(|_| SmfConfigError::InvalidBufferSize(content.to_string()))?;
    Ok(val * multiplier)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let cfg = SmfPrmConfig::default();
        assert!(cfg.active_types.is_empty());
        assert!(cfg.inactive_types.is_empty());
        assert_eq!(cfg.interval_secs, 1800);
        assert_eq!(cfg.max_buffer_size, 65536);
        assert_eq!(cfg.recording_mode, RecordingMode::Dataset);
    }

    #[test]
    fn test_parse_type_list() {
        let cfg = SmfPrmConfig::parse("TYPE(30,70:79,80)").unwrap();
        assert!(cfg.is_type_active(30));
        assert!(cfg.is_type_active(70));
        assert!(cfg.is_type_active(75));
        assert!(cfg.is_type_active(79));
        assert!(cfg.is_type_active(80));
        assert!(!cfg.is_type_active(4));
    }

    #[test]
    fn test_parse_notype() {
        let cfg = SmfPrmConfig::parse("NOTYPE(0:29)").unwrap();
        assert!(!cfg.is_type_active(0));
        assert!(!cfg.is_type_active(15));
        assert!(!cfg.is_type_active(29));
        assert!(cfg.is_type_active(30));
    }

    #[test]
    fn test_parse_exits() {
        let cfg = SmfPrmConfig::parse("EXITS(IEFU83,IEFU84)").unwrap();
        assert_eq!(cfg.exits.len(), 2);
        assert_eq!(cfg.exits[0].name, "IEFU83");
        assert_eq!(cfg.exits[1].name, "IEFU84");
    }

    #[test]
    fn test_parse_interval() {
        let cfg = SmfPrmConfig::parse("INTERVAL(SMF,0030)").unwrap();
        assert_eq!(cfg.interval_secs, 1800); // 30 minutes
        assert_eq!(cfg.interval(), Duration::from_secs(1800));
    }

    #[test]
    fn test_parse_interval_one_hour() {
        let cfg = SmfPrmConfig::parse("INTERVAL(SMF,0100)").unwrap();
        assert_eq!(cfg.interval_secs, 3600);
    }

    #[test]
    fn test_parse_recording_logstream() {
        let cfg = SmfPrmConfig::parse("RECORDING(LOGSTREAM)").unwrap();
        assert_eq!(cfg.recording_mode, RecordingMode::Logstream);
    }

    #[test]
    fn test_parse_recording_dataset() {
        let cfg = SmfPrmConfig::parse("RECORDING(DATASET)").unwrap();
        assert_eq!(cfg.recording_mode, RecordingMode::Dataset);
    }

    #[test]
    fn test_parse_maxbufsize_bytes() {
        let cfg = SmfPrmConfig::parse("MAXBUFSIZE(131072)").unwrap();
        assert_eq!(cfg.max_buffer_size, 131072);
    }

    #[test]
    fn test_parse_maxbufsize_kilobytes() {
        let cfg = SmfPrmConfig::parse("MAXBUFSIZE(64K)").unwrap();
        assert_eq!(cfg.max_buffer_size, 65536);
    }

    #[test]
    fn test_parse_combined_config() {
        let input = r#"
* SMFPRMxx member for test system
TYPE(30,70:79,80)
NOTYPE(0:3)
EXITS(IEFU83)
INTERVAL(SMF,0030)
RECORDING(LOGSTREAM)
MAXBUFSIZE(128K)
"#;
        let cfg = SmfPrmConfig::parse(input).unwrap();
        assert!(cfg.is_type_active(30));
        assert!(cfg.is_type_active(70));
        assert!(!cfg.is_type_active(0));
        assert!(!cfg.is_type_active(3));
        assert_eq!(cfg.exits.len(), 1);
        assert_eq!(cfg.interval_secs, 1800);
        assert_eq!(cfg.recording_mode, RecordingMode::Logstream);
        assert_eq!(cfg.max_buffer_size, 131072);
    }

    #[test]
    fn test_type_active_empty_means_all() {
        let cfg = SmfPrmConfig::default();
        assert!(cfg.is_type_active(0));
        assert!(cfg.is_type_active(128));
        assert!(cfg.is_type_active(255));
    }

    #[test]
    fn test_comments_ignored() {
        let input = "* This is a comment\nTYPE(30)\n/* Also a comment */";
        let cfg = SmfPrmConfig::parse(input).unwrap();
        assert!(cfg.is_type_active(30));
    }
}
