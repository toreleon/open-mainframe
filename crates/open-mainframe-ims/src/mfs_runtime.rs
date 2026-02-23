//! MFS-102: MFS Runtime Integration.
//!
//! Provides runtime formatting of input and output messages using
//! compiled MFS control blocks (MID, MOD, DIF, DOF). Also supports
//! MFS bypass mode for raw 3270 data streams.

use crate::mfs_compiler::{Mid, Mod, Dif, Dof};
use crate::ImsResult;

// ---------------------------------------------------------------------------
// Input formatting
// ---------------------------------------------------------------------------

/// Format raw terminal input data using a MID and DIF.
///
/// Maps device fields (described by the DIF) into message segments
/// (described by the MID).
///
/// # Arguments
/// * `mid` -- Message Input Descriptor.
/// * `dif` -- Device Input Format (screen layout).
/// * `raw_data` -- raw bytes from the terminal.
///
/// # Returns
/// Formatted message bytes ready for the application program.
pub fn format_input_message(mid: &Mid, dif: &Dif, raw_data: &[u8]) -> ImsResult<Vec<u8>> {
    // Build the formatted message based on the MID segments.
    let mut output = Vec::new();

    for seg in &mid.segments {
        let mut seg_buf = vec![0u8; seg.total_length];

        for (field_idx, field) in seg.fields.iter().enumerate() {
            // Try to find the corresponding DIF screen field
            if let Some(screen_field) = dif.fields.get(field_idx) {
                // Extract data from raw_data at the screen field's position
                // (simplified: use sequential offsets in raw_data)
                let raw_offset = screen_field_byte_offset(screen_field.row, screen_field.col);
                let end = (raw_offset + field.length).min(raw_data.len());
                if raw_offset < raw_data.len() {
                    let copy_len = (end - raw_offset).min(field.length);
                    let dest_end = field.offset + copy_len;
                    if dest_end <= seg_buf.len() {
                        seg_buf[field.offset..dest_end]
                            .copy_from_slice(&raw_data[raw_offset..raw_offset + copy_len]);
                    }
                }
            }
        }

        output.extend_from_slice(&seg_buf);
    }

    Ok(output)
}

/// Calculate a simplified byte offset from row/col (80-column screen).
fn screen_field_byte_offset(row: u16, col: u16) -> usize {
    ((row.saturating_sub(1)) as usize) * 80 + (col.saturating_sub(1)) as usize
}

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

/// Format application output segments for terminal display using a MOD and DOF.
///
/// Maps message segments (described by the MOD) into device fields
/// (described by the DOF) for display on the terminal.
///
/// # Arguments
/// * `mod_block` -- Message Output Descriptor.
/// * `dof` -- Device Output Format.
/// * `segments` -- application segment data, one `Vec<u8>` per segment.
///
/// # Returns
/// Formatted bytes representing the terminal screen image.
pub fn format_output_message(mod_block: &Mod, dof: &Dof, segments: &[Vec<u8>]) -> ImsResult<Vec<u8>> {
    // Build a screen buffer (24x80 = 1920 bytes, blank-filled)
    let screen_size = 24 * 80;
    let mut screen = vec![b' '; screen_size];

    for (seg_idx, seg) in mod_block.segments.iter().enumerate() {
        let seg_data = segments.get(seg_idx).map(|v| v.as_slice()).unwrap_or(&[]);

        for (field_idx, field) in seg.fields.iter().enumerate() {
            if let Some(screen_field) = dof.fields.get(field_idx) {
                let src_start = field.offset;
                let src_end = (src_start + field.length).min(seg_data.len());
                if src_start < seg_data.len() {
                    let data = &seg_data[src_start..src_end];
                    let dest_offset =
                        screen_field_byte_offset(screen_field.row, screen_field.col);
                    let copy_len = data.len().min(screen_size.saturating_sub(dest_offset));
                    if copy_len > 0 {
                        screen[dest_offset..dest_offset + copy_len]
                            .copy_from_slice(&data[..copy_len]);
                    }
                }
            }
        }
    }

    Ok(screen)
}

// ---------------------------------------------------------------------------
// MFS Bypass
// ---------------------------------------------------------------------------

/// MFS Bypass mode flag.
///
/// When bypass is active, MFS formatting is skipped and the application
/// receives / sends raw 3270 data stream bytes directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MfsBypassMode {
    /// Normal MFS formatting is active.
    Normal,
    /// MFS is bypassed -- raw data stream.
    Bypass,
}

/// MFS Bypass controller.
#[derive(Debug, Clone)]
pub struct MfsBypass {
    /// Current mode.
    mode: MfsBypassMode,
}

impl MfsBypass {
    /// Create a new bypass controller in normal mode.
    pub fn new() -> Self {
        Self {
            mode: MfsBypassMode::Normal,
        }
    }

    /// Enable bypass mode.
    pub fn enable(&mut self) {
        self.mode = MfsBypassMode::Bypass;
    }

    /// Disable bypass mode (return to normal MFS formatting).
    pub fn disable(&mut self) {
        self.mode = MfsBypassMode::Normal;
    }

    /// Return the current mode.
    pub fn mode(&self) -> MfsBypassMode {
        self.mode
    }

    /// Check if bypass is active.
    pub fn is_bypass(&self) -> bool {
        self.mode == MfsBypassMode::Bypass
    }

    /// Pass data through. In bypass mode, data is returned unchanged.
    /// In normal mode, this returns `None` to indicate MFS should format it.
    pub fn pass_through(&self, data: &[u8]) -> Option<Vec<u8>> {
        if self.is_bypass() {
            Some(data.to_vec())
        } else {
            None
        }
    }
}

impl Default for MfsBypass {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mfs_compiler::{
        CompiledField, CompiledScreenField, CompiledSegment, Dif, Dof, Mid, Mod,
    };

    fn test_mid() -> Mid {
        Mid {
            name: "TMID".to_string(),
            segments: vec![CompiledSegment {
                name: "SEG1".to_string(),
                total_length: 30,
                fields: vec![
                    CompiledField {
                        name: "F1".to_string(),
                        offset: 0,
                        length: 20,
                    },
                    CompiledField {
                        name: "F2".to_string(),
                        offset: 20,
                        length: 10,
                    },
                ],
            }],
        }
    }

    fn test_dif() -> Dif {
        Dif {
            name: "TDIF".to_string(),
            device_type: "3270-2".to_string(),
            fields: vec![
                CompiledScreenField {
                    name: "SF1".to_string(),
                    row: 1,
                    col: 1,
                    length: 20,
                },
                CompiledScreenField {
                    name: "SF2".to_string(),
                    row: 2,
                    col: 1,
                    length: 10,
                },
            ],
        }
    }

    fn test_mod_block() -> Mod {
        Mod {
            name: "TMOD".to_string(),
            segments: vec![CompiledSegment {
                name: "SEG1".to_string(),
                total_length: 15,
                fields: vec![
                    CompiledField {
                        name: "F1".to_string(),
                        offset: 0,
                        length: 10,
                    },
                    CompiledField {
                        name: "F2".to_string(),
                        offset: 10,
                        length: 5,
                    },
                ],
            }],
        }
    }

    fn test_dof() -> Dof {
        Dof {
            name: "TDOF".to_string(),
            device_type: "3270-2".to_string(),
            fields: vec![
                CompiledScreenField {
                    name: "SF1".to_string(),
                    row: 1,
                    col: 1,
                    length: 10,
                },
                CompiledScreenField {
                    name: "SF2".to_string(),
                    row: 2,
                    col: 1,
                    length: 5,
                },
            ],
        }
    }

    #[test]
    fn test_format_input_message() {
        let mid = test_mid();
        let dif = test_dif();
        // Create raw data with something at row 1, col 1
        let mut raw = vec![0u8; 240]; // 3 rows * 80
        raw[0..5].copy_from_slice(b"HELLO");
        raw[80..85].copy_from_slice(b"WORLD"); // row 2, col 1 (5 bytes)

        let result = format_input_message(&mid, &dif, &raw).unwrap();
        assert_eq!(result.len(), 30);
        assert_eq!(&result[0..5], b"HELLO");
    }

    #[test]
    fn test_format_input_empty_raw() {
        let mid = test_mid();
        let dif = test_dif();
        let result = format_input_message(&mid, &dif, &[]).unwrap();
        assert_eq!(result.len(), 30);
        assert!(result.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_format_output_message() {
        let mod_block = test_mod_block();
        let dof = test_dof();
        let seg_data = vec![{
            let mut d = vec![0u8; 15];
            d[0..5].copy_from_slice(b"ABCDE");
            d[10..13].copy_from_slice(b"XYZ");
            d
        }];

        let screen = format_output_message(&mod_block, &dof, &seg_data).unwrap();
        assert_eq!(screen.len(), 24 * 80);
        // Row 1, col 1 should have "ABCDE"
        assert_eq!(&screen[0..5], b"ABCDE");
        // Row 2, col 1 should have "XYZ"
        assert_eq!(&screen[80..83], b"XYZ");
    }

    #[test]
    fn test_format_output_no_segments() {
        let mod_block = test_mod_block();
        let dof = test_dof();
        let screen = format_output_message(&mod_block, &dof, &[]).unwrap();
        assert_eq!(screen.len(), 24 * 80);
        // Screen should be blank-filled
        assert!(screen.iter().all(|&b| b == b' '));
    }

    #[test]
    fn test_screen_field_byte_offset() {
        assert_eq!(screen_field_byte_offset(1, 1), 0);
        assert_eq!(screen_field_byte_offset(1, 2), 1);
        assert_eq!(screen_field_byte_offset(2, 1), 80);
        assert_eq!(screen_field_byte_offset(3, 5), 164);
    }

    // --- MFS Bypass ---

    #[test]
    fn test_bypass_new() {
        let bp = MfsBypass::new();
        assert_eq!(bp.mode(), MfsBypassMode::Normal);
        assert!(!bp.is_bypass());
    }

    #[test]
    fn test_bypass_default() {
        let bp = MfsBypass::default();
        assert!(!bp.is_bypass());
    }

    #[test]
    fn test_bypass_enable_disable() {
        let mut bp = MfsBypass::new();
        bp.enable();
        assert!(bp.is_bypass());
        assert_eq!(bp.mode(), MfsBypassMode::Bypass);
        bp.disable();
        assert!(!bp.is_bypass());
    }

    #[test]
    fn test_bypass_pass_through_active() {
        let mut bp = MfsBypass::new();
        bp.enable();
        let data = b"raw 3270 stream";
        let result = bp.pass_through(data);
        assert_eq!(result, Some(data.to_vec()));
    }

    #[test]
    fn test_bypass_pass_through_normal() {
        let bp = MfsBypass::new();
        assert!(bp.pass_through(b"data").is_none());
    }
}
