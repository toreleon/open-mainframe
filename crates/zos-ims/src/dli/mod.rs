//! DL/I (Data Language/I) call interface.
//!
//! Implements the IMS DL/I calls:
//! - GU (Get Unique)
//! - GN (Get Next)
//! - GNP (Get Next within Parent)
//! - GHU, GHN, GHNP (Get Hold variants)
//! - ISRT (Insert)
//! - DLET (Delete)
//! - REPL (Replace)

mod ssa;
pub mod store;

pub use ssa::{Ssa, SsaQualification, SsaOperator, CommandCode};

use crate::{ImsResult, ImsError, StatusCode};
use crate::psb::ProgramCommBlock;

/// DL/I function codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DliFunction {
    /// Get Unique - retrieve specific segment
    GU,
    /// Get Next - sequential retrieval
    GN,
    /// Get Next within Parent
    GNP,
    /// Get Hold Unique
    GHU,
    /// Get Hold Next
    GHN,
    /// Get Hold Next within Parent
    GHNP,
    /// Insert
    ISRT,
    /// Delete
    DLET,
    /// Replace
    REPL,
    /// Schedule PSB
    PCB,
    /// Terminate
    TERM,
}

impl DliFunction {
    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "GU" => Some(DliFunction::GU),
            "GN" => Some(DliFunction::GN),
            "GNP" => Some(DliFunction::GNP),
            "GHU" => Some(DliFunction::GHU),
            "GHN" => Some(DliFunction::GHN),
            "GHNP" => Some(DliFunction::GHNP),
            "ISRT" => Some(DliFunction::ISRT),
            "DLET" => Some(DliFunction::DLET),
            "REPL" => Some(DliFunction::REPL),
            "PCB" | "SCHD" => Some(DliFunction::PCB),
            "TERM" => Some(DliFunction::TERM),
            _ => None,
        }
    }

    /// Check if this is a "get hold" function.
    pub fn is_hold(&self) -> bool {
        matches!(self, DliFunction::GHU | DliFunction::GHN | DliFunction::GHNP)
    }

    /// Check if this is a retrieval function.
    pub fn is_get(&self) -> bool {
        matches!(
            self,
            DliFunction::GU
                | DliFunction::GN
                | DliFunction::GNP
                | DliFunction::GHU
                | DliFunction::GHN
                | DliFunction::GHNP
        )
    }
}

/// A DL/I call request.
#[derive(Debug, Clone)]
pub struct DliCall {
    /// Function code
    pub function: DliFunction,
    /// PCB index
    pub pcb_index: usize,
    /// I/O area (segment data)
    pub io_area: Vec<u8>,
    /// Segment Search Arguments
    pub ssas: Vec<Ssa>,
}

impl DliCall {
    /// Create a new DL/I call.
    pub fn new(function: DliFunction, pcb_index: usize) -> Self {
        Self {
            function,
            pcb_index,
            io_area: Vec::new(),
            ssas: Vec::new(),
        }
    }

    /// Set I/O area.
    pub fn with_io_area(mut self, data: Vec<u8>) -> Self {
        self.io_area = data;
        self
    }

    /// Add an SSA.
    pub fn with_ssa(mut self, ssa: Ssa) -> Self {
        self.ssas.push(ssa);
        self
    }

    /// Add multiple SSAs.
    pub fn with_ssas(mut self, ssas: Vec<Ssa>) -> Self {
        self.ssas = ssas;
        self
    }

    /// Parse SSA strings.
    pub fn parse_ssas(mut self, ssa_strs: &[&str]) -> ImsResult<Self> {
        for s in ssa_strs {
            let ssa = Ssa::parse(s)?;
            self.ssas.push(ssa);
        }
        Ok(self)
    }
}

/// Result of a DL/I call.
#[derive(Debug, Clone)]
pub struct DliResult {
    /// Status code
    pub status: StatusCode,
    /// Retrieved segment data (for GET calls)
    pub segment_data: Option<Vec<u8>>,
    /// Segment name
    pub segment_name: Option<String>,
    /// Key feedback
    pub key_feedback: Vec<u8>,
}

impl DliResult {
    /// Create a successful result.
    pub fn ok(segment_data: Vec<u8>, segment_name: &str) -> Self {
        Self {
            status: StatusCode::Ok,
            segment_data: Some(segment_data),
            segment_name: Some(segment_name.to_string()),
            key_feedback: Vec::new(),
        }
    }

    /// Create an error result.
    pub fn error(status: StatusCode) -> Self {
        Self {
            status,
            segment_data: None,
            segment_name: None,
            key_feedback: Vec::new(),
        }
    }

    /// Create a not-found result.
    pub fn not_found() -> Self {
        Self::error(StatusCode::GE)
    }

    /// Create an end-of-database result.
    pub fn end_of_db() -> Self {
        Self::error(StatusCode::GB)
    }

    /// Check if successful.
    pub fn is_ok(&self) -> bool {
        self.status.is_ok()
    }

    /// Check if not found.
    pub fn is_not_found(&self) -> bool {
        self.status.is_not_found()
    }
}

/// DL/I call processor.
pub struct DliProcessor {
    /// Current position in database (segment path)
    current_position: Vec<(String, u64)>, // (segment_name, record_id)
    /// Held segment for update/delete
    held_segment: Option<HeldSegment>,
    /// Parentage level
    parentage_level: usize,
}

/// A segment held for update/delete.
#[derive(Debug, Clone)]
struct HeldSegment {
    segment_name: String,
    record_id: u64,
    data: Vec<u8>,
}

impl DliProcessor {
    /// Create a new processor.
    pub fn new() -> Self {
        Self {
            current_position: Vec::new(),
            held_segment: None,
            parentage_level: 0,
        }
    }

    /// Execute a DL/I call.
    pub fn execute(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        match call.function {
            DliFunction::GU => self.execute_gu(call, pcb),
            DliFunction::GN => self.execute_gn(call, pcb),
            DliFunction::GNP => self.execute_gnp(call, pcb),
            DliFunction::GHU => self.execute_ghu(call, pcb),
            DliFunction::GHN => self.execute_ghn(call, pcb),
            DliFunction::GHNP => self.execute_ghnp(call, pcb),
            DliFunction::ISRT => self.execute_isrt(call, pcb),
            DliFunction::DLET => self.execute_dlet(call, pcb),
            DliFunction::REPL => self.execute_repl(call, pcb),
            _ => Err(ImsError::DliError { status: StatusCode::AD }),
        }
    }

    fn execute_gu(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Reset position for GU
        self.current_position.clear();
        self.held_segment = None;

        // Process SSAs to find target segment
        // This is a placeholder - actual implementation would query the database
        let target_segment = call.ssas.last()
            .map(|ssa| ssa.segment_name.clone())
            .unwrap_or_default();

        if target_segment.is_empty() {
            pcb.set_status(StatusCode::AD);
            return Ok(DliResult::error(StatusCode::AD));
        }

        // Check if segment is accessible
        if !pcb.is_segment_accessible(&target_segment) {
            pcb.set_status(StatusCode::GE);
            return Ok(DliResult::not_found());
        }

        // Placeholder: Return dummy data
        // Real implementation would query database
        let data = vec![0u8; 100];
        pcb.set_status(StatusCode::Ok);

        Ok(DliResult::ok(data, &target_segment))
    }

    fn execute_gn(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Sequential retrieval from current position
        self.held_segment = None;

        // Get target segment from SSA or use current level
        let target_segment = call.ssas.last()
            .map(|ssa| ssa.segment_name.clone())
            .unwrap_or_default();

        if !target_segment.is_empty() && !pcb.is_segment_accessible(&target_segment) {
            pcb.set_status(StatusCode::GE);
            return Ok(DliResult::not_found());
        }

        // Placeholder: Return end of database
        // Real implementation would continue from current position
        pcb.set_status(StatusCode::GB);
        Ok(DliResult::end_of_db())
    }

    fn execute_gnp(&mut self, _call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Get next within parent - limited to children of current parent
        self.held_segment = None;

        if self.parentage_level == 0 {
            pcb.set_status(StatusCode::GP);
            return Ok(DliResult::error(StatusCode::GP));
        }

        // Similar to GN but restricted to children
        pcb.set_status(StatusCode::GE);
        Ok(DliResult::not_found())
    }

    fn execute_ghu(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Get Hold Unique - same as GU but holds segment for update
        let result = self.execute_gu(call, pcb)?;

        if result.is_ok() {
            if let Some(ref data) = result.segment_data {
                self.held_segment = Some(HeldSegment {
                    segment_name: result.segment_name.clone().unwrap_or_default(),
                    record_id: 0, // Would be actual record ID
                    data: data.clone(),
                });
            }
        }

        Ok(result)
    }

    fn execute_ghn(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        let result = self.execute_gn(call, pcb)?;

        if result.is_ok() {
            if let Some(ref data) = result.segment_data {
                self.held_segment = Some(HeldSegment {
                    segment_name: result.segment_name.clone().unwrap_or_default(),
                    record_id: 0,
                    data: data.clone(),
                });
            }
        }

        Ok(result)
    }

    fn execute_ghnp(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        let result = self.execute_gnp(call, pcb)?;

        if result.is_ok() {
            if let Some(ref data) = result.segment_data {
                self.held_segment = Some(HeldSegment {
                    segment_name: result.segment_name.clone().unwrap_or_default(),
                    record_id: 0,
                    data: data.clone(),
                });
            }
        }

        Ok(result)
    }

    fn execute_isrt(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Insert segment
        let target_segment = call.ssas.last()
            .map(|ssa| ssa.segment_name.clone())
            .unwrap_or_default();

        if target_segment.is_empty() {
            pcb.set_status(StatusCode::AD);
            return Ok(DliResult::error(StatusCode::AD));
        }

        // Check if insert is allowed
        if !pcb.can_operate(&target_segment, crate::psb::Operation::Insert) {
            pcb.set_status(StatusCode::AD);
            return Ok(DliResult::error(StatusCode::AD));
        }

        // Placeholder: Would actually insert into database
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &target_segment))
    }

    fn execute_dlet(&mut self, _call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Delete requires prior Get Hold
        if self.held_segment.is_none() {
            pcb.set_status(StatusCode::DJ);
            return Ok(DliResult::error(StatusCode::DJ));
        }

        let held = self.held_segment.take().unwrap();

        // Check if delete is allowed
        if !pcb.can_operate(&held.segment_name, crate::psb::Operation::Delete) {
            pcb.set_status(StatusCode::AD);
            return Ok(DliResult::error(StatusCode::AD));
        }

        // Placeholder: Would actually delete from database
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &held.segment_name))
    }

    fn execute_repl(&mut self, call: &DliCall, pcb: &mut ProgramCommBlock) -> ImsResult<DliResult> {
        // Replace requires prior Get Hold
        if self.held_segment.is_none() {
            pcb.set_status(StatusCode::DJ);
            return Ok(DliResult::error(StatusCode::DJ));
        }

        let held = self.held_segment.take().unwrap();

        // Check if replace is allowed
        if !pcb.can_operate(&held.segment_name, crate::psb::Operation::Replace) {
            pcb.set_status(StatusCode::AD);
            return Ok(DliResult::error(StatusCode::AD));
        }

        // Check if key field changed (not allowed)
        // Placeholder: Would actually check key fields
        if call.io_area.is_empty() {
            pcb.set_status(StatusCode::RX);
            return Ok(DliResult::error(StatusCode::RX));
        }

        // Placeholder: Would actually update in database
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &held.segment_name))
    }

    /// Set parentage level (for GNP calls).
    pub fn set_parentage(&mut self, level: usize) {
        self.parentage_level = level;
    }

    /// Clear held segment.
    pub fn clear_hold(&mut self) {
        self.held_segment = None;
    }

    /// Reset position.
    pub fn reset(&mut self) {
        self.current_position.clear();
        self.held_segment = None;
        self.parentage_level = 0;
    }
}

impl Default for DliProcessor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::psb::{ProcessingOptions, SensitiveSegment};

    fn create_test_pcb() -> ProgramCommBlock {
        let mut pcb = ProgramCommBlock::new_db("TESTDB", ProcessingOptions::from_str("A"), 50);
        pcb.add_senseg(SensitiveSegment::new("CUSTOMER", "", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ORDER", "CUSTOMER", ProcessingOptions::from_str("A")));
        pcb
    }

    #[test]
    fn test_dli_function_parse() {
        assert_eq!(DliFunction::from_str("GU"), Some(DliFunction::GU));
        assert_eq!(DliFunction::from_str("gn"), Some(DliFunction::GN));
        assert_eq!(DliFunction::from_str("ISRT"), Some(DliFunction::ISRT));
        assert_eq!(DliFunction::from_str("INVALID"), None);
    }

    #[test]
    fn test_dli_function_is_hold() {
        assert!(DliFunction::GHU.is_hold());
        assert!(DliFunction::GHN.is_hold());
        assert!(DliFunction::GHNP.is_hold());
        assert!(!DliFunction::GU.is_hold());
        assert!(!DliFunction::ISRT.is_hold());
    }

    #[test]
    fn test_dli_call_builder() {
        let call = DliCall::new(DliFunction::GU, 0)
            .with_io_area(vec![1, 2, 3])
            .with_ssa(Ssa::unqualified("CUSTOMER"));

        assert_eq!(call.function, DliFunction::GU);
        assert_eq!(call.io_area, vec![1, 2, 3]);
        assert_eq!(call.ssas.len(), 1);
    }

    #[test]
    fn test_execute_gu() {
        let mut processor = DliProcessor::new();
        let mut pcb = create_test_pcb();

        let call = DliCall::new(DliFunction::GU, 0)
            .with_ssa(Ssa::unqualified("CUSTOMER"));

        let result = processor.execute(&call, &mut pcb).unwrap();
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_gu_not_accessible() {
        let mut processor = DliProcessor::new();
        let mut pcb = create_test_pcb();

        let call = DliCall::new(DliFunction::GU, 0)
            .with_ssa(Ssa::unqualified("INVALID"));

        let result = processor.execute(&call, &mut pcb).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_execute_dlet_no_hold() {
        let mut processor = DliProcessor::new();
        let mut pcb = create_test_pcb();

        let call = DliCall::new(DliFunction::DLET, 0);
        let result = processor.execute(&call, &mut pcb).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_execute_repl_no_hold() {
        let mut processor = DliProcessor::new();
        let mut pcb = create_test_pcb();

        let call = DliCall::new(DliFunction::REPL, 0)
            .with_io_area(vec![1, 2, 3]);
        let result = processor.execute(&call, &mut pcb).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_dli_result() {
        let ok_result = DliResult::ok(vec![1, 2, 3], "CUSTOMER");
        assert!(ok_result.is_ok());
        assert!(!ok_result.is_not_found());

        let nf_result = DliResult::not_found();
        assert!(!nf_result.is_ok());
        assert!(nf_result.is_not_found());
    }
}
