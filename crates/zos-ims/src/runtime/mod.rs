//! IMS Runtime support.
//!
//! Provides database access and transaction management for IMS programs.
//! Uses an in-memory hierarchical store that can be persisted to PostgreSQL.

use std::collections::HashMap;

use crate::dbd::DatabaseDefinition;
use crate::dli::store::{HierarchicalStore, QualOp};
use crate::dli::{DliCall, DliFunction, DliProcessor, DliResult, Ssa, SsaOperator};
use crate::psb::{ProgramCommBlock, ProgramSpecBlock};
use crate::{ImsError, ImsResult, StatusCode};

/// IMS Runtime for executing DL/I calls.
pub struct ImsRuntime {
    /// Loaded databases
    databases: HashMap<String, HierarchicalStore>,
    /// Database definitions
    dbds: HashMap<String, DatabaseDefinition>,
    /// Scheduled PSB
    psb: Option<ProgramSpecBlock>,
    /// DL/I processors for each PCB
    processors: Vec<DliProcessor>,
    /// Current position in each database
    positions: HashMap<String, DatabasePosition>,
}

/// Current position in a database for GN/GNP calls.
#[derive(Debug, Clone, Default)]
pub struct DatabasePosition {
    /// Current segment record ID
    pub current_id: u64,
    /// Parent ID for GNP calls
    pub parent_id: u64,
    /// Path of segment names to current position
    pub path: Vec<String>,
}

impl ImsRuntime {
    /// Create a new IMS runtime.
    pub fn new() -> Self {
        Self {
            databases: HashMap::new(),
            dbds: HashMap::new(),
            psb: None,
            processors: Vec::new(),
            positions: HashMap::new(),
        }
    }

    /// Load a database definition.
    pub fn load_dbd(&mut self, dbd: DatabaseDefinition) {
        let name = dbd.name.clone();
        self.databases
            .entry(name.clone())
            .or_insert_with(|| HierarchicalStore::new(&name));
        self.dbds.insert(name, dbd);
    }

    /// Get or create a database store.
    pub fn get_store(&mut self, dbname: &str) -> &mut HierarchicalStore {
        self.databases
            .entry(dbname.to_uppercase())
            .or_insert_with(|| HierarchicalStore::new(dbname))
    }

    /// Schedule a PSB for execution.
    pub fn schedule_psb(&mut self, psb: ProgramSpecBlock) -> ImsResult<()> {
        let pcb_count = psb.pcb_count();
        self.psb = Some(psb);
        self.processors = (0..pcb_count).map(|_| DliProcessor::new()).collect();
        Ok(())
    }

    /// Get the scheduled PSB.
    pub fn get_psb(&self) -> Option<&ProgramSpecBlock> {
        self.psb.as_ref()
    }

    /// Get a mutable PCB by index.
    pub fn get_pcb_mut(&mut self, index: usize) -> ImsResult<&mut ProgramCommBlock> {
        self.psb
            .as_mut()
            .and_then(|p| p.pcbs.get_mut(index))
            .ok_or(ImsError::PcbNotFound(format!("PCB index {}", index)))
    }

    /// Execute a DL/I call.
    pub fn execute(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        let psb = self
            .psb
            .as_mut()
            .ok_or(ImsError::DliError { status: StatusCode::AP })?;

        let pcb = psb
            .pcbs
            .get_mut(call.pcb_index)
            .ok_or(ImsError::PcbNotFound(format!("PCB index {}", call.pcb_index)))?;

        let dbname = pcb.dbname.clone();
        let store = self
            .databases
            .entry(dbname.clone())
            .or_insert_with(|| HierarchicalStore::new(&dbname));

        let position = self.positions.entry(dbname.clone()).or_default();

        match call.function {
            DliFunction::GU => execute_gu(store, call, pcb, position),
            DliFunction::GN => execute_gn(store, call, pcb, position),
            DliFunction::GNP => execute_gnp(store, call, pcb, position),
            DliFunction::GHU => execute_ghu(store, call, pcb, position),
            DliFunction::GHN => execute_ghn(store, call, pcb, position),
            DliFunction::GHNP => execute_ghnp(store, call, pcb, position),
            DliFunction::ISRT => execute_isrt(store, call, pcb, position),
            DliFunction::DLET => execute_dlet(store, call, pcb, position),
            DliFunction::REPL => execute_repl(store, call, pcb, position),
            _ => {
                pcb.set_status(StatusCode::AD);
                Ok(DliResult::error(StatusCode::AD))
            }
        }
    }

    /// Execute GU (Get Unique) - convenience method.
    pub fn gu(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GU, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute GN (Get Next) - convenience method.
    pub fn gn(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GN, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute GNP (Get Next within Parent) - convenience method.
    pub fn gnp(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GNP, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute ISRT (Insert) - convenience method.
    pub fn isrt(&mut self, pcb_index: usize, ssas: Vec<Ssa>, data: Vec<u8>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::ISRT, pcb_index)
            .with_ssas(ssas)
            .with_io_area(data);
        self.execute(&call)
    }

    /// Terminate PSB.
    pub fn terminate(&mut self) {
        self.psb = None;
        self.processors.clear();
        self.positions.clear();
    }

    /// Reset all positions.
    pub fn reset_positions(&mut self) {
        self.positions.clear();
    }
}

impl Default for ImsRuntime {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions for DL/I operations

fn convert_ssa_op(op: SsaOperator) -> QualOp {
    match op {
        SsaOperator::Eq => QualOp::Eq,
        SsaOperator::Ne => QualOp::Ne,
        SsaOperator::Gt => QualOp::Gt,
        SsaOperator::Ge => QualOp::Ge,
        SsaOperator::Lt => QualOp::Lt,
        SsaOperator::Le => QualOp::Le,
    }
}

/// Execute GU (Get Unique) call.
fn execute_gu(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Reset position for GU
    position.current_id = 0;
    position.parent_id = 0;
    position.path.clear();

    if call.ssas.is_empty() {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Navigate through SSAs
    let mut current_parent = 0u64;

    for (i, ssa) in call.ssas.iter().enumerate() {
        let is_last = i == call.ssas.len() - 1;

        // Check segment accessibility
        if !pcb.is_segment_accessible(&ssa.segment_name) {
            pcb.set_status(StatusCode::GE);
            return Ok(DliResult::not_found());
        }

        // Find segment
        let record = if current_parent == 0 {
            // Looking for root
            if let Some(ref qual) = ssa.qualification {
                store.find_root_qualified(
                    &ssa.segment_name,
                    &qual.field_name,
                    &qual.value,
                    convert_ssa_op(qual.operator),
                )
            } else {
                store.find_root(&ssa.segment_name)
            }
        } else {
            // Looking for child
            if let Some(ref qual) = ssa.qualification {
                store.find_child_qualified(
                    current_parent,
                    &ssa.segment_name,
                    &qual.field_name,
                    &qual.value,
                    convert_ssa_op(qual.operator),
                )
            } else {
                store.find_first_child(current_parent, &ssa.segment_name)
            }
        };

        match record {
            Some(rec) => {
                position.path.push(rec.segment_name.clone());
                current_parent = rec.id;

                if is_last {
                    // Found target segment
                    position.current_id = rec.id;
                    position.parent_id = rec.parent_id;
                    pcb.set_status(StatusCode::Ok);
                    return Ok(DliResult::ok(rec.data.clone(), &rec.segment_name));
                }
            }
            None => {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }
        }
    }

    pcb.set_status(StatusCode::GE);
    Ok(DliResult::not_found())
}

/// Execute GN (Get Next) call.
fn execute_gn(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    let target_segment = call.ssas.last().map(|s| s.segment_name.as_str());

    // If no current position, start from beginning
    if position.current_id == 0 {
        // Find first matching segment
        let record = if let Some(seg_name) = target_segment {
            if !pcb.is_segment_accessible(seg_name) {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }

            // Check qualification
            if let Some(ssa) = call.ssas.last() {
                if let Some(ref qual) = ssa.qualification {
                    store.find_root_qualified(
                        seg_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_root(seg_name)
                }
            } else {
                store.find_root(seg_name)
            }
        } else {
            // No segment specified, get first root
            store.first_root()
        };

        match record {
            Some(rec) => {
                position.current_id = rec.id;
                position.parent_id = rec.parent_id;
                position.path = vec![rec.segment_name.clone()];
                pcb.set_status(StatusCode::Ok);
                return Ok(DliResult::ok(rec.data.clone(), &rec.segment_name));
            }
            None => {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }
        }
    }

    // Get next from current position
    let record = store.get_next(position.current_id, target_segment);

    // Check qualification if present
    let record = if let Some(ssa) = call.ssas.last() {
        if let Some(ref qual) = ssa.qualification {
            // Need to keep searching until we find a match or hit end
            let mut current = record;
            while let Some(rec) = current {
                if rec.get_key(&qual.field_name)
                    .map(|v| convert_ssa_op(qual.operator).matches(v, &qual.value))
                    .unwrap_or(false)
                {
                    break;
                }
                current = store.get_next(rec.id, target_segment);
            }
            current
        } else {
            record
        }
    } else {
        record
    };

    match record {
        Some(rec) => {
            // Check accessibility
            if !pcb.is_segment_accessible(&rec.segment_name) {
                // Skip and continue
                position.current_id = rec.id;
                return execute_gn(store, call, pcb, position);
            }

            position.current_id = rec.id;
            position.parent_id = rec.parent_id;
            position.path.push(rec.segment_name.clone());
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(rec.data.clone(), &rec.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GB);
            Ok(DliResult::end_of_db())
        }
    }
}

/// Execute GNP (Get Next within Parent) call.
fn execute_gnp(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have established parentage
    if position.current_id == 0 {
        pcb.set_status(StatusCode::GP);
        return Ok(DliResult::error(StatusCode::GP));
    }

    let target_segment = call.ssas.last().map(|s| s.segment_name.as_str());

    // Determine parent - if current is a root segment (parent_id == 0),
    // then current IS the parent for GNP purposes
    let (parent_id, search_from) = if position.parent_id == 0 {
        // Current is root, search from it as parent
        (position.current_id, position.current_id)
    } else {
        // Current is not root, use its parent
        (position.parent_id, position.current_id)
    };

    // If we're AT the parent (not a child), get the first matching child
    let record = if search_from == parent_id {
        // We're positioned at the parent, find first child
        if let Some(seg_name) = target_segment {
            store.find_first_child(parent_id, seg_name)
        } else {
            store.find_children(parent_id, None).into_iter().next()
        }
    } else {
        // We're positioned at a child, get next within parent
        store.get_next_within_parent(search_from, parent_id, target_segment)
    };

    match record {
        Some(rec) => {
            if !pcb.is_segment_accessible(&rec.segment_name) {
                position.current_id = rec.id;
                return execute_gnp(store, call, pcb, position);
            }

            position.current_id = rec.id;
            // Keep parent_id pointing to the boundary parent
            if position.parent_id == 0 {
                position.parent_id = parent_id;
            }
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(rec.data.clone(), &rec.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GE);
            Ok(DliResult::not_found())
        }
    }
}

/// Execute GHU (Get Hold Unique) call.
fn execute_ghu(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Same as GU but marks for hold
    let result = execute_gu(store, call, pcb, position)?;

    // Position already set, so we're "holding" the current record
    Ok(result)
}

/// Execute GHN (Get Hold Next) call.
fn execute_ghn(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    execute_gn(store, call, pcb, position)
}

/// Execute GHNP (Get Hold Next within Parent) call.
fn execute_ghnp(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    execute_gnp(store, call, pcb, position)
}

/// Execute ISRT (Insert) call.
fn execute_isrt(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    if call.ssas.is_empty() {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    let last_ssa = call.ssas.last().unwrap();

    // Check permission
    if !pcb.can_operate(&last_ssa.segment_name, crate::psb::Operation::Insert) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Determine parent
    let parent_id = if call.ssas.len() == 1 {
        // Root insert
        0
    } else {
        // Navigate to parent
        let mut current_parent = 0u64;

        for ssa in call.ssas.iter().take(call.ssas.len() - 1) {
            let record = if current_parent == 0 {
                if let Some(ref qual) = ssa.qualification {
                    store.find_root_qualified(
                        &ssa.segment_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_root(&ssa.segment_name)
                }
            } else {
                if let Some(ref qual) = ssa.qualification {
                    store.find_child_qualified(
                        current_parent,
                        &ssa.segment_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_first_child(current_parent, &ssa.segment_name)
                }
            };

            match record {
                Some(rec) => current_parent = rec.id,
                None => {
                    pcb.set_status(StatusCode::GE);
                    return Ok(DliResult::not_found());
                }
            }
        }

        current_parent
    };

    // Build keys from qualification
    let mut keys = HashMap::new();
    if let Some(ref qual) = last_ssa.qualification {
        keys.insert(qual.field_name.to_uppercase(), qual.value.clone());
    }

    // Check for duplicate key
    if !keys.is_empty() {
        if let Some((field, value)) = keys.iter().next() {
            if store.duplicate_key_exists(&last_ssa.segment_name, parent_id, field, value) {
                pcb.set_status(StatusCode::II);
                return Ok(DliResult::error(StatusCode::II));
            }
        }
    }

    // Insert
    let id = store.insert_with_keys(parent_id, &last_ssa.segment_name, call.io_area.clone(), keys);

    match id {
        Some(id) => {
            position.current_id = id;
            position.parent_id = parent_id;
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(vec![], &last_ssa.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GE);
            Ok(DliResult::not_found())
        }
    }
}

/// Execute DLET (Delete) call.
fn execute_dlet(
    store: &mut HierarchicalStore,
    _call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have prior Get Hold
    if position.current_id == 0 {
        pcb.set_status(StatusCode::DJ);
        return Ok(DliResult::error(StatusCode::DJ));
    }

    // Get segment name for permission check
    let segment_name = store
        .get(position.current_id)
        .map(|r| r.segment_name.clone())
        .unwrap_or_default();

    if !pcb.can_operate(&segment_name, crate::psb::Operation::Delete) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Delete (cascades to children)
    if store.delete(position.current_id) {
        position.current_id = 0;
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &segment_name))
    } else {
        pcb.set_status(StatusCode::AK);
        Ok(DliResult::error(StatusCode::AK))
    }
}

/// Execute REPL (Replace) call.
fn execute_repl(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have prior Get Hold
    if position.current_id == 0 {
        pcb.set_status(StatusCode::DJ);
        return Ok(DliResult::error(StatusCode::DJ));
    }

    // Get segment info
    let (segment_name, old_keys) = store
        .get(position.current_id)
        .map(|r| (r.segment_name.clone(), r.keys.clone()))
        .unwrap_or_default();

    if !pcb.can_operate(&segment_name, crate::psb::Operation::Replace) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Build new keys from I/O area if needed
    // For now, preserve old keys (key change check would compare)
    let new_keys = old_keys.clone();

    // Check key change (simplified - in real impl, would parse segment to get key field)
    if let Some((key_field, _)) = old_keys.iter().next() {
        if store.key_changed(position.current_id, &new_keys, key_field) {
            pcb.set_status(StatusCode::RX);
            return Ok(DliResult::error(StatusCode::RX));
        }
    }

    // Update
    if store.update_with_keys(position.current_id, call.io_area.clone(), new_keys) {
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &segment_name))
    } else {
        pcb.set_status(StatusCode::AK);
        Ok(DliResult::error(StatusCode::AK))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dli::Ssa;
    use crate::psb::{ProcessingOptions, SensitiveSegment};

    fn create_test_runtime() -> ImsRuntime {
        let mut runtime = ImsRuntime::new();

        // Create PSB
        let mut psb = ProgramSpecBlock::new("TESTPSB", crate::psb::PsbLanguage::Cobol);
        let mut pcb = ProgramCommBlock::new_db("TESTDB", ProcessingOptions::from_str("A"), 50);
        pcb.add_senseg(SensitiveSegment::new("CUSTOMER", "", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ORDER", "CUSTOMER", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ITEM", "ORDER", ProcessingOptions::from_str("A")));
        psb.add_pcb(pcb);

        runtime.schedule_psb(psb).unwrap();

        // Load test data
        let store = runtime.get_store("TESTDB");

        let cust1 = store.insert_root("CUSTOMER", b"Customer One".to_vec());
        store.get_mut(cust1).unwrap().keys.insert("CUSTNO".to_string(), b"C001".to_vec());

        let ord1 = store.insert_child(cust1, "ORDER", b"Order 1-1".to_vec()).unwrap();
        store.get_mut(ord1).unwrap().keys.insert("ORDNO".to_string(), b"O001".to_vec());

        let item1 = store.insert_child(ord1, "ITEM", b"Item 1-1-1".to_vec()).unwrap();
        store.get_mut(item1).unwrap().keys.insert("ITEMNO".to_string(), b"I001".to_vec());

        store.insert_child(ord1, "ITEM", b"Item 1-1-2".to_vec()).unwrap();

        let ord2 = store.insert_child(cust1, "ORDER", b"Order 1-2".to_vec()).unwrap();
        store.get_mut(ord2).unwrap().keys.insert("ORDNO".to_string(), b"O002".to_vec());

        let cust2 = store.insert_root("CUSTOMER", b"Customer Two".to_vec());
        store.get_mut(cust2).unwrap().keys.insert("CUSTNO".to_string(), b"C002".to_vec());

        runtime
    }

    #[test]
    fn test_gu_unqualified() {
        let mut runtime = create_test_runtime();

        let result = runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer One");
    }

    #[test]
    fn test_gu_qualified() {
        let mut runtime = create_test_runtime();

        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Two");
    }

    #[test]
    fn test_gu_path() {
        let mut runtime = create_test_runtime();

        let result = runtime.gu(0, vec![
            Ssa::unqualified("CUSTOMER"),
            Ssa::unqualified("ORDER"),
        ]).unwrap();

        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-1");
    }

    #[test]
    fn test_gu_not_found() {
        let mut runtime = create_test_runtime();

        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C999");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_gn_sequential() {
        let mut runtime = create_test_runtime();

        // First GN starts at beginning
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer One");

        // Next GN gets second customer
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Two");

        // Next GN hits end
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert_eq!(result.status, StatusCode::GB);
    }

    #[test]
    fn test_gnp() {
        let mut runtime = create_test_runtime();

        // First establish position at CUSTOMER
        runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();

        // GNP to get children
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-1");

        // Get second order
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-2");

        // No more orders under this customer
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert_eq!(result.status, StatusCode::GE);
    }

    #[test]
    fn test_isrt() {
        let mut runtime = create_test_runtime();

        // Insert new customer
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        let result = runtime.isrt(0, vec![ssa], b"Customer Three".to_vec()).unwrap();
        assert!(result.is_ok());

        // Verify it exists
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Three");
    }

    #[test]
    fn test_isrt_duplicate() {
        let mut runtime = create_test_runtime();

        // Try to insert duplicate
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C001");
        let result = runtime.isrt(0, vec![ssa], b"Duplicate".to_vec()).unwrap();
        assert_eq!(result.status, StatusCode::II);
    }

    #[test]
    fn test_dlet() {
        let mut runtime = create_test_runtime();

        // Get hold
        let call = DliCall::new(DliFunction::GHU, 0)
            .with_ssa(Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002"));
        runtime.execute(&call).unwrap();

        // Delete
        let call = DliCall::new(DliFunction::DLET, 0);
        let result = runtime.execute(&call).unwrap();
        assert!(result.is_ok());

        // Verify gone
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_dlet_no_hold() {
        let mut runtime = create_test_runtime();

        // Try delete without hold
        let call = DliCall::new(DliFunction::DLET, 0);
        let result = runtime.execute(&call).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_repl() {
        let mut runtime = create_test_runtime();

        // Get hold
        let call = DliCall::new(DliFunction::GHU, 0)
            .with_ssa(Ssa::unqualified("CUSTOMER"));
        runtime.execute(&call).unwrap();

        // Replace
        let call = DliCall::new(DliFunction::REPL, 0)
            .with_io_area(b"Updated Customer One".to_vec());
        let result = runtime.execute(&call).unwrap();
        assert!(result.is_ok());

        // Verify updated
        let result = runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert_eq!(result.segment_data.unwrap(), b"Updated Customer One");
    }

    #[test]
    fn test_repl_no_hold() {
        let mut runtime = create_test_runtime();

        // Try replace without hold
        let call = DliCall::new(DliFunction::REPL, 0)
            .with_io_area(b"Update".to_vec());
        let result = runtime.execute(&call).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_terminate() {
        let mut runtime = create_test_runtime();

        // Do some operations
        runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();

        // Terminate
        runtime.terminate();

        assert!(runtime.get_psb().is_none());
    }
}
