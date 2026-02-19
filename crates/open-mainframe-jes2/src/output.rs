//! JES2 Output Processing — SYSOUT routing, output descriptors, and disposition.
//!
//! Implements JES2 output processing pipeline:
//! - **SYSOUT class routing**: Route output to destinations by class (A-Z, 0-9).
//! - **Output descriptors**: Named specifications for output processing (OUTPUT JCL).
//! - **Destination routing**: Local, remote node, and userid-based routing.
//! - **Output disposition**: WRITE, HOLD, KEEP, PURGE lifecycle actions.
//! - **MSGCLASS routing**: Job log and JES2 messages to a SYSOUT class.
//! - **Output groups**: Group related SYSOUT datasets for processing together.

use crate::job::JobId;
use crate::spool::SpoolManager;

// ---------------------------------------------------------------------------
//  Output disposition
// ---------------------------------------------------------------------------

/// Disposition actions for SYSOUT output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputDisposition {
    /// Write (print/punch) the output and then apply the abnormal disposition.
    Write,
    /// Hold the output for later retrieval.
    Hold,
    /// Keep the output on the spool (do not purge).
    Keep,
    /// Purge the output from the spool.
    Purge,
}

impl std::fmt::Display for OutputDisposition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Write => write!(f, "WRITE"),
            Self::Hold => write!(f, "HOLD"),
            Self::Keep => write!(f, "KEEP"),
            Self::Purge => write!(f, "PURGE"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Destination
// ---------------------------------------------------------------------------

/// Routing destination for SYSOUT output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Destination {
    /// Local output (default — processed on this system).
    Local,
    /// Remote node (NJE destination), e.g. `NODE01`.
    Node(String),
    /// Remote node and userid, e.g. `NODE01.USER01`.
    NodeUser(String, String),
    /// Named remote destination (RMTnnnn), e.g. `RMT001`.
    Remote(String),
}

impl Default for Destination {
    fn default() -> Self {
        Self::Local
    }
}

impl std::fmt::Display for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local => write!(f, "LOCAL"),
            Self::Node(n) => write!(f, "{n}"),
            Self::NodeUser(n, u) => write!(f, "{n}.{u}"),
            Self::Remote(r) => write!(f, "{r}"),
        }
    }
}

/// Parse a destination string.
///
/// Supports: `LOCAL`, `RMTnnn`, `NODE`, `NODE.USER`.
pub fn parse_destination(s: &str) -> Destination {
    let s = s.trim().to_uppercase();
    if s.is_empty() || s == "LOCAL" {
        return Destination::Local;
    }
    if s.starts_with("RMT") {
        return Destination::Remote(s);
    }
    if let Some((node, user)) = s.split_once('.') {
        Destination::NodeUser(node.to_string(), user.to_string())
    } else {
        Destination::Node(s)
    }
}

// ---------------------------------------------------------------------------
//  Output Descriptor
// ---------------------------------------------------------------------------

/// An OUTPUT JCL statement — named output processing specification.
///
/// Corresponds to `//name OUTPUT ...` in JCL.
#[derive(Debug, Clone)]
pub struct OutputDescriptor {
    /// Descriptor name (from JCL label, e.g. "OUT1").
    pub name: String,
    /// SYSOUT class override (if specified).
    pub class: Option<char>,
    /// Destination routing.
    pub dest: Destination,
    /// Normal completion disposition.
    pub normal_disp: OutputDisposition,
    /// Abnormal completion disposition.
    pub abnormal_disp: OutputDisposition,
    /// Forms name (paper forms for printing).
    pub forms: Option<String>,
    /// FCB (Forms Control Buffer) image name.
    pub fcb: Option<String>,
    /// UCS (Universal Character Set) name.
    pub ucs: Option<String>,
    /// JESDS — which JES datasets to include: ALL, LOG, MSG, or NONE.
    pub jesds: JesDatasets,
    /// Copies to produce.
    pub copies: u16,
}

impl Default for OutputDescriptor {
    fn default() -> Self {
        Self {
            name: String::new(),
            class: None,
            dest: Destination::Local,
            normal_disp: OutputDisposition::Write,
            abnormal_disp: OutputDisposition::Write,
            forms: None,
            fcb: None,
            ucs: None,
            jesds: JesDatasets::All,
            copies: 1,
        }
    }
}

/// Which JES-managed datasets to include in output processing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JesDatasets {
    /// All JES datasets (job log + JES messages + SYSOUT).
    All,
    /// Job log only.
    Log,
    /// JES messages only.
    Msg,
    /// No JES datasets.
    None,
}

impl std::fmt::Display for JesDatasets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::All => write!(f, "ALL"),
            Self::Log => write!(f, "LOG"),
            Self::Msg => write!(f, "MSG"),
            Self::None => write!(f, "NONE"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Output Group
// ---------------------------------------------------------------------------

/// A group of related SYSOUT datasets processed together.
#[derive(Debug, Clone)]
pub struct OutputGroup {
    /// Group identifier.
    pub id: u32,
    /// Owning job.
    pub job_id: JobId,
    /// Spool dataset keys in this group.
    pub spool_keys: Vec<u64>,
    /// Output descriptor applied to this group.
    pub descriptor: OutputDescriptor,
    /// Current disposition state.
    pub disposition: OutputDisposition,
    /// Whether this group has been processed (written/printed).
    pub processed: bool,
}

// ---------------------------------------------------------------------------
//  Output Processor
// ---------------------------------------------------------------------------

/// Manages JES2 output processing for jobs.
///
/// Tracks output descriptors, groups SYSOUT datasets, applies dispositions,
/// and routes output to destinations.
#[derive(Debug, Clone)]
pub struct OutputProcessor {
    /// Output groups indexed by group ID.
    groups: Vec<OutputGroup>,
    /// Next group ID.
    next_group_id: u32,
}

impl Default for OutputProcessor {
    fn default() -> Self {
        Self::new()
    }
}

impl OutputProcessor {
    /// Create a new output processor.
    pub fn new() -> Self {
        Self {
            groups: Vec::new(),
            next_group_id: 1,
        }
    }

    /// Create an output group for a job's SYSOUT datasets.
    ///
    /// Gathers all spool datasets for the job from the spool manager and
    /// creates a group with the given output descriptor.
    pub fn create_group(
        &mut self,
        job_id: JobId,
        spool: &SpoolManager,
        descriptor: OutputDescriptor,
    ) -> u32 {
        let spool_keys: Vec<u64> = spool
            .list_for_job(job_id)
            .into_iter()
            .map(|ds| ds.key)
            .collect();

        let id = self.next_group_id;
        self.next_group_id += 1;

        let disposition = descriptor.normal_disp;
        self.groups.push(OutputGroup {
            id,
            job_id,
            spool_keys,
            descriptor,
            disposition,
            processed: false,
        });
        id
    }

    /// Create an output group for a specific SYSOUT class from a job.
    ///
    /// Only includes spool datasets matching the given SYSOUT class.
    pub fn create_group_for_class(
        &mut self,
        job_id: JobId,
        spool: &SpoolManager,
        sysout_class: char,
        descriptor: OutputDescriptor,
    ) -> u32 {
        let spool_keys: Vec<u64> = spool
            .list_for_job(job_id)
            .into_iter()
            .filter(|ds| ds.sysout_class == sysout_class)
            .map(|ds| ds.key)
            .collect();

        let id = self.next_group_id;
        self.next_group_id += 1;

        let disposition = descriptor.normal_disp;
        self.groups.push(OutputGroup {
            id,
            job_id,
            spool_keys,
            descriptor,
            disposition,
            processed: false,
        });
        id
    }

    /// Set the disposition for a group (e.g., operator holds output).
    pub fn set_disposition(&mut self, group_id: u32, disp: OutputDisposition) -> bool {
        if let Some(group) = self.groups.iter_mut().find(|g| g.id == group_id) {
            group.disposition = disp;
            true
        } else {
            false
        }
    }

    /// Process an output group — marks it as processed.
    ///
    /// In a real system this would drive printing/punching; here we just
    /// mark the group as written and apply the post-processing disposition.
    pub fn process_group(&mut self, group_id: u32) -> Option<OutputDisposition> {
        let group = self.groups.iter_mut().find(|g| g.id == group_id)?;
        if group.disposition == OutputDisposition::Hold {
            // Held output is not processed until released.
            return Some(OutputDisposition::Hold);
        }
        group.processed = true;
        Some(group.disposition)
    }

    /// Release a held output group, changing its disposition to WRITE.
    pub fn release_held(&mut self, group_id: u32) -> bool {
        if let Some(group) = self.groups.iter_mut().find(|g| g.id == group_id) {
            if group.disposition == OutputDisposition::Hold {
                group.disposition = OutputDisposition::Write;
                return true;
            }
        }
        false
    }

    /// Purge all output groups for a job.
    ///
    /// Returns the spool keys that should be deallocated.
    pub fn purge_job(&mut self, job_id: JobId) -> Vec<u64> {
        let mut keys = Vec::new();
        self.groups.retain(|g| {
            if g.job_id == job_id {
                keys.extend(&g.spool_keys);
                false
            } else {
                true
            }
        });
        keys
    }

    /// Get an output group by ID.
    pub fn get_group(&self, group_id: u32) -> Option<&OutputGroup> {
        self.groups.iter().find(|g| g.id == group_id)
    }

    /// List all output groups for a job.
    pub fn groups_for_job(&self, job_id: JobId) -> Vec<&OutputGroup> {
        self.groups.iter().filter(|g| g.job_id == job_id).collect()
    }

    /// Count of output groups in the system.
    pub fn group_count(&self) -> usize {
        self.groups.len()
    }

    /// Count of held output groups.
    pub fn held_count(&self) -> usize {
        self.groups
            .iter()
            .filter(|g| g.disposition == OutputDisposition::Hold)
            .count()
    }

    /// Count of groups awaiting processing (WRITE disposition, not yet processed).
    pub fn pending_count(&self) -> usize {
        self.groups
            .iter()
            .filter(|g| g.disposition == OutputDisposition::Write && !g.processed)
            .count()
    }

    /// Apply MSGCLASS routing — creates an output group for the job log.
    ///
    /// The job log is routed to the MSGCLASS SYSOUT class. This creates
    /// an output group containing only spool datasets matching that class.
    pub fn route_msgclass(
        &mut self,
        job_id: JobId,
        msgclass: char,
        spool: &SpoolManager,
    ) -> u32 {
        let desc = OutputDescriptor {
            class: Some(msgclass),
            jesds: JesDatasets::Log,
            ..Default::default()
        };
        self.create_group_for_class(job_id, spool, msgclass, desc)
    }

    /// Apply SYSOUT class and destination routing for a job.
    ///
    /// Creates an output group for datasets in the given SYSOUT class,
    /// routed to the specified destination.
    pub fn route_sysout(
        &mut self,
        job_id: JobId,
        sysout_class: char,
        dest: Destination,
        spool: &SpoolManager,
    ) -> u32 {
        let desc = OutputDescriptor {
            class: Some(sysout_class),
            dest,
            ..Default::default()
        };
        self.create_group_for_class(job_id, spool, sysout_class, desc)
    }

    /// Apply OUTDISP processing for a job at completion.
    ///
    /// Given normal and abnormal dispositions, selects the appropriate one
    /// based on whether the job completed normally (max_rc <= threshold)
    /// and applies it to all output groups for the job.
    pub fn apply_outdisp(
        &mut self,
        job_id: JobId,
        normal_ok: bool,
        normal_disp: OutputDisposition,
        abnormal_disp: OutputDisposition,
    ) {
        let disp = if normal_ok {
            normal_disp
        } else {
            abnormal_disp
        };
        for group in &mut self.groups {
            if group.job_id == job_id && !group.processed {
                group.disposition = disp;
            }
        }
    }

    /// Generate a display report for output groups.
    pub fn display_groups(&self, job_id: JobId) -> Vec<String> {
        let mut lines = Vec::new();
        for group in &self.groups {
            if group.job_id != job_id {
                continue;
            }
            let status = if group.processed {
                "PROCESSED"
            } else {
                match group.disposition {
                    OutputDisposition::Hold => "HELD",
                    OutputDisposition::Write => "AWAITING",
                    OutputDisposition::Keep => "KEPT",
                    OutputDisposition::Purge => "PURGE",
                }
            };
            let class_str = match group.descriptor.class {
                Some(c) => format!("CLASS={c}"),
                None => "CLASS=*".to_string(),
            };
            let dest_str = format!("DEST={}", group.descriptor.dest);
            lines.push(format!(
                "$HASP890 GROUP{:03} {},{},{},DATASETS={},COPIES={}",
                group.id,
                status,
                class_str,
                dest_str,
                group.spool_keys.len(),
                group.descriptor.copies,
            ));
        }
        lines
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spool::SpoolManager;

    fn setup_spool() -> (SpoolManager, JobId) {
        let mut spool = SpoolManager::new();
        let job_id = JobId(1);
        let k1 = spool.allocate(job_id, "SYSPRINT", "STEP1", 'A');
        spool.write(k1, "PRINT LINE 1").unwrap();
        spool.write(k1, "PRINT LINE 2").unwrap();

        let k2 = spool.allocate(job_id, "SYSOUT", "STEP1", 'A');
        spool.write(k2, "SYSOUT LINE 1").unwrap();

        let k3 = spool.allocate(job_id, "JESMSGLG", "JES2", 'X');
        spool.write(k3, "JOB LOG LINE").unwrap();

        (spool, job_id)
    }

    // ─── J103.1: SYSOUT Class Routing ───

    #[test]
    fn test_parse_destination_local() {
        assert_eq!(parse_destination(""), Destination::Local);
        assert_eq!(parse_destination("LOCAL"), Destination::Local);
        assert_eq!(parse_destination("local"), Destination::Local);
    }

    #[test]
    fn test_parse_destination_remote() {
        assert_eq!(
            parse_destination("RMT001"),
            Destination::Remote("RMT001".to_string())
        );
    }

    #[test]
    fn test_parse_destination_node() {
        assert_eq!(
            parse_destination("NODE01"),
            Destination::Node("NODE01".to_string())
        );
    }

    #[test]
    fn test_parse_destination_node_user() {
        assert_eq!(
            parse_destination("NODE01.USER01"),
            Destination::NodeUser("NODE01".to_string(), "USER01".to_string())
        );
    }

    #[test]
    fn test_create_output_group() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid = proc.create_group(job_id, &spool, desc);
        assert_eq!(gid, 1);

        let group = proc.get_group(gid).unwrap();
        assert_eq!(group.spool_keys.len(), 3); // All 3 datasets
        assert_eq!(group.disposition, OutputDisposition::Write);
    }

    #[test]
    fn test_create_group_for_class() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid = proc.create_group_for_class(job_id, &spool, 'A', desc);

        let group = proc.get_group(gid).unwrap();
        assert_eq!(group.spool_keys.len(), 2); // Only class A datasets
    }

    #[test]
    fn test_sysout_routing_with_destination() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let gid = proc.route_sysout(
            job_id,
            'A',
            Destination::Remote("RMT001".to_string()),
            &spool,
        );

        let group = proc.get_group(gid).unwrap();
        assert_eq!(group.descriptor.dest, Destination::Remote("RMT001".to_string()));
        assert_eq!(group.descriptor.class, Some('A'));
        assert_eq!(group.spool_keys.len(), 2);
    }

    #[test]
    fn test_msgclass_routing() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let gid = proc.route_msgclass(job_id, 'X', &spool);

        let group = proc.get_group(gid).unwrap();
        assert_eq!(group.descriptor.class, Some('X'));
        assert_eq!(group.descriptor.jesds, JesDatasets::Log);
        assert_eq!(group.spool_keys.len(), 1); // Only the JESMSGLG dataset (class X)
    }

    // ─── J103.1: Output Disposition Processing ───

    #[test]
    fn test_outdisp_normal_completion() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor {
            normal_disp: OutputDisposition::Write,
            abnormal_disp: OutputDisposition::Hold,
            ..Default::default()
        };
        proc.create_group(job_id, &spool, desc);

        // Job completed normally.
        proc.apply_outdisp(job_id, true, OutputDisposition::Write, OutputDisposition::Hold);

        let groups = proc.groups_for_job(job_id);
        assert_eq!(groups[0].disposition, OutputDisposition::Write);
    }

    #[test]
    fn test_outdisp_abnormal_completion() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        proc.create_group(job_id, &spool, desc);

        // Job completed abnormally.
        proc.apply_outdisp(job_id, false, OutputDisposition::Write, OutputDisposition::Hold);

        let groups = proc.groups_for_job(job_id);
        assert_eq!(groups[0].disposition, OutputDisposition::Hold);
    }

    #[test]
    fn test_outdisp_write_then_purge() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid = proc.create_group(job_id, &spool, desc);

        // Process (write) the group.
        let result = proc.process_group(gid);
        assert_eq!(result, Some(OutputDisposition::Write));
        assert!(proc.get_group(gid).unwrap().processed);
    }

    #[test]
    fn test_hold_and_release() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid = proc.create_group(job_id, &spool, desc);

        // Hold the output.
        proc.set_disposition(gid, OutputDisposition::Hold);
        assert_eq!(proc.held_count(), 1);

        // Try to process — should stay held.
        let result = proc.process_group(gid);
        assert_eq!(result, Some(OutputDisposition::Hold));
        assert!(!proc.get_group(gid).unwrap().processed);

        // Release.
        assert!(proc.release_held(gid));
        assert_eq!(proc.held_count(), 0);
        assert_eq!(proc.get_group(gid).unwrap().disposition, OutputDisposition::Write);
    }

    #[test]
    fn test_purge_job_output() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        proc.create_group(job_id, &spool, desc);

        assert_eq!(proc.group_count(), 1);
        let keys = proc.purge_job(job_id);
        assert_eq!(keys.len(), 3); // All 3 spool datasets
        assert_eq!(proc.group_count(), 0);
    }

    #[test]
    fn test_pending_count() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid1 = proc.create_group(job_id, &spool, desc.clone());

        let mut held_desc = OutputDescriptor::default();
        held_desc.normal_disp = OutputDisposition::Hold;
        proc.create_group(job_id, &spool, held_desc);
        proc.apply_outdisp(
            job_id,
            true,
            OutputDisposition::Write,
            OutputDisposition::Hold,
        );

        // Only group 1 should be pending (WRITE, not processed).
        // Group 2 got WRITE from apply_outdisp since normal_ok=true.
        assert_eq!(proc.pending_count(), 2);

        proc.process_group(gid1);
        assert_eq!(proc.pending_count(), 1);
    }

    #[test]
    fn test_display_groups() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let mut desc = OutputDescriptor::default();
        desc.class = Some('A');
        desc.dest = Destination::Remote("RMT001".to_string());
        proc.create_group(job_id, &spool, desc);

        let lines = proc.display_groups(job_id);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("AWAITING"));
        assert!(lines[0].contains("CLASS=A"));
        assert!(lines[0].contains("DEST=RMT001"));
    }

    #[test]
    fn test_output_descriptor_default() {
        let desc = OutputDescriptor::default();
        assert_eq!(desc.normal_disp, OutputDisposition::Write);
        assert_eq!(desc.abnormal_disp, OutputDisposition::Write);
        assert_eq!(desc.copies, 1);
        assert_eq!(desc.jesds, JesDatasets::All);
        assert_eq!(desc.dest, Destination::Local);
    }

    #[test]
    fn test_destination_display() {
        assert_eq!(Destination::Local.to_string(), "LOCAL");
        assert_eq!(Destination::Node("SYS1".to_string()).to_string(), "SYS1");
        assert_eq!(
            Destination::NodeUser("SYS1".to_string(), "ADMIN".to_string()).to_string(),
            "SYS1.ADMIN"
        );
        assert_eq!(
            Destination::Remote("RMT003".to_string()).to_string(),
            "RMT003"
        );
    }

    #[test]
    fn test_disposition_display() {
        assert_eq!(OutputDisposition::Write.to_string(), "WRITE");
        assert_eq!(OutputDisposition::Hold.to_string(), "HOLD");
        assert_eq!(OutputDisposition::Keep.to_string(), "KEEP");
        assert_eq!(OutputDisposition::Purge.to_string(), "PURGE");
    }

    #[test]
    fn test_jesds_display() {
        assert_eq!(JesDatasets::All.to_string(), "ALL");
        assert_eq!(JesDatasets::Log.to_string(), "LOG");
        assert_eq!(JesDatasets::Msg.to_string(), "MSG");
        assert_eq!(JesDatasets::None.to_string(), "NONE");
    }

    #[test]
    fn test_multiple_jobs_output_groups() {
        let mut spool = SpoolManager::new();
        let job1 = JobId(1);
        let job2 = JobId(2);

        spool.allocate(job1, "SYSPRINT", "STEP1", 'A');
        spool.allocate(job2, "SYSPRINT", "STEP1", 'B');

        let mut proc = OutputProcessor::new();
        proc.create_group(job1, &spool, OutputDescriptor::default());
        proc.create_group(job2, &spool, OutputDescriptor::default());

        assert_eq!(proc.groups_for_job(job1).len(), 1);
        assert_eq!(proc.groups_for_job(job2).len(), 1);

        // Purge job1 only.
        proc.purge_job(job1);
        assert_eq!(proc.group_count(), 1);
        assert_eq!(proc.groups_for_job(job2).len(), 1);
    }

    #[test]
    fn test_release_non_held_returns_false() {
        let (spool, job_id) = setup_spool();
        let mut proc = OutputProcessor::new();

        let desc = OutputDescriptor::default();
        let gid = proc.create_group(job_id, &spool, desc);

        // Group is in WRITE disposition, not HOLD.
        assert!(!proc.release_held(gid));
    }

    #[test]
    fn test_output_descriptor_with_forms() {
        let desc = OutputDescriptor {
            name: "OUT1".to_string(),
            class: Some('A'),
            dest: Destination::Local,
            forms: Some("STD".to_string()),
            fcb: Some("STD1".to_string()),
            ucs: Some("PN".to_string()),
            copies: 3,
            ..Default::default()
        };
        assert_eq!(desc.forms.as_deref(), Some("STD"));
        assert_eq!(desc.fcb.as_deref(), Some("STD1"));
        assert_eq!(desc.ucs.as_deref(), Some("PN"));
        assert_eq!(desc.copies, 3);
    }
}
