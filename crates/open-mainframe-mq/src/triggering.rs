//! MQ Triggering — trigger monitors, process definitions, and initiation queues.
//!
//! Implements:
//! - **Trigger types** — FIRST, EVERY, DEPTH
//! - **Process definitions** — application start specifications
//! - **Trigger monitor** — monitors initiation queues and fires triggers
//! - **Dead-letter queue handler** — rule-based DLQ message processing

use crate::structures::{Mqtm, TriggerType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Process Definition
// ---------------------------------------------------------------------------

/// A process definition object (maps to DEFINE PROCESS).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessDefinition {
    /// Process name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Application type (CICS, IMS, Batch, etc.).
    pub appl_type: u32,
    /// Application ID (program name).
    pub appl_id: String,
    /// Environment data.
    pub env_data: String,
    /// User data.
    pub user_data: String,
}

impl Default for ProcessDefinition {
    fn default() -> Self {
        Self {
            name: String::new(),
            description: String::new(),
            appl_type: 0,
            appl_id: String::new(),
            env_data: String::new(),
            user_data: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Trigger configuration on a queue
// ---------------------------------------------------------------------------

/// Trigger configuration for a queue.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TriggerConfig {
    /// Trigger type.
    pub trigger_type: TriggerType,
    /// Associated process definition name.
    pub process_name: String,
    /// Initiation queue name.
    pub initiation_queue: String,
    /// Trigger data (passed to application).
    pub trigger_data: String,
    /// Trigger depth threshold (for DEPTH trigger type).
    pub trigger_depth: u32,
    /// Whether triggering is enabled.
    pub enabled: bool,
}

// ---------------------------------------------------------------------------
//  Trigger Monitor
// ---------------------------------------------------------------------------

/// The trigger monitor — collects triggered events and generates MQTM messages.
#[derive(Debug, Default)]
pub struct TriggerMonitor {
    /// Process definitions by name.
    processes: HashMap<String, ProcessDefinition>,
    /// Trigger configs by queue name.
    trigger_configs: HashMap<String, TriggerConfig>,
    /// Pending trigger messages (queue_name -> list of MQTM).
    pending_triggers: Vec<Mqtm>,
    /// Track previous depth for FIRST trigger (was queue empty before?).
    previous_depths: HashMap<String, u32>,
}

impl TriggerMonitor {
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a process.
    pub fn define_process(&mut self, process: ProcessDefinition) -> Result<(), String> {
        let name = process.name.to_uppercase();
        if self.processes.contains_key(&name) {
            return Err(format!("Process already exists: {name}"));
        }
        self.processes.insert(name, process);
        Ok(())
    }

    /// Delete a process.
    pub fn delete_process(&mut self, name: &str) -> Result<(), String> {
        let upper = name.to_uppercase();
        self.processes
            .remove(&upper)
            .map(|_| ())
            .ok_or_else(|| format!("Process not found: {upper}"))
    }

    /// Get a process definition.
    pub fn get_process(&self, name: &str) -> Option<&ProcessDefinition> {
        self.processes.get(&name.to_uppercase())
    }

    /// List all process names.
    pub fn list_processes(&self) -> Vec<String> {
        self.processes.keys().cloned().collect()
    }

    /// Configure triggering for a queue.
    pub fn set_trigger_config(&mut self, queue_name: &str, config: TriggerConfig) {
        self.trigger_configs
            .insert(queue_name.to_uppercase(), config);
    }

    /// Get trigger config for a queue.
    pub fn get_trigger_config(&self, queue_name: &str) -> Option<&TriggerConfig> {
        self.trigger_configs.get(&queue_name.to_uppercase())
    }

    /// Called when a message is put on a queue; checks if a trigger should fire.
    pub fn on_message_put(&mut self, queue_name: &str, new_depth: u32) {
        let upper = queue_name.to_uppercase();
        let config = match self.trigger_configs.get(&upper) {
            Some(c) if c.enabled => c.clone(),
            _ => return,
        };

        let previous = self.previous_depths.get(&upper).copied().unwrap_or(0);
        self.previous_depths.insert(upper.clone(), new_depth);

        let should_trigger = match config.trigger_type {
            TriggerType::None => false,
            TriggerType::First => previous == 0 && new_depth >= 1,
            TriggerType::Every => true,
            TriggerType::Depth => new_depth >= config.trigger_depth && previous < config.trigger_depth,
        };

        if should_trigger {
            let process = self.processes.get(&config.process_name.to_uppercase());
            let tm = Mqtm {
                queue_name: upper,
                process_name: config.process_name,
                trigger_data: config.trigger_data,
                appl_type: process.map(|p| p.appl_type).unwrap_or(0),
                appl_id: process.map(|p| p.appl_id.clone()).unwrap_or_default(),
                env_data: process.map(|p| p.env_data.clone()).unwrap_or_default(),
                user_data: process.map(|p| p.user_data.clone()).unwrap_or_default(),
            };
            self.pending_triggers.push(tm);
        }
    }

    /// Called when a message is got from a queue; updates depth tracking.
    pub fn on_message_get(&mut self, queue_name: &str, new_depth: u32) {
        let upper = queue_name.to_uppercase();
        self.previous_depths.insert(upper, new_depth);
    }

    /// Drain all pending trigger messages.
    pub fn drain_triggers(&mut self) -> Vec<Mqtm> {
        std::mem::take(&mut self.pending_triggers)
    }

    /// Number of pending triggers.
    pub fn pending_count(&self) -> usize {
        self.pending_triggers.len()
    }
}

// ---------------------------------------------------------------------------
//  DLQ Handler Rules
// ---------------------------------------------------------------------------

/// Action to take for a DLQ message.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DlqAction {
    /// Retry putting to the original destination.
    Retry,
    /// Forward to a different queue.
    Forward(String),
    /// Discard the message.
    Discard,
}

/// A DLQ handler rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DlqRule {
    /// Reason code to match (0 = any).
    pub reason: u32,
    /// Queue name pattern to match (empty = any).
    pub queue_pattern: String,
    /// Format to match (empty = any).
    pub format_pattern: String,
    /// Action to take.
    pub action: DlqAction,
}

/// DLQ handler — processes dead-letter queue messages based on rules.
#[derive(Debug, Default)]
pub struct DlqHandler {
    rules: Vec<DlqRule>,
}

impl DlqHandler {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a rule to the handler.
    pub fn add_rule(&mut self, rule: DlqRule) {
        self.rules.push(rule);
    }

    /// Find the matching action for a DLQ message.
    pub fn match_action(
        &self,
        reason: u32,
        queue_name: &str,
        format: &str,
    ) -> Option<&DlqAction> {
        for rule in &self.rules {
            let reason_match = rule.reason == 0 || rule.reason == reason;
            let queue_match =
                rule.queue_pattern.is_empty() || rule.queue_pattern == queue_name;
            let format_match =
                rule.format_pattern.is_empty() || rule.format_pattern == format;
            if reason_match && queue_match && format_match {
                return Some(&rule.action);
            }
        }
        None
    }

    /// Number of rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_process() {
        let mut tm = TriggerMonitor::new();
        tm.define_process(ProcessDefinition {
            name: "MYPROC".to_string(),
            appl_id: "MYPROG".to_string(),
            ..Default::default()
        })
        .unwrap();
        assert!(tm.get_process("MYPROC").is_some());
    }

    #[test]
    fn test_delete_process() {
        let mut tm = TriggerMonitor::new();
        tm.define_process(ProcessDefinition {
            name: "MYPROC".to_string(),
            ..Default::default()
        })
        .unwrap();
        tm.delete_process("MYPROC").unwrap();
        assert!(tm.get_process("MYPROC").is_none());
    }

    #[test]
    fn test_trigger_first() {
        let mut tm = TriggerMonitor::new();
        tm.define_process(ProcessDefinition {
            name: "PROC1".to_string(),
            appl_id: "APP1".to_string(),
            ..Default::default()
        })
        .unwrap();

        tm.set_trigger_config(
            "MY.Q",
            TriggerConfig {
                trigger_type: TriggerType::First,
                process_name: "PROC1".to_string(),
                initiation_queue: "SYSTEM.INIT.Q".to_string(),
                enabled: true,
                ..Default::default()
            },
        );

        // First message on empty queue => trigger.
        tm.on_message_put("MY.Q", 1);
        assert_eq!(tm.pending_count(), 1);

        // Second message => no trigger (queue not empty before).
        tm.on_message_put("MY.Q", 2);
        assert_eq!(tm.pending_count(), 1);

        let triggers = tm.drain_triggers();
        assert_eq!(triggers.len(), 1);
        assert_eq!(triggers[0].queue_name, "MY.Q");
    }

    #[test]
    fn test_trigger_every() {
        let mut tm = TriggerMonitor::new();
        tm.set_trigger_config(
            "MY.Q",
            TriggerConfig {
                trigger_type: TriggerType::Every,
                process_name: "PROC1".to_string(),
                enabled: true,
                ..Default::default()
            },
        );

        tm.on_message_put("MY.Q", 1);
        tm.on_message_put("MY.Q", 2);
        assert_eq!(tm.pending_count(), 2);
    }

    #[test]
    fn test_trigger_depth() {
        let mut tm = TriggerMonitor::new();
        tm.set_trigger_config(
            "MY.Q",
            TriggerConfig {
                trigger_type: TriggerType::Depth,
                trigger_depth: 3,
                process_name: "PROC1".to_string(),
                enabled: true,
                ..Default::default()
            },
        );

        tm.on_message_put("MY.Q", 1);
        tm.on_message_put("MY.Q", 2);
        assert_eq!(tm.pending_count(), 0);

        tm.on_message_put("MY.Q", 3);
        assert_eq!(tm.pending_count(), 1);

        // Already above threshold, no re-trigger.
        tm.on_message_put("MY.Q", 4);
        assert_eq!(tm.pending_count(), 1);
    }

    #[test]
    fn test_trigger_disabled() {
        let mut tm = TriggerMonitor::new();
        tm.set_trigger_config(
            "MY.Q",
            TriggerConfig {
                trigger_type: TriggerType::Every,
                enabled: false,
                ..Default::default()
            },
        );

        tm.on_message_put("MY.Q", 1);
        assert_eq!(tm.pending_count(), 0);
    }

    #[test]
    fn test_trigger_none() {
        let mut tm = TriggerMonitor::new();
        tm.set_trigger_config(
            "MY.Q",
            TriggerConfig {
                trigger_type: TriggerType::None,
                enabled: true,
                ..Default::default()
            },
        );

        tm.on_message_put("MY.Q", 1);
        assert_eq!(tm.pending_count(), 0);
    }

    #[test]
    fn test_dlq_handler_match() {
        let mut handler = DlqHandler::new();
        handler.add_rule(DlqRule {
            reason: 2035,
            queue_pattern: String::new(),
            format_pattern: String::new(),
            action: DlqAction::Discard,
        });
        handler.add_rule(DlqRule {
            reason: 0,
            queue_pattern: "RETRY.Q".to_string(),
            format_pattern: String::new(),
            action: DlqAction::Retry,
        });
        handler.add_rule(DlqRule {
            reason: 0,
            queue_pattern: String::new(),
            format_pattern: String::new(),
            action: DlqAction::Forward("ERROR.Q".to_string()),
        });

        assert_eq!(
            handler.match_action(2035, "ANY.Q", "MQSTR"),
            Some(&DlqAction::Discard)
        );
        assert_eq!(
            handler.match_action(2033, "RETRY.Q", "MQSTR"),
            Some(&DlqAction::Retry)
        );
        assert_eq!(
            handler.match_action(9999, "OTHER.Q", "MQHRF2"),
            Some(&DlqAction::Forward("ERROR.Q".to_string()))
        );
    }

    #[test]
    fn test_dlq_handler_no_match() {
        let handler = DlqHandler::new();
        assert!(handler.match_action(2035, "Q", "F").is_none());
    }
}
