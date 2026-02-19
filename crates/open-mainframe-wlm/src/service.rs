//! WLM Service Classes and Goals.
//!
//! A service class groups work with similar performance requirements.
//! Each service class has one or more performance goals (response time,
//! execution velocity, or discretionary).

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Goal types
// ---------------------------------------------------------------------------

/// Type of WLM performance goal.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum GoalType {
    /// Response time goal: percentage of work completing within a time target.
    ResponseTime {
        /// Target response time in seconds.
        target_seconds: f64,
        /// Percentage that must meet the target (e.g., 90.0 = 90%).
        percentile: f64,
    },
    /// Execution velocity: ratio of using vs. waiting for resources (1-100).
    Velocity(u8),
    /// Discretionary: work with no specific goal (lowest priority).
    Discretionary,
}

/// Importance level (1 = highest, 5 = lowest).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct Importance(pub u8);

impl Default for Importance {
    fn default() -> Self {
        Self(3)
    }
}

// ---------------------------------------------------------------------------
//  Service Goal
// ---------------------------------------------------------------------------

/// A performance goal for a service class period.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ServiceGoal {
    /// Goal type.
    pub goal: GoalType,
    /// Importance (1-5).
    pub importance: Importance,
    /// Duration of this period (in service units, 0 = single period).
    pub duration: u64,
}

// ---------------------------------------------------------------------------
//  Service Class
// ---------------------------------------------------------------------------

/// A WLM service class.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ServiceClass {
    /// Service class name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Performance periods (up to 8, executed sequentially as work ages).
    pub periods: Vec<ServiceGoal>,
    /// CPU-critical flag.
    pub cpu_critical: bool,
    /// Storage-critical flag.
    pub storage_critical: bool,
}

impl ServiceClass {
    /// Create a new service class with a single goal period.
    pub fn new(name: &str, goal: GoalType, importance: Importance) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            periods: vec![ServiceGoal {
                goal,
                importance,
                duration: 0,
            }],
            cpu_critical: false,
            storage_critical: false,
        }
    }

    /// Add an additional period.
    pub fn add_period(&mut self, goal: GoalType, importance: Importance, duration: u64) {
        self.periods.push(ServiceGoal {
            goal,
            importance,
            duration,
        });
    }

    /// Get the current goal (first period).
    pub fn primary_goal(&self) -> Option<&ServiceGoal> {
        self.periods.first()
    }
}

// ---------------------------------------------------------------------------
//  Service Policy
// ---------------------------------------------------------------------------

/// A named service policy containing service classes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ServicePolicy {
    /// Policy name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Service classes by name.
    pub service_classes: HashMap<String, ServiceClass>,
    /// Whether this policy is active.
    pub active: bool,
}

impl Default for ServicePolicy {
    fn default() -> Self {
        Self::new("DEFAULT")
    }
}

impl ServicePolicy {
    /// Create a new service policy.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            service_classes: HashMap::new(),
            active: false,
        }
    }

    /// Define a service class in this policy.
    pub fn define_class(&mut self, class: ServiceClass) -> Result<(), WlmError> {
        let name = class.name.clone();
        if self.service_classes.contains_key(&name) {
            return Err(WlmError::ClassExists(name));
        }
        self.service_classes.insert(name, class);
        Ok(())
    }

    /// Modify an existing service class.
    pub fn modify_class(&mut self, name: &str, class: ServiceClass) -> Result<(), WlmError> {
        let upper = name.to_uppercase();
        if !self.service_classes.contains_key(&upper) {
            return Err(WlmError::ClassNotFound(upper));
        }
        self.service_classes.insert(upper, class);
        Ok(())
    }

    /// Remove a service class.
    pub fn remove_class(&mut self, name: &str) -> Result<(), WlmError> {
        let upper = name.to_uppercase();
        self.service_classes
            .remove(&upper)
            .map(|_| ())
            .ok_or(WlmError::ClassNotFound(upper))
    }

    /// Look up a service class.
    pub fn lookup_class(&self, name: &str) -> Option<&ServiceClass> {
        self.service_classes.get(&name.to_uppercase())
    }

    /// Number of service classes.
    pub fn class_count(&self) -> usize {
        self.service_classes.len()
    }

    /// Activate this policy.
    pub fn activate(&mut self) {
        self.active = true;
    }

    /// List all service class names.
    pub fn list_classes(&self) -> Vec<String> {
        self.service_classes.keys().cloned().collect()
    }
}

// ---------------------------------------------------------------------------
//  Error
// ---------------------------------------------------------------------------

/// WLM error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum WlmError {
    #[error("Service class already exists: {0}")]
    ClassExists(String),
    #[error("Service class not found: {0}")]
    ClassNotFound(String),
    #[error("No matching classification rule")]
    NoMatch,
    #[error("WLM error: {0}")]
    Other(String),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_class_create() {
        let sc = ServiceClass::new("PRODCICS", GoalType::ResponseTime {
            target_seconds: 0.5,
            percentile: 95.0,
        }, Importance(1));
        assert_eq!(sc.name, "PRODCICS");
        assert_eq!(sc.periods.len(), 1);
    }

    #[test]
    fn test_service_class_multi_period() {
        let mut sc = ServiceClass::new("BATCH", GoalType::Velocity(50), Importance(3));
        sc.add_period(GoalType::Discretionary, Importance(5), 1000);
        assert_eq!(sc.periods.len(), 2);
    }

    #[test]
    fn test_service_policy_define() {
        let mut policy = ServicePolicy::new("PROD");
        let sc = ServiceClass::new("ONLINE", GoalType::Velocity(80), Importance(1));
        policy.define_class(sc).unwrap();
        assert_eq!(policy.class_count(), 1);
        assert!(policy.lookup_class("ONLINE").is_some());
    }

    #[test]
    fn test_service_policy_duplicate() {
        let mut policy = ServicePolicy::new("PROD");
        let sc = ServiceClass::new("ONLINE", GoalType::Velocity(80), Importance(1));
        policy.define_class(sc).unwrap();
        let sc2 = ServiceClass::new("ONLINE", GoalType::Discretionary, Importance(5));
        assert!(policy.define_class(sc2).is_err());
    }

    #[test]
    fn test_service_policy_remove() {
        let mut policy = ServicePolicy::new("PROD");
        let sc = ServiceClass::new("ONLINE", GoalType::Velocity(80), Importance(1));
        policy.define_class(sc).unwrap();
        policy.remove_class("ONLINE").unwrap();
        assert_eq!(policy.class_count(), 0);
    }

    #[test]
    fn test_service_policy_modify() {
        let mut policy = ServicePolicy::new("PROD");
        let sc = ServiceClass::new("ONLINE", GoalType::Velocity(80), Importance(1));
        policy.define_class(sc).unwrap();

        let sc2 = ServiceClass::new("ONLINE", GoalType::Velocity(90), Importance(1));
        policy.modify_class("ONLINE", sc2).unwrap();

        let looked = policy.lookup_class("ONLINE").unwrap();
        assert_eq!(looked.periods[0].goal, GoalType::Velocity(90));
    }

    #[test]
    fn test_service_policy_activate() {
        let mut policy = ServicePolicy::new("PROD");
        assert!(!policy.active);
        policy.activate();
        assert!(policy.active);
    }

    #[test]
    fn test_primary_goal() {
        let sc = ServiceClass::new("TSO", GoalType::ResponseTime {
            target_seconds: 1.0,
            percentile: 90.0,
        }, Importance(2));
        let goal = sc.primary_goal().unwrap();
        assert_eq!(goal.importance, Importance(2));
    }

    #[test]
    fn test_importance_default() {
        assert_eq!(Importance::default().0, 3);
    }

    #[test]
    fn test_importance_ordering() {
        assert!(Importance(1) < Importance(5));
    }
}
