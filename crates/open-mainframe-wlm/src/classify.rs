//! WLM Workload Classification Rules.
//!
//! Classification rules map incoming work requests to service classes
//! based on attributes like subsystem type, transaction name, user ID,
//! job class, and more.

use crate::service::{ServicePolicy, WlmError};

// ---------------------------------------------------------------------------
//  Work attributes
// ---------------------------------------------------------------------------

/// Attributes of a work request used for classification.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct WorkAttribute {
    /// Subsystem type (e.g., "CICS", "IMS", "JES2", "TSO", "DB2", "STC").
    pub subsystem_type: String,
    /// Subsystem name.
    pub subsystem_name: String,
    /// Transaction or job name.
    pub transaction_name: String,
    /// User ID.
    pub user_id: String,
    /// Accounting information.
    pub accounting: String,
    /// Job class (for batch).
    pub job_class: String,
    /// Service class override (if directly assigned).
    pub service_class_override: Option<String>,
    /// Procedure name (for started tasks).
    pub procedure_name: String,
    /// Performance group.
    pub performance_group: String,
    /// LU name (for CICS/IMS terminals).
    pub lu_name: String,
}

/// A work request to be classified.
#[derive(Debug, Clone)]
pub struct WorkRequest {
    /// Attributes for classification.
    pub attributes: WorkAttribute,
}

impl WorkRequest {
    /// Create a work request with the given subsystem type and transaction name.
    pub fn new(subsystem_type: &str, transaction_name: &str) -> Self {
        Self {
            attributes: WorkAttribute {
                subsystem_type: subsystem_type.to_uppercase(),
                transaction_name: transaction_name.to_uppercase(),
                ..Default::default()
            },
        }
    }

    /// Set the user ID.
    pub fn with_user(mut self, user_id: &str) -> Self {
        self.attributes.user_id = user_id.to_uppercase();
        self
    }

    /// Set the job class.
    pub fn with_job_class(mut self, class: &str) -> Self {
        self.attributes.job_class = class.to_uppercase();
        self
    }
}

// ---------------------------------------------------------------------------
//  Classification rule
// ---------------------------------------------------------------------------

/// Qualifier type for a classification rule.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum QualifierType {
    /// Match on subsystem type.
    SubsystemType,
    /// Match on subsystem name.
    SubsystemName,
    /// Match on transaction name.
    TransactionName,
    /// Match on user ID.
    UserId,
    /// Match on job class.
    JobClass,
    /// Match on accounting info.
    Accounting,
    /// Match on procedure name.
    ProcedureName,
    /// Match on LU name.
    LuName,
    /// Match on performance group.
    PerformanceGroup,
}

/// A single qualifier within a rule.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Qualifier {
    /// What to match on.
    pub qualifier_type: QualifierType,
    /// Value to match (supports * wildcard at end).
    pub value: String,
}

impl Qualifier {
    /// Check if this qualifier matches the given work attributes.
    pub fn matches(&self, attrs: &WorkAttribute) -> bool {
        let field = match self.qualifier_type {
            QualifierType::SubsystemType => &attrs.subsystem_type,
            QualifierType::SubsystemName => &attrs.subsystem_name,
            QualifierType::TransactionName => &attrs.transaction_name,
            QualifierType::UserId => &attrs.user_id,
            QualifierType::JobClass => &attrs.job_class,
            QualifierType::Accounting => &attrs.accounting,
            QualifierType::ProcedureName => &attrs.procedure_name,
            QualifierType::LuName => &attrs.lu_name,
            QualifierType::PerformanceGroup => &attrs.performance_group,
        };

        let val = self.value.to_uppercase();
        let field_upper = field.to_uppercase();

        if val == "*" {
            return true;
        }
        if let Some(prefix) = val.strip_suffix('*') {
            field_upper.starts_with(prefix)
        } else {
            field_upper == val
        }
    }
}

/// A classification rule that maps work to a service class.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ClassificationRule {
    /// Rule name.
    pub name: String,
    /// Qualifiers (all must match — AND logic).
    pub qualifiers: Vec<Qualifier>,
    /// Target service class name.
    pub service_class: String,
    /// Rule priority (lower = higher priority, evaluated first).
    pub priority: u32,
}

impl ClassificationRule {
    /// Create a new rule.
    pub fn new(name: &str, service_class: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            qualifiers: Vec::new(),
            service_class: service_class.to_uppercase(),
            priority: 100,
        }
    }

    /// Add a qualifier.
    pub fn add_qualifier(&mut self, qt: QualifierType, value: &str) {
        self.qualifiers.push(Qualifier {
            qualifier_type: qt,
            value: value.to_uppercase(),
        });
    }

    /// Check if this rule matches the given work attributes.
    pub fn matches(&self, attrs: &WorkAttribute) -> bool {
        self.qualifiers.iter().all(|q| q.matches(attrs))
    }
}

// ---------------------------------------------------------------------------
//  Classifier
// ---------------------------------------------------------------------------

/// The workload classifier evaluates rules and assigns service classes.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Classifier {
    /// Classification rules, sorted by priority.
    rules: Vec<ClassificationRule>,
    /// Default service class if no rule matches.
    pub default_class: String,
}

impl Classifier {
    /// Create a new classifier with a default service class.
    pub fn new(default_class: &str) -> Self {
        Self {
            rules: Vec::new(),
            default_class: default_class.to_uppercase(),
        }
    }

    /// Add a classification rule.
    pub fn add_rule(&mut self, rule: ClassificationRule) {
        self.rules.push(rule);
        self.rules.sort_by_key(|r| r.priority);
    }

    /// Remove a rule by name.
    pub fn remove_rule(&mut self, name: &str) -> bool {
        let upper = name.to_uppercase();
        let before = self.rules.len();
        self.rules.retain(|r| r.name != upper);
        self.rules.len() < before
    }

    /// Classify a work request, returning the service class name.
    pub fn classify(&self, request: &WorkRequest) -> String {
        // Check for explicit override first.
        if let Some(ref override_class) = request.attributes.service_class_override {
            if !override_class.is_empty() {
                return override_class.to_uppercase();
            }
        }

        // Evaluate rules in priority order.
        for rule in &self.rules {
            if rule.matches(&request.attributes) {
                return rule.service_class.clone();
            }
        }

        // Return default.
        self.default_class.clone()
    }

    /// Classify and verify the service class exists in the policy.
    pub fn classify_and_verify(
        &self,
        request: &WorkRequest,
        policy: &ServicePolicy,
    ) -> Result<String, WlmError> {
        let class_name = self.classify(request);
        if policy.lookup_class(&class_name).is_some() {
            Ok(class_name)
        } else {
            Err(WlmError::ClassNotFound(class_name))
        }
    }

    /// Number of rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }

    /// List all rule names.
    pub fn list_rules(&self) -> Vec<String> {
        self.rules.iter().map(|r| r.name.clone()).collect()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::service::{GoalType, Importance, ServiceClass, ServicePolicy};

    #[test]
    fn test_simple_classification() {
        let mut classifier = Classifier::new("DEFAULT");
        let mut rule = ClassificationRule::new("CICS_RULE", "PRODCICS");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule.priority = 10;
        classifier.add_rule(rule);

        let request = WorkRequest::new("CICS", "TXN01");
        assert_eq!(classifier.classify(&request), "PRODCICS");
    }

    #[test]
    fn test_default_classification() {
        let classifier = Classifier::new("DEFAULT");
        let request = WorkRequest::new("TSO", "LOGON");
        assert_eq!(classifier.classify(&request), "DEFAULT");
    }

    #[test]
    fn test_wildcard_match() {
        let mut classifier = Classifier::new("DEFAULT");
        let mut rule = ClassificationRule::new("DB2_RULE", "DB2CLASS");
        rule.add_qualifier(QualifierType::SubsystemType, "DB2");
        rule.add_qualifier(QualifierType::TransactionName, "DDF*");
        classifier.add_rule(rule);

        let req1 = WorkRequest::new("DB2", "DDFQUERY");
        assert_eq!(classifier.classify(&req1), "DB2CLASS");

        let req2 = WorkRequest::new("DB2", "UTILITY");
        assert_eq!(classifier.classify(&req2), "DEFAULT");
    }

    #[test]
    fn test_multi_qualifier() {
        let mut classifier = Classifier::new("DEFAULT");
        let mut rule = ClassificationRule::new("CICS_PROD", "CICSHIGH");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule.add_qualifier(QualifierType::SubsystemName, "CICSPROD");
        rule.priority = 10;
        classifier.add_rule(rule);

        let mut rule2 = ClassificationRule::new("CICS_TEST", "CICSLOW");
        rule2.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule2.priority = 20;
        classifier.add_rule(rule2);

        // Request with matching subsystem name → higher priority rule.
        let mut req = WorkRequest::new("CICS", "TXN01");
        req.attributes.subsystem_name = "CICSPROD".to_string();
        assert_eq!(classifier.classify(&req), "CICSHIGH");

        // Request without matching subsystem name → falls to second rule.
        let req2 = WorkRequest::new("CICS", "TXN01");
        assert_eq!(classifier.classify(&req2), "CICSLOW");
    }

    #[test]
    fn test_priority_ordering() {
        let mut classifier = Classifier::new("DEFAULT");

        let mut rule1 = ClassificationRule::new("LOW", "LOWCLASS");
        rule1.add_qualifier(QualifierType::SubsystemType, "*");
        rule1.priority = 100;
        classifier.add_rule(rule1);

        let mut rule2 = ClassificationRule::new("HIGH", "HIGHCLASS");
        rule2.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule2.priority = 10;
        classifier.add_rule(rule2);

        let request = WorkRequest::new("CICS", "TXN01");
        assert_eq!(classifier.classify(&request), "HIGHCLASS");
    }

    #[test]
    fn test_override() {
        let classifier = Classifier::new("DEFAULT");
        let mut request = WorkRequest::new("CICS", "TXN01");
        request.attributes.service_class_override = Some("OVERRIDE".to_string());
        assert_eq!(classifier.classify(&request), "OVERRIDE");
    }

    #[test]
    fn test_classify_and_verify() {
        let mut policy = ServicePolicy::new("PROD");
        let sc = ServiceClass::new("PRODCICS", GoalType::Velocity(80), Importance(1));
        policy.define_class(sc).unwrap();

        let mut classifier = Classifier::new("DEFAULT");
        let mut rule = ClassificationRule::new("CICS_RULE", "PRODCICS");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        classifier.add_rule(rule);

        let request = WorkRequest::new("CICS", "TXN01");
        let result = classifier.classify_and_verify(&request, &policy).unwrap();
        assert_eq!(result, "PRODCICS");

        // Unregistered class.
        let request2 = WorkRequest::new("TSO", "LOGON");
        assert!(classifier.classify_and_verify(&request2, &policy).is_err());
    }

    #[test]
    fn test_remove_rule() {
        let mut classifier = Classifier::new("DEFAULT");
        let mut rule = ClassificationRule::new("RULE1", "CLASS1");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        classifier.add_rule(rule);
        assert_eq!(classifier.rule_count(), 1);
        assert!(classifier.remove_rule("RULE1"));
        assert_eq!(classifier.rule_count(), 0);
    }

    #[test]
    fn test_work_request_builder() {
        let req = WorkRequest::new("JES2", "MYJOB")
            .with_user("USER01")
            .with_job_class("A");
        assert_eq!(req.attributes.subsystem_type, "JES2");
        assert_eq!(req.attributes.user_id, "USER01");
        assert_eq!(req.attributes.job_class, "A");
    }

    #[test]
    fn test_star_wildcard_matches_all() {
        let q = Qualifier {
            qualifier_type: QualifierType::SubsystemType,
            value: "*".to_string(),
        };
        let attrs = WorkAttribute {
            subsystem_type: "ANYTHING".to_string(),
            ..Default::default()
        };
        assert!(q.matches(&attrs));
    }

    #[test]
    fn test_list_rules() {
        let mut classifier = Classifier::new("DEFAULT");
        let mut r1 = ClassificationRule::new("R1", "C1");
        r1.add_qualifier(QualifierType::SubsystemType, "A");
        let mut r2 = ClassificationRule::new("R2", "C2");
        r2.add_qualifier(QualifierType::SubsystemType, "B");
        classifier.add_rule(r1);
        classifier.add_rule(r2);
        let names = classifier.list_rules();
        assert_eq!(names.len(), 2);
    }
}
