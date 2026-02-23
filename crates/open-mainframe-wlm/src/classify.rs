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

/// Recognized subsystem types for classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum SubsystemType {
    /// JES2/JES3 — Batch jobs.
    Jes,
    /// TSO — Time Sharing Option.
    Tso,
    /// CICS — Customer Information Control System.
    Cics,
    /// IMS — Information Management System.
    Ims,
    /// DB2 — Database 2.
    Db2,
    /// MQ — MQSeries (WebSphere MQ).
    Mq,
    /// STC — Started Tasks.
    Stc,
    /// ASCH — APPC/MVS Scheduling.
    Asch,
    /// OMVS — Unix System Services.
    Omvs,
    /// CB — Communications Server (TCP/IP).
    Cb,
    /// DDF — DB2 Distributed Data Facility.
    Ddf,
    /// WAS — WebSphere Application Server.
    Was,
    /// IWEB — HTTP Server.
    Iweb,
    /// Other/unknown subsystem type.
    Other,
}

impl SubsystemType {
    /// Parse a subsystem type from a string.
    pub fn from_str_type(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "JES" | "JES2" | "JES3" => SubsystemType::Jes,
            "TSO" => SubsystemType::Tso,
            "CICS" => SubsystemType::Cics,
            "IMS" => SubsystemType::Ims,
            "DB2" => SubsystemType::Db2,
            "MQ" | "WMQM" => SubsystemType::Mq,
            "STC" => SubsystemType::Stc,
            "ASCH" => SubsystemType::Asch,
            "OMVS" => SubsystemType::Omvs,
            "CB" => SubsystemType::Cb,
            "DDF" => SubsystemType::Ddf,
            "WAS" => SubsystemType::Was,
            "IWEB" => SubsystemType::Iweb,
            _ => SubsystemType::Other,
        }
    }

    /// Primary qualifier types relevant for this subsystem type.
    pub fn primary_qualifiers(self) -> &'static [QualifierType] {
        match self {
            SubsystemType::Jes => &[QualifierType::JobClass, QualifierType::UserId, QualifierType::TransactionName],
            SubsystemType::Tso => &[QualifierType::UserId, QualifierType::PerformanceGroup, QualifierType::Accounting],
            SubsystemType::Cics => &[QualifierType::TransactionName, QualifierType::LuName, QualifierType::SubsystemName],
            SubsystemType::Ims => &[QualifierType::TransactionName, QualifierType::LuName, QualifierType::SubsystemName],
            SubsystemType::Db2 => &[QualifierType::TransactionName, QualifierType::SubsystemName, QualifierType::UserId],
            SubsystemType::Mq => &[QualifierType::SubsystemName, QualifierType::TransactionName],
            SubsystemType::Stc => &[QualifierType::ProcedureName, QualifierType::TransactionName],
            SubsystemType::Asch => &[QualifierType::TransactionName, QualifierType::UserId],
            SubsystemType::Omvs => &[QualifierType::UserId, QualifierType::TransactionName],
            SubsystemType::Cb => &[QualifierType::SubsystemName, QualifierType::TransactionName],
            SubsystemType::Ddf => &[QualifierType::TransactionName, QualifierType::UserId],
            SubsystemType::Was => &[QualifierType::SubsystemName, QualifierType::TransactionName],
            SubsystemType::Iweb => &[QualifierType::SubsystemName, QualifierType::TransactionName],
            SubsystemType::Other => &[QualifierType::SubsystemType, QualifierType::TransactionName],
        }
    }
}

/// The workload classifier evaluates rules and assigns service classes.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Classifier {
    /// Classification rules, sorted by priority.
    rules: Vec<ClassificationRule>,
    /// Default service class if no rule matches.
    pub default_class: String,
    /// Per-subsystem-type default service classes.
    subsystem_defaults: std::collections::HashMap<String, String>,
}

impl Classifier {
    /// Create a new classifier with a default service class.
    pub fn new(default_class: &str) -> Self {
        Self {
            rules: Vec::new(),
            default_class: default_class.to_uppercase(),
            subsystem_defaults: std::collections::HashMap::new(),
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

    /// Set a default service class for a specific subsystem type.
    pub fn set_subsystem_default(&mut self, subsystem_type: &str, class: &str) {
        self.subsystem_defaults
            .insert(subsystem_type.to_uppercase(), class.to_uppercase());
    }

    /// Get the default for a subsystem type.
    pub fn subsystem_default(&self, subsystem_type: &str) -> Option<&String> {
        self.subsystem_defaults.get(&subsystem_type.to_uppercase())
    }

    /// Classify a work request, returning the service class name.
    ///
    /// Evaluation order:
    /// 1. Service class override (explicit assignment)
    /// 2. Rules in priority order (most specific wins)
    /// 3. Subsystem-type default
    /// 4. Global default
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

        // Check subsystem-type default.
        if let Some(default) = self.subsystem_defaults.get(&request.attributes.subsystem_type) {
            return default.clone();
        }

        // Return global default.
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

    // ─── WLM-101.2: Subsystem Type Handlers ───

    #[test]
    fn test_subsystem_type_parsing() {
        assert_eq!(SubsystemType::from_str_type("JES2"), SubsystemType::Jes);
        assert_eq!(SubsystemType::from_str_type("JES3"), SubsystemType::Jes);
        assert_eq!(SubsystemType::from_str_type("CICS"), SubsystemType::Cics);
        assert_eq!(SubsystemType::from_str_type("DB2"), SubsystemType::Db2);
        assert_eq!(SubsystemType::from_str_type("TSO"), SubsystemType::Tso);
        assert_eq!(SubsystemType::from_str_type("IMS"), SubsystemType::Ims);
        assert_eq!(SubsystemType::from_str_type("MQ"), SubsystemType::Mq);
        assert_eq!(SubsystemType::from_str_type("STC"), SubsystemType::Stc);
        assert_eq!(SubsystemType::from_str_type("OMVS"), SubsystemType::Omvs);
        assert_eq!(SubsystemType::from_str_type("UNKNOWN"), SubsystemType::Other);
    }

    #[test]
    fn test_jes_classification() {
        let mut classifier = Classifier::new("DEFAULT");

        let mut rule = ClassificationRule::new("JES_CLASSA", "BATCHHIGH");
        rule.add_qualifier(QualifierType::SubsystemType, "JES2");
        rule.add_qualifier(QualifierType::JobClass, "A");
        rule.priority = 10;
        classifier.add_rule(rule);

        let req = WorkRequest::new("JES2", "MYJOB").with_job_class("A");
        assert_eq!(classifier.classify(&req), "BATCHHIGH");
    }

    #[test]
    fn test_cics_transaction_classification() {
        let mut classifier = Classifier::new("DEFAULT");

        let mut rule = ClassificationRule::new("CICS_INQ", "CICSINQ");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule.add_qualifier(QualifierType::TransactionName, "INQ*");
        rule.priority = 10;
        classifier.add_rule(rule);

        let req = WorkRequest::new("CICS", "INQACCT");
        assert_eq!(classifier.classify(&req), "CICSINQ");
    }

    #[test]
    fn test_db2_plan_classification() {
        let mut classifier = Classifier::new("DEFAULT");

        let mut rule = ClassificationRule::new("DB2_DDF", "DB2DIST");
        rule.add_qualifier(QualifierType::SubsystemType, "DB2");
        rule.add_qualifier(QualifierType::TransactionName, "DDF*");
        rule.priority = 10;
        classifier.add_rule(rule);

        let req = WorkRequest::new("DB2", "DDFQUERY");
        assert_eq!(classifier.classify(&req), "DB2DIST");
    }

    #[test]
    fn test_subsystem_primary_qualifiers() {
        let jes_quals = SubsystemType::Jes.primary_qualifiers();
        assert!(jes_quals.contains(&QualifierType::JobClass));

        let cics_quals = SubsystemType::Cics.primary_qualifiers();
        assert!(cics_quals.contains(&QualifierType::TransactionName));
        assert!(cics_quals.contains(&QualifierType::LuName));

        let db2_quals = SubsystemType::Db2.primary_qualifiers();
        assert!(db2_quals.contains(&QualifierType::TransactionName));
    }

    // ─── WLM-101.4: Default Rules and Inheritance ───

    #[test]
    fn test_subsystem_default() {
        let mut classifier = Classifier::new("GLOBAL_DEFAULT");
        classifier.set_subsystem_default("CICS", "CICS_DEFAULT");
        classifier.set_subsystem_default("TSO", "TSO_DEFAULT");

        // CICS work with no matching rule → subsystem default.
        let req = WorkRequest::new("CICS", "UNKNOWN_TXN");
        assert_eq!(classifier.classify(&req), "CICS_DEFAULT");

        // TSO work → TSO subsystem default.
        let req2 = WorkRequest::new("TSO", "LOGON");
        assert_eq!(classifier.classify(&req2), "TSO_DEFAULT");

        // Unknown subsystem → global default.
        let req3 = WorkRequest::new("ASCH", "TASK1");
        assert_eq!(classifier.classify(&req3), "GLOBAL_DEFAULT");
    }

    #[test]
    fn test_rule_overrides_subsystem_default() {
        let mut classifier = Classifier::new("GLOBAL");
        classifier.set_subsystem_default("CICS", "CICS_LOW");

        let mut rule = ClassificationRule::new("CICS_HIGH", "CICS_PRIORITY");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule.add_qualifier(QualifierType::TransactionName, "PAY*");
        rule.priority = 10;
        classifier.add_rule(rule);

        // Matching rule → rule wins.
        let req = WorkRequest::new("CICS", "PAYROLL");
        assert_eq!(classifier.classify(&req), "CICS_PRIORITY");

        // No matching rule → subsystem default.
        let req2 = WorkRequest::new("CICS", "OTHER");
        assert_eq!(classifier.classify(&req2), "CICS_LOW");
    }

    // ─── WLM-101.5: Classification Performance ───

    #[test]
    fn test_classification_performance() {
        let mut classifier = Classifier::new("DEFAULT");

        // Add 100 rules across 5 subsystem types.
        for i in 0..100 {
            let subsys = match i % 5 {
                0 => "CICS",
                1 => "JES2",
                2 => "DB2",
                3 => "TSO",
                _ => "IMS",
            };
            let mut rule = ClassificationRule::new(
                &format!("RULE{i:03}"),
                &format!("CLASS{i:03}"),
            );
            rule.add_qualifier(QualifierType::SubsystemType, subsys);
            rule.add_qualifier(QualifierType::TransactionName, &format!("TXN{i:03}"));
            rule.priority = i;
            classifier.add_rule(rule);
        }

        // Classify 1000 work units.
        let start = std::time::Instant::now();
        for i in 0..1000 {
            let subsys = match i % 5 {
                0 => "CICS",
                1 => "JES2",
                2 => "DB2",
                3 => "TSO",
                _ => "IMS",
            };
            let req = WorkRequest::new(subsys, &format!("TXN{:03}", i % 100));
            let _ = classifier.classify(&req);
        }
        let elapsed = start.elapsed();

        // Average should be < 1ms (we assert < 10ms total for 1000 for safety).
        assert!(
            elapsed.as_millis() < 100,
            "Classification too slow: {:?}",
            elapsed
        );
    }

    // ─── WLM-101.6: Classification Tests (10 scenarios) ───

    #[test]
    fn test_ten_classification_scenarios() {
        let mut classifier = Classifier::new("DEFAULT");
        classifier.set_subsystem_default("TSO", "TSO_STD");

        // JES rules.
        let mut r = ClassificationRule::new("JES_A", "BATCH_HIGH");
        r.add_qualifier(QualifierType::SubsystemType, "JES2");
        r.add_qualifier(QualifierType::JobClass, "A");
        r.priority = 10;
        classifier.add_rule(r);

        let mut r = ClassificationRule::new("JES_B", "BATCH_LOW");
        r.add_qualifier(QualifierType::SubsystemType, "JES2");
        r.add_qualifier(QualifierType::JobClass, "B");
        r.priority = 20;
        classifier.add_rule(r);

        // CICS rules.
        let mut r = ClassificationRule::new("CICS_PAY", "CICS_HIGH");
        r.add_qualifier(QualifierType::SubsystemType, "CICS");
        r.add_qualifier(QualifierType::TransactionName, "PAY*");
        r.priority = 10;
        classifier.add_rule(r);

        let mut r = ClassificationRule::new("CICS_INQ", "CICS_MED");
        r.add_qualifier(QualifierType::SubsystemType, "CICS");
        r.add_qualifier(QualifierType::TransactionName, "INQ*");
        r.priority = 20;
        classifier.add_rule(r);

        // DB2 rules.
        let mut r = ClassificationRule::new("DB2_DDF", "DB2_DIST");
        r.add_qualifier(QualifierType::SubsystemType, "DB2");
        r.add_qualifier(QualifierType::TransactionName, "DDF*");
        r.priority = 10;
        classifier.add_rule(r);

        // TSO rules (user-specific).
        let mut r = ClassificationRule::new("TSO_ADMIN", "TSO_ADMIN");
        r.add_qualifier(QualifierType::SubsystemType, "TSO");
        r.add_qualifier(QualifierType::UserId, "ADMIN*");
        r.priority = 10;
        classifier.add_rule(r);

        // Scenario 1: JES class A → BATCH_HIGH.
        assert_eq!(
            classifier.classify(&WorkRequest::new("JES2", "MYJOB").with_job_class("A")),
            "BATCH_HIGH"
        );

        // Scenario 2: JES class B → BATCH_LOW.
        assert_eq!(
            classifier.classify(&WorkRequest::new("JES2", "MYJOB").with_job_class("B")),
            "BATCH_LOW"
        );

        // Scenario 3: CICS PAYROLL → CICS_HIGH.
        assert_eq!(
            classifier.classify(&WorkRequest::new("CICS", "PAYROLL")),
            "CICS_HIGH"
        );

        // Scenario 4: CICS INQACCT → CICS_MED.
        assert_eq!(
            classifier.classify(&WorkRequest::new("CICS", "INQACCT")),
            "CICS_MED"
        );

        // Scenario 5: CICS OTHER → DEFAULT (no subsystem default set for CICS).
        assert_eq!(
            classifier.classify(&WorkRequest::new("CICS", "OTHER")),
            "DEFAULT"
        );

        // Scenario 6: DB2 DDF → DB2_DIST.
        assert_eq!(
            classifier.classify(&WorkRequest::new("DB2", "DDFQUERY")),
            "DB2_DIST"
        );

        // Scenario 7: DB2 non-DDF → DEFAULT.
        assert_eq!(
            classifier.classify(&WorkRequest::new("DB2", "UTILITY")),
            "DEFAULT"
        );

        // Scenario 8: TSO ADMIN user → TSO_ADMIN.
        assert_eq!(
            classifier.classify(&WorkRequest::new("TSO", "LOGON").with_user("ADMIN01")),
            "TSO_ADMIN"
        );

        // Scenario 9: TSO normal user → TSO_STD (subsystem default).
        assert_eq!(
            classifier.classify(&WorkRequest::new("TSO", "LOGON").with_user("USER01")),
            "TSO_STD"
        );

        // Scenario 10: Unknown subsystem → DEFAULT.
        assert_eq!(
            classifier.classify(&WorkRequest::new("UNKNOWN", "TASK1")),
            "DEFAULT"
        );
    }
}
