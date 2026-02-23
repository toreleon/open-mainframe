//! # IWM Services API
//!
//! Implements the z/OS WLM callable services: IWMCLSFY, IWMCONN/IWMDISC,
//! IWMSSEL, IWMRPT, IWMQRYS, and IWMSRSRG/IWMSRSDG.

use std::collections::HashMap;

use crate::classify::{Classifier, WorkRequest};
use crate::goals::{PerformanceIndex, SlidingWindow};
use crate::service::{GoalType, ServicePolicy, WlmError};

// ─────────────────────── IWMCLSFY Result ───────────────────────

/// Result of IWMCLSFY — classify work.
#[derive(Debug, Clone)]
pub struct ClassifyResult {
    /// Assigned service class name.
    pub service_class: String,
    /// Assigned report class name (if any).
    pub report_class: Option<String>,
}

// ─────────────────────── Connected Subsystem ───────────────────────

/// A subsystem connected to WLM via IWMCONN.
#[derive(Debug, Clone)]
pub struct ConnectedSubsystem {
    /// Subsystem name.
    pub name: String,
    /// Subsystem type.
    pub subsystem_type: String,
    /// Connection token.
    pub token: u64,
    /// Whether connected.
    pub connected: bool,
}

// ─────────────────────── Application Environment ───────────────────────

/// A registered server in an application environment.
#[derive(Debug, Clone)]
pub struct RegisteredServer {
    /// Server name.
    pub name: String,
    /// Application environment.
    pub app_env: String,
    /// Current load (0.0 = idle, 1.0 = fully loaded).
    pub load: f64,
    /// Whether available for selection.
    pub available: bool,
}

// ─────────────────────── Transaction Report ───────────────────────

/// A transaction completion report (IWMRPT).
#[derive(Debug, Clone)]
pub struct TransactionReport {
    /// Service class.
    pub service_class: String,
    /// Response time in seconds.
    pub response_time: f64,
    /// Timestamp.
    pub timestamp: f64,
}

// ─────────────────────── IWM Services ───────────────────────

/// The IWM services engine.
#[derive(Debug)]
pub struct IwmServices {
    /// Connected subsystems by token.
    subsystems: HashMap<u64, ConnectedSubsystem>,
    /// Next connection token.
    next_token: u64,
    /// Registered servers by application environment.
    servers: HashMap<String, Vec<RegisteredServer>>,
    /// PI sliding windows by service class.
    pi_windows: HashMap<String, SlidingWindow>,
    /// Report class assignments (service class → report class).
    report_class_map: HashMap<String, String>,
}

impl IwmServices {
    /// Create a new IWM services engine.
    pub fn new() -> Self {
        Self {
            subsystems: HashMap::new(),
            next_token: 1,
            servers: HashMap::new(),
            pi_windows: HashMap::new(),
            report_class_map: HashMap::new(),
        }
    }

    /// Set a report class mapping.
    pub fn set_report_class(&mut self, service_class: &str, report_class: &str) {
        self.report_class_map
            .insert(service_class.to_uppercase(), report_class.to_uppercase());
    }

    // ─── IWMCLSFY ───

    /// IWMCLSFY — Classify work.
    pub fn iwmclsfy(
        &self,
        request: &WorkRequest,
        classifier: &Classifier,
        policy: &ServicePolicy,
    ) -> Result<ClassifyResult, WlmError> {
        let service_class = classifier.classify_and_verify(request, policy)?;
        let report_class = self.report_class_map.get(&service_class).cloned();
        Ok(ClassifyResult {
            service_class,
            report_class,
        })
    }

    // ─── IWMCONN / IWMDISC ───

    /// IWMCONN — Connect a subsystem to WLM.
    pub fn iwmconn(&mut self, name: &str, subsystem_type: &str) -> u64 {
        let token = self.next_token;
        self.next_token += 1;

        self.subsystems.insert(
            token,
            ConnectedSubsystem {
                name: name.to_uppercase(),
                subsystem_type: subsystem_type.to_uppercase(),
                token,
                connected: true,
            },
        );

        token
    }

    /// IWMDISC — Disconnect a subsystem.
    pub fn iwmdisc(&mut self, token: u64) -> bool {
        if let Some(sub) = self.subsystems.get_mut(&token) {
            sub.connected = false;
            true
        } else {
            false
        }
    }

    /// Check if a subsystem is connected.
    pub fn is_connected(&self, token: u64) -> bool {
        self.subsystems
            .get(&token)
            .is_some_and(|s| s.connected)
    }

    // ─── IWMSSEL ───

    /// IWMSSEL — Select the least-loaded server in an application environment.
    pub fn iwmssel(&self, app_env: &str) -> Option<String> {
        let servers = self.servers.get(&app_env.to_uppercase())?;
        servers
            .iter()
            .filter(|s| s.available)
            .min_by(|a, b| a.load.partial_cmp(&b.load).unwrap_or(std::cmp::Ordering::Equal))
            .map(|s| s.name.clone())
    }

    // ─── IWMRPT ───

    /// IWMRPT — Report a transaction completion.
    pub fn iwmrpt(&mut self, report: &TransactionReport, goal: &GoalType) {
        let window = self
            .pi_windows
            .entry(report.service_class.clone())
            .or_default();

        let pi = PerformanceIndex::from_goal(goal, report.response_time, 0.0);
        window.add_sample(pi.value, report.timestamp);
    }

    /// Get current PI for a service class.
    pub fn current_pi(&self, service_class: &str) -> Option<f64> {
        self.pi_windows
            .get(&service_class.to_uppercase())
            .map(|w| w.average())
    }

    // ─── IWMQRYS ───

    /// IWMQRYS — Query service definition.
    pub fn iwmqrys(&self, policy: &ServicePolicy) -> Vec<ServiceClassInfo> {
        policy
            .service_classes
            .values()
            .map(|sc| {
                let pi = self
                    .pi_windows
                    .get(&sc.name)
                    .map(|w| w.average())
                    .unwrap_or(0.0);

                ServiceClassInfo {
                    name: sc.name.clone(),
                    description: sc.description.clone(),
                    goal: sc.primary_goal().map(|g| format!("{:?}", g.goal)),
                    importance: sc
                        .primary_goal()
                        .map(|g| g.importance.0)
                        .unwrap_or(5),
                    current_pi: pi,
                }
            })
            .collect()
    }

    // ─── IWMSRSRG / IWMSRSDG ───

    /// IWMSRSRG — Register a server.
    pub fn iwmsrsrg(&mut self, server: RegisteredServer) {
        let env = server.app_env.to_uppercase();
        self.servers
            .entry(env)
            .or_default()
            .push(server);
    }

    /// IWMSRSDG — Deregister a server.
    pub fn iwmsrsdg(&mut self, app_env: &str, server_name: &str) -> bool {
        let env = app_env.to_uppercase();
        if let Some(servers) = self.servers.get_mut(&env) {
            let before = servers.len();
            servers.retain(|s| s.name != server_name);
            return servers.len() < before;
        }
        false
    }

    /// Get server count for an environment.
    pub fn server_count(&self, app_env: &str) -> usize {
        self.servers
            .get(&app_env.to_uppercase())
            .map(|s| s.len())
            .unwrap_or(0)
    }

    /// Get connected subsystem count.
    pub fn connected_count(&self) -> usize {
        self.subsystems.values().filter(|s| s.connected).count()
    }
}

impl Default for IwmServices {
    fn default() -> Self {
        Self::new()
    }
}

/// Service class info returned by IWMQRYS.
#[derive(Debug, Clone)]
pub struct ServiceClassInfo {
    /// Service class name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Goal description.
    pub goal: Option<String>,
    /// Importance level.
    pub importance: u8,
    /// Current PI.
    pub current_pi: f64,
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::classify::{ClassificationRule, QualifierType};
    use crate::service::{Importance, ServiceClass};

    fn setup() -> (IwmServices, Classifier, ServicePolicy) {
        let mut iwm = IwmServices::new();

        let mut policy = ServicePolicy::new("PROD");
        policy
            .define_class(ServiceClass::new(
                "ONLINE",
                GoalType::ResponseTime {
                    target_seconds: 0.2,
                    percentile: 95.0,
                },
                Importance(1),
            ))
            .unwrap();
        policy
            .define_class(ServiceClass::new(
                "BATCH",
                GoalType::Velocity(50),
                Importance(3),
            ))
            .unwrap();

        let mut classifier = Classifier::new("BATCH");
        let mut rule = ClassificationRule::new("CICS_RULE", "ONLINE");
        rule.add_qualifier(QualifierType::SubsystemType, "CICS");
        rule.priority = 10;
        classifier.add_rule(rule);

        iwm.set_report_class("ONLINE", "CICSTXNS");

        (iwm, classifier, policy)
    }

    // ─── WLM-106.1: IWMCLSFY ───

    #[test]
    fn test_iwmclsfy_classify() {
        let (iwm, classifier, policy) = setup();
        let req = WorkRequest::new("CICS", "INQACCT");
        let result = iwm.iwmclsfy(&req, &classifier, &policy).unwrap();
        assert_eq!(result.service_class, "ONLINE");
        assert_eq!(result.report_class, Some("CICSTXNS".into()));
    }

    #[test]
    fn test_iwmclsfy_default() {
        let (iwm, classifier, policy) = setup();
        let req = WorkRequest::new("JES2", "MYJOB");
        let result = iwm.iwmclsfy(&req, &classifier, &policy).unwrap();
        assert_eq!(result.service_class, "BATCH");
        assert_eq!(result.report_class, None);
    }

    // ─── WLM-106.2: IWMCONN/IWMDISC ───

    #[test]
    fn test_iwmconn_iwmdisc() {
        let mut iwm = IwmServices::new();
        let token = iwm.iwmconn("CICSPROD", "CICS");
        assert!(iwm.is_connected(token));
        assert_eq!(iwm.connected_count(), 1);

        iwm.iwmdisc(token);
        assert!(!iwm.is_connected(token));
        assert_eq!(iwm.connected_count(), 0);
    }

    #[test]
    fn test_multiple_connections() {
        let mut iwm = IwmServices::new();
        let t1 = iwm.iwmconn("CICSPROD", "CICS");
        let t2 = iwm.iwmconn("DB2PROD", "DB2");
        assert_eq!(iwm.connected_count(), 2);
        iwm.iwmdisc(t1);
        assert_eq!(iwm.connected_count(), 1);
        assert!(iwm.is_connected(t2));
    }

    // ─── WLM-106.3: IWMSSEL ───

    #[test]
    fn test_iwmssel_selects_least_loaded() {
        let mut iwm = IwmServices::new();

        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV1".into(),
            app_env: "WASENV".into(),
            load: 0.8,
            available: true,
        });
        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV2".into(),
            app_env: "WASENV".into(),
            load: 0.3,
            available: true,
        });
        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV3".into(),
            app_env: "WASENV".into(),
            load: 0.5,
            available: true,
        });

        let selected = iwm.iwmssel("WASENV").unwrap();
        assert_eq!(selected, "SRV2");
    }

    #[test]
    fn test_iwmssel_skips_unavailable() {
        let mut iwm = IwmServices::new();

        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV1".into(),
            app_env: "ENV1".into(),
            load: 0.1,
            available: false,
        });
        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV2".into(),
            app_env: "ENV1".into(),
            load: 0.5,
            available: true,
        });

        let selected = iwm.iwmssel("ENV1").unwrap();
        assert_eq!(selected, "SRV2");
    }

    // ─── WLM-106.4: IWMRPT ───

    #[test]
    fn test_iwmrpt_records_sample() {
        let mut iwm = IwmServices::new();
        let goal = GoalType::ResponseTime {
            target_seconds: 0.2,
            percentile: 95.0,
        };

        iwm.iwmrpt(
            &TransactionReport {
                service_class: "ONLINE".into(),
                response_time: 0.15,
                timestamp: 100.0,
            },
            &goal,
        );

        let pi = iwm.current_pi("ONLINE").unwrap();
        assert!(pi < 1.0); // 0.15 / 0.2 = 0.75.
    }

    #[test]
    fn test_iwmrpt_multiple_samples() {
        let mut iwm = IwmServices::new();
        let goal = GoalType::ResponseTime {
            target_seconds: 0.2,
            percentile: 95.0,
        };

        for i in 0..5 {
            iwm.iwmrpt(
                &TransactionReport {
                    service_class: "ONLINE".into(),
                    response_time: 0.1 + (i as f64 * 0.05),
                    timestamp: 100.0 + i as f64,
                },
                &goal,
            );
        }

        let pi = iwm.current_pi("ONLINE").unwrap();
        assert!(pi > 0.0);
    }

    // ─── WLM-106.5: IWMQRYS ───

    #[test]
    fn test_iwmqrys() {
        let (iwm, _, policy) = setup();
        let info = iwm.iwmqrys(&policy);
        assert_eq!(info.len(), 2);

        let online = info.iter().find(|i| i.name == "ONLINE").unwrap();
        assert_eq!(online.importance, 1);
    }

    // ─── WLM-106.6: Server Register/Deregister ───

    #[test]
    fn test_server_register_deregister() {
        let mut iwm = IwmServices::new();

        iwm.iwmsrsrg(RegisteredServer {
            name: "SRV1".into(),
            app_env: "ENV1".into(),
            load: 0.5,
            available: true,
        });
        assert_eq!(iwm.server_count("ENV1"), 1);

        iwm.iwmsrsdg("ENV1", "SRV1");
        assert_eq!(iwm.server_count("ENV1"), 0);
    }

    // ─── WLM-106.7: Integration ───

    #[test]
    fn test_full_iwm_workflow() {
        let (mut iwm, classifier, policy) = setup();

        // Connect.
        let token = iwm.iwmconn("CICSPROD", "CICS");
        assert!(iwm.is_connected(token));

        // Register server.
        iwm.iwmsrsrg(RegisteredServer {
            name: "CICS01".into(),
            app_env: "CICSENV".into(),
            load: 0.2,
            available: true,
        });

        // Classify work.
        let req = WorkRequest::new("CICS", "PAYROLL");
        let class_result = iwm.iwmclsfy(&req, &classifier, &policy).unwrap();
        assert_eq!(class_result.service_class, "ONLINE");

        // Report transaction.
        let goal = GoalType::ResponseTime {
            target_seconds: 0.2,
            percentile: 95.0,
        };
        iwm.iwmrpt(
            &TransactionReport {
                service_class: "ONLINE".into(),
                response_time: 0.15,
                timestamp: 100.0,
            },
            &goal,
        );

        // Query.
        let info = iwm.iwmqrys(&policy);
        let online = info.iter().find(|i| i.name == "ONLINE").unwrap();
        assert!(online.current_pi > 0.0);

        // Select server.
        let server = iwm.iwmssel("CICSENV").unwrap();
        assert_eq!(server, "CICS01");

        // Disconnect.
        iwm.iwmdisc(token);
        assert!(!iwm.is_connected(token));
    }
}
