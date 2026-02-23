//! # Goal Evaluation & Performance Index
//!
//! Calculates Performance Index (PI) for WLM service classes,
//! tracks service unit accounting, and provides sliding-window aggregation.

use std::collections::VecDeque;

use crate::service::{GoalType, Importance};

// ─────────────────────── Service Unit Accounting ───────────────────────

/// Service unit components tracked per work unit.
#[derive(Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize)]
pub struct ServiceUnits {
    /// CPU service units.
    pub cpu: u64,
    /// Main Storage Occupancy service units.
    pub mso: u64,
    /// I/O Connect service units.
    pub ioc: u64,
    /// SRB (System Request Block) service units.
    pub srb: u64,
}

impl ServiceUnits {
    /// Create new service units.
    pub fn new(cpu: u64, mso: u64, ioc: u64, srb: u64) -> Self {
        Self { cpu, mso, ioc, srb }
    }

    /// Total composite service units.
    pub fn total(&self) -> u64 {
        self.cpu + self.mso + self.ioc + self.srb
    }

    /// Add another set of service units.
    pub fn add(&mut self, other: &ServiceUnits) {
        self.cpu += other.cpu;
        self.mso += other.mso;
        self.ioc += other.ioc;
        self.srb += other.srb;
    }
}

// ─────────────────────── Performance Index ───────────────────────

/// A Performance Index value.
///
/// PI < 1.0 means goal is being met (work is faster than required).
/// PI = 1.0 means exactly meeting goal.
/// PI > 1.0 means goal is not being met.
#[derive(Debug, Clone, Copy)]
pub struct PerformanceIndex {
    /// The PI value.
    pub value: f64,
    /// Whether the goal is being met (PI <= 1.0).
    pub goal_met: bool,
}

impl PerformanceIndex {
    /// Calculate PI for a response time goal.
    ///
    /// PI = actual / goal (lower is better).
    pub fn response_time(goal_seconds: f64, actual_seconds: f64) -> Self {
        let value = if goal_seconds > 0.0 {
            actual_seconds / goal_seconds
        } else {
            0.0
        };
        Self {
            value,
            goal_met: value <= 1.0,
        }
    }

    /// Calculate PI for a velocity goal.
    ///
    /// PI = goal / actual (so higher actual velocity → lower PI → better).
    pub fn velocity(goal_velocity: u8, actual_velocity: f64) -> Self {
        let value = if actual_velocity > 0.0 {
            f64::from(goal_velocity) / actual_velocity
        } else {
            f64::MAX
        };
        Self {
            value,
            goal_met: value <= 1.0,
        }
    }

    /// PI for discretionary workloads is always 0 (no goal to miss).
    pub fn discretionary() -> Self {
        Self {
            value: 0.0,
            goal_met: true,
        }
    }

    /// Calculate PI from a goal type and actual measurements.
    pub fn from_goal(goal: &GoalType, actual_response_time: f64, actual_velocity: f64) -> Self {
        match goal {
            GoalType::ResponseTime {
                target_seconds, ..
            } => Self::response_time(*target_seconds, actual_response_time),
            GoalType::Velocity(v) => Self::velocity(*v, actual_velocity),
            GoalType::Discretionary => Self::discretionary(),
        }
    }
}

// ─────────────────────── PI Sample ───────────────────────

/// A timestamped PI sample.
#[derive(Debug, Clone, Copy)]
pub struct PiSample {
    /// PI value.
    pub pi: f64,
    /// Timestamp in seconds since epoch.
    pub timestamp: f64,
}

// ─────────────────────── Sliding Window ───────────────────────

/// Sliding window for PI aggregation.
///
/// Maintains a 5-minute (300-second) window of PI samples.
#[derive(Debug, Clone)]
pub struct SlidingWindow {
    /// PI samples.
    samples: VecDeque<PiSample>,
    /// Window duration in seconds.
    window_seconds: f64,
}

impl SlidingWindow {
    /// Create a new sliding window with default 5-minute duration.
    pub fn new() -> Self {
        Self {
            samples: VecDeque::new(),
            window_seconds: 300.0,
        }
    }

    /// Create with a custom window duration.
    pub fn with_duration(seconds: f64) -> Self {
        Self {
            samples: VecDeque::new(),
            window_seconds: seconds,
        }
    }

    /// Add a PI sample.
    pub fn add_sample(&mut self, pi: f64, timestamp: f64) {
        // Remove expired samples.
        let cutoff = timestamp - self.window_seconds;
        while self.samples.front().is_some_and(|s| s.timestamp < cutoff) {
            self.samples.pop_front();
        }

        self.samples.push_back(PiSample { pi, timestamp });
    }

    /// Get the average PI over the window.
    pub fn average(&self) -> f64 {
        if self.samples.is_empty() {
            return 0.0;
        }
        let sum: f64 = self.samples.iter().map(|s| s.pi).sum();
        sum / self.samples.len() as f64
    }

    /// Get the number of samples in the window.
    pub fn sample_count(&self) -> usize {
        self.samples.len()
    }

    /// Get the maximum PI in the window.
    pub fn max_pi(&self) -> f64 {
        self.samples
            .iter()
            .map(|s| s.pi)
            .fold(0.0_f64, f64::max)
    }

    /// Expire old samples given the current time.
    pub fn expire(&mut self, current_time: f64) {
        let cutoff = current_time - self.window_seconds;
        while self.samples.front().is_some_and(|s| s.timestamp < cutoff) {
            self.samples.pop_front();
        }
    }
}

impl Default for SlidingWindow {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Work Unit Tracker ───────────────────────

/// Tracks service units for a work unit and determines period transitions.
#[derive(Debug, Clone)]
pub struct WorkUnitTracker {
    /// Accumulated service units.
    pub service_units: ServiceUnits,
    /// Current period index (0-based).
    pub current_period: usize,
    /// Period thresholds (cumulative SU at which to transition).
    pub period_thresholds: Vec<u64>,
}

impl WorkUnitTracker {
    /// Create a new tracker with the given period thresholds.
    pub fn new(thresholds: Vec<u64>) -> Self {
        Self {
            service_units: ServiceUnits::default(),
            current_period: 0,
            period_thresholds: thresholds,
        }
    }

    /// Add service units and check for period transitions.
    /// Returns true if a period transition occurred.
    pub fn accumulate(&mut self, su: &ServiceUnits) -> bool {
        self.service_units.add(su);
        let total = self.service_units.total();

        // Check if we should advance to next period.
        if self.current_period < self.period_thresholds.len()
            && total >= self.period_thresholds[self.current_period]
        {
            self.current_period += 1;
            return true;
        }
        false
    }
}

// ─────────────────────── Importance-Based Prioritization ───────────────────────

/// A class needing resources, ranked by importance and PI.
#[derive(Debug, Clone)]
pub struct ResourceDemand {
    /// Service class name.
    pub class_name: String,
    /// Importance level.
    pub importance: Importance,
    /// Current PI.
    pub pi: f64,
}

/// Rank classes by urgency for resource allocation.
///
/// Classes with PI > 1.0 (not meeting goal) get priority.
/// Among those, lower importance number (higher priority) comes first.
/// Among same importance, higher PI (further from goal) comes first.
pub fn rank_by_urgency(demands: &mut [ResourceDemand]) {
    demands.sort_by(|a, b| {
        // First: classes not meeting goals (PI > 1.0) before those meeting goals.
        let a_unmet = a.pi > 1.0;
        let b_unmet = b.pi > 1.0;
        if a_unmet != b_unmet {
            return b_unmet.cmp(&a_unmet);
        }

        // Second: lower importance number = higher priority.
        let imp_cmp = a.importance.cmp(&b.importance);
        if imp_cmp != std::cmp::Ordering::Equal {
            return imp_cmp;
        }

        // Third: higher PI = more urgent.
        b.pi.partial_cmp(&a.pi)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── WLM-102.1: Response Time PI ───

    #[test]
    fn test_response_time_pi_not_met() {
        let pi = PerformanceIndex::response_time(0.200, 0.240);
        assert!((pi.value - 1.20).abs() < 0.01);
        assert!(!pi.goal_met);
    }

    #[test]
    fn test_response_time_pi_met() {
        let pi = PerformanceIndex::response_time(0.200, 0.180);
        assert!((pi.value - 0.90).abs() < 0.01);
        assert!(pi.goal_met);
    }

    #[test]
    fn test_response_time_pi_exact() {
        let pi = PerformanceIndex::response_time(0.200, 0.200);
        assert!((pi.value - 1.0).abs() < 0.01);
        assert!(pi.goal_met);
    }

    // ─── WLM-102.2: Velocity PI ───

    #[test]
    fn test_velocity_pi_slow() {
        let pi = PerformanceIndex::velocity(50, 40.0);
        assert!((pi.value - 1.25).abs() < 0.01);
        assert!(!pi.goal_met);
    }

    #[test]
    fn test_velocity_pi_fast() {
        let pi = PerformanceIndex::velocity(50, 60.0);
        assert!((pi.value - 0.833).abs() < 0.01);
        assert!(pi.goal_met);
    }

    #[test]
    fn test_discretionary_pi() {
        let pi = PerformanceIndex::discretionary();
        assert_eq!(pi.value, 0.0);
        assert!(pi.goal_met);
    }

    #[test]
    fn test_pi_from_goal() {
        let goal = GoalType::ResponseTime {
            target_seconds: 0.5,
            percentile: 95.0,
        };
        let pi = PerformanceIndex::from_goal(&goal, 0.4, 0.0);
        assert!((pi.value - 0.8).abs() < 0.01);
        assert!(pi.goal_met);
    }

    // ─── WLM-102.3: Service Unit Accounting ───

    #[test]
    fn test_service_units_total() {
        let su = ServiceUnits::new(100, 50, 30, 20);
        assert_eq!(su.total(), 200);
    }

    #[test]
    fn test_service_units_add() {
        let mut su = ServiceUnits::new(100, 50, 30, 20);
        su.add(&ServiceUnits::new(50, 25, 15, 10));
        assert_eq!(su.cpu, 150);
        assert_eq!(su.total(), 300);
    }

    #[test]
    fn test_period_transition() {
        let mut tracker = WorkUnitTracker::new(vec![500, 1000]);
        assert_eq!(tracker.current_period, 0);

        // Add 300 SU — no transition.
        let transitioned = tracker.accumulate(&ServiceUnits::new(200, 50, 30, 20));
        assert!(!transitioned);
        assert_eq!(tracker.current_period, 0);

        // Add 300 more → 600 total → crosses 500 threshold.
        let transitioned = tracker.accumulate(&ServiceUnits::new(200, 50, 30, 20));
        assert!(transitioned);
        assert_eq!(tracker.current_period, 1);
    }

    // ─── WLM-102.4: Sliding Window ───

    #[test]
    fn test_sliding_window_average() {
        let mut window = SlidingWindow::with_duration(10.0);
        window.add_sample(1.0, 1.0);
        window.add_sample(2.0, 2.0);
        window.add_sample(3.0, 3.0);
        assert!((window.average() - 2.0).abs() < 0.01);
    }

    #[test]
    fn test_sliding_window_expiry() {
        let mut window = SlidingWindow::with_duration(10.0);
        window.add_sample(5.0, 1.0); // Old sample.
        window.add_sample(1.0, 8.0);
        window.add_sample(1.0, 12.0); // Adding at t=12 expires sample at t=1 (cutoff=2).

        assert_eq!(window.sample_count(), 2);
        assert!((window.average() - 1.0).abs() < 0.01);
    }

    #[test]
    fn test_sliding_window_empty() {
        let window = SlidingWindow::new();
        assert_eq!(window.average(), 0.0);
        assert_eq!(window.sample_count(), 0);
    }

    #[test]
    fn test_sliding_window_max_pi() {
        let mut window = SlidingWindow::with_duration(10.0);
        window.add_sample(1.0, 1.0);
        window.add_sample(2.5, 2.0);
        window.add_sample(1.5, 3.0);
        assert!((window.max_pi() - 2.5).abs() < 0.01);
    }

    #[test]
    fn test_sliding_window_expire_method() {
        let mut window = SlidingWindow::with_duration(10.0);
        window.add_sample(1.0, 1.0);
        window.add_sample(2.0, 5.0);
        window.add_sample(3.0, 9.0);

        window.expire(15.0); // cutoff = 5.0, so t=1.0 dropped
        assert_eq!(window.sample_count(), 2);
    }

    // ─── WLM-102.5: Importance-Based Prioritization ───

    #[test]
    fn test_importance_ranking() {
        let mut demands = vec![
            ResourceDemand {
                class_name: "LOW_IMP".into(),
                importance: Importance(3),
                pi: 1.5,
            },
            ResourceDemand {
                class_name: "HIGH_IMP".into(),
                importance: Importance(1),
                pi: 1.5,
            },
            ResourceDemand {
                class_name: "MET_GOAL".into(),
                importance: Importance(1),
                pi: 0.8,
            },
        ];

        rank_by_urgency(&mut demands);

        // HIGH_IMP (imp=1, PI=1.5 > 1.0) should be first.
        assert_eq!(demands[0].class_name, "HIGH_IMP");
        // LOW_IMP (imp=3, PI=1.5 > 1.0) second.
        assert_eq!(demands[1].class_name, "LOW_IMP");
        // MET_GOAL (PI=0.8, goal met) last.
        assert_eq!(demands[2].class_name, "MET_GOAL");
    }

    #[test]
    fn test_ranking_same_importance_higher_pi_first() {
        let mut demands = vec![
            ResourceDemand {
                class_name: "MILD".into(),
                importance: Importance(2),
                pi: 1.2,
            },
            ResourceDemand {
                class_name: "SEVERE".into(),
                importance: Importance(2),
                pi: 2.0,
            },
        ];

        rank_by_urgency(&mut demands);
        assert_eq!(demands[0].class_name, "SEVERE");
    }

    // ─── WLM-102.6: Goal Evaluation Tests ───

    #[test]
    fn test_synthetic_workload_pi() {
        // Simulate 4 service classes with known metrics.
        let cases = vec![
            ("ONLINE", GoalType::ResponseTime { target_seconds: 0.2, percentile: 95.0 }, 0.15, 0.0, true),
            ("BATCH", GoalType::Velocity(50), 0.0, 40.0, false),
            ("TSO", GoalType::ResponseTime { target_seconds: 1.0, percentile: 90.0 }, 1.2, 0.0, false),
            ("LOWPRI", GoalType::Discretionary, 0.0, 0.0, true),
        ];

        for (name, goal, resp, vel, expected_met) in cases {
            let pi = PerformanceIndex::from_goal(&goal, resp, vel);
            assert_eq!(
                pi.goal_met, expected_met,
                "PI for {name} expected goal_met={expected_met}, got PI={:.3}",
                pi.value
            );
        }
    }
}
