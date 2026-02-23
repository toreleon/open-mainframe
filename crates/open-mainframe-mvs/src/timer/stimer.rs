//! STIMER/STIMERM — interval timer services.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use serde::{Deserialize, Serialize};
use tokio::sync::Notify;

/// STIMER interval specification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StimerInterval {
    /// Hundredths of seconds.
    BinaryTime(u32),
    /// TOD clock units.
    ClockTime(u64),
    /// Real time components.
    RealTime {
        hours: u8,
        minutes: u8,
        seconds: u8,
        hundredths: u8,
    },
}

impl StimerInterval {
    /// Convert to a `Duration`.
    pub fn to_duration(&self) -> Duration {
        match self {
            StimerInterval::BinaryTime(hundredths) => {
                Duration::from_millis(*hundredths as u64 * 10)
            }
            StimerInterval::ClockTime(units) => {
                // TOD clock: bit 51 = 1 microsecond, so units >> 12 = microseconds
                let micros = units >> 12;
                Duration::from_micros(micros)
            }
            StimerInterval::RealTime {
                hours,
                minutes,
                seconds,
                hundredths,
            } => {
                let total_ms = (*hours as u64) * 3_600_000
                    + (*minutes as u64) * 60_000
                    + (*seconds as u64) * 1_000
                    + (*hundredths as u64) * 10;
                Duration::from_millis(total_ms)
            }
        }
    }
}

/// Handle to a pending STIMER that can be cancelled.
#[derive(Debug, Clone)]
pub struct StimerHandle {
    cancelled: Arc<AtomicBool>,
    completed: Arc<AtomicBool>,
    cancel_notify: Arc<Notify>,
    duration: Duration,
}

impl StimerHandle {
    /// Cancel the timer.
    ///
    /// Returns the approximate remaining time if the timer was still active.
    pub fn cancel(&self) -> Option<Duration> {
        if self.completed.load(Ordering::Acquire) {
            return None; // Already completed
        }
        self.cancelled.store(true, Ordering::Release);
        self.cancel_notify.notify_one();
        // Approximate remaining — we can't know exactly, return full duration as upper bound
        Some(self.duration)
    }

    /// Check if the timer has completed.
    pub fn is_complete(&self) -> bool {
        self.completed.load(Ordering::Acquire)
    }

    /// Check if the timer was cancelled.
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Acquire)
    }
}

/// STIMER WAIT — suspend the current task until the timer expires.
pub async fn stimer_wait(interval: StimerInterval) {
    let duration = interval.to_duration();
    tokio::time::sleep(duration).await;
}

/// STIMER EXIT — set a timer that invokes a callback when it expires.
///
/// Returns a handle that can be used to cancel the timer.
pub fn stimer_exit<F>(interval: StimerInterval, callback: F) -> StimerHandle
where
    F: FnOnce() + Send + 'static,
{
    let duration = interval.to_duration();
    let cancelled = Arc::new(AtomicBool::new(false));
    let completed = Arc::new(AtomicBool::new(false));
    let cancel_notify = Arc::new(Notify::new());

    let handle = StimerHandle {
        cancelled: cancelled.clone(),
        completed: completed.clone(),
        cancel_notify: cancel_notify.clone(),
        duration,
    };

    tokio::spawn(async move {
        tokio::select! {
            biased;
            _ = cancel_notify.notified() => {
                // Timer was cancelled
            }
            _ = tokio::time::sleep(duration) => {
                if !cancelled.load(Ordering::Acquire) {
                    completed.store(true, Ordering::Release);
                    callback();
                }
            }
        }
    });

    handle
}

/// STIMERM — set multiple independent timers (returns handles).
pub fn stimerm_set(
    intervals: Vec<(StimerInterval, Box<dyn FnOnce() + Send + 'static>)>,
) -> Vec<StimerHandle> {
    intervals
        .into_iter()
        .map(|(interval, callback)| stimer_exit(interval, callback))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_time_to_duration() {
        let interval = StimerInterval::BinaryTime(100); // 1 second
        let dur = interval.to_duration();
        assert_eq!(dur, Duration::from_secs(1));
    }

    #[test]
    fn real_time_to_duration() {
        let interval = StimerInterval::RealTime {
            hours: 0,
            minutes: 0,
            seconds: 5,
            hundredths: 0,
        };
        let dur = interval.to_duration();
        assert_eq!(dur, Duration::from_secs(5));
    }

    #[test]
    fn real_time_complex_to_duration() {
        let interval = StimerInterval::RealTime {
            hours: 1,
            minutes: 30,
            seconds: 0,
            hundredths: 0,
        };
        let dur = interval.to_duration();
        assert_eq!(dur, Duration::from_secs(5400));
    }

    #[tokio::test]
    async fn stimer_wait_completes() {
        let start = tokio::time::Instant::now();
        tokio::time::pause();
        let interval = StimerInterval::BinaryTime(10); // 100ms
        let handle = tokio::spawn(async move {
            stimer_wait(interval).await;
        });
        tokio::time::advance(Duration::from_millis(100)).await;
        handle.await.unwrap();
        let elapsed = start.elapsed();
        assert!(elapsed >= Duration::from_millis(100));
    }

    #[tokio::test]
    async fn stimer_exit_fires_callback() {
        let fired = Arc::new(AtomicBool::new(false));
        let fired_clone = fired.clone();

        let _handle = stimer_exit(
            StimerInterval::BinaryTime(1), // 10ms
            move || {
                fired_clone.store(true, Ordering::Release);
            },
        );

        // Wait enough real time for the timer to fire
        tokio::time::sleep(Duration::from_millis(50)).await;
        assert!(fired.load(Ordering::Acquire));
    }

    #[tokio::test]
    async fn stimer_cancel_prevents_callback() {
        let fired = Arc::new(AtomicBool::new(false));
        let fired_clone = fired.clone();

        let handle = stimer_exit(
            StimerInterval::BinaryTime(50), // 500ms
            move || {
                fired_clone.store(true, Ordering::Release);
            },
        );

        // Cancel immediately before it fires
        let remaining = handle.cancel();
        assert!(remaining.is_some());

        // Wait past the original timer
        tokio::time::sleep(Duration::from_millis(600)).await;
        assert!(!fired.load(Ordering::Acquire));
    }

    #[tokio::test]
    async fn stimerm_multiple_timers() {
        let counter = Arc::new(std::sync::atomic::AtomicU32::new(0));
        let c1 = counter.clone();
        let c2 = counter.clone();

        let handles = stimerm_set(vec![
            (
                StimerInterval::BinaryTime(1), // 10ms
                Box::new(move || {
                    c1.fetch_add(1, Ordering::Relaxed);
                }) as Box<dyn FnOnce() + Send>,
            ),
            (
                StimerInterval::BinaryTime(2), // 20ms
                Box::new(move || {
                    c2.fetch_add(1, Ordering::Relaxed);
                }),
            ),
        ]);

        assert_eq!(handles.len(), 2);

        tokio::time::sleep(Duration::from_millis(100)).await;
        assert_eq!(counter.load(Ordering::Relaxed), 2);
    }
}
