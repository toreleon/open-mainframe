//! WAIT/POST — task suspension on ECBs.

use std::sync::Arc;

use super::ecb::Ecb;

/// Wait on a single ECB until it is posted.
pub async fn wait(ecb: &Ecb) {
    ecb.wait_on().await;
}

/// Wait on multiple ECBs until at least one is posted.
///
/// Returns the index of the first ECB found to be complete.
pub async fn wait_multiple(ecbs: &[Arc<Ecb>]) -> usize {
    // Fast path: check if any ECB is already posted
    for (i, ecb) in ecbs.iter().enumerate() {
        if ecb.is_complete() {
            return i;
        }
    }

    // Spawn a waiter for each ECB and race them via a shared notification
    let found = Arc::new(std::sync::atomic::AtomicUsize::new(usize::MAX));
    let notify = Arc::new(tokio::sync::Notify::new());

    for (i, ecb) in ecbs.iter().enumerate() {
        let ecb = ecb.clone();
        let found = found.clone();
        let notify = notify.clone();
        tokio::spawn(async move {
            ecb.wait_on().await;
            // CAS: only the first completion wins
            let _ = found.compare_exchange(
                usize::MAX,
                i,
                std::sync::atomic::Ordering::AcqRel,
                std::sync::atomic::Ordering::Acquire,
            );
            notify.notify_one();
        });
    }

    loop {
        let idx = found.load(std::sync::atomic::Ordering::Acquire);
        if idx != usize::MAX {
            return idx;
        }
        notify.notified().await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn wait_single_ecb() {
        let ecb = Ecb::new();
        ecb.post(0);
        wait(&ecb).await;
        assert!(ecb.is_complete());
    }

    #[tokio::test]
    async fn wait_multiple_first_posted() {
        let ecb0 = Arc::new(Ecb::new());
        let ecb1 = Arc::new(Ecb::new());
        let ecb2 = Arc::new(Ecb::new());

        ecb1.post(0);

        let idx = wait_multiple(&[ecb0, ecb1, ecb2]).await;
        assert_eq!(idx, 1);
    }

    #[tokio::test]
    async fn wait_multiple_async_post() {
        let ecb0 = Arc::new(Ecb::new());
        let ecb1 = Arc::new(Ecb::new());
        let ecb1_clone = ecb1.clone();

        tokio::spawn(async move {
            tokio::task::yield_now().await;
            ecb1_clone.post(42);
        });

        let idx = wait_multiple(&[ecb0, ecb1.clone()]).await;
        assert_eq!(idx, 1);
        assert_eq!(ecb1.completion_code(), 42);
    }
}
