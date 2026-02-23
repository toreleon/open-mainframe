//! WTOR reply management — correlate replies to outstanding requests.

use std::collections::HashMap;
use std::sync::Arc;

use crate::sync::Ecb;

/// Token returned by WTOR to identify a pending reply.
#[derive(Debug, Clone)]
pub struct ReplyToken {
    /// Unique reply request identifier.
    pub id: u32,
    /// ECB that will be posted when the reply arrives.
    pub ecb: Arc<Ecb>,
}

/// Manages outstanding WTOR reply requests.
#[derive(Debug, Default)]
pub struct ReplyManager {
    pending: HashMap<u32, PendingReply>,
}

#[derive(Debug)]
struct PendingReply {
    ecb: Arc<Ecb>,
    reply_text: Option<String>,
}

impl ReplyManager {
    /// Create a new reply manager.
    pub fn new() -> Self {
        Self {
            pending: HashMap::new(),
        }
    }

    /// Register a new WTOR reply request.
    pub fn register(&mut self, id: u32, ecb: Arc<Ecb>) -> ReplyToken {
        self.pending.insert(
            id,
            PendingReply {
                ecb: ecb.clone(),
                reply_text: None,
            },
        );
        ReplyToken { id, ecb }
    }

    /// Provide a reply to a pending WTOR.
    ///
    /// Posts the ECB to wake the waiting task and stores the reply text.
    pub fn reply(&mut self, id: u32, text: &str) -> bool {
        if let Some(pending) = self.pending.get_mut(&id) {
            pending.reply_text = Some(text.to_string());
            pending.ecb.post(0);
            true
        } else {
            false
        }
    }

    /// Retrieve the reply text for a completed WTOR.
    pub fn get_reply(&self, id: u32) -> Option<&str> {
        self.pending
            .get(&id)
            .and_then(|p| p.reply_text.as_deref())
    }

    /// Remove a completed reply request.
    pub fn remove(&mut self, id: u32) -> Option<String> {
        self.pending.remove(&id).and_then(|p| p.reply_text)
    }

    /// Number of pending reply requests.
    pub fn pending_count(&self) -> usize {
        self.pending
            .values()
            .filter(|p| p.reply_text.is_none())
            .count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_and_reply() {
        let mut mgr = ReplyManager::new();
        let ecb = Arc::new(Ecb::new());
        let token = mgr.register(1, ecb.clone());
        assert_eq!(token.id, 1);
        assert_eq!(mgr.pending_count(), 1);

        assert!(mgr.reply(1, "YES"));
        assert!(ecb.is_complete());
        assert_eq!(mgr.get_reply(1), Some("YES"));
        assert_eq!(mgr.pending_count(), 0);
    }

    #[test]
    fn reply_to_unknown_id_returns_false() {
        let mut mgr = ReplyManager::new();
        assert!(!mgr.reply(999, "NO"));
    }

    #[test]
    fn remove_completed_reply() {
        let mut mgr = ReplyManager::new();
        let ecb = Arc::new(Ecb::new());
        mgr.register(1, ecb);
        mgr.reply(1, "CANCEL");
        let text = mgr.remove(1);
        assert_eq!(text, Some("CANCEL".to_string()));
        assert!(mgr.get_reply(1).is_none());
    }
}
