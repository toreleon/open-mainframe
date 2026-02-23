//! WTO/WTOR/DOM — operator message services.

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use tokio::sync::mpsc;
use tracing::debug;

use super::message::{ConsoleMessage, DescriptorCode, RoutingCode};
use super::reply::{ReplyManager, ReplyToken};
use crate::sync::Ecb;

/// Global message ID counter.
static NEXT_MSG_ID: AtomicU32 = AtomicU32::new(1);

/// Console actor — receives and manages WTO/WTOR messages.
#[derive(Debug)]
pub struct Console {
    sender: mpsc::Sender<ConsoleMessage>,
    receiver: mpsc::Receiver<ConsoleMessage>,
    messages: Vec<ConsoleMessage>,
    action_messages: Vec<u32>,
    reply_manager: ReplyManager,
}

impl Console {
    /// Create a new console with the given channel capacity.
    pub fn new(capacity: usize) -> Self {
        let (sender, receiver) = mpsc::channel(capacity);
        Self {
            sender,
            receiver,
            messages: Vec::new(),
            action_messages: Vec::new(),
            reply_manager: ReplyManager::new(),
        }
    }

    /// Get a sender handle for issuing WTO messages.
    pub fn sender(&self) -> mpsc::Sender<ConsoleMessage> {
        self.sender.clone()
    }

    /// Process pending messages from the channel.
    pub async fn process_pending(&mut self) {
        while let Ok(msg) = self.receiver.try_recv() {
            let has_action = msg.descriptor_codes & DescriptorCode::IMMEDIATE_ACTION.0 != 0
                || msg.descriptor_codes & DescriptorCode::EVENTUAL_ACTION.0 != 0;
            if has_action {
                self.action_messages.push(msg.id);
            }
            self.messages.push(msg);
        }
    }

    /// Get all received messages.
    pub fn messages(&self) -> &[ConsoleMessage] {
        &self.messages
    }

    /// Get outstanding action message IDs.
    pub fn action_messages(&self) -> &[u32] {
        &self.action_messages
    }

    /// Delete operator message (DOM) — remove from action message queue.
    pub fn dom(&mut self, msg_id: u32) {
        self.action_messages.retain(|&id| id != msg_id);
    }

    /// Register a WTOR reply request.
    pub fn register_wtor(&mut self, ecb: Arc<Ecb>) -> ReplyToken {
        let id = NEXT_MSG_ID.fetch_add(1, Ordering::Relaxed);
        self.reply_manager.register(id, ecb)
    }

    /// Provide a reply to a WTOR.
    pub fn reply(&mut self, token_id: u32, text: &str) -> bool {
        self.reply_manager.reply(token_id, text)
    }

    /// Get reply text for a completed WTOR.
    pub fn get_reply(&self, token_id: u32) -> Option<&str> {
        self.reply_manager.get_reply(token_id)
    }
}

/// Issue a WTO (Write To Operator) message.
///
/// Sends the message to the console channel with routing and descriptor codes.
pub async fn wto(
    sender: &mpsc::Sender<ConsoleMessage>,
    text: &str,
    routing: RoutingCode,
    descriptor: DescriptorCode,
) -> u32 {
    let id = NEXT_MSG_ID.fetch_add(1, Ordering::Relaxed);
    let msg = ConsoleMessage {
        id,
        text: text.to_string(),
        routing_codes: routing.0,
        descriptor_codes: descriptor.0,
        is_multiline: false,
        lines: vec![text.to_string()],
        timestamp: chrono::Utc::now(),
    };
    debug!(id, text, "WTO");
    let _ = sender.send(msg).await;
    id
}

/// Issue a multi-line WTO message.
pub async fn wto_multiline(
    sender: &mpsc::Sender<ConsoleMessage>,
    lines: &[&str],
    routing: RoutingCode,
    descriptor: DescriptorCode,
) -> u32 {
    let id = NEXT_MSG_ID.fetch_add(1, Ordering::Relaxed);
    let text = lines.join("\n");
    let msg = ConsoleMessage {
        id,
        text: text.clone(),
        routing_codes: routing.0,
        descriptor_codes: descriptor.0,
        is_multiline: true,
        lines: lines.iter().map(|s| s.to_string()).collect(),
        timestamp: chrono::Utc::now(),
    };
    debug!(id, text, "WTO multiline");
    let _ = sender.send(msg).await;
    id
}

/// Issue a WTOR (Write To Operator with Reply).
///
/// Sends the message and returns a reply token. The caller waits on the
/// token's ECB until an operator reply is provided.
pub async fn wtor(
    sender: &mpsc::Sender<ConsoleMessage>,
    console: &mut Console,
    text: &str,
    routing: RoutingCode,
) -> ReplyToken {
    let ecb = Arc::new(Ecb::new());
    let token = console.register_wtor(ecb);

    let msg = ConsoleMessage {
        id: token.id,
        text: text.to_string(),
        routing_codes: routing.0,
        descriptor_codes: DescriptorCode::IMMEDIATE_ACTION.0,
        is_multiline: false,
        lines: vec![text.to_string()],
        timestamp: chrono::Utc::now(),
    };
    debug!(id = token.id, text, "WTOR");
    let _ = sender.send(msg).await;
    token
}

/// Delete Operator Message (DOM) — remove a message from the action queue.
pub fn dom(console: &mut Console, msg_id: u32) {
    debug!(msg_id, "DOM");
    console.dom(msg_id);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn wto_sends_message_to_console() {
        let mut console = Console::new(16);
        let sender = console.sender();
        let id = wto(
            &sender,
            "IEF142I JOB01 - STEP WAS EXECUTED",
            RoutingCode::OPERATOR_INFO,
            DescriptorCode::JOB_STATUS,
        )
        .await;

        console.process_pending().await;
        assert_eq!(console.messages().len(), 1);
        let msg = &console.messages()[0];
        assert_eq!(msg.id, id);
        assert_eq!(msg.text, "IEF142I JOB01 - STEP WAS EXECUTED");
        assert_eq!(msg.routing_codes, RoutingCode::OPERATOR_INFO.0);
    }

    #[tokio::test]
    async fn wto_multiline_sends_all_lines() {
        let mut console = Console::new(16);
        let sender = console.sender();
        let _id = wto_multiline(
            &sender,
            &["LINE1 - FIRST", "LINE2 - SECOND", "LINE3 - THIRD"],
            RoutingCode::MASTER,
            DescriptorCode::INFORMATIONAL,
        )
        .await;

        console.process_pending().await;
        let msg = &console.messages()[0];
        assert!(msg.is_multiline);
        assert_eq!(msg.lines.len(), 3);
        assert_eq!(msg.lines[0], "LINE1 - FIRST");
    }

    #[tokio::test]
    async fn wtor_and_reply() {
        let mut console = Console::new(16);
        let sender = console.sender();
        let token = wtor(
            &sender,
            &mut console,
            "IEE302A REPLY Y OR N",
            RoutingCode::MASTER,
        )
        .await;

        // Operator provides reply
        assert!(console.reply(token.id, "Y"));
        assert!(token.ecb.is_complete());
        assert_eq!(console.get_reply(token.id), Some("Y"));
    }

    #[tokio::test]
    async fn dom_removes_action_message() {
        let mut console = Console::new(16);
        let sender = console.sender();
        let id = wto(
            &sender,
            "IMPORTANT MESSAGE",
            RoutingCode::MASTER,
            DescriptorCode::IMMEDIATE_ACTION,
        )
        .await;

        console.process_pending().await;
        assert!(console.action_messages().contains(&id));

        dom(&mut console, id);
        assert!(!console.action_messages().contains(&id));
    }

    #[tokio::test]
    async fn routing_codes_are_preserved() {
        let mut console = Console::new(16);
        let sender = console.sender();
        let routing = RoutingCode::MASTER.union(RoutingCode::OPERATOR_INFO);
        wto(
            &sender,
            "TEST",
            routing,
            DescriptorCode::INFORMATIONAL,
        )
        .await;

        console.process_pending().await;
        let msg = &console.messages()[0];
        let rc = RoutingCode(msg.routing_codes);
        assert!(rc.has_code(1)); // master
        assert!(rc.has_code(2)); // operator info
        assert!(!rc.has_code(3));
    }
}
