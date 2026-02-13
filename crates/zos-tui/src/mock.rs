//! Mock terminal for headless testing.
//!
//! Provides a `MockTerminal` that implements `TerminalCallback` and captures
//! all screen output, allowing tests to verify CICS application behavior
//! without requiring an actual terminal.

use std::collections::{HashMap, VecDeque};

use zos_cics::bms::BmsMap;
use zos_cics::terminal::{SendMapOptions, TerminalCallback};
use zos_cics::CicsResult;

/// A recorded SEND MAP event.
#[derive(Debug, Clone)]
pub struct SendMapEvent {
    /// The BMS map that was sent.
    pub map_name: String,
    /// Field data that was sent.
    pub data: HashMap<String, Vec<u8>>,
    /// Send options.
    pub erase: bool,
    pub dataonly: bool,
    pub maponly: bool,
    pub frset: bool,
}

/// A recorded SEND TEXT event.
#[derive(Debug, Clone)]
pub struct SendTextEvent {
    /// The text that was sent.
    pub text: String,
    /// Whether ERASE was specified.
    pub erase: bool,
}

/// Queued input for a RECEIVE MAP call.
#[derive(Debug, Clone)]
pub struct QueuedInput {
    /// AID key to simulate.
    pub aid: u8,
    /// Field name -> field data pairs.
    pub fields: HashMap<String, Vec<u8>>,
}

/// Mock terminal that captures screen events for testing.
pub struct MockTerminal {
    /// Recorded SEND MAP events (in order).
    pub send_map_events: Vec<SendMapEvent>,
    /// Recorded SEND TEXT events (in order).
    pub send_text_events: Vec<SendTextEvent>,
    /// Queued inputs for RECEIVE MAP calls.
    input_queue: VecDeque<QueuedInput>,
    /// Default AID key to return if queue is empty.
    default_aid: u8,
}

impl MockTerminal {
    /// Create a new mock terminal.
    pub fn new() -> Self {
        Self {
            send_map_events: Vec::new(),
            send_text_events: Vec::new(),
            input_queue: VecDeque::new(),
            default_aid: zos_cics::runtime::eib::aid::ENTER,
        }
    }

    /// Queue an input event for the next RECEIVE MAP call.
    pub fn queue_input(&mut self, aid: u8, fields: HashMap<String, Vec<u8>>) {
        self.input_queue.push_back(QueuedInput { aid, fields });
    }

    /// Queue a simple ENTER key press with no field data.
    pub fn queue_enter(&mut self) {
        self.queue_input(zos_cics::runtime::eib::aid::ENTER, HashMap::new());
    }

    /// Queue a PF3 key press.
    pub fn queue_pf3(&mut self) {
        self.queue_input(zos_cics::runtime::eib::aid::PF3, HashMap::new());
    }

    /// Get the number of SEND MAP events recorded.
    pub fn send_map_count(&self) -> usize {
        self.send_map_events.len()
    }

    /// Get the last SEND MAP event.
    pub fn last_send_map(&self) -> Option<&SendMapEvent> {
        self.send_map_events.last()
    }

    /// Get the number of SEND TEXT events recorded.
    pub fn send_text_count(&self) -> usize {
        self.send_text_events.len()
    }

    /// Clear all recorded events.
    pub fn clear_events(&mut self) {
        self.send_map_events.clear();
        self.send_text_events.clear();
    }
}

impl Default for MockTerminal {
    fn default() -> Self {
        Self::new()
    }
}

impl TerminalCallback for MockTerminal {
    fn on_send_map(
        &mut self,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: &SendMapOptions,
    ) {
        self.send_map_events.push(SendMapEvent {
            map_name: map.name.clone(),
            data: data.clone(),
            erase: options.erase,
            dataonly: options.dataonly,
            maponly: options.maponly,
            frset: options.frset,
        });
    }

    fn on_send_text(&mut self, text: &str, erase: bool) {
        self.send_text_events.push(SendTextEvent {
            text: text.to_string(),
            erase,
        });
    }

    fn on_receive_map(
        &mut self,
        _map: &BmsMap,
    ) -> CicsResult<(u8, HashMap<String, Vec<u8>>)> {
        if let Some(input) = self.input_queue.pop_front() {
            Ok((input.aid, input.fields))
        } else {
            // Return default (ENTER with no data)
            Ok((self.default_aid, HashMap::new()))
        }
    }

    fn on_receive(&mut self, max_length: usize) -> CicsResult<(Vec<u8>, u8)> {
        if let Some(input) = self.input_queue.pop_front() {
            // Flatten field data into a single buffer
            let mut data = Vec::new();
            for (_, v) in &input.fields {
                data.extend_from_slice(v);
            }
            data.truncate(max_length);
            Ok((data, input.aid))
        } else {
            Ok((Vec::new(), self.default_aid))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zos_cics::runtime::eib::aid;

    #[test]
    fn test_mock_terminal_creation() {
        let mock = MockTerminal::new();
        assert_eq!(mock.send_map_count(), 0);
        assert_eq!(mock.send_text_count(), 0);
    }

    #[test]
    fn test_queue_and_receive_input() {
        let mut mock = MockTerminal::new();

        let mut fields = HashMap::new();
        fields.insert("USERID".to_string(), b"ADMIN".to_vec());
        mock.queue_input(aid::ENTER, fields);

        // Create a dummy BMS map for the receive call
        let map = zos_cics::bms::BmsMap {
            name: "TEST".to_string(),
            size: (24, 80),
            fields: Vec::new(),
            ..Default::default()
        };

        let (received_aid, received_fields) = mock.on_receive_map(&map).unwrap();
        assert_eq!(received_aid, aid::ENTER);
        assert_eq!(
            received_fields.get("USERID").map(|v| v.as_slice()),
            Some(b"ADMIN".as_slice())
        );
    }

    #[test]
    fn test_send_map_recording() {
        let mut mock = MockTerminal::new();

        let map = zos_cics::bms::BmsMap {
            name: "TESTMAP".to_string(),
            size: (24, 80),
            fields: Vec::new(),
            ..Default::default()
        };

        let data = HashMap::new();
        let opts = SendMapOptions {
            erase: true,
            ..Default::default()
        };

        mock.on_send_map(&map, &data, &opts);

        assert_eq!(mock.send_map_count(), 1);
        let event = mock.last_send_map().unwrap();
        assert_eq!(event.map_name, "TESTMAP");
        assert!(event.erase);
    }

    #[test]
    fn test_send_text_recording() {
        let mut mock = MockTerminal::new();

        mock.on_send_text("Hello, World!", true);

        assert_eq!(mock.send_text_count(), 1);
        assert_eq!(mock.send_text_events[0].text, "Hello, World!");
        assert!(mock.send_text_events[0].erase);
    }

    #[test]
    fn test_queue_multiple_inputs() {
        let mut mock = MockTerminal::new();

        mock.queue_enter();
        mock.queue_pf3();

        let map = zos_cics::bms::BmsMap {
            name: "TEST".to_string(),
            size: (24, 80),
            fields: Vec::new(),
            ..Default::default()
        };

        let (aid1, _) = mock.on_receive_map(&map).unwrap();
        assert_eq!(aid1, aid::ENTER);

        let (aid2, _) = mock.on_receive_map(&map).unwrap();
        assert_eq!(aid2, aid::PF3);
    }
}
