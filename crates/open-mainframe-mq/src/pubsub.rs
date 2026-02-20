//! MQ Publish/Subscribe Engine — topic tree, subscriptions, and publication
//! distribution.
//!
//! Implements:
//! - **Topic tree** — hierarchical topic strings with `/` separator
//! - **MQSUB** — create durable and non-durable subscriptions
//! - **MQPUB** — publish messages to topics with wildcard distribution
//! - **Wildcards** — `#` (multilevel) and `+` (single-level)
//! - **Retained publications** — latest message stored per topic
//! - **Selection strings** — SQL92-like message property filtering
//! - **MQSUBRQ** — request retained publication re-delivery

use crate::structures::Mqmd;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, VecDeque};

// ---------------------------------------------------------------------------
//  Subscription options
// ---------------------------------------------------------------------------

/// Subscription durability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubDurability {
    /// Durable — persists after disconnect.
    Durable,
    /// Non-durable — exists only while connected.
    NonDurable,
}

/// Subscription destination management.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubManaged {
    /// Queue manager creates and manages the destination queue.
    Managed,
    /// Application specifies the destination queue.
    Unmanaged,
}

/// Subscription create/alter flags (MQSO_*).
#[derive(Debug, Clone)]
pub struct SubOptions {
    pub durability: SubDurability,
    pub managed: SubManaged,
    /// Selection string (SQL92-like filter on message properties).
    pub selection_string: Option<String>,
    /// User-specified destination queue name (for unmanaged).
    pub dest_queue: Option<String>,
}

impl Default for SubOptions {
    fn default() -> Self {
        Self {
            durability: SubDurability::NonDurable,
            managed: SubManaged::Managed,
            selection_string: None,
            dest_queue: None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Publish options
// ---------------------------------------------------------------------------

/// Publish options (MQPMO_*).
#[derive(Debug, Clone, Default)]
pub struct PubOptions {
    /// Retain this publication.
    pub retain: bool,
}

// ---------------------------------------------------------------------------
//  Core types
// ---------------------------------------------------------------------------

/// A published message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Publication {
    /// Message descriptor.
    pub mqmd: Mqmd,
    /// Message body.
    pub data: Vec<u8>,
    /// Message properties (for selection string filtering).
    pub properties: HashMap<String, PropValue>,
}

/// A message property value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PropValue {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

/// A subscription record.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subscription {
    /// Unique subscription name.
    pub sub_name: String,
    /// Topic string pattern (may contain wildcards).
    pub topic_string: String,
    /// Durability.
    pub durability: SubDurability,
    /// Managed flag.
    pub managed: SubManaged,
    /// Destination queue name.
    pub dest_queue: String,
    /// Selection string filter.
    pub selection_string: Option<String>,
    /// Whether subscription is currently active (connected).
    pub active: bool,
}

/// Result of an MQSUB call.
#[derive(Debug, Clone)]
pub struct SubResult {
    /// Subscription handle.
    pub sub_handle: u64,
    /// Destination queue name (especially useful for managed queues).
    pub dest_queue: String,
}

// ---------------------------------------------------------------------------
//  Topic tree node
// ---------------------------------------------------------------------------

/// A node in the topic tree.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct TopicNode {
    /// Children by topic level name.
    children: BTreeMap<String, TopicNode>,
    /// Retained publication for this exact topic.
    retained: Option<Publication>,
}

// ---------------------------------------------------------------------------
//  Pub/Sub Engine
// ---------------------------------------------------------------------------

/// The MQ Publish/Subscribe engine.
#[derive(Debug, Clone, Default)]
pub struct PubSubEngine {
    /// Topic tree root.
    root: TopicNode,
    /// All subscriptions by sub_name.
    subscriptions: HashMap<String, Subscription>,
    /// Subscription handle → sub_name.
    handle_map: HashMap<u64, String>,
    /// Next subscription handle.
    next_handle: u64,
    /// Next managed queue counter.
    next_managed_id: u64,
    /// Queued publications per destination queue.
    dest_queues: HashMap<String, VecDeque<Publication>>,
}

impl PubSubEngine {
    pub fn new() -> Self {
        Self {
            next_handle: 1,
            next_managed_id: 1,
            ..Default::default()
        }
    }

    // -------------------------------------------------------------------
    //  MQSUB — subscribe
    // -------------------------------------------------------------------

    /// Create a subscription to a topic string.
    pub fn subscribe(
        &mut self,
        sub_name: &str,
        topic_string: &str,
        options: &SubOptions,
    ) -> SubResult {
        // Determine destination queue.
        let dest_queue = match options.managed {
            SubManaged::Managed => {
                let name = format!("SYSTEM.MANAGED.{:06}", self.next_managed_id);
                self.next_managed_id += 1;
                name
            }
            SubManaged::Unmanaged => options
                .dest_queue
                .clone()
                .unwrap_or_else(|| format!("SYSTEM.MANAGED.{:06}", {
                    let id = self.next_managed_id;
                    self.next_managed_id += 1;
                    id
                })),
        };

        let handle = self.next_handle;
        self.next_handle += 1;

        let sub = Subscription {
            sub_name: sub_name.to_string(),
            topic_string: topic_string.to_string(),
            durability: options.durability,
            managed: options.managed,
            dest_queue: dest_queue.clone(),
            selection_string: options.selection_string.clone(),
            active: true,
        };

        self.subscriptions.insert(sub_name.to_string(), sub);
        self.handle_map.insert(handle, sub_name.to_string());
        self.dest_queues.entry(dest_queue.clone()).or_default();

        // Deliver retained publication if one exists.
        if let Some(pub_msg) = self.get_retained(topic_string) {
            self.dest_queues
                .entry(dest_queue.clone())
                .or_default()
                .push_back(pub_msg);
        }

        SubResult {
            sub_handle: handle,
            dest_queue,
        }
    }

    /// Close/remove a subscription by handle.
    pub fn unsubscribe(&mut self, handle: u64) -> bool {
        let sub_name = match self.handle_map.remove(&handle) {
            Some(n) => n,
            None => return false,
        };
        if let Some(sub) = self.subscriptions.remove(&sub_name) {
            if sub.durability == SubDurability::NonDurable {
                // Clean up managed queue.
                self.dest_queues.remove(&sub.dest_queue);
            }
        }
        true
    }

    // -------------------------------------------------------------------
    //  MQPUB — publish
    // -------------------------------------------------------------------

    /// Publish a message to a topic string.
    pub fn publish(
        &mut self,
        topic_string: &str,
        mqmd: Mqmd,
        data: Vec<u8>,
        properties: HashMap<String, PropValue>,
        options: &PubOptions,
    ) -> usize {
        let pub_msg = Publication {
            mqmd,
            data,
            properties,
        };

        // Store retained if requested.
        if options.retain {
            self.set_retained(topic_string, pub_msg.clone());
        }

        // Find matching subscriptions and deliver.
        let mut delivered = 0;
        let matching: Vec<String> = self
            .subscriptions
            .values()
            .filter(|s| s.active && topic_matches(&s.topic_string, topic_string))
            .filter(|s| selection_matches(&s.selection_string, &pub_msg.properties))
            .map(|s| s.dest_queue.clone())
            .collect();

        for dest in matching {
            self.dest_queues
                .entry(dest)
                .or_default()
                .push_back(pub_msg.clone());
            delivered += 1;
        }

        delivered
    }

    // -------------------------------------------------------------------
    //  MQSUBRQ — request retained publication
    // -------------------------------------------------------------------

    /// Request re-delivery of the retained publication for a subscription.
    pub fn subrq(&mut self, handle: u64) -> bool {
        let sub_name = match self.handle_map.get(&handle) {
            Some(n) => n.clone(),
            None => return false,
        };
        let sub = match self.subscriptions.get(&sub_name) {
            Some(s) => s.clone(),
            None => return false,
        };

        if let Some(pub_msg) = self.get_retained(&sub.topic_string) {
            self.dest_queues
                .entry(sub.dest_queue)
                .or_default()
                .push_back(pub_msg);
            true
        } else {
            false
        }
    }

    // -------------------------------------------------------------------
    //  Destination queue access
    // -------------------------------------------------------------------

    /// Get the next message from a destination queue (for subscribers).
    pub fn get_from_queue(&mut self, queue_name: &str) -> Option<Publication> {
        self.dest_queues
            .get_mut(queue_name)
            .and_then(|q| q.pop_front())
    }

    /// Peek at the queue depth for a destination queue.
    pub fn queue_depth(&self, queue_name: &str) -> usize {
        self.dest_queues
            .get(queue_name)
            .map(|q| q.len())
            .unwrap_or(0)
    }

    // -------------------------------------------------------------------
    //  Retained publications
    // -------------------------------------------------------------------

    fn set_retained(&mut self, topic: &str, pub_msg: Publication) {
        let levels: Vec<&str> = topic.split('/').collect();
        let node = self.ensure_node(&levels);
        node.retained = Some(pub_msg);
    }

    fn get_retained(&self, topic: &str) -> Option<Publication> {
        let levels: Vec<&str> = topic.split('/').collect();
        self.find_node(&levels).and_then(|n| n.retained.clone())
    }

    fn ensure_node(&mut self, levels: &[&str]) -> &mut TopicNode {
        let mut current = &mut self.root;
        for level in levels {
            current = current
                .children
                .entry(level.to_string())
                .or_default();
        }
        current
    }

    fn find_node(&self, levels: &[&str]) -> Option<&TopicNode> {
        let mut current = &self.root;
        for level in levels {
            current = current.children.get(*level)?;
        }
        Some(current)
    }

    // -------------------------------------------------------------------
    //  Query helpers
    // -------------------------------------------------------------------

    /// List all subscriptions.
    pub fn list_subscriptions(&self) -> Vec<&Subscription> {
        self.subscriptions.values().collect()
    }

    /// Get a subscription by name.
    pub fn get_subscription(&self, name: &str) -> Option<&Subscription> {
        self.subscriptions.get(name)
    }

    /// Total subscription count.
    pub fn sub_count(&self) -> usize {
        self.subscriptions.len()
    }
}

// ---------------------------------------------------------------------------
//  Topic matching with wildcards
// ---------------------------------------------------------------------------

/// Match a subscription topic pattern against a published topic string.
///
/// Wildcards:
/// - `#` matches zero or more levels.
/// - `+` matches exactly one level.
pub fn topic_matches(pattern: &str, topic: &str) -> bool {
    let pat_levels: Vec<&str> = pattern.split('/').collect();
    let top_levels: Vec<&str> = topic.split('/').collect();
    match_levels(&pat_levels, &top_levels)
}

fn match_levels(pat: &[&str], top: &[&str]) -> bool {
    if pat.is_empty() {
        return top.is_empty();
    }

    match pat[0] {
        "#" => {
            // # matches zero or more levels.
            if pat.len() == 1 {
                return true; // Trailing # matches everything.
            }
            for skip in 0..=top.len() {
                if match_levels(&pat[1..], &top[skip..]) {
                    return true;
                }
            }
            false
        }
        "+" => {
            // + matches exactly one level.
            if top.is_empty() {
                return false;
            }
            match_levels(&pat[1..], &top[1..])
        }
        _ => {
            if top.is_empty() || pat[0] != top[0] {
                return false;
            }
            match_levels(&pat[1..], &top[1..])
        }
    }
}

// ---------------------------------------------------------------------------
//  Selection string evaluation
// ---------------------------------------------------------------------------

/// Evaluate a selection string against message properties.
///
/// Supports a simplified SQL92-like syntax:
/// - `property = 'value'` (string equality)
/// - `property = number` (integer equality)
/// - `property > number`, `property < number` (numeric comparison)
/// - `condition AND condition`
/// - `condition OR condition`
pub fn selection_matches(
    selection: &Option<String>,
    properties: &HashMap<String, PropValue>,
) -> bool {
    let sel = match selection {
        Some(s) if !s.is_empty() => s,
        _ => return true, // No filter — match all.
    };
    eval_selection(sel, properties)
}

fn eval_selection(sel: &str, props: &HashMap<String, PropValue>) -> bool {
    let sel = sel.trim();

    // Handle OR (lowest precedence).
    if let Some(idx) = find_keyword(sel, " OR ") {
        let left = &sel[..idx];
        let right = &sel[idx + 4..];
        return eval_selection(left, props) || eval_selection(right, props);
    }

    // Handle AND.
    if let Some(idx) = find_keyword(sel, " AND ") {
        let left = &sel[..idx];
        let right = &sel[idx + 5..];
        return eval_selection(left, props) && eval_selection(right, props);
    }

    // Handle comparison operators.
    for (op, op_len) in &[(">=", 2), ("<=", 2), ("!=", 2), ("<>", 2), (">", 1), ("<", 1), ("=", 1)] {
        if let Some(idx) = sel.find(op) {
            let prop_name = sel[..idx].trim();
            let value_str = sel[idx + op_len..].trim();

            let prop = match props.get(prop_name) {
                Some(p) => p,
                None => return false,
            };

            return match *op {
                "=" => prop_equals(prop, value_str),
                "!=" | "<>" => !prop_equals(prop, value_str),
                ">" => prop_compare(prop, value_str) == Some(std::cmp::Ordering::Greater),
                "<" => prop_compare(prop, value_str) == Some(std::cmp::Ordering::Less),
                ">=" => matches!(prop_compare(prop, value_str), Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)),
                "<=" => matches!(prop_compare(prop, value_str), Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal)),
                _ => false,
            };
        }
    }

    false
}

fn find_keyword(s: &str, keyword: &str) -> Option<usize> {
    let upper = s.to_uppercase();
    let kw_upper = keyword.to_uppercase();
    // Find keyword outside quotes.
    let mut in_quote = false;
    let bytes = upper.as_bytes();
    let kw_bytes = kw_upper.as_bytes();
    let kw_len = kw_bytes.len();

    for i in 0..bytes.len() {
        if bytes[i] == b'\'' {
            in_quote = !in_quote;
        }
        if !in_quote && i + kw_len <= bytes.len() && &bytes[i..i + kw_len] == kw_bytes {
            return Some(i);
        }
    }
    None
}

fn prop_equals(prop: &PropValue, value_str: &str) -> bool {
    match prop {
        PropValue::Str(s) => {
            let unquoted = value_str.trim_matches('\'');
            s == unquoted
        }
        PropValue::Int(n) => {
            value_str.trim().parse::<i64>().ok() == Some(*n)
        }
        PropValue::Float(f) => {
            value_str.trim().parse::<f64>().ok() == Some(*f)
        }
        PropValue::Bool(b) => match value_str.trim().to_uppercase().as_str() {
            "TRUE" => *b,
            "FALSE" => !*b,
            _ => false,
        },
    }
}

fn prop_compare(prop: &PropValue, value_str: &str) -> Option<std::cmp::Ordering> {
    match prop {
        PropValue::Int(n) => {
            let v = value_str.trim().parse::<i64>().ok()?;
            Some(n.cmp(&v))
        }
        PropValue::Float(f) => {
            let v = value_str.trim().parse::<f64>().ok()?;
            f.partial_cmp(&v)
        }
        PropValue::Str(s) => {
            let unquoted = value_str.trim_matches('\'');
            Some(s.as_str().cmp(unquoted))
        }
        PropValue::Bool(_) => None,
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn default_mqmd() -> Mqmd {
        Mqmd::default()
    }

    fn no_props() -> HashMap<String, PropValue> {
        HashMap::new()
    }

    fn default_pub_opts() -> PubOptions {
        PubOptions::default()
    }

    // -- Topic matching --

    #[test]
    fn test_exact_match() {
        assert!(topic_matches("Price/Fruit/Apples", "Price/Fruit/Apples"));
        assert!(!topic_matches("Price/Fruit/Apples", "Price/Fruit/Oranges"));
    }

    #[test]
    fn test_hash_wildcard() {
        assert!(topic_matches("Price/Fruit/#", "Price/Fruit/Apples"));
        assert!(topic_matches("Price/Fruit/#", "Price/Fruit/Citrus/Lemons"));
        assert!(topic_matches("Price/#", "Price/Fruit/Apples"));
        assert!(!topic_matches("Price/Fruit/#", "Price/Vegetables/Carrots"));
        // # matches zero levels.
        assert!(topic_matches("Price/Fruit/#", "Price/Fruit"));
    }

    #[test]
    fn test_plus_wildcard() {
        assert!(topic_matches("Price/+/Apples", "Price/Fruit/Apples"));
        assert!(!topic_matches("Price/+/Apples", "Price/Fruit/Oranges"));
        // + matches exactly one level.
        assert!(!topic_matches("Price/+", "Price/Fruit/Apples"));
    }

    #[test]
    fn test_combined_wildcards() {
        assert!(topic_matches("+/Fruit/#", "Price/Fruit/Apples"));
        assert!(topic_matches("+/+/+", "A/B/C"));
        assert!(!topic_matches("+/+", "A/B/C"));
    }

    #[test]
    fn test_hash_at_start() {
        assert!(topic_matches("#", "Any/Topic/At/All"));
        assert!(topic_matches("#", "Single"));
    }

    // -- Subscribe and publish --

    #[test]
    fn test_basic_pubsub() {
        let mut engine = PubSubEngine::new();

        let result = engine.subscribe("sub1", "Price/Fruit/Apples", &SubOptions::default());
        let dest = result.dest_queue.clone();

        let count = engine.publish(
            "Price/Fruit/Apples",
            default_mqmd(),
            b"Apple price: $1.50".to_vec(),
            no_props(),
            &default_pub_opts(),
        );

        assert_eq!(count, 1);
        let msg = engine.get_from_queue(&dest).unwrap();
        assert_eq!(msg.data, b"Apple price: $1.50");
    }

    #[test]
    fn test_wildcard_subscription() {
        let mut engine = PubSubEngine::new();

        let result = engine.subscribe("sub1", "Price/Fruit/#", &SubOptions::default());
        let dest = result.dest_queue.clone();

        engine.publish("Price/Fruit/Apples", default_mqmd(), b"apples".to_vec(), no_props(), &default_pub_opts());
        engine.publish("Price/Fruit/Citrus/Lemons", default_mqmd(), b"lemons".to_vec(), no_props(), &default_pub_opts());
        engine.publish("Price/Vegetables/Carrots", default_mqmd(), b"carrots".to_vec(), no_props(), &default_pub_opts());

        assert_eq!(engine.queue_depth(&dest), 2);
    }

    #[test]
    fn test_multiple_subscribers() {
        let mut engine = PubSubEngine::new();

        let r1 = engine.subscribe("sub1", "News/Sports/#", &SubOptions::default());
        let r2 = engine.subscribe("sub2", "News/#", &SubOptions::default());

        let count = engine.publish("News/Sports/Football", default_mqmd(), b"goal!".to_vec(), no_props(), &default_pub_opts());
        assert_eq!(count, 2);

        assert_eq!(engine.queue_depth(&r1.dest_queue), 1);
        assert_eq!(engine.queue_depth(&r2.dest_queue), 1);
    }

    #[test]
    fn test_no_matching_subscribers() {
        let mut engine = PubSubEngine::new();
        engine.subscribe("sub1", "A/B", &SubOptions::default());
        let count = engine.publish("X/Y", default_mqmd(), b"data".to_vec(), no_props(), &default_pub_opts());
        assert_eq!(count, 0);
    }

    // -- Durable subscriptions --

    #[test]
    fn test_durable_subscription() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            durability: SubDurability::Durable,
            ..Default::default()
        };
        let result = engine.subscribe("durable1", "Events/#", &opts);

        let sub = engine.get_subscription("durable1").unwrap();
        assert_eq!(sub.durability, SubDurability::Durable);
        assert_eq!(sub.dest_queue, result.dest_queue);
    }

    // -- Unsubscribe --

    #[test]
    fn test_unsubscribe() {
        let mut engine = PubSubEngine::new();
        let result = engine.subscribe("sub1", "A/B", &SubOptions::default());
        assert_eq!(engine.sub_count(), 1);

        engine.unsubscribe(result.sub_handle);
        assert_eq!(engine.sub_count(), 0);
    }

    // -- Retained publications --

    #[test]
    fn test_retained_publication() {
        let mut engine = PubSubEngine::new();

        // Publish retained BEFORE subscribing.
        let opts = PubOptions { retain: true };
        engine.publish("Sensors/Temp/Room1", default_mqmd(), b"22.5C".to_vec(), no_props(), &opts);

        // Subscribe — should receive retained.
        let result = engine.subscribe("sub1", "Sensors/Temp/Room1", &SubOptions::default());
        assert_eq!(engine.queue_depth(&result.dest_queue), 1);

        let msg = engine.get_from_queue(&result.dest_queue).unwrap();
        assert_eq!(msg.data, b"22.5C");
    }

    #[test]
    fn test_retained_overwrite() {
        let mut engine = PubSubEngine::new();
        let opts = PubOptions { retain: true };

        engine.publish("Topic/A", default_mqmd(), b"old".to_vec(), no_props(), &opts);
        engine.publish("Topic/A", default_mqmd(), b"new".to_vec(), no_props(), &opts);

        let result = engine.subscribe("sub1", "Topic/A", &SubOptions::default());
        let msg = engine.get_from_queue(&result.dest_queue).unwrap();
        assert_eq!(msg.data, b"new");
    }

    // -- MQSUBRQ --

    #[test]
    fn test_subrq() {
        let mut engine = PubSubEngine::new();
        let opts = PubOptions { retain: true };
        engine.publish("T/A", default_mqmd(), b"retained".to_vec(), no_props(), &opts);

        let result = engine.subscribe("sub1", "T/A", &SubOptions::default());
        // Drain the initial retained delivery.
        engine.get_from_queue(&result.dest_queue);
        assert_eq!(engine.queue_depth(&result.dest_queue), 0);

        // Re-request.
        assert!(engine.subrq(result.sub_handle));
        assert_eq!(engine.queue_depth(&result.dest_queue), 1);
    }

    // -- Selection strings --

    #[test]
    fn test_selection_string_match() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            selection_string: Some("Color = 'Red'".to_string()),
            ..Default::default()
        };
        let result = engine.subscribe("sub1", "Products/#", &opts);

        let mut props_red = HashMap::new();
        props_red.insert("Color".to_string(), PropValue::Str("Red".to_string()));

        let mut props_blue = HashMap::new();
        props_blue.insert("Color".to_string(), PropValue::Str("Blue".to_string()));

        engine.publish("Products/Item1", default_mqmd(), b"red item".to_vec(), props_red, &default_pub_opts());
        engine.publish("Products/Item2", default_mqmd(), b"blue item".to_vec(), props_blue, &default_pub_opts());

        assert_eq!(engine.queue_depth(&result.dest_queue), 1);
        let msg = engine.get_from_queue(&result.dest_queue).unwrap();
        assert_eq!(msg.data, b"red item");
    }

    #[test]
    fn test_selection_numeric_comparison() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            selection_string: Some("Weight > 100".to_string()),
            ..Default::default()
        };
        let result = engine.subscribe("sub1", "Items/#", &opts);

        let mut props_heavy = HashMap::new();
        props_heavy.insert("Weight".to_string(), PropValue::Int(150));

        let mut props_light = HashMap::new();
        props_light.insert("Weight".to_string(), PropValue::Int(50));

        engine.publish("Items/A", default_mqmd(), b"heavy".to_vec(), props_heavy, &default_pub_opts());
        engine.publish("Items/B", default_mqmd(), b"light".to_vec(), props_light, &default_pub_opts());

        assert_eq!(engine.queue_depth(&result.dest_queue), 1);
    }

    #[test]
    fn test_selection_and() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            selection_string: Some("Color = 'Red' AND Weight > 100".to_string()),
            ..Default::default()
        };
        let result = engine.subscribe("sub1", "Items/#", &opts);

        let mut props = HashMap::new();
        props.insert("Color".to_string(), PropValue::Str("Red".to_string()));
        props.insert("Weight".to_string(), PropValue::Int(150));

        engine.publish("Items/A", default_mqmd(), b"match".to_vec(), props, &default_pub_opts());

        let mut props2 = HashMap::new();
        props2.insert("Color".to_string(), PropValue::Str("Red".to_string()));
        props2.insert("Weight".to_string(), PropValue::Int(50));

        engine.publish("Items/B", default_mqmd(), b"no match".to_vec(), props2, &default_pub_opts());

        assert_eq!(engine.queue_depth(&result.dest_queue), 1);
    }

    #[test]
    fn test_selection_or() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            selection_string: Some("Color = 'Red' OR Color = 'Blue'".to_string()),
            ..Default::default()
        };
        let result = engine.subscribe("sub1", "Items/#", &opts);

        let mut p1 = HashMap::new();
        p1.insert("Color".to_string(), PropValue::Str("Red".to_string()));
        let mut p2 = HashMap::new();
        p2.insert("Color".to_string(), PropValue::Str("Green".to_string()));

        engine.publish("Items/A", default_mqmd(), b"red".to_vec(), p1, &default_pub_opts());
        engine.publish("Items/B", default_mqmd(), b"green".to_vec(), p2, &default_pub_opts());

        assert_eq!(engine.queue_depth(&result.dest_queue), 1);
    }

    #[test]
    fn test_no_selection_matches_all() {
        assert!(selection_matches(&None, &no_props()));
        assert!(selection_matches(&Some(String::new()), &no_props()));
    }

    // -- Managed vs unmanaged --

    #[test]
    fn test_unmanaged_subscription() {
        let mut engine = PubSubEngine::new();
        let opts = SubOptions {
            managed: SubManaged::Unmanaged,
            dest_queue: Some("MY.QUEUE".to_string()),
            ..Default::default()
        };
        let result = engine.subscribe("sub1", "A/B", &opts);
        assert_eq!(result.dest_queue, "MY.QUEUE");
    }

    // -- List subscriptions --

    #[test]
    fn test_list_subscriptions() {
        let mut engine = PubSubEngine::new();
        engine.subscribe("sub1", "A/#", &SubOptions::default());
        engine.subscribe("sub2", "B/#", &SubOptions::default());

        let subs = engine.list_subscriptions();
        assert_eq!(subs.len(), 2);
    }
}
