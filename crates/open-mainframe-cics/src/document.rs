//! # CICS DOCUMENT Commands
//!
//! Build dynamic HTML/text documents from templates and data.
//! Supports CREATE, INSERT, SET, RETRIEVE, and DELETE operations.

use std::collections::HashMap;

// ─────────────────────── Document ───────────────────────

/// A CICS document built from templates.
#[derive(Debug, Clone)]
pub struct Document {
    /// Document token (unique identifier).
    pub token: u64,
    /// Original template content.
    template: String,
    /// Symbol substitution values.
    symbols: HashMap<String, String>,
    /// Inserted text segments (before/after).
    inserts: Vec<InsertSegment>,
}

#[derive(Debug, Clone)]
struct InsertSegment {
    text: String,
    position: InsertPosition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InsertPosition {
    Before,
    After,
}

impl Document {
    /// Create a new document from a template.
    fn new(token: u64, template: &str) -> Self {
        Self {
            token,
            template: template.to_string(),
            symbols: HashMap::new(),
            inserts: Vec::new(),
        }
    }

    /// DOCUMENT INSERT — add text before or after the template content.
    pub fn insert(&mut self, text: &str, after: bool) {
        self.inserts.push(InsertSegment {
            text: text.to_string(),
            position: if after {
                InsertPosition::After
            } else {
                InsertPosition::Before
            },
        });
    }

    /// DOCUMENT SET — set a symbol value for template substitution.
    /// Replaces all occurrences of `&SYMBOL;` in the template.
    pub fn set_symbol(&mut self, symbol: &str, value: &str) {
        self.symbols.insert(symbol.to_uppercase(), value.to_string());
    }

    /// DOCUMENT RETRIEVE — assemble the complete document.
    pub fn retrieve(&self) -> String {
        // Start with template.
        let mut content = self.template.clone();

        // Apply symbol substitutions.
        for (symbol, value) in &self.symbols {
            let marker = format!("&{};", symbol);
            content = content.replace(&marker, value);
            // Also try lowercase marker.
            let marker_lower = format!("&{};", symbol.to_lowercase());
            content = content.replace(&marker_lower, value);
        }

        // Apply inserts.
        let mut before_text = String::new();
        let mut after_text = String::new();

        for insert in &self.inserts {
            match insert.position {
                InsertPosition::Before => before_text.push_str(&insert.text),
                InsertPosition::After => after_text.push_str(&insert.text),
            }
        }

        format!("{before_text}{content}{after_text}")
    }

    /// Get the number of symbols set.
    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }
}

// ─────────────────────── Document Manager ───────────────────────

/// Manager for CICS documents.
#[derive(Debug)]
pub struct DocumentManager {
    documents: HashMap<u64, Document>,
    next_token: u64,
    /// Named templates.
    templates: HashMap<String, String>,
}

impl DocumentManager {
    /// Create a new document manager.
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            next_token: 1,
            templates: HashMap::new(),
        }
    }

    /// Register a named template.
    pub fn register_template(&mut self, name: &str, content: &str) {
        self.templates
            .insert(name.to_uppercase(), content.to_string());
    }

    /// DOCUMENT CREATE — create a new document from a template.
    pub fn create(&mut self, template_name: &str) -> Result<u64, DocumentError> {
        let upper = template_name.to_uppercase();
        let template = self
            .templates
            .get(&upper)
            .ok_or_else(|| DocumentError::TemplateNotFound(upper.clone()))?;

        let token = self.next_token;
        self.next_token += 1;

        let doc = Document::new(token, template);
        self.documents.insert(token, doc);
        Ok(token)
    }

    /// DOCUMENT CREATE with inline content (no template name).
    pub fn create_inline(&mut self, content: &str) -> u64 {
        let token = self.next_token;
        self.next_token += 1;
        let doc = Document::new(token, content);
        self.documents.insert(token, doc);
        token
    }

    /// Get a mutable reference to a document.
    pub fn get_mut(&mut self, token: u64) -> Result<&mut Document, DocumentError> {
        self.documents
            .get_mut(&token)
            .ok_or(DocumentError::DocumentNotFound(token))
    }

    /// Get a reference to a document.
    pub fn get(&self, token: u64) -> Result<&Document, DocumentError> {
        self.documents
            .get(&token)
            .ok_or(DocumentError::DocumentNotFound(token))
    }

    /// DOCUMENT RETRIEVE — get the assembled content.
    pub fn retrieve(&self, token: u64) -> Result<String, DocumentError> {
        let doc = self.get(token)?;
        Ok(doc.retrieve())
    }

    /// DOCUMENT DELETE — free the document.
    pub fn delete(&mut self, token: u64) -> Result<(), DocumentError> {
        self.documents
            .remove(&token)
            .map(|_| ())
            .ok_or(DocumentError::DocumentNotFound(token))
    }

    /// Number of active documents.
    pub fn active_count(&self) -> usize {
        self.documents.len()
    }
}

impl Default for DocumentManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Document errors.
#[derive(Debug, thiserror::Error)]
pub enum DocumentError {
    #[error("template not found: {0}")]
    TemplateNotFound(String),
    #[error("document not found: token={0}")]
    DocumentNotFound(u64),
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── CICS-210.1: DOCUMENT CREATE ───

    #[test]
    fn test_document_create_from_template() {
        let mut mgr = DocumentManager::new();
        mgr.register_template("EMPRPT", "Employee: &EMPNAME; ID: &EMPID;");

        let token = mgr.create("EMPRPT").unwrap();
        assert!(token > 0);
        assert_eq!(mgr.active_count(), 1);
    }

    #[test]
    fn test_document_create_not_found() {
        let mut mgr = DocumentManager::new();
        assert!(mgr.create("NOSUCH").is_err());
    }

    #[test]
    fn test_document_create_inline() {
        let mut mgr = DocumentManager::new();
        let token = mgr.create_inline("Hello &NAME;!");
        assert!(token > 0);
    }

    // ─── CICS-210.2: DOCUMENT INSERT and SET ───

    #[test]
    fn test_document_insert_after() {
        let mut mgr = DocumentManager::new();
        let token = mgr.create_inline("Main Content");
        let doc = mgr.get_mut(token).unwrap();
        doc.insert(" [Footer]", true);

        let output = mgr.retrieve(token).unwrap();
        assert_eq!(output, "Main Content [Footer]");
    }

    #[test]
    fn test_document_insert_before() {
        let mut mgr = DocumentManager::new();
        let token = mgr.create_inline("Main Content");
        let doc = mgr.get_mut(token).unwrap();
        doc.insert("[Header] ", false);

        let output = mgr.retrieve(token).unwrap();
        assert_eq!(output, "[Header] Main Content");
    }

    #[test]
    fn test_document_set_symbol() {
        let mut mgr = DocumentManager::new();
        mgr.register_template("RPT", "Employee: &EMPNAME; ID: &EMPID;");
        let token = mgr.create("RPT").unwrap();

        let doc = mgr.get_mut(token).unwrap();
        doc.set_symbol("EMPNAME", "Smith");
        doc.set_symbol("EMPID", "12345");

        let output = mgr.retrieve(token).unwrap();
        assert!(output.contains("Smith"));
        assert!(output.contains("12345"));
        assert!(!output.contains("&EMPNAME;"));
    }

    #[test]
    fn test_document_multiple_substitutions() {
        let mut mgr = DocumentManager::new();
        mgr.register_template("MULTI", "&X; and &X; and &Y;");
        let token = mgr.create("MULTI").unwrap();

        let doc = mgr.get_mut(token).unwrap();
        doc.set_symbol("X", "hello");
        doc.set_symbol("Y", "world");

        let output = mgr.retrieve(token).unwrap();
        assert_eq!(output, "hello and hello and world");
    }

    // ─── CICS-210.3: DOCUMENT RETRIEVE ───

    #[test]
    fn test_document_retrieve() {
        let mut mgr = DocumentManager::new();
        mgr.register_template("SIMPLE", "Hello &NAME;!");
        let token = mgr.create("SIMPLE").unwrap();

        let doc = mgr.get_mut(token).unwrap();
        doc.set_symbol("NAME", "World");

        let output = mgr.retrieve(token).unwrap();
        assert_eq!(output, "Hello World!");
    }

    #[test]
    fn test_document_retrieve_with_inserts_and_symbols() {
        let mut mgr = DocumentManager::new();
        mgr.register_template("HTML", "<h1>&TITLE;</h1><p>&BODY;</p>");
        let token = mgr.create("HTML").unwrap();

        let doc = mgr.get_mut(token).unwrap();
        doc.set_symbol("TITLE", "Report");
        doc.set_symbol("BODY", "Employee data");
        doc.insert("<html>", false);
        doc.insert("</html>", true);

        let output = mgr.retrieve(token).unwrap();
        assert!(output.starts_with("<html>"));
        assert!(output.ends_with("</html>"));
        assert!(output.contains("<h1>Report</h1>"));
    }

    // ─── CICS-210.4: DOCUMENT DELETE ───

    #[test]
    fn test_document_delete() {
        let mut mgr = DocumentManager::new();
        let token = mgr.create_inline("test");
        assert_eq!(mgr.active_count(), 1);

        mgr.delete(token).unwrap();
        assert_eq!(mgr.active_count(), 0);
        assert!(mgr.retrieve(token).is_err());
    }

    #[test]
    fn test_document_delete_not_found() {
        let mut mgr = DocumentManager::new();
        assert!(mgr.delete(999).is_err());
    }

    // ─── CICS-210.5: Document Integration Tests ───

    #[test]
    fn test_full_document_lifecycle() {
        let mut mgr = DocumentManager::new();

        // Register templates.
        mgr.register_template("HEADER", "<header>&TITLE;</header>");
        mgr.register_template("BODY", "<body>&CONTENT;</body>");

        // Create documents.
        let t1 = mgr.create("HEADER").unwrap();
        let t2 = mgr.create("BODY").unwrap();

        // Set symbols.
        mgr.get_mut(t1).unwrap().set_symbol("TITLE", "My Page");
        mgr.get_mut(t2).unwrap().set_symbol("CONTENT", "Hello!");

        // Retrieve.
        assert_eq!(
            mgr.retrieve(t1).unwrap(),
            "<header>My Page</header>"
        );
        assert_eq!(
            mgr.retrieve(t2).unwrap(),
            "<body>Hello!</body>"
        );

        // Delete.
        mgr.delete(t1).unwrap();
        mgr.delete(t2).unwrap();
        assert_eq!(mgr.active_count(), 0);
    }
}
