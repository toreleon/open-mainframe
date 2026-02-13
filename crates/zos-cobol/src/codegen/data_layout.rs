//! Data layout generation for COBOL DATA DIVISION.
//!
//! This module handles the translation of COBOL data descriptions
//! into LLVM memory layouts.

use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use inkwell::values::GlobalValue;

use crate::ast::{DataDivision, DataItem, DataItemName};

use super::types::LlvmType;

/// Information about a data item in the generated layout.
#[derive(Debug, Clone)]
pub struct DataItemInfo<'ctx> {
    /// LLVM type for this item.
    pub llvm_type: LlvmType<'ctx>,
    /// Global variable (for WORKING-STORAGE).
    pub global: Option<GlobalValue<'ctx>>,
    /// Offset within parent (for group members).
    pub offset: u32,
    /// Total size in bytes.
    pub size: u32,
}

/// Data layout manager.
pub struct DataLayout<'ctx> {
    /// Map from item name to info.
    items: HashMap<String, DataItemInfo<'ctx>>,
    /// Current offset in WORKING-STORAGE.
    ws_offset: u32,
}

impl<'ctx> DataLayout<'ctx> {
    /// Create a new data layout manager.
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
            ws_offset: 0,
        }
    }

    /// Get information about a data item by name.
    pub fn get(&self, name: &str) -> Option<&DataItemInfo<'ctx>> {
        self.items.get(&name.to_uppercase())
    }

    /// Calculate the total size of WORKING-STORAGE.
    pub fn working_storage_size(&self) -> u32 {
        self.ws_offset
    }

    /// Process DATA DIVISION and build layout information.
    pub fn process_data_division(&mut self, context: &'ctx Context, data: &DataDivision) {
        // Process WORKING-STORAGE
        self.ws_offset = 0;
        for item in &data.working_storage {
            self.process_item(context, item, 0);
        }
    }

    /// Process a single data item.
    fn process_item(&mut self, context: &'ctx Context, item: &DataItem, parent_offset: u32) -> u32 {
        let name = match &item.name {
            DataItemName::Named(n) => n.clone(),
            DataItemName::Filler => format!("FILLER_{}", self.items.len()),
        };

        let llvm_type = LlvmType::from_data_item(context, item);
        let size = self.calculate_item_size(context, item);

        let info = DataItemInfo {
            llvm_type,
            global: None,
            offset: self.ws_offset,
            size,
        };

        self.items.insert(name.to_uppercase(), info);

        // Process children
        let mut child_offset = 0;
        for child in &item.children {
            child_offset += self.process_item(context, child, child_offset);
        }

        // Update working storage offset
        self.ws_offset += size;

        size
    }

    /// Calculate the size of a data item in bytes.
    fn calculate_item_size(&self, context: &'ctx Context, item: &DataItem) -> u32 {
        // If group item, sum children sizes
        if item.picture.is_none() && !item.children.is_empty() {
            return item
                .children
                .iter()
                .map(|child| self.calculate_item_size(context, child))
                .sum();
        }

        // Elementary item
        let pic_size = item.picture.as_ref().map(|p| p.size).unwrap_or(1);
        let usage = item.usage.unwrap_or(crate::ast::Usage::Display);
        let element_size = LlvmType::calculate_storage_size(pic_size, usage);

        // Handle OCCURS
        let occurs = item.occurs.as_ref().map(|o| o.times).unwrap_or(1);

        element_size * occurs
    }
}

impl Default for DataLayout<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{OccursClause, Picture, PictureCategory, Usage};
    use crate::lexer::{FileId, Span};

    fn make_span() -> Span {
        Span::new(FileId::MAIN, 0, 0)
    }

    #[test]
    fn test_data_layout_creation() {
        let layout = DataLayout::new();
        assert_eq!(layout.working_storage_size(), 0);
    }

    #[test]
    fn test_calculate_item_size() {
        let context = Context::create();
        let layout = DataLayout::new();

        // Simple alphanumeric item: 10 bytes
        let item = DataItem {
            level: 1,
            name: DataItemName::Named("WS-FIELD".to_string()),
            picture: Some(Picture {
                picture: "X(10)".to_string(),
                category: PictureCategory::Alphanumeric,
                size: 10,
                decimal_positions: 0,
                span: make_span(),
            }),
            usage: Some(Usage::Display),
            value: None,
            redefines: None,
            occurs: None,
            children: Vec::new(),
            condition_values: Vec::new(),
            is_filler: false,
            span: make_span(),
        };

        assert_eq!(layout.calculate_item_size(&context, &item), 10);
    }

    #[test]
    fn test_calculate_occurs_size() {
        let context = Context::create();
        let layout = DataLayout::new();

        // Item with OCCURS 5 TIMES, 10 bytes each = 50 bytes
        let item = DataItem {
            level: 1,
            name: DataItemName::Named("WS-TABLE".to_string()),
            picture: Some(Picture {
                picture: "X(10)".to_string(),
                category: PictureCategory::Alphanumeric,
                size: 10,
                decimal_positions: 0,
                span: make_span(),
            }),
            usage: Some(Usage::Display),
            value: None,
            redefines: None,
            occurs: Some(OccursClause {
                times: 5,
                min: None,
                max: None,
                depending_on: None,
                key_clauses: Vec::new(),
                indexed_by: Vec::new(),
                span: make_span(),
            }),
            children: Vec::new(),
            condition_values: Vec::new(),
            is_filler: false,
            span: make_span(),
        };

        assert_eq!(layout.calculate_item_size(&context, &item), 50);
    }
}
