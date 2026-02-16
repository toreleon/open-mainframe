//! DATA DIVISION parsers.
//!
//! This module contains the parser methods for the DATA DIVISION,
//! including file section, working-storage, local-storage, and linkage
//! section parsing, as well as data item, picture clause, usage, occurs,
//! and sign clause parsing.

use super::Result;
use crate::ast::*;
use crate::lexer::{Keyword, TokenKind};

impl super::Parser {
    // ========================================================================
    // DATA DIVISION
    // ========================================================================

    pub(super) fn parse_data_division(&mut self) -> Result<DataDivision> {
        let start = self.current_span();

        // DATA DIVISION.
        self.expect_keyword(Keyword::Data)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        let mut file_section = Vec::new();
        let mut working_storage = Vec::new();
        let mut local_storage = Vec::new();
        let mut linkage = Vec::new();

        // Parse sections
        while !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::File) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                file_section = self.parse_file_section()?;
            } else if self.check_keyword(Keyword::WorkingStorage) {
                self.advance(); // WORKING-STORAGE
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                working_storage = self.parse_data_items()?;
            } else if self.check_keyword(Keyword::LocalStorage) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                local_storage = self.parse_data_items()?;
            } else if self.check_keyword(Keyword::Linkage) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                linkage = self.parse_data_items()?;
            } else {
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(DataDivision {
            file_section,
            working_storage,
            local_storage,
            linkage,
            span: start.extend(end),
        })
    }

    fn parse_file_section(&mut self) -> Result<Vec<FileDescription>> {
        let mut files = Vec::new();

        while self.check_keyword(Keyword::Fd) || self.check_keyword(Keyword::Sd) {
            files.push(self.parse_file_description()?);
        }

        Ok(files)
    }

    fn parse_file_description(&mut self) -> Result<FileDescription> {
        let start = self.current_span();

        let is_sort_file = self.check_keyword(Keyword::Sd);
        self.advance(); // FD or SD

        let name = self.expect_identifier()?;

        // Parse FD clauses until period
        let record_contains = None;
        let block_contains = None;

        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Record) {
                self.advance();
                // RECORD CONTAINS clause
                // Skip for now
                while !self.check(TokenKind::Period) && !self.is_at_end() {
                    self.advance();
                }
            } else {
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        // Parse record descriptions
        let records = self.parse_data_items()?;

        let end = self.previous_span();

        Ok(FileDescription {
            name,
            is_sort_file,
            records,
            record_contains,
            block_contains,
            code_set: None,
            linage: None,
            span: start.extend(end),
        })
    }

    fn parse_data_items(&mut self) -> Result<Vec<DataItem>> {
        let mut items = Vec::new();

        while self.check_level_number() {
            items.push(self.parse_data_item()?);
        }

        Ok(items)
    }

    fn parse_data_item(&mut self) -> Result<DataItem> {
        let start = self.current_span();

        // Level number
        let level = self.expect_level_number()?;

        // Data name or FILLER
        let name = if self.check_keyword(Keyword::Filler) {
            self.advance();
            DataItemName::Filler
        } else if self.check_identifier() {
            DataItemName::Named(self.expect_identifier()?)
        } else {
            DataItemName::Filler // Implicit FILLER
        };

        let mut picture = None;
        let mut usage = None;
        let mut value = None;
        let mut occurs = None;
        let mut redefines = None;
        let mut renames = None;
        let mut sign = None;
        let mut justified = false;
        let mut blank_when_zero = false;
        let mut external = false;
        let mut global = false;
        let mut synchronized = None;

        // Level 66 RENAMES
        if level == 66 {
            if self.check_keyword(Keyword::Renames) {
                self.advance();
                let from = self.parse_qualified_name()?;
                let thru = if self.check_keyword(Keyword::Through) || self.check_keyword(Keyword::Thru) {
                    self.advance();
                    Some(self.parse_qualified_name()?)
                } else {
                    None
                };
                renames = Some((from, thru));
            }
        }

        // Parse clauses until period
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Pic) || self.check_keyword(Keyword::Picture) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                picture = Some(self.parse_picture_clause()?);
            } else if self.check_keyword(Keyword::Usage) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                usage = Some(self.parse_usage()?);
            } else if self.check_keyword(Keyword::Value) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                value = Some(self.parse_literal()?);
            } else if self.check_keyword(Keyword::Occurs) {
                self.advance();
                occurs = Some(self.parse_occurs_clause()?);
            } else if self.check_keyword(Keyword::Redefines) {
                self.advance();
                redefines = Some(self.parse_qualified_name()?);
            } else if self.check_keyword(Keyword::Sign) {
                self.advance();
                sign = Some(self.parse_sign_clause()?);
            } else if self.check_keyword(Keyword::Justified) || self.check_keyword(Keyword::Just) {
                self.advance();
                if self.check_keyword(Keyword::Right) {
                    self.advance();
                }
                justified = true;
            } else if self.check_keyword(Keyword::Blank) {
                self.advance();
                if self.check_keyword(Keyword::When) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Zero) {
                    self.advance();
                }
                blank_when_zero = true;
            } else if self.check_identifier_value("EXTERNAL") {
                self.advance();
                external = true;
            } else if self.check_identifier_value("GLOBAL") {
                self.advance();
                global = true;
            } else if self.check_identifier_value("SYNCHRONIZED") || self.check_identifier_value("SYNC") {
                self.advance();
                synchronized = Some(if self.check_keyword(Keyword::Left) {
                    self.advance();
                    SyncDirection::Left
                } else if self.check_keyword(Keyword::Right) {
                    self.advance();
                    SyncDirection::Right
                } else {
                    SyncDirection::Default
                });
            } else if self.is_usage_keyword() {
                // Implicit USAGE
                usage = Some(self.parse_usage()?);
            } else {
                // Unknown clause, skip
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        // Parse subordinate items (for group items) and level-88 conditions
        let mut children = Vec::new();
        let mut condition_values = Vec::new();

        // Level-88 condition names can follow any data item (with or without PIC)
        // Group items (no PIC) can also have child data items
        if level != 88 && level != 66 {
            while self.check_level_number() {
                let child_level = self.peek_level_number();
                if child_level == 88 {
                    // Level-88 condition name - always collect under current item
                    let child = self.parse_data_item()?;
                    let cond_values = if let Some(ref val) = child.value {
                        vec![ConditionValueEntry::Single(val.clone())]
                    } else {
                        vec![]
                    };
                    condition_values.push(ConditionValue {
                        name: child.name.as_str().unwrap_or("").to_string(),
                        values: cond_values,
                        span: child.span,
                    });
                } else if picture.is_none() && level < 77 && child_level > level {
                    // Child data item of a group (no PIC clause)
                    let child = self.parse_data_item()?;
                    children.push(child);
                } else {
                    break;
                }
            }
        }

        let end = self.previous_span();

        Ok(DataItem {
            level,
            name,
            picture,
            usage,
            value,
            occurs,
            redefines,
            sign,
            justified,
            blank_when_zero,
            external,
            global,
            synchronized,
            renames,
            dynamic_length: false,
            group_usage: None,
            children,
            condition_values,
            span: start.extend(end),
        })
    }

    fn parse_picture_clause(&mut self) -> Result<PictureClause> {
        let start = self.current_span();

        let picture = if let TokenKind::PictureString(s) = &self.current().kind {
            let pic = s.clone();
            self.advance();
            pic
        } else {
            // Fallback: collect tokens until next clause
            let mut pic = String::new();
            while !self.is_data_clause_start()
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                if let TokenKind::Identifier(s) = &self.current().kind {
                    pic.push_str(s);
                } else if let TokenKind::IntegerLiteral(n) = &self.current().kind {
                    pic.push_str(&n.to_string());
                } else if let TokenKind::LeftParen = &self.current().kind {
                    pic.push('(');
                } else if let TokenKind::RightParen = &self.current().kind {
                    pic.push(')');
                }
                self.advance();
            }
            pic
        };

        // Analyze picture to determine category and size
        let (category, size, decimal_positions) = analyze_picture(&picture);

        let end = self.previous_span();

        Ok(PictureClause {
            picture,
            category,
            size,
            decimal_positions,
            span: start.extend(end),
        })
    }

    fn parse_usage(&mut self) -> Result<Usage> {
        if self.check_keyword(Keyword::Display) {
            self.advance();
            Ok(Usage::Display)
        } else if self.check_keyword(Keyword::Binary)
            || self.check_keyword(Keyword::Comp)
            || self.check_keyword(Keyword::Comp4)
            || self.check_keyword(Keyword::Computational)
            || self.check_keyword(Keyword::Computational4)
        {
            self.advance();
            Ok(Usage::Binary)
        } else if self.check_keyword(Keyword::Comp1) || self.check_keyword(Keyword::Computational1)
        {
            self.advance();
            Ok(Usage::Comp1)
        } else if self.check_keyword(Keyword::Comp2) || self.check_keyword(Keyword::Computational2)
        {
            self.advance();
            Ok(Usage::Comp2)
        } else if self.check_keyword(Keyword::Comp3)
            || self.check_keyword(Keyword::Computational3)
            || self.check_keyword(Keyword::PackedDecimal)
        {
            self.advance();
            Ok(Usage::PackedDecimal)
        } else if self.check_keyword(Keyword::Comp5) || self.check_keyword(Keyword::Computational5)
        {
            self.advance();
            Ok(Usage::Comp5)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Ok(Usage::Pointer)
        } else if self.check_keyword(Keyword::Indexed) {
            self.advance();
            Ok(Usage::Index)
        } else if self.check_identifier_value("FUNCTION-POINTER") {
            self.advance();
            Ok(Usage::FunctionPointer)
        } else if self.check_identifier_value("PROCEDURE-POINTER") {
            self.advance();
            Ok(Usage::ProcedurePointer)
        } else if self.check_identifier_value("NATIONAL") {
            self.advance();
            Ok(Usage::National)
        } else {
            Ok(Usage::Display)
        }
    }

    fn parse_occurs_clause(&mut self) -> Result<OccursClause> {
        let start = self.current_span();

        // n TIMES or n TO m TIMES
        let times = self.expect_integer()? as u32;
        let mut max_times = None;
        let mut depending_on = None;
        let mut indexed_by = Vec::new();
        let mut keys = Vec::new();

        if self.check_keyword(Keyword::To) {
            self.advance();
            max_times = Some(self.expect_integer()? as u32);
        }

        if self.check_keyword(Keyword::Times) {
            self.advance();
        }

        // DEPENDING ON
        if self.check_keyword(Keyword::Depending) {
            self.advance();
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            depending_on = Some(self.parse_qualified_name()?);
        }

        // ASCENDING/DESCENDING KEY
        while self.check_keyword(Keyword::Ascending) || self.check_keyword(Keyword::Descending) {
            let ascending = self.check_keyword(Keyword::Ascending);
            self.advance();
            if self.check_keyword(Keyword::Key) {
                self.advance();
            }
            if self.check_keyword(Keyword::Is) {
                self.advance();
            }
            let key_name = self.parse_qualified_name()?;
            keys.push(OccursKey {
                name: key_name.clone(),
                ascending,
                span: key_name.span,
            });
        }

        // INDEXED BY
        if self.check_keyword(Keyword::Indexed) {
            self.advance();
            if self.check_keyword(Keyword::By) {
                self.advance();
            }
            while self.check_identifier() {
                indexed_by.push(self.expect_identifier()?);
            }
        }

        let end = self.previous_span();

        Ok(OccursClause {
            times,
            max_times,
            depending_on,
            indexed_by,
            keys,
            span: start.extend(end),
        })
    }

    fn parse_sign_clause(&mut self) -> Result<SignClause> {
        let mut leading = true;
        let mut separate = false;

        if self.check_keyword(Keyword::Is) {
            self.advance();
        }

        if self.check_keyword(Keyword::Leading) {
            self.advance();
            leading = true;
        } else if self.check_keyword(Keyword::Trailing) {
            self.advance();
            leading = false;
        }

        if self.check_keyword(Keyword::Separate) {
            self.advance();
            separate = true;
            if self.check_keyword(Keyword::Character) {
                self.advance();
            }
        }

        Ok(SignClause { leading, separate })
    }
}

/// Analyze a PICTURE string to determine its category and size.
pub(super) fn analyze_picture(picture: &str) -> (PictureCategory, u32, u32) {
    let upper = picture.to_uppercase();
    let mut size = 0u32;
    let mut decimal_pos = 0u32;
    let mut seen_v = false;
    let mut has_9 = false;
    let mut has_x = false;
    let mut has_a = false;
    let mut has_edit = false;

    let chars: Vec<char> = upper.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];
        let count = if i + 1 < chars.len() && chars[i + 1] == '(' {
            // Parse repeat count
            let mut end = i + 2;
            while end < chars.len() && chars[end] != ')' {
                end += 1;
            }
            let count_str: String = chars[i + 2..end].iter().collect();
            let count = count_str.parse::<u32>().unwrap_or(1);
            i = end + 1;
            count
        } else {
            i += 1;
            1
        };

        match ch {
            '9' => {
                has_9 = true;
                size += count;
                if seen_v {
                    decimal_pos += count;
                }
            }
            'X' => {
                has_x = true;
                size += count;
            }
            'A' => {
                has_a = true;
                size += count;
            }
            'S' => { /* Sign, doesn't add to size for DISPLAY */ }
            'V' => {
                seen_v = true;
            }
            'P' => {
                // Assumed decimal position
                if seen_v {
                    decimal_pos += count;
                }
            }
            'Z' | '*' | '+' | '-' | '$' | ',' | '.' | '/' | 'B' | '0' => {
                has_edit = true;
                size += count;
            }
            'C' | 'D' => {
                // CR/DB
                has_edit = true;
                size += 2;
            }
            _ => {}
        }
    }

    let category = if has_edit && has_9 {
        PictureCategory::NumericEdited
    } else if has_9 && !has_x && !has_a {
        PictureCategory::Numeric
    } else if has_x {
        PictureCategory::Alphanumeric
    } else if has_a {
        PictureCategory::Alphabetic
    } else {
        PictureCategory::Alphanumeric
    };

    (category, size, decimal_pos)
}
