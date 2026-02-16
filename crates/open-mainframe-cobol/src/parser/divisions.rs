//! IDENTIFICATION DIVISION and ENVIRONMENT DIVISION parsers.
//!
//! This module contains the parser methods for the IDENTIFICATION DIVISION
//! and the ENVIRONMENT DIVISION, including configuration and input-output
//! sections.

use super::Result;
use crate::ast::*;
use crate::lexer::{Keyword, TokenKind};

impl super::Parser {
    // ========================================================================
    // IDENTIFICATION DIVISION
    // ========================================================================

    pub(super) fn parse_identification_division(&mut self) -> Result<IdentificationDivision> {
        let start = self.current_span();

        // IDENTIFICATION DIVISION.
        self.expect_keyword(Keyword::Identification)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        // PROGRAM-ID. name.
        self.expect_keyword(Keyword::ProgramId)?;
        self.expect(TokenKind::Period)?;

        let program_name = self.expect_identifier()?;
        let mut is_common = false;
        let mut is_initial = false;

        // Check for [IS] COMMON/INITIAL [PROGRAM]
        // IBM allows: PROGRAM-ID. name [IS] [COMMON] [INITIAL] [PROGRAM].
        if self.check_keyword(Keyword::Is) {
            self.advance();
        }
        // COMMON and INITIAL can appear in any order
        for _ in 0..2 {
            if self.check_keyword(Keyword::Common) {
                self.advance();
                is_common = true;
            } else if self.check_keyword(Keyword::Initial) {
                self.advance();
                is_initial = true;
            }
        }
        if self.check_keyword(Keyword::Program) {
            self.advance();
        }

        self.expect(TokenKind::Period)?;

        let program_id = ProgramId {
            name: program_name.clone(),
            is_common,
            is_initial,
            span: start.extend(self.previous_span()),
        };

        // Parse optional paragraphs
        let mut author = None;
        let mut installation = None;
        let mut date_written = None;
        let mut date_compiled = None;
        let mut security = None;

        while !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::Author) {
                self.advance();
                self.skip_if(TokenKind::Period);
                author = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::Installation) {
                self.advance();
                self.skip_if(TokenKind::Period);
                installation = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::DateWritten) {
                self.advance();
                self.skip_if(TokenKind::Period);
                date_written = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::DateCompiled) {
                self.advance();
                self.skip_if(TokenKind::Period);
                date_compiled = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::Security) {
                self.advance();
                self.skip_if(TokenKind::Period);
                security = Some(self.consume_until_period());
            } else {
                // Unknown paragraph, skip to next period
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(IdentificationDivision {
            program_id,
            author,
            installation,
            date_written,
            date_compiled,
            security,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // ENVIRONMENT DIVISION
    // ========================================================================

    pub(super) fn parse_environment_division(&mut self) -> Result<EnvironmentDivision> {
        let start = self.current_span();

        // ENVIRONMENT DIVISION.
        self.expect_keyword(Keyword::Environment)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        let mut configuration = None;
        let mut input_output = None;

        // CONFIGURATION SECTION
        if self.check_keyword(Keyword::Configuration) {
            configuration = Some(self.parse_configuration_section()?);
        }

        // INPUT-OUTPUT SECTION
        if self.check_keyword(Keyword::InputOutput) {
            input_output = Some(self.parse_input_output_section()?);
        }

        let end = self.previous_span();

        Ok(EnvironmentDivision {
            configuration,
            input_output,
            span: start.extend(end),
        })
    }

    fn parse_configuration_section(&mut self) -> Result<ConfigurationSection> {
        let start = self.current_span();

        // CONFIGURATION SECTION.
        self.expect_keyword(Keyword::Configuration)?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut source_computer = None;
        let mut object_computer = None;
        let special_names = Vec::new();

        while !self.is_at_section_start() && !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::SourceComputer) {
                self.advance();
                self.skip_if(TokenKind::Period);
                source_computer = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::ObjectComputer) {
                self.advance();
                self.skip_if(TokenKind::Period);
                object_computer = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::SpecialNames) {
                self.advance();
                self.skip_if(TokenKind::Period);
                // TODO: Parse special-names entries
                self.advance_to_next_sentence();
            } else {
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(ConfigurationSection {
            source_computer,
            object_computer,
            special_names,
            span: start.extend(end),
        })
    }

    fn parse_input_output_section(&mut self) -> Result<InputOutputSection> {
        let start = self.current_span();

        // INPUT-OUTPUT SECTION.
        self.expect_keyword(Keyword::InputOutput)?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut file_control = Vec::new();

        // FILE-CONTROL.
        if self.check_keyword(Keyword::FileControl) {
            self.advance();
            self.expect(TokenKind::Period)?;

            // Parse SELECT statements
            while self.check_keyword(Keyword::Select) {
                file_control.push(self.parse_file_control_entry()?);
            }
        }

        let end = self.previous_span();

        Ok(InputOutputSection {
            file_control,
            span: start.extend(end),
        })
    }

    fn parse_file_control_entry(&mut self) -> Result<FileControlEntry> {
        let start = self.current_span();

        // SELECT file-name
        self.expect_keyword(Keyword::Select)?;
        let file_name = self.expect_identifier()?;

        // ASSIGN TO external-name
        self.expect_keyword(Keyword::Assign)?;
        if self.check_keyword(Keyword::To) {
            self.advance();
        }
        let assign_to = self.expect_identifier_or_string()?;

        let mut organization = FileOrganization::Sequential;
        let mut access_mode = AccessMode::Sequential;
        let mut record_key = None;
        let mut file_status = None;

        // Parse optional clauses until period
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Organization) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                organization = self.parse_file_organization()?;
            } else if self.check_keyword(Keyword::AccessMode) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                access_mode = self.parse_access_mode()?;
            } else if self.check_keyword(Keyword::RecordKey) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                record_key = Some(self.parse_qualified_name()?);
            } else if self.check_keyword(Keyword::FileStatus) || self.check_keyword(Keyword::File) {
                if self.check_keyword(Keyword::File) {
                    self.advance(); // FILE
                    self.expect_keyword(Keyword::Status)?; // "FILE STATUS"
                }
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                file_status = Some(self.parse_qualified_name()?);
            } else {
                // Skip unknown clause
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        let end = self.previous_span();

        Ok(FileControlEntry {
            file_name,
            assign_to,
            organization,
            access_mode,
            record_key,
            file_status,
            span: start.extend(end),
        })
    }

    fn parse_file_organization(&mut self) -> Result<FileOrganization> {
        if self.check_keyword(Keyword::Sequential) {
            self.advance();
            Ok(FileOrganization::Sequential)
        } else if self.check_keyword(Keyword::Indexed) {
            self.advance();
            Ok(FileOrganization::Indexed)
        } else if self.check_keyword(Keyword::Relative) {
            self.advance();
            Ok(FileOrganization::Relative)
        } else {
            Ok(FileOrganization::Sequential)
        }
    }

    fn parse_access_mode(&mut self) -> Result<AccessMode> {
        if self.check_keyword(Keyword::Sequential) {
            self.advance();
            Ok(AccessMode::Sequential)
        } else if self.check_keyword(Keyword::Random) {
            self.advance();
            Ok(AccessMode::Random)
        } else if self.check_keyword(Keyword::Dynamic) {
            self.advance();
            Ok(AccessMode::Dynamic)
        } else {
            Ok(AccessMode::Sequential)
        }
    }
}
