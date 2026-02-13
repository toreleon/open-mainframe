//! Keyword recognition for COBOL reserved words.
//!
//! COBOL has many reserved words. This module provides efficient
//! lookup to distinguish keywords from user-defined identifiers.

use crate::lexer::token::Keyword;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Map of uppercase keyword strings to Keyword enum values.
static KEYWORDS: LazyLock<HashMap<&'static str, Keyword>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    // Division and Section
    map.insert("IDENTIFICATION", Keyword::Identification);
    map.insert("ENVIRONMENT", Keyword::Environment);
    map.insert("DATA", Keyword::Data);
    map.insert("PROCEDURE", Keyword::Procedure);
    map.insert("DIVISION", Keyword::Division);
    map.insert("SECTION", Keyword::Section);

    // Identification Division paragraphs
    map.insert("PROGRAM-ID", Keyword::ProgramId);
    map.insert("AUTHOR", Keyword::Author);
    map.insert("INSTALLATION", Keyword::Installation);
    map.insert("DATE-WRITTEN", Keyword::DateWritten);
    map.insert("DATE-COMPILED", Keyword::DateCompiled);
    map.insert("SECURITY", Keyword::Security);

    // Environment Division
    map.insert("CONFIGURATION", Keyword::Configuration);
    map.insert("SOURCE-COMPUTER", Keyword::SourceComputer);
    map.insert("OBJECT-COMPUTER", Keyword::ObjectComputer);
    map.insert("SPECIAL-NAMES", Keyword::SpecialNames);
    map.insert("INPUT-OUTPUT", Keyword::InputOutput);
    map.insert("FILE-CONTROL", Keyword::FileControl);
    map.insert("SELECT", Keyword::Select);
    map.insert("ASSIGN", Keyword::Assign);
    map.insert("ORGANIZATION", Keyword::Organization);
    map.insert("SEQUENTIAL", Keyword::Sequential);
    map.insert("ACCESS", Keyword::AccessMode);
    map.insert("RANDOM", Keyword::Random);
    map.insert("DYNAMIC", Keyword::Dynamic);
    map.insert("ALTERNATE", Keyword::AlternateRecordKey);

    // Data Division
    map.insert("FILE", Keyword::File);
    map.insert("WORKING", Keyword::Working);
    map.insert("STORAGE", Keyword::Storage);
    map.insert("STATUS", Keyword::Status);
    map.insert("WORKING-STORAGE", Keyword::WorkingStorage);
    map.insert("LOCAL-STORAGE", Keyword::LocalStorage);
    map.insert("LINKAGE", Keyword::Linkage);
    map.insert("FD", Keyword::Fd);
    map.insert("SD", Keyword::Sd);
    map.insert("PIC", Keyword::Pic);
    map.insert("PICTURE", Keyword::Picture);
    map.insert("VALUE", Keyword::Value);
    map.insert("VALUES", Keyword::Values);
    map.insert("REDEFINES", Keyword::Redefines);
    map.insert("OCCURS", Keyword::Occurs);
    map.insert("TIMES", Keyword::Times);
    map.insert("INDEXED", Keyword::Indexed);
    map.insert("BY", Keyword::By);
    map.insert("ASCENDING", Keyword::Ascending);
    map.insert("DESCENDING", Keyword::Descending);
    map.insert("KEY", Keyword::Key);
    map.insert("DEPENDING", Keyword::Depending);
    map.insert("ON", Keyword::On);

    // Usage clauses
    map.insert("COMP", Keyword::Comp);
    map.insert("COMP-1", Keyword::Comp1);
    map.insert("COMP-2", Keyword::Comp2);
    map.insert("COMP-3", Keyword::Comp3);
    map.insert("COMP-4", Keyword::Comp4);
    map.insert("COMP-5", Keyword::Comp5);
    map.insert("COMPUTATIONAL", Keyword::Computational);
    map.insert("COMPUTATIONAL-1", Keyword::Computational1);
    map.insert("COMPUTATIONAL-2", Keyword::Computational2);
    map.insert("COMPUTATIONAL-3", Keyword::Computational3);
    map.insert("COMPUTATIONAL-4", Keyword::Computational4);
    map.insert("COMPUTATIONAL-5", Keyword::Computational5);
    map.insert("BINARY", Keyword::Binary);
    map.insert("PACKED-DECIMAL", Keyword::PackedDecimal);
    map.insert("DISPLAY", Keyword::Display);
    map.insert("USAGE", Keyword::Usage);

    // Sign and justify
    map.insert("SIGN", Keyword::Sign);
    map.insert("LEADING", Keyword::Leading);
    map.insert("TRAILING", Keyword::Trailing);
    map.insert("SEPARATE", Keyword::Separate);
    map.insert("CHARACTER", Keyword::Character);
    map.insert("JUSTIFIED", Keyword::Justified);
    map.insert("JUST", Keyword::Just);
    map.insert("RIGHT", Keyword::Right);
    map.insert("LEFT", Keyword::Left);
    map.insert("BLANK", Keyword::Blank);
    map.insert("WHEN", Keyword::When);

    // Figurative constants
    map.insert("ZERO", Keyword::Zero);
    map.insert("ZEROS", Keyword::Zeros);
    map.insert("ZEROES", Keyword::Zeroes);
    map.insert("SPACE", Keyword::Space);
    map.insert("SPACES", Keyword::Spaces);
    map.insert("HIGH-VALUE", Keyword::HighValue);
    map.insert("HIGH-VALUES", Keyword::HighValues);
    map.insert("LOW-VALUE", Keyword::LowValue);
    map.insert("LOW-VALUES", Keyword::LowValues);
    map.insert("QUOTE", Keyword::Quote);
    map.insert("QUOTES", Keyword::Quotes);
    map.insert("ALL", Keyword::All);
    map.insert("FILLER", Keyword::Filler);

    // Data item clauses
    map.insert("RENAMES", Keyword::Renames);
    map.insert("THROUGH", Keyword::Through);
    map.insert("THRU", Keyword::Thru);

    // Copybook
    map.insert("COPY", Keyword::Copy);
    map.insert("REPLACING", Keyword::Replacing);

    // Procedure Division verbs
    map.insert("PERFORM", Keyword::Perform);
    map.insert("UNTIL", Keyword::Until);
    map.insert("VARYING", Keyword::Varying);
    map.insert("FROM", Keyword::From);
    map.insert("AFTER", Keyword::After);
    map.insert("MOVE", Keyword::Move);
    map.insert("TO", Keyword::To);
    map.insert("CORRESPONDING", Keyword::Corresponding);
    map.insert("CORR", Keyword::Corr);
    map.insert("ADD", Keyword::Add);
    map.insert("SUBTRACT", Keyword::Subtract);
    map.insert("MULTIPLY", Keyword::Multiply);
    map.insert("DIVIDE", Keyword::Divide);
    map.insert("COMPUTE", Keyword::Compute);
    map.insert("GIVING", Keyword::Giving);
    map.insert("ROUNDED", Keyword::Rounded);
    map.insert("SIZE", Keyword::Size);
    map.insert("REMAINDER", Keyword::Remainder);

    // Control flow
    map.insert("IF", Keyword::If);
    map.insert("THEN", Keyword::Then);
    map.insert("ELSE", Keyword::Else);
    map.insert("END-IF", Keyword::EndIf);
    map.insert("EVALUATE", Keyword::Evaluate);
    map.insert("ALSO", Keyword::Also);
    map.insert("ANY", Keyword::Any);
    map.insert("TRUE", Keyword::True);
    map.insert("FALSE", Keyword::False);
    map.insert("OTHER", Keyword::Other);
    map.insert("END-EVALUATE", Keyword::EndEvaluate);
    map.insert("GO", Keyword::Go);
    map.insert("STOP", Keyword::Stop);
    map.insert("RUN", Keyword::Run);
    map.insert("EXIT", Keyword::Exit);
    map.insert("PROGRAM", Keyword::Program);
    map.insert("PARAGRAPH", Keyword::Paragraph);
    map.insert("CONTINUE", Keyword::Continue);

    // CALL
    map.insert("CALL", Keyword::Call);
    map.insert("USING", Keyword::Using);
    map.insert("RETURNING", Keyword::Returning);
    map.insert("REFERENCE", Keyword::Reference);
    map.insert("CONTENT", Keyword::Content);
    map.insert("LENGTH", Keyword::Length);
    map.insert("EXCEPTION", Keyword::OnException);
    map.insert("CANCEL", Keyword::Cancel);

    // String handling
    map.insert("INITIALIZE", Keyword::Initialize);
    map.insert("INSPECT", Keyword::Inspect);
    map.insert("TALLYING", Keyword::Tallying);
    map.insert("FOR", Keyword::For);
    map.insert("CHARACTERS", Keyword::Characters);
    map.insert("INITIAL", Keyword::Initial);
    map.insert("CONVERTING", Keyword::Converting);
    map.insert("BEFORE", Keyword::Before);
    map.insert("STRING", Keyword::String);
    map.insert("DELIMITED", Keyword::Delimited);
    map.insert("INTO", Keyword::Into);
    map.insert("POINTER", Keyword::Pointer);
    map.insert("OVERFLOW", Keyword::Overflow);
    map.insert("END-STRING", Keyword::EndString);
    map.insert("UNSTRING", Keyword::Unstring);
    map.insert("COUNT", Keyword::Count);
    map.insert("DELIMITER", Keyword::Delimiter);
    map.insert("END-UNSTRING", Keyword::EndUnstring);

    // I/O
    map.insert("ACCEPT", Keyword::Accept);
    map.insert("DATE", Keyword::Date);
    map.insert("DAY", Keyword::Day);
    map.insert("DAY-OF-WEEK", Keyword::DayOfWeek);
    map.insert("TIME", Keyword::Time);
    map.insert("OPEN", Keyword::Open);
    map.insert("INPUT", Keyword::Input);
    map.insert("OUTPUT", Keyword::Output);
    map.insert("I-O", Keyword::Io);
    map.insert("EXTEND", Keyword::Extend);
    map.insert("CLOSE", Keyword::Close);
    map.insert("READ", Keyword::Read);
    map.insert("NEXT", Keyword::Next);
    map.insert("RECORD", Keyword::Record);
    map.insert("END", Keyword::End);
    map.insert("AT", Keyword::AtEnd);
    map.insert("INVALID", Keyword::InvalidKey);
    map.insert("END-READ", Keyword::EndRead);
    map.insert("WRITE", Keyword::Write);
    map.insert("ADVANCING", Keyword::Advancing);
    map.insert("PAGE", Keyword::Page);
    map.insert("LINE", Keyword::Line);
    map.insert("LINES", Keyword::Lines);
    map.insert("END-WRITE", Keyword::EndWrite);
    map.insert("REWRITE", Keyword::Rewrite);
    map.insert("END-REWRITE", Keyword::EndRewrite);
    map.insert("DELETE", Keyword::Delete);
    map.insert("END-DELETE", Keyword::EndDelete);
    map.insert("START", Keyword::Start);
    map.insert("END-START", Keyword::EndStart);
    map.insert("RETURN", Keyword::Return);
    map.insert("END-RETURN", Keyword::EndReturn);

    // Sort/Merge
    map.insert("SORT", Keyword::Sort);
    map.insert("MERGE", Keyword::Merge);
    map.insert("RELEASE", Keyword::Release);
    map.insert("SEARCH", Keyword::Search);
    map.insert("END-SEARCH", Keyword::EndSearch);
    map.insert("SET", Keyword::Set);
    map.insert("UP", Keyword::UpBy);
    map.insert("DOWN", Keyword::DownBy);
    map.insert("ADDRESS", Keyword::Address);
    map.insert("OF", Keyword::Of);
    map.insert("UPON", Keyword::Upon);
    map.insert("NO", Keyword::NoAdvancing);

    // Conditions
    map.insert("AND", Keyword::And);
    map.insert("OR", Keyword::Or);
    map.insert("NOT", Keyword::Not);
    map.insert("IS", Keyword::Is);
    map.insert("ARE", Keyword::Are);
    map.insert("EQUAL", Keyword::Equal);
    map.insert("GREATER", Keyword::Greater);
    map.insert("LESS", Keyword::Less);
    map.insert("THAN", Keyword::Than);
    map.insert("NUMERIC", Keyword::Numeric);
    map.insert("ALPHABETIC", Keyword::Alphabetic);
    map.insert("ALPHABETIC-LOWER", Keyword::AlphabeticLower);
    map.insert("ALPHABETIC-UPPER", Keyword::AlphabeticUpper);
    map.insert("POSITIVE", Keyword::Positive);
    map.insert("NEGATIVE", Keyword::Negative);
    map.insert("CLASS", Keyword::Class);
    map.insert("OMITTED", Keyword::Omitted);

    // EXEC CICS/SQL support
    map.insert("EXEC", Keyword::Exec);
    map.insert("CICS", Keyword::Cics);
    map.insert("SQL", Keyword::Sql);
    map.insert("END-EXEC", Keyword::EndExec);

    // COBOL-2002 / Extensions
    map.insert("FUNCTION", Keyword::Function);
    map.insert("END-PROGRAM", Keyword::EndProgram);
    map.insert("END-PERFORM", Keyword::EndPerform);
    map.insert("END-COMPUTE", Keyword::EndCompute);
    map.insert("END-ADD", Keyword::EndAdd);
    map.insert("END-SUBTRACT", Keyword::EndSubtract);
    map.insert("END-MULTIPLY", Keyword::EndMultiply);
    map.insert("END-DIVIDE", Keyword::EndDivide);
    map.insert("END-CALL", Keyword::EndCall);
    map.insert("END-ACCEPT", Keyword::EndAccept);
    map.insert("NATIVE", Keyword::Native);
    map.insert("STANDARD-1", Keyword::Standard1);
    map.insert("STANDARD-2", Keyword::Standard2);
    map.insert("WITH", Keyword::With);
    map.insert("DEBUGGING", Keyword::Debugging);
    map.insert("MODE", Keyword::Mode);
    map.insert("COMMON", Keyword::Common);
    map.insert("EXTERNAL", Keyword::External);
    map.insert("GLOBAL", Keyword::Global);
    map.insert("CURRENCY", Keyword::CurrencySign);
    map.insert("DECIMAL-POINT", Keyword::DecimalPoint);
    map.insert("COMMA", Keyword::Comma);
    map.insert("ALPHABET", Keyword::AlphabetName);
    map.insert("SYMBOLIC", Keyword::SymbolicCharacters);
    map.insert("CONDITION", Keyword::ConditionName);
    map.insert("RELATIVE", Keyword::Relative);

    map
});

/// Look up a word to see if it's a reserved keyword.
///
/// The input is normalized to uppercase before lookup.
pub fn lookup_keyword(word: &str) -> Option<Keyword> {
    KEYWORDS.get(word.to_uppercase().as_str()).copied()
}

/// Check if a word is a reserved keyword.
pub fn is_keyword(word: &str) -> bool {
    lookup_keyword(word).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_lookup() {
        assert_eq!(
            lookup_keyword("IDENTIFICATION"),
            Some(Keyword::Identification)
        );
        assert_eq!(
            lookup_keyword("identification"),
            Some(Keyword::Identification)
        );
        assert_eq!(
            lookup_keyword("Identification"),
            Some(Keyword::Identification)
        );
        assert_eq!(lookup_keyword("PROGRAM-ID"), Some(Keyword::ProgramId));
        assert_eq!(lookup_keyword("DISPLAY"), Some(Keyword::Display));
    }

    #[test]
    fn test_non_keyword() {
        assert_eq!(lookup_keyword("MY-VARIABLE"), None);
        assert_eq!(lookup_keyword("CUSTOMER-NAME"), None);
    }

    #[test]
    fn test_is_keyword() {
        assert!(is_keyword("MOVE"));
        assert!(is_keyword("move"));
        assert!(!is_keyword("CUSTOMER"));
    }
}
