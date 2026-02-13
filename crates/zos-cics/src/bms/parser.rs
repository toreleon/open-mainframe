//! BMS map definition parser.
//!
//! Parses BMS macro definitions (DFHMSD, DFHMDI, DFHMDF).

use super::field::{BmsField, FieldAttribute, FieldJustify};
use super::{FieldColor, FieldHighlight, ScreenSize};
use crate::{CicsError, CicsResult};
use std::collections::HashMap;

/// A BMS mapset (collection of maps).
#[derive(Debug, Clone)]
pub struct BmsMapset {
    /// Mapset name
    pub name: String,
    /// Storage type
    pub storage: StorageType,
    /// Terminal type
    pub term_type: TerminalType,
    /// Language
    pub lang: MapLanguage,
    /// Control options
    pub ctrl: ControlOptions,
    /// Maps in this mapset
    pub maps: Vec<BmsMap>,
}

impl BmsMapset {
    /// Create a new mapset.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            storage: StorageType::Auto,
            term_type: TerminalType::Type3270_2,
            lang: MapLanguage::Cobol,
            ctrl: ControlOptions::default(),
            maps: Vec::new(),
        }
    }

    /// Add a map to the mapset.
    pub fn add_map(&mut self, map: BmsMap) {
        self.maps.push(map);
    }

    /// Get map by name.
    pub fn get_map(&self, name: &str) -> Option<&BmsMap> {
        self.maps.iter().find(|m| m.name.eq_ignore_ascii_case(name))
    }
}

/// A single BMS map.
#[derive(Debug, Clone, Default)]
pub struct BmsMap {
    /// Map name
    pub name: String,
    /// Map size (rows, columns)
    pub size: (usize, usize),
    /// Starting line
    pub line: usize,
    /// Starting column
    pub column: usize,
    /// Map justification
    pub justify: MapJustify,
    /// Control options
    pub ctrl: ControlOptions,
    /// Fields in this map
    pub fields: Vec<BmsField>,
}

impl BmsMap {
    /// Create a new map.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            size: (24, 80),
            line: 1,
            column: 1,
            justify: MapJustify::First,
            ctrl: ControlOptions::default(),
            fields: Vec::new(),
        }
    }

    /// Set map size.
    pub fn with_size(mut self, rows: usize, columns: usize) -> Self {
        self.size = (rows, columns);
        self
    }

    /// Set starting position.
    pub fn at_position(mut self, line: usize, column: usize) -> Self {
        self.line = line;
        self.column = column;
        self
    }

    /// Add a field.
    pub fn add_field(&mut self, field: BmsField) {
        self.fields.push(field);
    }

    /// Get field by name.
    pub fn get_field(&self, name: &str) -> Option<&BmsField> {
        self.fields.iter().find(|f| f.name.eq_ignore_ascii_case(name))
    }

    /// Get all input fields.
    pub fn input_fields(&self) -> impl Iterator<Item = &BmsField> {
        self.fields.iter().filter(|f| f.is_input())
    }

    /// Get all output fields.
    pub fn output_fields(&self) -> impl Iterator<Item = &BmsField> {
        self.fields.iter().filter(|f| f.is_output())
    }
}

/// Storage type for mapset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StorageType {
    /// Auto storage
    #[default]
    Auto,
    /// Base storage
    Base,
}

/// Terminal type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TerminalType {
    /// 3270 Model 2 (24x80)
    #[default]
    Type3270_2,
    /// 3270 Model 3 (32x80)
    Type3270_3,
    /// 3270 Model 4 (43x80)
    Type3270_4,
    /// 3270 Model 5 (27x132)
    Type3270_5,
}

impl TerminalType {
    /// Get screen size.
    pub fn screen_size(&self) -> ScreenSize {
        match self {
            TerminalType::Type3270_2 => ScreenSize::Model2,
            TerminalType::Type3270_3 => ScreenSize::Model3,
            TerminalType::Type3270_4 => ScreenSize::Model4,
            TerminalType::Type3270_5 => ScreenSize::Model5,
        }
    }
}

/// Map language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MapLanguage {
    #[default]
    Cobol,
    Assembler,
    Pli,
    C,
}

/// Control options.
#[derive(Debug, Clone, Default)]
pub struct ControlOptions {
    /// Erase screen before display
    pub freekb: bool,
    /// Free keyboard
    pub frset: bool,
    /// Reset MDT
    pub alarm: bool,
    /// Sound alarm
    pub cursor: Option<(usize, usize)>,
    /// Print
    pub print: bool,
}

/// Map justification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MapJustify {
    #[default]
    First,
    Last,
}

/// BMS macro parser.
pub struct BmsParser {
    current_mapset: Option<BmsMapset>,
    current_map: Option<BmsMap>,
    current_line: usize,
}

impl BmsParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self {
            current_mapset: None,
            current_map: None,
            current_line: 0,
        }
    }

    /// Parse BMS source.
    pub fn parse(&mut self, source: &str) -> CicsResult<BmsMapset> {
        // Join continuation lines (lines ending with X in column 72 or thereabouts)
        let joined_lines = self.join_continuation_lines(source);

        for (line_num, line) in joined_lines.iter().enumerate() {
            self.current_line = line_num + 1;
            self.parse_line(line)?;
        }

        // Finalize any pending map
        self.finalize_map();

        self.current_mapset.take().ok_or_else(|| {
            CicsError::SyntaxError {
                line: 0,
                message: "No DFHMSD found".to_string(),
            }
        })
    }

    fn join_continuation_lines(&self, source: &str) -> Vec<String> {
        let mut result = Vec::new();
        let mut current = String::new();

        for line in source.lines() {
            let trimmed_end = line.trim_end();

            // BMS continuation: line ends with '-' or 'X'/'x' as the continuation
            // character (typically in column 72 of assembler format).
            // Also handle cases where the '-' is the last non-space character.
            let is_continuation = trimmed_end.ends_with('-')
                || trimmed_end.ends_with('X')
                || trimmed_end.ends_with('x');

            if is_continuation && !trimmed_end.starts_with('*') {
                // Remove continuation char and trailing spaces before it
                let without_cont = &trimmed_end[..trimmed_end.len()-1];
                current.push_str(without_cont.trim_end());
                current.push(' ');
            } else {
                current.push_str(line);
                result.push(current);
                current = String::new();
            }
        }

        // Don't forget the last line
        if !current.is_empty() {
            result.push(current);
        }

        result
    }

    fn parse_line(&mut self, line: &str) -> CicsResult<()> {
        let trimmed = line.trim();

        // Skip comments and empty lines
        if trimmed.is_empty() || trimmed.starts_with('*') {
            return Ok(());
        }

        // Look for macro name - pass full line for label extraction
        if trimmed.contains("DFHMSD") {
            return self.parse_dfhmsd(trimmed);
        }
        if trimmed.contains("DFHMDI") {
            return self.parse_dfhmdi(trimmed);
        }
        if trimmed.contains("DFHMDF") {
            return self.parse_dfhmdf(trimmed);
        }

        Ok(())
    }

    fn parse_dfhmsd(&mut self, text: &str) -> CicsResult<()> {
        let params = self.parse_parameters(text);

        // Check for TYPE=FINAL
        if params.get("TYPE").map(|s| s.as_str()) == Some("FINAL") {
            return Ok(());
        }

        let name = self.extract_label(text);
        let mut mapset = BmsMapset::new(&name);

        // Parse parameters
        if let Some(storage) = params.get("STORAGE") {
            mapset.storage = match storage.to_uppercase().as_str() {
                "AUTO" => StorageType::Auto,
                "BASE" => StorageType::Base,
                _ => StorageType::Auto,
            };
        }

        if let Some(lang) = params.get("LANG") {
            mapset.lang = match lang.to_uppercase().as_str() {
                "COBOL" => MapLanguage::Cobol,
                "ASM" | "ASSEMBLER" => MapLanguage::Assembler,
                "PLI" | "PL/I" => MapLanguage::Pli,
                "C" => MapLanguage::C,
                _ => MapLanguage::Cobol,
            };
        }

        self.current_mapset = Some(mapset);
        Ok(())
    }

    fn parse_dfhmdi(&mut self, text: &str) -> CicsResult<()> {
        // Finalize previous map if any
        self.finalize_map();

        let name = self.extract_label(text);
        let params = self.parse_parameters(text);
        let mut map = BmsMap::new(&name);

        // Parse SIZE
        if let Some(size) = params.get("SIZE") {
            if let Some((rows, cols)) = self.parse_pair(size) {
                map.size = (rows, cols);
            }
        }

        // Parse LINE
        if let Some(line) = params.get("LINE") {
            map.line = line.parse().unwrap_or(1);
        }

        // Parse COLUMN
        if let Some(col) = params.get("COLUMN") {
            map.column = col.parse().unwrap_or(1);
        }

        self.current_map = Some(map);
        Ok(())
    }

    fn parse_dfhmdf(&mut self, text: &str) -> CicsResult<()> {
        let name = self.extract_label(text);
        let params = self.parse_parameters(text);

        // Parse POS
        let (row, col) = if let Some(pos) = params.get("POS") {
            self.parse_pair(pos).unwrap_or((1, 1))
        } else {
            (1, 1)
        };

        // Parse LENGTH
        let length = params.get("LENGTH")
            .and_then(|s| s.parse().ok())
            .unwrap_or(1);

        let mut field = BmsField::new(&name, row, col, length);

        // Parse ATTRB
        if let Some(attrb) = params.get("ATTRB") {
            field.attributes = self.parse_attributes(attrb);
            field.field_type = self.determine_field_type(&field.attributes);
        }

        // Parse INITIAL
        if let Some(initial) = params.get("INITIAL") {
            let value = initial.trim_matches('\'').to_string();
            field.initial = Some(value);
        }

        // Parse PICIN/PICOUT
        if let Some(pic) = params.get("PICIN") {
            field.picture = Some(pic.trim_matches('\'').to_string());
        } else if let Some(pic) = params.get("PICOUT") {
            field.picture = Some(pic.trim_matches('\'').to_string());
        }

        // Parse COLOR
        if let Some(color) = params.get("COLOR") {
            field.attributes.color = Some(self.parse_color(color));
        }

        // Parse HILIGHT
        if let Some(hl) = params.get("HILIGHT") {
            field.attributes.highlight = Some(self.parse_highlight(hl));
        }

        // Parse JUSTIFY
        if let Some(just) = params.get("JUSTIFY") {
            field.attributes.justify = match just.to_uppercase().as_str() {
                "RIGHT" => FieldJustify::Right,
                "ZERO" => FieldJustify::RightZero,
                "BLANK" => FieldJustify::RightBlank,
                _ => FieldJustify::Left,
            };
        }

        // Parse GRPNAME
        if let Some(grp) = params.get("GRPNAME") {
            field.group = Some(grp.to_string());
        }

        // Parse OCCURS
        if let Some(occ) = params.get("OCCURS") {
            field.occurs = occ.parse().ok();
        }

        // Add field to current map
        if let Some(map) = &mut self.current_map {
            map.add_field(field);
        }

        Ok(())
    }

    fn finalize_map(&mut self) {
        if let Some(map) = self.current_map.take() {
            if let Some(mapset) = &mut self.current_mapset {
                mapset.add_map(map);
            }
        }
    }

    fn extract_label(&self, text: &str) -> String {
        // Label is before the macro name
        let parts: Vec<&str> = text.split_whitespace().collect();
        if !parts.is_empty() && !parts[0].starts_with("DFHM") {
            parts[0].to_string()
        } else {
            String::new()
        }
    }

    fn parse_parameters(&self, text: &str) -> HashMap<String, String> {
        let mut params = HashMap::new();

        // Find macro name (DFHMSD, DFHMDI, or DFHMDF)
        let macro_pos = text.find("DFHMSD")
            .or_else(|| text.find("DFHMDI"))
            .or_else(|| text.find("DFHMDF"))
            .unwrap_or(0);

        let after_macro = &text[macro_pos..];

        // Find parameters after macro name
        let param_start = after_macro.find([' ', '\t'])
            .map(|p| p + 1)
            .unwrap_or(after_macro.len());

        let param_text = &after_macro[param_start..];

        // Split on commas, handling parentheses and quotes
        let mut current_key = String::new();
        let mut current_value = String::new();
        let mut in_parens = 0;
        let mut in_quotes = false;
        let mut in_value = false;

        for c in param_text.chars() {
            match c {
                '\'' => {
                    in_quotes = !in_quotes;
                    if in_value {
                        current_value.push(c);
                    }
                }
                '=' if !in_quotes => {
                    in_value = true;
                }
                '(' if !in_quotes => {
                    in_parens += 1;
                    if in_value {
                        current_value.push(c);
                    }
                }
                ')' if !in_quotes => {
                    in_parens -= 1;
                    if in_value {
                        current_value.push(c);
                    }
                }
                ',' if in_parens == 0 && !in_quotes => {
                    if !current_key.is_empty() {
                        params.insert(
                            current_key.trim().to_uppercase(),
                            current_value.trim().to_string()
                        );
                    }
                    current_key.clear();
                    current_value.clear();
                    in_value = false;
                }
                ' ' | '\t' if in_parens == 0 && !in_quotes => {
                    // Skip whitespace outside parens and quotes
                }
                _ => {
                    if in_value {
                        current_value.push(c);
                    } else {
                        current_key.push(c);
                    }
                }
            }
        }

        // Add last parameter
        if !current_key.is_empty() {
            params.insert(
                current_key.trim().to_uppercase(),
                current_value.trim().to_string()
            );
        }

        params
    }

    fn parse_pair(&self, text: &str) -> Option<(usize, usize)> {
        let clean = text.trim_matches(|c| c == '(' || c == ')');
        let parts: Vec<&str> = clean.split(',').collect();
        if parts.len() >= 2 {
            Some((
                parts[0].trim().parse().ok()?,
                parts[1].trim().parse().ok()?
            ))
        } else {
            None
        }
    }

    fn parse_attributes(&self, text: &str) -> FieldAttribute {
        let clean = text.trim_matches(|c| c == '(' || c == ')').to_uppercase();
        let parts: Vec<&str> = clean.split(',').collect();
        let mut attr = FieldAttribute::default();

        for part in parts {
            match part.trim() {
                "PROT" => attr.protected = true,
                "UNPROT" => attr.protected = false,
                "NUM" => attr.numeric = true,
                "BRT" | "BRIGHT" => {
                    attr.bright = true;
                    attr.dark = false;
                }
                "DRK" | "DARK" => {
                    attr.dark = true;
                    attr.bright = false;
                }
                "NORM" => {
                    attr.bright = false;
                    attr.dark = false;
                }
                "ASKIP" | "AUTOSKIP" => {
                    attr.protected = true;
                    attr.numeric = true;
                }
                "IC" => attr.initial_cursor = true,
                "FSET" => attr.modified = true,
                _ => {}
            }
        }

        // Determine input/output
        if attr.protected {
            attr.output = true;
        } else {
            attr.input = true;
            attr.output = true;
        }

        attr
    }

    fn determine_field_type(&self, attr: &FieldAttribute) -> super::field::FieldType {
        use super::field::FieldType;

        if attr.protected && attr.numeric {
            FieldType::Skip
        } else if attr.protected {
            FieldType::Protected
        } else if attr.numeric {
            FieldType::Numeric
        } else {
            FieldType::Alphanumeric
        }
    }

    fn parse_color(&self, text: &str) -> FieldColor {
        match text.to_uppercase().as_str() {
            "BLUE" => FieldColor::Blue,
            "RED" => FieldColor::Red,
            "PINK" => FieldColor::Pink,
            "GREEN" => FieldColor::Green,
            "TURQUOISE" | "TURQ" => FieldColor::Turquoise,
            "YELLOW" => FieldColor::Yellow,
            "WHITE" => FieldColor::White,
            _ => FieldColor::Default,
        }
    }

    fn parse_highlight(&self, text: &str) -> FieldHighlight {
        match text.to_uppercase().as_str() {
            "BLINK" => FieldHighlight::Blink,
            "REVERSE" => FieldHighlight::Reverse,
            "UNDERLINE" | "USCORE" => FieldHighlight::Underscore,
            _ => FieldHighlight::Normal,
        }
    }
}

impl Default for BmsParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_mapset() {
        let source = r#"
CUSTMAP  DFHMSD TYPE=&SYSPARM,LANG=COBOL,STORAGE=AUTO
CUSTM1   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
TITLE    DFHMDF POS=(1,30),LENGTH=20,ATTRB=(ASKIP,BRT),                X
               INITIAL='CUSTOMER INQUIRY'
CUSTNO   DFHMDF POS=(5,15),LENGTH=8,ATTRB=(NUM,UNPROT,IC)
CUSTNM   DFHMDF POS=(5,25),LENGTH=30,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();

        assert_eq!(mapset.name, "CUSTMAP");
        assert_eq!(mapset.maps.len(), 1);

        let map = &mapset.maps[0];
        assert_eq!(map.name, "CUSTM1");
        assert_eq!(map.size, (24, 80));
        assert_eq!(map.fields.len(), 3);

        // Check TITLE field
        let title = map.get_field("TITLE").unwrap();
        assert_eq!(title.row, 1);
        assert_eq!(title.column, 30);
        assert_eq!(title.length, 20);
        assert!(title.attributes.protected);
        assert!(title.attributes.bright);
        assert_eq!(title.initial, Some("CUSTOMER INQUIRY".to_string()));

        // Check CUSTNO field
        let custno = map.get_field("CUSTNO").unwrap();
        assert!(custno.is_input());
        assert!(custno.attributes.numeric);
        assert!(custno.attributes.initial_cursor);
    }

    #[test]
    fn test_parse_parameters() {
        let parser = BmsParser::new();
        let params = parser.parse_parameters("DFHMDF POS=(5,15),LENGTH=8,ATTRB=(NUM,UNPROT)");

        assert_eq!(params.get("POS"), Some(&"(5,15)".to_string()));
        assert_eq!(params.get("LENGTH"), Some(&"8".to_string()));
        assert_eq!(params.get("ATTRB"), Some(&"(NUM,UNPROT)".to_string()));
    }

    #[test]
    fn test_parse_attributes() {
        let parser = BmsParser::new();

        let attr = parser.parse_attributes("(UNPROT,NUM,BRT,IC)");
        assert!(!attr.protected);
        assert!(attr.numeric);
        assert!(attr.bright);
        assert!(attr.initial_cursor);

        let attr2 = parser.parse_attributes("(ASKIP,NORM)");
        assert!(attr2.protected);
        assert!(attr2.numeric);
        assert!(!attr2.bright);
    }

    #[test]
    fn test_input_output_fields() {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
INP1     DFHMDF POS=(1,1),LENGTH=10,ATTRB=(UNPROT)
OUT1     DFHMDF POS=(2,1),LENGTH=10,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let input_count = map.input_fields().count();
        let output_count = map.output_fields().count();

        assert_eq!(input_count, 1);
        assert_eq!(output_count, 2); // OUT1 and INP1 (input fields are also output)
    }

    #[test]
    fn test_color_parsing() {
        let parser = BmsParser::new();

        assert_eq!(parser.parse_color("GREEN"), FieldColor::Green);
        assert_eq!(parser.parse_color("BLUE"), FieldColor::Blue);
        assert_eq!(parser.parse_color("YELLOW"), FieldColor::Yellow);
    }

    #[test]
    fn test_parse_carddemo_login_bms() {
        // Real CardDemo login screen BMS (COSGN00) using '-' continuation
        let source = r#"
COSGN00 DFHMSD CTRL=(ALARM,FREEKB),                                    -
               LANG=COBOL,                                             -
               STORAGE=AUTO,                                           -
               TYPE=&&SYSPARM
COSGN0A DFHMDI COLUMN=1,                                               -
               LINE=1,                                                 -
               SIZE=(24,80)
        DFHMDF ATTRB=(ASKIP,NORM),                                     -
               COLOR=BLUE,                                             -
               LENGTH=6,                                               -
               POS=(1,1),                                              -
               INITIAL='Tran :'
TRNNAME DFHMDF ATTRB=(ASKIP,FSET,NORM),                                -
               COLOR=BLUE,                                             -
               LENGTH=4,                                               -
               POS=(1,8)
USERID  DFHMDF ATTRB=(FSET,IC,NORM,UNPROT),                            -
               COLOR=GREEN,                                            -
               LENGTH=8,                                               -
               POS=(19,43)
PASSWD  DFHMDF ATTRB=(DRK,FSET,UNPROT),                                -
               COLOR=GREEN,                                            -
               LENGTH=8,                                               -
               POS=(20,43)
ERRMSG  DFHMDF ATTRB=(ASKIP,BRT,FSET),                                 -
               COLOR=RED,                                              -
               LENGTH=78,                                              -
               POS=(23,1)
        DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();

        assert_eq!(mapset.name, "COSGN00");
        assert_eq!(mapset.lang, MapLanguage::Cobol);
        assert_eq!(mapset.maps.len(), 1);

        let map = &mapset.maps[0];
        assert_eq!(map.name, "COSGN0A");
        assert_eq!(map.size, (24, 80));

        // Should have 5 named fields + 1 unnamed
        let named: Vec<_> = map.fields.iter().filter(|f| !f.name.is_empty()).collect();
        assert_eq!(named.len(), 4); // TRNNAME, USERID, PASSWD, ERRMSG

        // Check USERID field
        let userid = map.get_field("USERID").unwrap();
        assert_eq!(userid.row, 19);
        assert_eq!(userid.column, 43);
        assert_eq!(userid.length, 8);
        assert!(userid.is_input());
        assert!(userid.attributes.initial_cursor);

        // Check PASSWD field (dark)
        let passwd = map.get_field("PASSWD").unwrap();
        assert!(passwd.attributes.dark);
        assert!(!passwd.attributes.protected);

        // Check ERRMSG field (bright, protected)
        let errmsg = map.get_field("ERRMSG").unwrap();
        assert!(errmsg.attributes.bright);
        assert!(errmsg.attributes.protected);
    }
}
