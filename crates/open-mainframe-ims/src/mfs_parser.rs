//! MFS-100: MFS Source Language Parser.
//!
//! Parses MFS (Message Format Service) source statements into an
//! abstract syntax tree. MFS definitions consist of MSG/FMT macros
//! with nested sub-structures (LPAGE, SEG, MFLD, DEV, DIV, DPAGE, DFLD).

// ---------------------------------------------------------------------------
// Extended attributes
// ---------------------------------------------------------------------------

/// Extended display attributes for MFS fields.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExtendedAttributes {
    /// Display color.
    pub color: Option<MfsColor>,
    /// Highlighting mode.
    pub highlighting: Option<MfsHighlight>,
}

/// MFS color attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MfsColor {
    /// Default terminal color.
    Default,
    /// Blue.
    Blue,
    /// Red.
    Red,
    /// Green.
    Green,
    /// Yellow.
    Yellow,
    /// White.
    White,
    /// Turquoise.
    Turquoise,
    /// Pink.
    Pink,
}

/// MFS highlighting attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MfsHighlight {
    /// Normal display.
    Normal,
    /// Blink.
    Blink,
    /// Reverse video.
    Reverse,
    /// Underline.
    Underline,
}

// ---------------------------------------------------------------------------
// AST node types
// ---------------------------------------------------------------------------

/// An MFLD (Message Field) definition within a SEG.
#[derive(Debug, Clone)]
pub struct MfsMfld {
    /// Field name.
    pub name: String,
    /// Length of the field.
    pub length: usize,
    /// Offset within the segment.
    pub offset: usize,
    /// Optional literal value.
    pub literal: Option<String>,
    /// Extended attributes.
    pub attributes: ExtendedAttributes,
}

/// A SEG (Segment) definition within an LPAGE.
#[derive(Debug, Clone)]
pub struct MfsSeg {
    /// Segment name.
    pub name: String,
    /// Message fields.
    pub mflds: Vec<MfsMfld>,
}

/// An LPAGE (Logical Page) definition within a MSG.
#[derive(Debug, Clone)]
pub struct MfsLpage {
    /// Page name.
    pub name: String,
    /// Segments in this page.
    pub segs: Vec<MfsSeg>,
}

/// A MSG definition -- the top-level message format.
#[derive(Debug, Clone)]
pub struct MfsMsgDef {
    /// Message name.
    pub name: String,
    /// Message type: INPUT or OUTPUT.
    pub msg_type: MsgType,
    /// Logical pages.
    pub lpages: Vec<MfsLpage>,
}

/// Message type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MsgType {
    /// Input message.
    Input,
    /// Output message.
    Output,
}

// ---------------------------------------------------------------------------
// FMT definitions
// ---------------------------------------------------------------------------

/// A DFLD (Device Field) definition within a DPAGE.
#[derive(Debug, Clone)]
pub struct MfsDfld {
    /// Field name.
    pub name: String,
    /// Row position.
    pub row: u16,
    /// Column position.
    pub col: u16,
    /// Field length.
    pub length: usize,
    /// Extended attributes.
    pub attributes: ExtendedAttributes,
}

/// A DPAGE (Device Page) definition within a DIV.
#[derive(Debug, Clone)]
pub struct MfsDpage {
    /// Page name.
    pub name: String,
    /// Device fields.
    pub dflds: Vec<MfsDfld>,
}

/// A DIV (Device Division) -- INPUT or OUTPUT portion of a DEV.
#[derive(Debug, Clone)]
pub struct MfsDiv {
    /// Division type: input or output.
    pub div_type: MsgType,
    /// Device pages.
    pub dpages: Vec<MfsDpage>,
}

/// A DEV (Device) definition within a FMT.
#[derive(Debug, Clone)]
pub struct MfsDev {
    /// Device type (e.g., "3270-2", "3270-A2").
    pub device_type: String,
    /// Divisions.
    pub divs: Vec<MfsDiv>,
}

/// A FMT definition -- the top-level device format.
#[derive(Debug, Clone)]
pub struct MfsFmtDef {
    /// Format name.
    pub name: String,
    /// Device definitions.
    pub devs: Vec<MfsDev>,
}

// ---------------------------------------------------------------------------
// MFS Statement enum
// ---------------------------------------------------------------------------

/// An MFS source statement.
#[derive(Debug, Clone)]
pub enum MfsStatement {
    /// MSG statement.
    Msg(MfsMsgDef),
    /// LPAGE statement.
    Lpage(MfsLpage),
    /// SEG statement.
    Seg(MfsSeg),
    /// MFLD statement.
    Mfld(MfsMfld),
    /// FMT statement.
    Fmt(MfsFmtDef),
    /// DEV statement.
    Dev(MfsDev),
    /// DIV statement.
    Div(MfsDiv),
    /// DPAGE statement.
    Dpage(MfsDpage),
    /// DFLD statement.
    Dfld(MfsDfld),
    /// COPY statement (include another source member).
    Copy(String),
    /// TABLE statement.
    Table(String),
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

use crate::ImsResult;

/// Parses MFS source text into a collection of MSG and FMT definitions.
#[derive(Debug, Default)]
pub struct MfsParser;

impl MfsParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self
    }

    /// Parse MFS source text, returning MSG and FMT definitions.
    pub fn parse(&self, source: &str) -> ImsResult<MfsParseResult> {
        let mut msgs: Vec<MfsMsgDef> = Vec::new();
        let mut fmts: Vec<MfsFmtDef> = Vec::new();

        let lines: Vec<&str> = source.lines().collect();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i].trim();
            if line.is_empty() || line.starts_with('*') {
                i += 1;
                continue;
            }

            let tokens: Vec<&str> = line.split_whitespace().collect();
            if tokens.len() < 2 {
                i += 1;
                continue;
            }

            match tokens[1].to_uppercase().as_str() {
                "MSG" => {
                    let (msg, next_i) = self.parse_msg(&tokens, &lines, i)?;
                    msgs.push(msg);
                    i = next_i;
                }
                "FMT" => {
                    let (fmt, next_i) = self.parse_fmt(&tokens, &lines, i)?;
                    fmts.push(fmt);
                    i = next_i;
                }
                _ => {
                    i += 1;
                }
            }
        }

        Ok(MfsParseResult { msgs, fmts })
    }

    fn parse_msg(
        &self,
        tokens: &[&str],
        lines: &[&str],
        start: usize,
    ) -> ImsResult<(MfsMsgDef, usize)> {
        let name = tokens[0].to_string();
        let msg_type = if tokens.len() > 2
            && tokens[2..].iter().any(|t| t.to_uppercase().contains("INPUT"))
        {
            MsgType::Input
        } else {
            MsgType::Output
        };

        let mut lpages: Vec<MfsLpage> = Vec::new();
        let mut current_lpage: Option<MfsLpage> = None;
        let mut current_seg: Option<MfsSeg> = None;
        let mut i = start + 1;

        while i < lines.len() {
            let line = lines[i].trim();
            if line.is_empty() || line.starts_with('*') {
                i += 1;
                continue;
            }
            let toks: Vec<&str> = line.split_whitespace().collect();
            if toks.len() < 2 {
                i += 1;
                continue;
            }
            match toks[1].to_uppercase().as_str() {
                "LPAGE" => {
                    if let Some(seg) = current_seg.take() {
                        if let Some(ref mut lp) = current_lpage {
                            lp.segs.push(seg);
                        }
                    }
                    if let Some(lp) = current_lpage.take() {
                        lpages.push(lp);
                    }
                    current_lpage = Some(MfsLpage {
                        name: toks[0].to_string(),
                        segs: Vec::new(),
                    });
                    i += 1;
                }
                "SEG" => {
                    // Create implicit LPAGE if none exists
                    if current_lpage.is_none() {
                        current_lpage = Some(MfsLpage {
                            name: String::new(),
                            segs: Vec::new(),
                        });
                    }
                    if let Some(seg) = current_seg.take() {
                        if let Some(ref mut lp) = current_lpage {
                            lp.segs.push(seg);
                        }
                    }
                    current_seg = Some(MfsSeg {
                        name: toks[0].to_string(),
                        mflds: Vec::new(),
                    });
                    i += 1;
                }
                "MFLD" => {
                    let mfld = self.parse_mfld(&toks);
                    if let Some(ref mut seg) = current_seg {
                        seg.mflds.push(mfld);
                    }
                    i += 1;
                }
                "MSGEND" => {
                    i += 1;
                    break;
                }
                "MSG" | "FMT" => {
                    // Start of next definition
                    break;
                }
                _ => {
                    i += 1;
                }
            }
        }

        if let Some(seg) = current_seg.take() {
            if let Some(ref mut lp) = current_lpage {
                lp.segs.push(seg);
            }
        }
        if let Some(lp) = current_lpage.take() {
            lpages.push(lp);
        }

        // If no explicit LPAGE, wrap everything in an implicit one
        if lpages.is_empty() {
            lpages.push(MfsLpage {
                name: String::new(),
                segs: Vec::new(),
            });
        }

        Ok((
            MfsMsgDef {
                name,
                msg_type,
                lpages,
            },
            i,
        ))
    }

    fn parse_mfld(&self, toks: &[&str]) -> MfsMfld {
        let name = toks[0].to_string();
        let mut length = 0;
        let mut offset = 0;
        let mut literal = None;
        let mut attributes = ExtendedAttributes::default();

        for &tok in &toks[2..] {
            let upper = tok.to_uppercase();
            if let Some(rest) = upper.strip_prefix("LTH=") {
                length = rest.parse().unwrap_or(0);
            } else if let Some(rest) = upper.strip_prefix("OFS=") {
                offset = rest.parse().unwrap_or(0);
            } else if let Some(rest) = tok.strip_prefix("LIT=") {
                literal = Some(rest.trim_matches('\'').to_string());
            } else if let Some(rest) = upper.strip_prefix("COLOR=") {
                attributes.color = parse_color(rest);
            } else if let Some(rest) = upper.strip_prefix("HILITE=") {
                attributes.highlighting = parse_highlight(rest);
            }
        }

        MfsMfld {
            name,
            length,
            offset,
            literal,
            attributes,
        }
    }

    fn parse_fmt(
        &self,
        tokens: &[&str],
        lines: &[&str],
        start: usize,
    ) -> ImsResult<(MfsFmtDef, usize)> {
        let name = tokens[0].to_string();
        let mut devs: Vec<MfsDev> = Vec::new();
        let mut current_dev: Option<MfsDev> = None;
        let mut current_div: Option<MfsDiv> = None;
        let mut current_dpage: Option<MfsDpage> = None;
        let mut i = start + 1;

        while i < lines.len() {
            let line = lines[i].trim();
            if line.is_empty() || line.starts_with('*') {
                i += 1;
                continue;
            }
            let toks: Vec<&str> = line.split_whitespace().collect();
            if toks.len() < 2 {
                i += 1;
                continue;
            }
            match toks[1].to_uppercase().as_str() {
                "DEV" => {
                    self.flush_fmt_state(
                        &mut devs,
                        &mut current_dev,
                        &mut current_div,
                        &mut current_dpage,
                    );
                    let dtype = if toks.len() > 2 {
                        toks[2].to_string()
                    } else {
                        "3270-2".to_string()
                    };
                    current_dev = Some(MfsDev {
                        device_type: dtype,
                        divs: Vec::new(),
                    });
                    i += 1;
                }
                "DIV" => {
                    if let Some(dp) = current_dpage.take() {
                        if let Some(ref mut div) = current_div {
                            div.dpages.push(dp);
                        }
                    }
                    if let Some(div) = current_div.take() {
                        if let Some(ref mut dev) = current_dev {
                            dev.divs.push(div);
                        }
                    }
                    let div_type =
                        if toks.iter().any(|t| t.to_uppercase().contains("INPUT")) {
                            MsgType::Input
                        } else {
                            MsgType::Output
                        };
                    current_div = Some(MfsDiv {
                        div_type,
                        dpages: Vec::new(),
                    });
                    i += 1;
                }
                "DPAGE" => {
                    if let Some(dp) = current_dpage.take() {
                        if let Some(ref mut div) = current_div {
                            div.dpages.push(dp);
                        }
                    }
                    current_dpage = Some(MfsDpage {
                        name: toks[0].to_string(),
                        dflds: Vec::new(),
                    });
                    i += 1;
                }
                "DFLD" => {
                    let dfld = self.parse_dfld(&toks);
                    if let Some(ref mut dp) = current_dpage {
                        dp.dflds.push(dfld);
                    }
                    i += 1;
                }
                "FMTEND" => {
                    i += 1;
                    break;
                }
                "FMT" | "MSG" => {
                    break;
                }
                _ => {
                    i += 1;
                }
            }
        }

        self.flush_fmt_state(&mut devs, &mut current_dev, &mut current_div, &mut current_dpage);

        Ok((MfsFmtDef { name, devs }, i))
    }

    fn flush_fmt_state(
        &self,
        devs: &mut Vec<MfsDev>,
        current_dev: &mut Option<MfsDev>,
        current_div: &mut Option<MfsDiv>,
        current_dpage: &mut Option<MfsDpage>,
    ) {
        if let Some(dp) = current_dpage.take() {
            if let Some(ref mut div) = current_div {
                div.dpages.push(dp);
            }
        }
        if let Some(div) = current_div.take() {
            if let Some(ref mut dev) = current_dev {
                dev.divs.push(div);
            }
        }
        if let Some(dev) = current_dev.take() {
            devs.push(dev);
        }
    }

    fn parse_dfld(&self, toks: &[&str]) -> MfsDfld {
        let name = toks[0].to_string();
        let mut row = 1;
        let mut col = 1;
        let mut length = 0;
        let mut attributes = ExtendedAttributes::default();

        for &tok in &toks[2..] {
            let upper = tok.to_uppercase();
            if let Some(rest) = upper.strip_prefix("POS=(") {
                // POS=(row,col)
                let inner = rest.trim_end_matches(')');
                let parts: Vec<&str> = inner.split(',').collect();
                if parts.len() == 2 {
                    row = parts[0].parse().unwrap_or(1);
                    col = parts[1].parse().unwrap_or(1);
                }
            } else if let Some(rest) = upper.strip_prefix("LTH=") {
                length = rest.parse().unwrap_or(0);
            } else if let Some(rest) = upper.strip_prefix("COLOR=") {
                attributes.color = parse_color(rest);
            } else if let Some(rest) = upper.strip_prefix("HILITE=") {
                attributes.highlighting = parse_highlight(rest);
            }
        }

        MfsDfld {
            name,
            row,
            col,
            length,
            attributes,
        }
    }
}

/// Result of parsing MFS source.
#[derive(Debug, Clone)]
pub struct MfsParseResult {
    /// Parsed MSG definitions.
    pub msgs: Vec<MfsMsgDef>,
    /// Parsed FMT definitions.
    pub fmts: Vec<MfsFmtDef>,
}

/// Parse a color name.
fn parse_color(s: &str) -> Option<MfsColor> {
    match s {
        "BLUE" => Some(MfsColor::Blue),
        "RED" => Some(MfsColor::Red),
        "GREEN" => Some(MfsColor::Green),
        "YELLOW" => Some(MfsColor::Yellow),
        "WHITE" => Some(MfsColor::White),
        "TURQUOISE" => Some(MfsColor::Turquoise),
        "PINK" => Some(MfsColor::Pink),
        "DEFAULT" => Some(MfsColor::Default),
        _ => None,
    }
}

/// Parse a highlighting mode name.
fn parse_highlight(s: &str) -> Option<MfsHighlight> {
    match s {
        "NORMAL" => Some(MfsHighlight::Normal),
        "BLINK" => Some(MfsHighlight::Blink),
        "REVERSE" => Some(MfsHighlight::Reverse),
        "UNDERLINE" => Some(MfsHighlight::Underline),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_msg_source() -> &'static str {
        concat!(
            "OUTMSG   MSG   TYPE=OUTPUT\n",
            "PAGE1    LPAGE\n",
            "SEG1     SEG\n",
            "FLD1     MFLD  LTH=20 OFS=0\n",
            "FLD2     MFLD  LTH=10 OFS=20 COLOR=GREEN\n",
            "         MSGEND\n",
        )
    }

    fn sample_fmt_source() -> &'static str {
        concat!(
            "FMT01    FMT\n",
            "DEV01    DEV   3270-2\n",
            "DIV01    DIV   TYPE=OUTPUT\n",
            "DP01     DPAGE\n",
            "DF01     DFLD  POS=(1,1) LTH=20\n",
            "DF02     DFLD  POS=(2,1) LTH=10 COLOR=RED HILITE=REVERSE\n",
            "         FMTEND\n",
        )
    }

    #[test]
    fn test_parse_msg() {
        let parser = MfsParser::new();
        let result = parser.parse(sample_msg_source()).unwrap();
        assert_eq!(result.msgs.len(), 1);
        let msg = &result.msgs[0];
        assert_eq!(msg.name, "OUTMSG");
        assert_eq!(msg.msg_type, MsgType::Output);
        assert_eq!(msg.lpages.len(), 1);
        assert_eq!(msg.lpages[0].segs.len(), 1);
        assert_eq!(msg.lpages[0].segs[0].mflds.len(), 2);
    }

    #[test]
    fn test_parse_mfld_attributes() {
        let parser = MfsParser::new();
        let result = parser.parse(sample_msg_source()).unwrap();
        let fld1 = &result.msgs[0].lpages[0].segs[0].mflds[0];
        assert_eq!(fld1.name, "FLD1");
        assert_eq!(fld1.length, 20);
        assert_eq!(fld1.offset, 0);

        let fld2 = &result.msgs[0].lpages[0].segs[0].mflds[1];
        assert_eq!(fld2.length, 10);
        assert_eq!(fld2.offset, 20);
        assert_eq!(fld2.attributes.color, Some(MfsColor::Green));
    }

    #[test]
    fn test_parse_fmt() {
        let parser = MfsParser::new();
        let result = parser.parse(sample_fmt_source()).unwrap();
        assert_eq!(result.fmts.len(), 1);
        let fmt = &result.fmts[0];
        assert_eq!(fmt.name, "FMT01");
        assert_eq!(fmt.devs.len(), 1);
        assert_eq!(fmt.devs[0].device_type, "3270-2");
        assert_eq!(fmt.devs[0].divs.len(), 1);
        assert_eq!(fmt.devs[0].divs[0].dpages.len(), 1);
        assert_eq!(fmt.devs[0].divs[0].dpages[0].dflds.len(), 2);
    }

    #[test]
    fn test_parse_dfld_attributes() {
        let parser = MfsParser::new();
        let result = parser.parse(sample_fmt_source()).unwrap();
        let dfld2 = &result.fmts[0].devs[0].divs[0].dpages[0].dflds[1];
        assert_eq!(dfld2.name, "DF02");
        assert_eq!(dfld2.row, 2);
        assert_eq!(dfld2.col, 1);
        assert_eq!(dfld2.length, 10);
        assert_eq!(dfld2.attributes.color, Some(MfsColor::Red));
        assert_eq!(
            dfld2.attributes.highlighting,
            Some(MfsHighlight::Reverse)
        );
    }

    #[test]
    fn test_parse_empty_source() {
        let parser = MfsParser::new();
        let result = parser.parse("").unwrap();
        assert!(result.msgs.is_empty());
        assert!(result.fmts.is_empty());
    }

    #[test]
    fn test_parse_comments() {
        let source = "* This is a comment\n* Another comment\n";
        let parser = MfsParser::new();
        let result = parser.parse(source).unwrap();
        assert!(result.msgs.is_empty());
    }

    #[test]
    fn test_parse_input_msg() {
        let source = concat!(
            "INMSG    MSG   TYPE=INPUT\n",
            "SEG1     SEG\n",
            "FLD1     MFLD  LTH=8 OFS=0\n",
            "         MSGEND\n",
        );
        let parser = MfsParser::new();
        let result = parser.parse(source).unwrap();
        assert_eq!(result.msgs[0].msg_type, MsgType::Input);
    }

    #[test]
    fn test_parse_literal_mfld() {
        let source = concat!(
            "M1       MSG   TYPE=OUTPUT\n",
            "S1       SEG\n",
            "LIT1     MFLD  LTH=5 LIT='HELLO'\n",
            "         MSGEND\n",
        );
        let parser = MfsParser::new();
        let result = parser.parse(source).unwrap();
        let fld = &result.msgs[0].lpages[0].segs[0].mflds[0];
        assert_eq!(fld.literal.as_deref(), Some("HELLO"));
    }

    #[test]
    fn test_mfs_statement_variants() {
        // Verify that MfsStatement can hold all variant types
        let stmt = MfsStatement::Copy("MEMBER1".to_string());
        if let MfsStatement::Copy(name) = stmt {
            assert_eq!(name, "MEMBER1");
        }

        let stmt = MfsStatement::Table("TABLE1".to_string());
        if let MfsStatement::Table(name) = stmt {
            assert_eq!(name, "TABLE1");
        }
    }

    #[test]
    fn test_color_parsing() {
        assert_eq!(parse_color("BLUE"), Some(MfsColor::Blue));
        assert_eq!(parse_color("RED"), Some(MfsColor::Red));
        assert_eq!(parse_color("GREEN"), Some(MfsColor::Green));
        assert_eq!(parse_color("YELLOW"), Some(MfsColor::Yellow));
        assert_eq!(parse_color("WHITE"), Some(MfsColor::White));
        assert_eq!(parse_color("TURQUOISE"), Some(MfsColor::Turquoise));
        assert_eq!(parse_color("PINK"), Some(MfsColor::Pink));
        assert_eq!(parse_color("DEFAULT"), Some(MfsColor::Default));
        assert_eq!(parse_color("INVALID"), None);
    }

    #[test]
    fn test_highlight_parsing() {
        assert_eq!(parse_highlight("NORMAL"), Some(MfsHighlight::Normal));
        assert_eq!(parse_highlight("BLINK"), Some(MfsHighlight::Blink));
        assert_eq!(parse_highlight("REVERSE"), Some(MfsHighlight::Reverse));
        assert_eq!(parse_highlight("UNDERLINE"), Some(MfsHighlight::Underline));
        assert_eq!(parse_highlight("INVALID"), None);
    }

    #[test]
    fn test_msg_type_values() {
        assert_ne!(MsgType::Input, MsgType::Output);
    }
}
