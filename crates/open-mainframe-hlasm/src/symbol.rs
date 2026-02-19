//! HLASM symbol table and expression evaluator.
//!
//! - Symbol table with address, length, type, and EQU value tracking
//! - Two-pass resolution: define symbols in pass 1, resolve in pass 2
//! - Expression evaluation with +, -, *, / and parentheses
//! - Attribute references: L' (length), T' (type), S' (scale), I' (integer), D' (defined)

use std::collections::HashMap;
use crate::lexer::Token;

// ---------------------------------------------------------------------------
//  Symbol table
// ---------------------------------------------------------------------------

/// A symbol table entry.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Symbol name.
    pub name: String,
    /// Address / location counter value at definition.
    pub address: i64,
    /// Length in bytes.
    pub length: u32,
    /// Type attribute (e.g., 'F' for fullword, 'H' for halfword, 'C' for character).
    pub sym_type: char,
    /// Scale attribute (for packed/float).
    pub scale: i32,
    /// Integer attribute.
    pub integer: i32,
    /// Whether this symbol was defined via EQU.
    pub is_equ: bool,
    /// EQU value (if is_equ).
    pub equ_value: i64,
    /// Section (CSECT) this symbol belongs to.
    pub section: String,
    /// Whether the symbol has been defined.
    pub defined: bool,
}

impl Default for Symbol {
    fn default() -> Self {
        Self {
            name: String::new(),
            address: 0,
            length: 1,
            sym_type: 'U', // Unknown/undefined.
            scale: 0,
            integer: 0,
            is_equ: false,
            equ_value: 0,
            section: String::new(),
            defined: false,
        }
    }
}

/// The HLASM symbol table.
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Symbols by name.
    symbols: HashMap<String, Symbol>,
    /// Current location counter.
    pub location_counter: i64,
    /// Current section name.
    pub current_section: String,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a label at the current location counter.
    pub fn define_label(&mut self, name: &str, length: u32, sym_type: char) {
        let upper = name.to_uppercase();
        let sym = Symbol {
            name: upper.clone(),
            address: self.location_counter,
            length,
            sym_type,
            section: self.current_section.clone(),
            defined: true,
            ..Default::default()
        };
        self.symbols.insert(upper, sym);
    }

    /// Define an EQU symbol.
    pub fn define_equ(&mut self, name: &str, value: i64) {
        let upper = name.to_uppercase();
        let sym = Symbol {
            name: upper.clone(),
            address: value,
            length: 1,
            sym_type: 'U',
            is_equ: true,
            equ_value: value,
            section: self.current_section.clone(),
            defined: true,
            ..Default::default()
        };
        self.symbols.insert(upper, sym);
    }

    /// Define an EQU symbol with length and type overrides.
    pub fn define_equ_full(&mut self, name: &str, value: i64, length: u32, sym_type: char) {
        let upper = name.to_uppercase();
        let sym = Symbol {
            name: upper.clone(),
            address: value,
            length,
            sym_type,
            is_equ: true,
            equ_value: value,
            section: self.current_section.clone(),
            defined: true,
            ..Default::default()
        };
        self.symbols.insert(upper, sym);
    }

    /// Look up a symbol by name.
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(&name.to_uppercase())
    }

    /// Check if a symbol is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.symbols.get(&name.to_uppercase()).map_or(false, |s| s.defined)
    }

    /// Get the length attribute of a symbol.
    pub fn length_attr(&self, name: &str) -> Option<u32> {
        self.symbols.get(&name.to_uppercase()).map(|s| s.length)
    }

    /// Get the type attribute of a symbol.
    pub fn type_attr(&self, name: &str) -> Option<char> {
        self.symbols.get(&name.to_uppercase()).map(|s| s.sym_type)
    }

    /// Get the scale attribute of a symbol.
    pub fn scale_attr(&self, name: &str) -> Option<i32> {
        self.symbols.get(&name.to_uppercase()).map(|s| s.scale)
    }

    /// Get the integer attribute of a symbol.
    pub fn integer_attr(&self, name: &str) -> Option<i32> {
        self.symbols.get(&name.to_uppercase()).map(|s| s.integer)
    }

    /// Get the defined attribute (D').
    pub fn defined_attr(&self, name: &str) -> bool {
        self.is_defined(name)
    }

    /// Advance the location counter.
    pub fn advance(&mut self, bytes: i64) {
        self.location_counter += bytes;
    }

    /// Set the location counter (e.g., ORG).
    pub fn set_location_counter(&mut self, value: i64) {
        self.location_counter = value;
    }

    /// Set the current section.
    pub fn set_section(&mut self, name: &str) {
        self.current_section = name.to_uppercase();
    }

    /// Get all symbol names.
    pub fn symbol_names(&self) -> Vec<String> {
        self.symbols.keys().cloned().collect()
    }

    /// Number of symbols defined.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Whether the table is empty.
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

// ---------------------------------------------------------------------------
//  Expression evaluator
// ---------------------------------------------------------------------------

/// Expression evaluation error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ExprError {
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Invalid expression: {0}")]
    Invalid(String),
}

/// Evaluate an expression from a token stream.
/// Supports: +, -, *, /, parentheses, symbols, numbers, location counter, attribute refs.
pub fn eval_expression(tokens: &[Token], symbols: &SymbolTable) -> Result<i64, ExprError> {
    let mut pos = 0;
    let result = eval_additive(tokens, &mut pos, symbols)?;
    Ok(result)
}

fn eval_additive(tokens: &[Token], pos: &mut usize, symbols: &SymbolTable) -> Result<i64, ExprError> {
    let mut left = eval_multiplicative(tokens, pos, symbols)?;

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator('+') => {
                *pos += 1;
                let right = eval_multiplicative(tokens, pos, symbols)?;
                left += right;
            }
            Token::Operator('-') => {
                *pos += 1;
                let right = eval_multiplicative(tokens, pos, symbols)?;
                left -= right;
            }
            _ => break,
        }
    }

    Ok(left)
}

fn eval_multiplicative(tokens: &[Token], pos: &mut usize, symbols: &SymbolTable) -> Result<i64, ExprError> {
    let mut left = eval_unary(tokens, pos, symbols)?;

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator('*') => {
                *pos += 1;
                let right = eval_unary(tokens, pos, symbols)?;
                left *= right;
            }
            Token::Operator('/') => {
                *pos += 1;
                let right = eval_unary(tokens, pos, symbols)?;
                if right == 0 {
                    return Err(ExprError::DivisionByZero);
                }
                left /= right;
            }
            _ => break,
        }
    }

    Ok(left)
}

fn eval_unary(tokens: &[Token], pos: &mut usize, symbols: &SymbolTable) -> Result<i64, ExprError> {
    if *pos >= tokens.len() {
        return Err(ExprError::Invalid("Unexpected end of expression".to_string()));
    }

    // Unary minus.
    if let Token::Operator('-') = &tokens[*pos] {
        *pos += 1;
        let val = eval_primary(tokens, pos, symbols)?;
        return Ok(-val);
    }

    // Unary plus.
    if let Token::Operator('+') = &tokens[*pos] {
        *pos += 1;
        return eval_primary(tokens, pos, symbols);
    }

    eval_primary(tokens, pos, symbols)
}

fn eval_primary(tokens: &[Token], pos: &mut usize, symbols: &SymbolTable) -> Result<i64, ExprError> {
    if *pos >= tokens.len() {
        return Err(ExprError::Invalid("Unexpected end of expression".to_string()));
    }

    match &tokens[*pos] {
        Token::Number(n) => {
            let val = *n;
            *pos += 1;
            Ok(val)
        }
        Token::Symbol(name) => {
            *pos += 1;
            if let Some(sym) = symbols.lookup(name) {
                if sym.is_equ {
                    Ok(sym.equ_value)
                } else {
                    Ok(sym.address)
                }
            } else {
                Err(ExprError::UndefinedSymbol(name.clone()))
            }
        }
        Token::LocationCounter => {
            *pos += 1;
            Ok(symbols.location_counter)
        }
        Token::LengthAttr(name) => {
            *pos += 1;
            symbols.length_attr(name)
                .map(|l| l as i64)
                .ok_or_else(|| ExprError::UndefinedSymbol(name.clone()))
        }
        Token::HexTerm(h) => {
            *pos += 1;
            i64::from_str_radix(h, 16)
                .map_err(|_| ExprError::Invalid(format!("Invalid hex: {h}")))
        }
        Token::CharTerm(c) => {
            *pos += 1;
            // EBCDIC-style: character value. For simplicity, use ASCII byte values.
            let val = c.bytes().fold(0i64, |acc, b| (acc << 8) | b as i64);
            Ok(val)
        }
        Token::BinTerm(b) => {
            *pos += 1;
            i64::from_str_radix(b, 2)
                .map_err(|_| ExprError::Invalid(format!("Invalid binary: {b}")))
        }
        Token::LParen => {
            *pos += 1; // skip (
            let val = eval_additive(tokens, pos, symbols)?;
            if *pos < tokens.len() && tokens[*pos] == Token::RParen {
                *pos += 1; // skip )
            }
            Ok(val)
        }
        Token::Register(n) => {
            // Register number as value in expressions (rare but valid).
            let val = *n as i64;
            *pos += 1;
            Ok(val)
        }
        other => Err(ExprError::Invalid(format!("Unexpected token: {other}"))),
    }
}

// ---------------------------------------------------------------------------
//  Convenience: evaluate a string expression
// ---------------------------------------------------------------------------

/// Parse and evaluate a string expression against a symbol table.
pub fn eval_expr_str(expr: &str, symbols: &SymbolTable) -> Result<i64, ExprError> {
    let tokens = crate::lexer::tokenize_operands(expr);
    eval_expression(&tokens, symbols)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_define_label() {
        let mut st = SymbolTable::new();
        st.location_counter = 0x100;
        st.define_label("MYDATA", 4, 'F');
        let sym = st.lookup("MYDATA").unwrap();
        assert_eq!(sym.address, 0x100);
        assert_eq!(sym.length, 4);
        assert_eq!(sym.sym_type, 'F');
        assert!(sym.defined);
    }

    #[test]
    fn test_symbol_table_define_equ() {
        let mut st = SymbolTable::new();
        st.define_equ("TEN", 10);
        let sym = st.lookup("TEN").unwrap();
        assert!(sym.is_equ);
        assert_eq!(sym.equ_value, 10);
    }

    #[test]
    fn test_symbol_table_lookup_case_insensitive() {
        let mut st = SymbolTable::new();
        st.define_equ("myvar", 42);
        assert!(st.lookup("MYVAR").is_some());
        assert!(st.lookup("myvar").is_some());
    }

    #[test]
    fn test_symbol_table_is_defined() {
        let mut st = SymbolTable::new();
        assert!(!st.is_defined("X"));
        st.define_equ("X", 1);
        assert!(st.is_defined("X"));
    }

    #[test]
    fn test_length_attr() {
        let mut st = SymbolTable::new();
        st.define_label("MYFIELD", 20, 'C');
        assert_eq!(st.length_attr("MYFIELD"), Some(20));
    }

    #[test]
    fn test_type_attr() {
        let mut st = SymbolTable::new();
        st.define_label("FW", 4, 'F');
        assert_eq!(st.type_attr("FW"), Some('F'));
    }

    #[test]
    fn test_advance_location_counter() {
        let mut st = SymbolTable::new();
        st.location_counter = 0;
        st.advance(4);
        assert_eq!(st.location_counter, 4);
        st.advance(8);
        assert_eq!(st.location_counter, 12);
    }

    #[test]
    fn test_eval_number() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("42", &st).unwrap(), 42);
    }

    #[test]
    fn test_eval_addition() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("10+20", &st).unwrap(), 30);
    }

    #[test]
    fn test_eval_subtraction() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("50-20", &st).unwrap(), 30);
    }

    #[test]
    fn test_eval_multiplication() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("4*8", &st).unwrap(), 32);
    }

    #[test]
    fn test_eval_division() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("100/4", &st).unwrap(), 25);
    }

    #[test]
    fn test_eval_division_by_zero() {
        let st = SymbolTable::new();
        assert!(eval_expr_str("10/0", &st).is_err());
    }

    #[test]
    fn test_eval_precedence() {
        let st = SymbolTable::new();
        // 2 + 3 * 4 = 14 (not 20).
        assert_eq!(eval_expr_str("2+3*4", &st).unwrap(), 14);
    }

    #[test]
    fn test_eval_parentheses() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("(2+3)*4", &st).unwrap(), 20);
    }

    #[test]
    fn test_eval_symbol() {
        let mut st = SymbolTable::new();
        st.define_equ("ENTLEN", 8);
        assert_eq!(eval_expr_str("4*ENTLEN", &st).unwrap(), 32);
    }

    #[test]
    fn test_eval_symbol_address() {
        let mut st = SymbolTable::new();
        st.location_counter = 0x200;
        st.define_label("MYTABLE", 100, 'C');
        // MYTABLE + 4*8 = 0x200 + 32 = 544.
        st.define_equ("ENTLEN", 8);
        assert_eq!(eval_expr_str("MYTABLE+(4*ENTLEN)", &st).unwrap(), 0x200 + 32);
    }

    #[test]
    fn test_eval_location_counter() {
        let mut st = SymbolTable::new();
        st.location_counter = 0x300;
        assert_eq!(eval_expr_str("*", &st).unwrap(), 0x300);
        assert_eq!(eval_expr_str("*+4", &st).unwrap(), 0x304);
    }

    #[test]
    fn test_eval_length_attr() {
        let mut st = SymbolTable::new();
        st.define_label("MYFIELD", 20, 'C');
        assert_eq!(eval_expr_str("L'MYFIELD", &st).unwrap(), 20);
    }

    #[test]
    fn test_eval_hex_term() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("X'FF'", &st).unwrap(), 255);
    }

    #[test]
    fn test_eval_char_term() {
        let st = SymbolTable::new();
        // C'A' = 65 (ASCII).
        assert_eq!(eval_expr_str("C'A'", &st).unwrap(), 65);
    }

    #[test]
    fn test_eval_bin_term() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("B'11001010'", &st).unwrap(), 0xCA);
    }

    #[test]
    fn test_eval_undefined_symbol() {
        let st = SymbolTable::new();
        assert!(eval_expr_str("NOSUCH", &st).is_err());
    }

    #[test]
    fn test_eval_unary_minus() {
        let st = SymbolTable::new();
        assert_eq!(eval_expr_str("-5", &st).unwrap(), -5);
        assert_eq!(eval_expr_str("-5+10", &st).unwrap(), 5);
    }

    #[test]
    fn test_eval_complex_expression() {
        let mut st = SymbolTable::new();
        st.define_equ("A", 10);
        st.define_equ("B", 3);
        // A * B + 5 = 30 + 5 = 35.
        assert_eq!(eval_expr_str("A*B+5", &st).unwrap(), 35);
    }

    #[test]
    fn test_define_equ_full() {
        let mut st = SymbolTable::new();
        st.define_equ_full("AREA", 0x500, 256, 'C');
        let sym = st.lookup("AREA").unwrap();
        assert_eq!(sym.equ_value, 0x500);
        assert_eq!(sym.length, 256);
        assert_eq!(sym.sym_type, 'C');
    }

    #[test]
    fn test_symbol_table_section() {
        let mut st = SymbolTable::new();
        st.set_section("CSECT1");
        st.define_label("A", 4, 'F');
        assert_eq!(st.lookup("A").unwrap().section, "CSECT1");
    }
}
