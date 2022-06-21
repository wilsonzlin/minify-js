use std::str::FromStr;

use crate::ast::ClassOrObjectMemberKey;
use crate::error::{SyntaxError, SyntaxErrorType, SyntaxResult};
use crate::num::JsNumber;
use crate::parse::expr::parse_expr;
use crate::parse::parser::Parser;
use crate::source::SourceRange;
use crate::symbol::ScopeId;
use crate::token::TokenType;

use super::pattern::ParsePatternSyntax;

fn parse_radix(raw: &str, radix: u32) -> Result<f64, ()> {
    u64::from_str_radix(raw, radix)
        .map_err(|_| ())
        // TODO This is lossy, but there is no TryFrom for converting from u64 to f64, and u32 cannot represent all possible JS values.
        .map(|v| v as f64)
}

pub fn normalise_literal_number(raw: &SourceRange) -> SyntaxResult<JsNumber> {
    // TODO We assume that the Rust parser follows ECMAScript spec and that different representations
    // of the same value get parsed into the same f64 value/bit pattern (e.g. `5.1e10` and `0.51e11`).
    match raw.as_str() {
        s if s.starts_with("0b") || s.starts_with("0B") => parse_radix(&s[2..], 2),
        s if s.starts_with("0o") || s.starts_with("0o") => parse_radix(&s[2..], 8),
        s if s.starts_with("0x") || s.starts_with("0X") => parse_radix(&s[2..], 16),
        s => f64::from_str(s).map_err(|_| ()),
    }
    .map(|n| JsNumber(n))
    .map_err(|_| SyntaxError::from_loc(raw, SyntaxErrorType::MalformedLiteralNumber, None))
}

pub fn normalise_literal_string(raw: &SourceRange) -> SyntaxResult<String> {
    // TODO Handle escapes.
    Ok(raw.as_str()[1..raw.len() - 1].to_string())
}

pub fn parse_and_normalise_literal_string(parser: &mut Parser) -> SyntaxResult<String> {
    let t = parser.require(TokenType::LiteralString)?;
    let s = normalise_literal_string(t.loc())?;
    Ok(s)
}

pub fn parse_class_or_object_member_key(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<ClassOrObjectMemberKey> {
    Ok(if parser.consume_if(TokenType::BracketOpen)?.is_match() {
        let expr = parse_expr(scope, parser, TokenType::BracketClose, syntax)?;
        parser.require(TokenType::BracketClose)?;
        ClassOrObjectMemberKey::Computed(expr)
    } else {
        let name = parser.require(TokenType::Identifier)?;
        ClassOrObjectMemberKey::Direct(name.loc().clone())
    })
}
