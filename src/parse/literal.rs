use std::str::FromStr;

use crate::ast::ClassOrObjectMemberKey;
use crate::error::{TsError, TsErrorType, TsResult};
use crate::num::JsNumber;
use crate::parse::expr::parse_expr;
use crate::parse::parser::Parser;
use crate::source::SourceRange;
use crate::token::TokenType;

fn parse_radix(raw: &str, radix: u32) -> Result<f64, ()> {
    u32::from_str_radix(raw, radix)
        .map_err(|_| ())
        .and_then(|v| f64::try_from(v).map_err(|_| ()))
}

pub fn normalise_literal_number(raw: &SourceRange) -> TsResult<JsNumber> {
    // TODO We assume that the Rust parser follows ECMAScript spec and that different representations
    // of the same value get parsed into the same f64 value/bit pattern (e.g. `5.1e10` and `0.51e11`).
    match raw.as_str() {
        s if s.starts_with("0b") || s.starts_with("0B") => parse_radix(&s[2..], 2),
        s if s.starts_with("0o") || s.starts_with("0o") => parse_radix(&s[2..], 8),
        s if s.starts_with("0x") || s.starts_with("0X") => parse_radix(&s[2..], 16),
        s => f64::from_str(s).map_err(|_| ()),
    }
    .map(|n| JsNumber(n))
    .map_err(|_| TsError::from_loc(raw, TsErrorType::MalformedLiteralNumber))
}

pub fn normalise_literal_string(raw: &SourceRange) -> TsResult<String> {
    // TODO Handle escapes.
    Ok(raw.as_str()[1..raw.len() - 1].to_string())
}

pub fn parse_and_normalise_literal_string(parser: &mut Parser) -> TsResult<String> {
    let t = parser.require(TokenType::LiteralString)?;
    let s = normalise_literal_string(t.loc())?;
    Ok(s)
}

pub fn parse_class_or_object_member_key(parser: &mut Parser) -> TsResult<ClassOrObjectMemberKey> {
    Ok(if parser.consume_if(TokenType::BracketOpen)?.is_match() {
        let expr = parse_expr(parser, TokenType::BracketClose)?;
        parser.require(TokenType::BracketClose)?;
        ClassOrObjectMemberKey::Computed(expr)
    } else {
        let name = parser.require(TokenType::Identifier)?;
        ClassOrObjectMemberKey::Direct(name.loc().clone())
    })
}
