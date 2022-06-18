use crate::ast::{Node, Syntax};
use crate::error::TsResult;
use crate::parse::parser::Parser;
use crate::parse::stmt::parse_stmt;
use crate::token::TokenType;

pub fn parse_top_level(parser: &mut Parser) -> TsResult<Node> {
    let mut body: Vec<Node> = Vec::new();
    while !parser.consume_if(TokenType::EOF)?.is_match() {
        body.push(parse_stmt(parser)?);
    }
    Ok(Node::new(parser.source_range(), Syntax::TopLevel { body }))
}
