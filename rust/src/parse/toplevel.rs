use crate::ast::{NodeId, Syntax};
use crate::error::TsResult;
use crate::parse::parser::Parser;
use crate::parse::stmt::parse_stmt;
use crate::symbol::ScopeId;
use crate::token::TokenType;

pub struct ParseTopLevelResult {
    pub top_level_node_id: NodeId,
    pub top_level_scope_id: ScopeId,
}

pub fn parse_top_level(parser: &mut Parser) -> TsResult<ParseTopLevelResult> {
    let top_level_scope_id = parser.create_global_scope();
    let mut body: Vec<NodeId> = Vec::new();
    while !parser.consume_if(TokenType::EOF)?.is_match() {
        body.push(parse_stmt(top_level_scope_id, parser)?);
    }
    let top_level_node_id = parser.create_node(
        top_level_scope_id,
        parser.source_range(),
        Syntax::TopLevel { body },
    );
    Ok(ParseTopLevelResult {
        top_level_node_id,
        top_level_scope_id,
    })
}
