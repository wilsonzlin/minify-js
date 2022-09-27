use std::str::FromStr;

use crate::ast::{NodeId, Syntax};
use crate::error::SyntaxResult;
use crate::parse::parser::Parser;
use crate::parse::stmt::parse_stmt;
use crate::symbol::ScopeId;
use crate::token::TokenType;

use super::pattern::ParsePatternSyntax;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TopLevelMode {
    Global,
    Module,
}

impl FromStr for TopLevelMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "global" => Ok(TopLevelMode::Global),
            "module" => Ok(TopLevelMode::Module),
            _ => Err("invalid top-level mode"),
        }
    }
}

pub struct ParseTopLevelResult {
    pub top_level_node_id: NodeId,
    pub top_level_scope_id: ScopeId,
}

pub fn parse_top_level(
    parser: &mut Parser,
    top_level_mode: TopLevelMode,
) -> SyntaxResult<ParseTopLevelResult> {
    let top_level_scope_id = match top_level_mode {
        TopLevelMode::Global => parser.create_global_scope(),
        TopLevelMode::Module => parser.create_module_scope(),
    };
    let mut body: Vec<NodeId> = Vec::new();
    let syntax = ParsePatternSyntax {
        await_allowed: true,
        yield_allowed: true,
    };
    while !parser.consume_if(TokenType::EOF)?.is_match() {
        body.push(parse_stmt(top_level_scope_id, parser, &syntax)?);
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
