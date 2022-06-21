use crate::ast::{NodeId, Syntax};
use crate::error::SyntaxResult;
use crate::parse::expr::parse_expr_until_either;
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::symbol::ScopeId;
use crate::token::TokenType;

use super::pattern::{ParsePatternAction, ParsePatternSyntax};

// `scope` should be a newly created closure scope for this function.
pub fn parse_signature_function(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start_pos = parser.checkpoint();

    let mut parameters = Vec::new();
    parser.require(TokenType::ParenthesisOpen)?;
    loop {
        if parser.consume_if(TokenType::ParenthesisClose)?.is_match() {
            break;
        };

        let rest = parser.consume_if(TokenType::DotDotDot)?.is_match();
        let pattern = parse_pattern(scope, parser, ParsePatternAction::AddToClosureScope, syntax)?;
        let default_value = parser.consume_if(TokenType::Equals)?.and_then(|| {
            parse_expr_until_either(
                scope,
                parser,
                TokenType::Comma,
                TokenType::ParenthesisClose,
                syntax,
            )
        })?;

        // TODO Location
        parameters.push(parser.create_node(
            scope,
            parser[pattern].loc().clone(),
            Syntax::ParamDecl {
                rest,
                pattern,
                default_value,
            },
        ));

        if !parser.consume_if(TokenType::Comma)?.is_match() {
            parser.require(TokenType::ParenthesisClose)?;
            break;
        };
    }

    Ok(parser.create_node(
        scope,
        parser.since_checkpoint(start_pos),
        Syntax::FunctionSignature { parameters },
    ))
}
