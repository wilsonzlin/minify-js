use crate::ast::{Node, Syntax};
use crate::error::TsResult;
use crate::parse::expr::parse_expr_until_either;
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::token::TokenType;

pub fn parse_signature_function(parser: &mut Parser) -> TsResult<Node> {
    let start_pos = parser.checkpoint();

    let mut parameters = Vec::new();
    parser.require(TokenType::ParenthesisOpen)?;
    loop {
        if parser.consume_if(TokenType::ParenthesisClose)?.is_match() {
            break;
        };

        let rest = parser.consume_if(TokenType::DotDotDot)?.is_match();
        let pattern = parse_pattern(parser)?;
        let default_value = parser.consume_if(TokenType::Equals)?.and_then(|| {
            parse_expr_until_either(parser, TokenType::Comma, TokenType::ParenthesisClose)
        })?;

        // TODO Location
        parameters.push(Node::new(
            pattern.loc().clone(),
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

    Ok(Node::new(
        parser.since_checkpoint(start_pos),
        Syntax::FunctionSignature { parameters },
    ))
}
