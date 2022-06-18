use crate::ast::{
    ArrayPatternElement, ClassOrObjectMemberKey, Node, ObjectPatternProperty, Syntax,
};
use crate::error::{SyntaxErrorType, TsResult};
use crate::parse::expr::parse_expr_until_either;
use crate::parse::literal::parse_class_or_object_member_key;
use crate::parse::parser::Parser;
use crate::token::TokenType;

pub fn parse_pattern(parser: &mut Parser) -> TsResult<Node> {
    let t = parser.next()?;
    Ok(match t.typ() {
        TokenType::Identifier => Node::new(
            t.loc().clone(),
            Syntax::IdentifierPattern {
                name: t.loc().clone(),
            },
        ),
        TokenType::BraceOpen => {
            let mut properties = Vec::<ObjectPatternProperty>::new();
            let mut rest = None;
            loop {
                if parser.peek()?.typ() == TokenType::BraceClose {
                    break;
                };
                // Check inside loop to ensure that it must come first or after a comma.
                if parser.consume_if(TokenType::DotDotDot)?.is_match() {
                    rest = Some(parser.require(TokenType::Identifier)?.loc().clone());
                    break;
                };

                let key = parse_class_or_object_member_key(parser)?;
                let target = if parser.consume_if(TokenType::Colon)?.is_match() {
                    Some(parse_pattern(parser)?)
                } else {
                    if let ClassOrObjectMemberKey::Computed(name) = key {
                        return Err(name.error(SyntaxErrorType::ExpectedSyntax(
                            "object pattern property subpattern",
                        )));
                    };
                    None
                };
                let default_value = if parser.consume_if(TokenType::Equals)?.is_match() {
                    Some(parse_expr_until_either(
                        parser,
                        TokenType::Comma,
                        TokenType::BraceClose,
                    )?)
                } else {
                    None
                };
                properties.push(ObjectPatternProperty {
                    key,
                    target,
                    default_value,
                });
                // This will break if `}`.
                if !parser.consume_if(TokenType::Comma)?.is_match() {
                    break;
                };
            }
            let close = parser.require(TokenType::BraceClose)?;
            Node::new(
                t.loc() + close.loc(),
                Syntax::ObjectPattern { properties, rest },
            )
        }
        TokenType::BracketOpen => {
            let mut elements = Vec::<Option<ArrayPatternElement>>::new();
            let mut rest = None;
            loop {
                if parser.consume_if(TokenType::BracketClose)?.is_match() {
                    break;
                };
                // Check inside loop to ensure that it must come first or after a comma.
                if parser.consume_if(TokenType::DotDotDot)?.is_match() {
                    rest = Some(parse_pattern(parser)?);
                    break;
                };

                // An unnamed element is allowed to ignore that element.
                if parser.consume_if(TokenType::Comma)?.is_match() {
                    elements.push(None);
                } else {
                    let target = parse_pattern(parser)?;
                    let default_value = if parser.consume_if(TokenType::Equals)?.is_match() {
                        Some(parse_expr_until_either(
                            parser,
                            TokenType::Comma,
                            TokenType::BracketClose,
                        )?)
                    } else {
                        None
                    };
                    elements.push(Some(ArrayPatternElement {
                        target,
                        default_value,
                    }));
                    // This will break if `]`.
                    if !parser.consume_if(TokenType::Comma)?.is_match() {
                        break;
                    };
                };
            }
            let close = parser.require(TokenType::BracketClose)?;
            Node::new(
                t.loc() + close.loc(),
                Syntax::ArrayPattern { elements, rest },
            )
        }
        _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("pattern"))),
    })
}
