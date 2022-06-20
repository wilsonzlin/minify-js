use crate::ast::{ArrayPatternElement, ClassOrObjectMemberKey, NodeId, Syntax};
use crate::error::{SyntaxErrorType, TsResult};
use crate::parse::expr::parse_expr_until_either;
use crate::parse::literal::parse_class_or_object_member_key;
use crate::parse::parser::Parser;
use crate::symbol::{ScopeId, Symbol};
use crate::token::TokenType;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsePatternAction {
    None,
    AddToBlockScope,
    // For var statements. Note that this won't add to the top-level if it's a global and that's the closest, since global declarators cannot be minified.
    AddToClosureScope,
}

fn parse_pattern_identifier(
    scope: ScopeId,
    parser: &mut Parser,
    action: ParsePatternAction,
) -> TsResult<NodeId> {
    let t = parser.require(TokenType::Identifier)?;
    let node_id = parser.create_node(
        scope,
        t.loc().clone(),
        Syntax::IdentifierPattern {
            name: t.loc().clone(),
        },
    );
    match action {
        ParsePatternAction::None => {}
        ParsePatternAction::AddToBlockScope => {
            let scope = &mut parser[scope];
            scope.add_block_symbol(t.loc().clone(), Symbol::new(node_id))?;
        }
        ParsePatternAction::AddToClosureScope => {
            if let Some(closure_id) = parser[scope].self_or_ancestor_closure() {
                parser[closure_id].add_symbol(t.loc().clone(), Symbol::new(node_id))?;
            };
        }
    };
    Ok(node_id)
}

pub fn parse_pattern(
    scope: ScopeId,
    parser: &mut Parser,
    action: ParsePatternAction,
) -> TsResult<NodeId> {
    let checkpoint = parser.checkpoint();
    let t = parser.next()?;
    Ok(match t.typ() {
        TokenType::Identifier => {
            parser.restore_checkpoint(checkpoint);
            parse_pattern_identifier(scope, parser, action)?
        }
        TokenType::BraceOpen => {
            let mut properties = Vec::<NodeId>::new();
            let mut rest = None;
            loop {
                if parser.peek()?.typ() == TokenType::BraceClose {
                    break;
                };
                let mut loc = parser.peek()?.loc_take();
                // Check inside loop to ensure that it must come first or after a comma.
                if parser.consume_if(TokenType::DotDotDot)?.is_match() {
                    rest = Some(parse_pattern_identifier(scope, parser, action)?);
                    break;
                };

                let key = parse_class_or_object_member_key(scope, parser)?;
                let target = if parser.consume_if(TokenType::Colon)?.is_match() {
                    Some(parse_pattern(scope, parser, action)?)
                } else {
                    if let ClassOrObjectMemberKey::Computed(name) = key {
                        return Err(parser[name].error(SyntaxErrorType::ExpectedSyntax(
                            "object pattern property subpattern",
                        )));
                    };
                    None
                };
                let default_value = if parser.consume_if(TokenType::Equals)?.is_match() {
                    Some(parse_expr_until_either(
                        scope,
                        parser,
                        TokenType::Comma,
                        TokenType::BraceClose,
                    )?)
                } else {
                    None
                };
                if let Some(n) = default_value.or(target) {
                    loc.extend(parser[n].loc());
                };
                let direct_key_name = match &key {
                    ClassOrObjectMemberKey::Direct(name) => Some(name.clone()),
                    _ => None,
                };
                let property = parser.create_node(
                    scope,
                    loc,
                    Syntax::ObjectPatternProperty {
                        key,
                        target,
                        default_value,
                    },
                );
                properties.push(property);
                match (direct_key_name, target, action) {
                    (Some(name), None, ParsePatternAction::AddToBlockScope) => {
                        parser[scope].add_block_symbol(name, Symbol::new(property))?;
                    }
                    (Some(name), None, ParsePatternAction::AddToClosureScope) => {
                        if let Some(closure_id) = parser[scope].self_or_ancestor_closure() {
                            parser[closure_id].add_symbol(name, Symbol::new(property))?;
                        }
                    }
                    _ => {}
                };
                // This will break if `}`.
                if !parser.consume_if(TokenType::Comma)?.is_match() {
                    break;
                };
            }
            let close = parser.require(TokenType::BraceClose)?;
            parser.create_node(
                scope,
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
                    rest = Some(parse_pattern(scope, parser, action)?);
                    break;
                };

                // An unnamed element is allowed to ignore that element.
                if parser.consume_if(TokenType::Comma)?.is_match() {
                    elements.push(None);
                } else {
                    let target = parse_pattern(scope, parser, action)?;
                    let default_value = if parser.consume_if(TokenType::Equals)?.is_match() {
                        Some(parse_expr_until_either(
                            scope,
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
            parser.create_node(
                scope,
                t.loc() + close.loc(),
                Syntax::ArrayPattern { elements, rest },
            )
        }
        _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("pattern"))),
    })
}
