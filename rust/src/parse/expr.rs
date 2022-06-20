use crate::ast::{
    ArrayElement, ArrayPatternElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue,
    LiteralTemplatePart, NodeData, NodeId, ObjectMemberType, Syntax,
};
use crate::error::{SyntaxErrorType, TsResult};
use crate::lex::{lex_template_string_continue, LexMode, KEYWORDS_MAPPING};
use crate::operator::{Associativity, OperatorName, OPERATORS};
use crate::parse::literal::{
    normalise_literal_number, normalise_literal_string, parse_and_normalise_literal_string,
};
use crate::parse::operator::{MULTARY_OPERATOR_MAPPING, UNARY_OPERATOR_MAPPING};
use crate::parse::parser::Parser;
use crate::parse::signature::parse_signature_function;
use crate::parse::stmt::parse_stmt;
use crate::symbol::{ScopeId, ScopeType, Symbol};
use crate::token::TokenType;
use crate::update::NodeUpdates;

use super::class_or_object::{parse_class_or_object_member, ParseClassOrObjectMemberResult};
use super::pattern::{is_valid_pattern_identifier, ParsePatternSyntax};
use super::stmt::parse_stmt_block;

pub struct Asi {
    pub can_end_with_asi: bool,
    pub did_end_with_asi: bool,
}

impl Asi {
    pub fn can() -> Asi {
        Asi {
            can_end_with_asi: true,
            did_end_with_asi: false,
        }
    }

    pub fn no() -> Asi {
        Asi {
            can_end_with_asi: false,
            did_end_with_asi: false,
        }
    }
}

pub fn parse_call_args(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> TsResult<Vec<NodeId>> {
    let mut args = Vec::<NodeId>::new();
    loop {
        if parser.peek()?.typ() == TokenType::ParenthesisClose {
            break;
        };
        let spread = parser.consume_if(TokenType::DotDotDot)?.is_match();
        let value = parse_expr_until_either(
            scope,
            parser,
            TokenType::Comma,
            TokenType::ParenthesisClose,
            syntax,
        )?;
        args.push(parser.create_node(
            scope,
            parser[value].loc().clone(),
            Syntax::CallArg { spread, value },
        ));
        if !parser.consume_if(TokenType::Comma)?.is_match() {
            break;
        };
    }
    Ok(args)
}

pub fn parse_expr(
    scope: ScopeId,
    parser: &mut Parser,
    terminator: TokenType,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let mut asi = Asi::no();
    parse_expr_with_min_prec(
        scope,
        parser,
        1,
        terminator,
        TokenType::_Dummy,
        false,
        &mut asi,
        syntax,
    )
}

pub fn parse_expr_with_asi(
    scope: ScopeId,
    parser: &mut Parser,
    terminator: TokenType,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    parse_expr_with_min_prec(
        scope,
        parser,
        1,
        terminator,
        TokenType::_Dummy,
        false,
        asi,
        syntax,
    )
}

pub fn parse_expr_until_either(
    scope: ScopeId,
    parser: &mut Parser,
    terminator_a: TokenType,
    terminator_b: TokenType,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let mut asi = Asi::no();
    parse_expr_with_min_prec(
        scope,
        parser,
        1,
        terminator_a,
        terminator_b,
        false,
        &mut asi,
        syntax,
    )
}

pub fn parse_expr_until_either_with_asi(
    scope: ScopeId,
    parser: &mut Parser,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    parse_expr_with_min_prec(
        scope,
        parser,
        1,
        terminator_a,
        terminator_b,
        false,
        asi,
        syntax,
    )
}

pub fn parse_grouping(
    scope: ScopeId,
    parser: &mut Parser,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    parser.require(TokenType::ParenthesisOpen)?;
    let expr = parse_expr_with_min_prec(
        scope,
        parser,
        1,
        TokenType::ParenthesisClose,
        TokenType::_Dummy,
        true,
        asi,
        syntax,
    )?;
    parser.require(TokenType::ParenthesisClose)?;
    Ok(expr)
}

pub fn parse_expr_array(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let loc_start = parser.require(TokenType::BracketOpen)?.loc_take();
    let mut elements = Vec::<ArrayElement>::new();
    loop {
        if parser.consume_if(TokenType::Comma)?.is_match() {
            elements.push(ArrayElement::Empty);
            continue;
        };
        if parser.peek()?.typ() == TokenType::BracketClose {
            break;
        };
        let rest = parser.consume_if(TokenType::DotDotDot)?.is_match();
        let value = parse_expr_until_either(
            scope,
            parser,
            TokenType::Comma,
            TokenType::BracketClose,
            syntax,
        )?;
        elements.push(if rest {
            ArrayElement::Rest(value)
        } else {
            ArrayElement::Single(value)
        });
        if parser.peek()?.typ() == TokenType::BracketClose {
            break;
        };
        parser.require(TokenType::Comma)?;
    }
    let loc_end = parser.require(TokenType::BracketClose)?.loc_take();
    Ok(parser.create_node(
        scope,
        &loc_start + &loc_end,
        Syntax::LiteralArrayExpr { elements },
    ))
}

pub fn parse_expr_object(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let loc_start = parser.require(TokenType::BraceOpen)?.loc_take();
    let mut members = Vec::<NodeId>::new();
    loop {
        if parser.peek()?.typ() == TokenType::BraceClose {
            break;
        };
        let rest = parser.consume_if(TokenType::DotDotDot)?.is_match();
        if rest {
            let value = parse_expr_until_either(
                scope,
                parser,
                TokenType::Comma,
                TokenType::BraceClose,
                syntax,
            )?;
            let loc = parser[value].loc().clone();
            members.push(parser.create_node(
                scope,
                loc,
                Syntax::ObjectMember {
                    typ: ObjectMemberType::Rest { value },
                },
            ));
        } else {
            let loc_checkpoint = parser.checkpoint();
            let ParseClassOrObjectMemberResult { key, value } = parse_class_or_object_member(
                scope,
                parser,
                TokenType::Colon,
                TokenType::Comma,
                &mut Asi::no(),
                syntax,
            )?;
            members.push(parser.create_node(
                scope,
                parser.since_checkpoint(loc_checkpoint),
                Syntax::ObjectMember {
                    typ: match value {
                        ClassOrObjectMemberValue::Property { initializer: None } => {
                            ObjectMemberType::Shorthand {
                                name: match key {
                                    ClassOrObjectMemberKey::Direct(key) => key.clone(),
                                    _ => unreachable!(),
                                },
                            }
                        }
                        _ => ObjectMemberType::Valued { key, value },
                    },
                },
            ));
        }
        if parser.peek()?.typ() == TokenType::BraceClose {
            break;
        };
        parser.require(TokenType::Comma)?;
    }
    let loc_end = parser.require(TokenType::BraceClose)?.loc_take();
    Ok(parser.create_node(
        scope,
        &loc_start + &loc_end,
        Syntax::LiteralObjectExpr { members },
    ))
}

pub fn parse_expr_arrow_function_or_grouping(
    scope: ScopeId,
    parser: &mut Parser,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    // Try and parse as arrow function signature first.
    // If we fail, backtrack and parse as grouping instead.
    // After we see `=>`, we assume it's definitely an arrow function and do not backtrack.

    // NOTE: We originally implemented conversion from parameters to expression to prevent the need
    // for backtracking. However, this ended up being too complex for little performance gain,
    // as most usages of grouping involve a non-comma binary operator (such as `+`) and so parsing
    // as arrow function fails quickly. Complex patterns like `{a, b: { c: [d, e] } = f }` are
    // unlikely to be used as operands in a grouping.

    let cp = parser.checkpoint();

    let fn_scope = parser.create_child_scope(scope, ScopeType::Closure);

    let signature = match parse_signature_function(fn_scope, parser, syntax).and_then(|sig| {
        let arrow = parser.require(TokenType::EqualsChevronRight)?;
        if arrow.preceded_by_line_terminator() {
            // Illegal under Automatic Semicolon Insertion rules.
            return Err(arrow.error(SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters));
        }
        Ok(sig)
    }) {
        Ok(sig) => sig,
        Err(err) if err.typ() == SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters => {
            return Err(err.clone())
        }
        Err(_) => {
            parser.restore_checkpoint(cp);
            return parse_grouping(scope, parser, asi, syntax);
        }
    };
    let body = match parser.peek()?.typ() {
        TokenType::BraceOpen => parse_stmt(fn_scope, parser, syntax)?,
        _ => parse_expr_until_either(fn_scope, parser, terminator_a, terminator_b, syntax)?,
    };
    Ok(parser.create_node(
        scope,
        parser[signature].loc() + parser[body].loc(),
        Syntax::ArrowFunctionExpr { signature, body },
    ))
}

pub fn parse_expr_import(scope: ScopeId, parser: &mut Parser) -> TsResult<NodeId> {
    let start = parser.require(TokenType::KeywordImport)?;
    parser.require(TokenType::ParenthesisOpen)?;
    // TODO Non-literal-string imports.
    let module = parse_and_normalise_literal_string(parser)?;
    parser.require(TokenType::ParenthesisClose)?;
    let end = parser.require(TokenType::ParenthesisClose)?;
    Ok(parser.create_node(
        scope,
        start.loc() + end.loc(),
        Syntax::ImportExpr { module },
    ))
}

pub fn parse_expr_function(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let fn_scope = parser.create_child_scope(scope, ScopeType::Closure);
    let start = parser.require(TokenType::KeywordFunction)?.loc().clone();
    let generator = parser.consume_if(TokenType::Asterisk)?.is_match();
    let name = match parser.consume_if(TokenType::Identifier)?.match_loc_take() {
        Some(name) => {
            let name_node = parser.create_node(
                fn_scope,
                name.clone(),
                Syntax::ClassOrFunctionName { name: name.clone() },
            );
            parser[fn_scope].add_symbol(name.clone(), Symbol::new(name_node))?;
            Some(name_node)
        }
        None => None,
    };
    let signature = parse_signature_function(fn_scope, parser, syntax)?;
    let body = parse_stmt_block(fn_scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        &start + parser[body].loc(),
        Syntax::FunctionExpr {
            parenthesised: false,
            generator,
            name,
            signature,
            body,
        },
    ))
}

fn parse_expr_operand(
    scope: ScopeId,
    parser: &mut Parser,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let cp = parser.checkpoint();
    let t = parser.next_with_mode(LexMode::SlashIsRegex)?;
    let operand = if let Some(operator) = UNARY_OPERATOR_MAPPING.get(&t.typ()) {
        let operator = if operator.name == OperatorName::Yield
            && parser.consume_if(TokenType::Asterisk)?.is_match()
        {
            &OPERATORS[&OperatorName::YieldDelegated]
        } else {
            *operator
        };
        let next_min_prec =
            operator.precedence + (operator.associativity == Associativity::Left) as u8;
        let operand = parse_expr_with_min_prec(
            scope,
            parser,
            next_min_prec,
            terminator_a,
            terminator_b,
            false,
            asi,
            syntax,
        )?;
        parser.create_node(
            scope,
            t.loc() + parser[operand].loc(),
            Syntax::UnaryExpr {
                parenthesised: false,
                operator: operator.name,
                argument: operand,
            },
        )
    } else {
        match t.typ() {
            TokenType::BracketOpen => {
                parser.restore_checkpoint(cp);
                parse_expr_array(scope, parser, syntax)?
            }
            TokenType::BraceOpen => {
                parser.restore_checkpoint(cp);
                parse_expr_object(scope, parser, syntax)?
            }
            typ if is_valid_pattern_identifier(
                typ,
                &ParsePatternSyntax {
                    async_allowed: syntax.async_allowed,
                    await_allowed: false,
                    yield_allowed: syntax.yield_allowed,
                },
            ) =>
            {
                if parser.peek()?.typ() == TokenType::EqualsChevronRight {
                    // Single-unparenthesised-parameter arrow function.
                    let fn_scope = parser.create_child_scope(scope, ScopeType::Closure);
                    let pattern = parser.create_node(
                        fn_scope,
                        t.loc().clone(),
                        Syntax::IdentifierPattern {
                            name: t.loc().clone(),
                        },
                    );
                    parser[fn_scope].add_block_symbol(t.loc().clone(), Symbol::new(pattern))?;
                    let param = parser.create_node(
                        scope,
                        t.loc().clone(),
                        Syntax::ParamDecl {
                            rest: false,
                            pattern,
                            default_value: None,
                        },
                    );
                    let signature = parser.create_node(
                        scope,
                        t.loc().clone(),
                        Syntax::FunctionSignature {
                            parameters: vec![param],
                        },
                    );
                    let arrow = parser.next()?;
                    if arrow.preceded_by_line_terminator() {
                        // Illegal under Automatic Semicolon Insertion rules.
                        return Err(arrow
                            .error(SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters));
                    };
                    let body = match parser.peek()?.typ() {
                        TokenType::BraceOpen => parse_stmt(fn_scope, parser, syntax)?,
                        _ => parse_expr_until_either(
                            fn_scope,
                            parser,
                            terminator_a,
                            terminator_b,
                            syntax,
                        )?,
                    };
                    parser.create_node(
                        scope,
                        parser[signature].loc() + parser[body].loc(),
                        Syntax::ArrowFunctionExpr { signature, body },
                    )
                } else {
                    parser.create_node(
                        scope,
                        t.loc().clone(),
                        Syntax::IdentifierExpr {
                            name: t.loc().clone(),
                        },
                    )
                }
            }
            TokenType::KeywordFunction => {
                parser.restore_checkpoint(cp);
                parse_expr_function(scope, parser, syntax)?
            }
            TokenType::KeywordImport => {
                parser.restore_checkpoint(cp);
                parse_expr_import(scope, parser)?
            }
            TokenType::KeywordSuper => {
                parser.create_node(scope, t.loc().clone(), Syntax::SuperExpr {})
            }
            TokenType::KeywordThis => {
                parser.create_node(scope, t.loc().clone(), Syntax::ThisExpr {})
            }
            TokenType::LiteralTrue | TokenType::LiteralFalse => parser.create_node(
                scope,
                t.loc().clone(),
                Syntax::LiteralBooleanExpr {
                    value: t.typ() == TokenType::LiteralTrue,
                },
            ),
            TokenType::LiteralNull => {
                parser.create_node(scope, t.loc().clone(), Syntax::LiteralNull {})
            }
            TokenType::LiteralNumber => parser.create_node(
                scope,
                t.loc().clone(),
                Syntax::LiteralNumberExpr {
                    value: normalise_literal_number(t.loc())?,
                },
            ),
            TokenType::LiteralRegex => {
                parser.create_node(scope, t.loc().clone(), Syntax::LiteralRegexExpr {})
            }
            TokenType::LiteralString => parser.create_node(
                scope,
                t.loc().clone(),
                Syntax::LiteralStringExpr {
                    value: normalise_literal_string(t.loc())?,
                },
            ),
            TokenType::LiteralTemplatePartString => {
                let mut loc = t.loc().clone();
                let mut parts = vec![LiteralTemplatePart::String(t.loc().clone())];
                loop {
                    let substitution = parse_expr(scope, parser, TokenType::BraceClose, syntax)?;
                    parser.require(TokenType::BraceClose)?;
                    parts.push(LiteralTemplatePart::Substitution(substitution));
                    let string = lex_template_string_continue(parser.lexer_mut(), false)?;
                    loc.extend(string.loc());
                    parts.push(LiteralTemplatePart::String(string.loc().clone()));
                    parser.clear_buffered();
                    match string.typ() {
                        TokenType::LiteralTemplatePartStringEnd => break,
                        _ => {}
                    };
                }
                parser.create_node(scope, loc, Syntax::LiteralTemplateExpr { parts })
            }
            TokenType::LiteralTemplatePartStringEnd => {
                let parts = vec![LiteralTemplatePart::String(t.loc().clone())];
                parser.create_node(
                    scope,
                    t.loc().clone(),
                    Syntax::LiteralTemplateExpr { parts },
                )
            }
            TokenType::LiteralUndefined => {
                parser.create_node(scope, t.loc().clone(), Syntax::LiteralUndefined {})
            }
            TokenType::ParenthesisOpen => {
                parser.restore_checkpoint(cp);
                parse_expr_arrow_function_or_grouping(
                    scope,
                    parser,
                    terminator_a,
                    terminator_b,
                    asi,
                    syntax,
                )?
            }
            _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("expression operand"))),
        }
    };
    Ok(operand)
}

fn transform_literal_expr_to_destructuring_pattern(
    scope: ScopeId,
    parser: &Parser,
    node: NodeId,
    updates: &mut NodeUpdates,
) -> TsResult<NodeId> {
    let loc = parser[node].loc();
    match parser[node].stx() {
        Syntax::LiteralArrayExpr { elements } => {
            let mut pat_elements = Vec::<Option<ArrayPatternElement>>::new();
            let mut rest = None;
            for element in elements {
                if rest.is_some() {
                    return Err(parser[node].error(SyntaxErrorType::InvalidAssigmentTarget));
                };
                match element {
                    ArrayElement::Single(elem) => {
                        match parser[*elem].stx() {
                            Syntax::BinaryExpr {
                                parenthesised,
                                operator,
                                left,
                                right,
                            } => {
                                if *parenthesised || *operator != OperatorName::Assignment {
                                    return Err(
                                        parser[node].error(SyntaxErrorType::InvalidAssigmentTarget)
                                    );
                                };
                                pat_elements.push(Some(ArrayPatternElement {
                                    target: transform_literal_expr_to_destructuring_pattern(
                                        scope, parser, *left, updates,
                                    )?,
                                    default_value: Some(*right),
                                }));
                            }
                            _ => pat_elements.push(Some(ArrayPatternElement {
                                target: transform_literal_expr_to_destructuring_pattern(
                                    scope, parser, *elem, updates,
                                )?,
                                default_value: None,
                            })),
                        };
                    }
                    ArrayElement::Rest(expr) => {
                        rest = Some(transform_literal_expr_to_destructuring_pattern(
                            scope, parser, *expr, updates,
                        )?);
                    }
                    ArrayElement::Empty => pat_elements.push(None),
                };
            }
            Ok(updates.create_node(
                scope,
                loc.clone(),
                Syntax::ArrayPattern {
                    elements: pat_elements,
                    rest,
                },
            ))
        }
        Syntax::LiteralObjectExpr { members } => {
            let mut properties = Vec::<NodeId>::new();
            let mut rest = None;
            for member in members {
                if rest.is_some() {
                    return Err(parser[node].error(SyntaxErrorType::InvalidAssigmentTarget));
                };
                match parser[*member].stx() {
                    Syntax::ObjectMember { typ } => match typ {
                        ObjectMemberType::Valued { key, value } => {
                            let (target, default_value) = match value {
                                ClassOrObjectMemberValue::Property {
                                    initializer: Some(initializer),
                                } => match parser[*initializer].stx() {
                                    Syntax::BinaryExpr {
                                        parenthesised,
                                        operator,
                                        left,
                                        right,
                                    } => {
                                        if *parenthesised || *operator != OperatorName::Assignment {
                                            return Err(parser[node]
                                                .error(SyntaxErrorType::InvalidAssigmentTarget));
                                        };
                                        (
                                            transform_literal_expr_to_destructuring_pattern(
                                                scope, parser, *left, updates,
                                            )?,
                                            Some(*right),
                                        )
                                    }
                                    _ => (
                                        transform_literal_expr_to_destructuring_pattern(
                                            scope,
                                            parser,
                                            *initializer,
                                            updates,
                                        )?,
                                        None,
                                    ),
                                },
                                _ => {
                                    return Err(
                                        parser[node].error(SyntaxErrorType::InvalidAssigmentTarget)
                                    )
                                }
                            };
                            properties.push(updates.create_node(
                                scope,
                                loc.clone(),
                                Syntax::ObjectPatternProperty {
                                    key: key.clone(),
                                    target: Some(target),
                                    default_value,
                                },
                            ));
                        }
                        ObjectMemberType::Shorthand { name } => {
                            properties.push(updates.create_node(
                                scope,
                                loc.clone(),
                                Syntax::ObjectPatternProperty {
                                    key: ClassOrObjectMemberKey::Direct(name.clone()),
                                    target: None,
                                    default_value: None,
                                },
                            ));
                        }
                        ObjectMemberType::Rest { value } => {
                            rest = Some(transform_literal_expr_to_destructuring_pattern(
                                scope, parser, *value, updates,
                            )?);
                        }
                    },
                    _ => unreachable!(),
                };
            }
            Ok(updates.create_node(
                scope,
                loc.clone(),
                Syntax::ObjectPattern { properties, rest },
            ))
        }
        // It's possible to encounter an IdentifierPattern e.g. `{ a: b = 1 } = x`, where `b = 1` is already parsed as an assignment.
        Syntax::IdentifierExpr { name } | Syntax::IdentifierPattern { name } => Ok(updates
            .create_node(
                scope,
                loc.clone(),
                Syntax::IdentifierPattern { name: name.clone() },
            )),
        _ => Err(parser[node].error(SyntaxErrorType::InvalidAssigmentTarget)),
    }
}

// Trying to check if every object, array, or identifier expression operand is actually an assignment target first is too expensive wasteful, so simply retroactively transform the LHS of a BinaryExpr with Assignment* operator into a target, raising an error if it can't (and is an invalid assignment target). A valid target is:
// - A chain of non-optional-chaining member, computed member, and call operators, not ending in a call.
// - A pattern.
fn convert_assignment_lhs_to_target(
    scope: ScopeId,
    parser: &mut Parser,
    lhs: NodeId,
    operator_name: OperatorName,
) -> TsResult<NodeId> {
    match parser[lhs].stx() {
        e @ (Syntax::LiteralArrayExpr { .. }
        | Syntax::LiteralObjectExpr { .. }
        | Syntax::IdentifierExpr { .. }) => {
            if operator_name != OperatorName::Assignment
                && match e {
                    Syntax::IdentifierExpr { .. } => false,
                    _ => true,
                }
            {
                return Err(parser[lhs].error(SyntaxErrorType::InvalidAssigmentTarget));
            }
            // We must transform into a pattern.
            let mut updates = NodeUpdates::new(parser.node_map());
            let root =
                transform_literal_expr_to_destructuring_pattern(scope, parser, lhs, &mut updates)?;
            updates.apply_updates(parser.node_map_mut());
            Ok(root)
        }
        Syntax::ComputedMemberExpr { .. } | Syntax::MemberExpr { .. } => {
            // As long as the expression ends with ComputedMemberExpr or MemberExpr, it's valid e.g. `(a, b?.a ?? 3, c = d || {})[1] = x`.
            Ok(lhs)
        }
        _ => Err(parser[lhs].error(SyntaxErrorType::InvalidAssigmentTarget)),
    }
}

pub fn parse_expr_with_min_prec(
    scope: ScopeId,
    parser: &mut Parser,
    min_prec: u8,
    terminator_a: TokenType,
    terminator_b: TokenType,
    parenthesised: bool,
    asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> TsResult<NodeId> {
    let mut left = parse_expr_operand(scope, parser, terminator_a, terminator_b, asi, syntax)?;

    loop {
        let cp = parser.checkpoint();
        let t = parser.next()?;

        if t.typ() == terminator_a || t.typ() == terminator_b {
            parser.restore_checkpoint(cp);
            break;
        };

        match t.typ() {
            // Automatic Semicolon Insertion rules: no newline between operand and postfix operator.
            TokenType::PlusPlus | TokenType::HyphenHyphen if !t.preceded_by_line_terminator() => {
                let operator_name = match t.typ() {
                    TokenType::PlusPlus => OperatorName::PostfixIncrement,
                    TokenType::HyphenHyphen => OperatorName::PostfixDecrement,
                    _ => unreachable!(),
                };
                let operator = &OPERATORS[&operator_name];
                if operator.precedence < min_prec {
                    parser.restore_checkpoint(cp);
                    break;
                };
                left = parser.create_node(
                    scope,
                    parser[left].loc() + t.loc(),
                    Syntax::UnaryPostfixExpr {
                        parenthesised: false,
                        operator: operator_name,
                        argument: left,
                    },
                );
                continue;
            }
            _ => {}
        };

        match MULTARY_OPERATOR_MAPPING.get(&t.typ()) {
            None => {
                if asi.can_end_with_asi
                    && (t.preceded_by_line_terminator() || t.typ() == TokenType::BraceClose)
                {
                    // Automatic Semicolon Insertion.
                    // TODO Exceptions (e.g. for loop header).
                    parser.restore_checkpoint(cp);
                    asi.did_end_with_asi = true;
                    break;
                };
                return Err(t.error(SyntaxErrorType::ExpectedSyntax("expression operator")));
            }
            Some(operator) => {
                if operator.precedence < min_prec {
                    parser.restore_checkpoint(cp);
                    break;
                };

                let next_min_prec =
                    operator.precedence + (operator.associativity == Associativity::Left) as u8;

                left = match operator.name {
                    OperatorName::Call | OperatorName::OptionalChainingCall => {
                        let arguments = parse_call_args(scope, parser, syntax)?;
                        let end = parser.require(TokenType::ParenthesisClose)?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + end.loc(),
                            Syntax::CallExpr {
                                parenthesised: false,
                                optional_chaining: match operator.name {
                                    OperatorName::OptionalChainingCall => true,
                                    _ => false,
                                },
                                arguments,
                                callee: left,
                            },
                        )
                    }
                    OperatorName::ComputedMemberAccess
                    | OperatorName::OptionalChainingComputedMemberAccess => {
                        let member = parse_expr(scope, parser, TokenType::BracketClose, syntax)?;
                        let end = parser.require(TokenType::BracketClose)?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + end.loc(),
                            Syntax::ComputedMemberExpr {
                                optional_chaining: match operator.name {
                                    OperatorName::OptionalChainingComputedMemberAccess => true,
                                    _ => false,
                                },
                                object: left,
                                member,
                            },
                        )
                    }
                    OperatorName::Conditional => {
                        let consequent = parse_expr_with_min_prec(
                            scope,
                            parser,
                            1,
                            TokenType::Colon,
                            TokenType::_Dummy,
                            false,
                            &mut Asi::no(),
                            syntax,
                        )?;
                        parser.require(TokenType::Colon)?;
                        let alternate = parse_expr_with_min_prec(
                            scope,
                            parser,
                            1,
                            terminator_a,
                            terminator_b,
                            false,
                            asi,
                            syntax,
                        )?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + parser[alternate].loc(),
                            Syntax::ConditionalExpr {
                                parenthesised: false,
                                test: left,
                                consequent,
                                alternate,
                            },
                        )
                    }
                    OperatorName::MemberAccess | OperatorName::OptionalChainingMemberAccess => {
                        let right_tok = parser.next()?;
                        match right_tok.typ() {
                            TokenType::Identifier => {}
                            t if KEYWORDS_MAPPING.contains_key(&t) => {}
                            _ => {
                                return Err(right_tok.error(SyntaxErrorType::ExpectedSyntax(
                                    "member access property",
                                )))
                            }
                        };
                        let right = right_tok.loc_take();
                        parser.create_node(
                            scope,
                            parser[left].loc() + &right,
                            Syntax::MemberExpr {
                                parenthesised: false,
                                optional_chaining: match operator.name {
                                    OperatorName::OptionalChainingMemberAccess => true,
                                    _ => false,
                                },
                                left,
                                right,
                            },
                        )
                    }
                    _ => {
                        match operator.name {
                            OperatorName::Assignment
                            | OperatorName::AssignmentAddition
                            | OperatorName::AssignmentBitwiseAnd
                            | OperatorName::AssignmentBitwiseLeftShift
                            | OperatorName::AssignmentBitwiseOr
                            | OperatorName::AssignmentBitwiseRightShift
                            | OperatorName::AssignmentBitwiseUnsignedRightShift
                            | OperatorName::AssignmentBitwiseXor
                            | OperatorName::AssignmentDivision
                            | OperatorName::AssignmentExponentiation
                            | OperatorName::AssignmentLogicalAnd
                            | OperatorName::AssignmentLogicalOr
                            | OperatorName::AssignmentMultiplication
                            | OperatorName::AssignmentNullishCoalescing
                            | OperatorName::AssignmentRemainder
                            | OperatorName::AssignmentSubtraction => {
                                left = convert_assignment_lhs_to_target(
                                    scope,
                                    parser,
                                    left,
                                    operator.name,
                                )?;
                            }
                            _ => {}
                        };
                        let right = parse_expr_with_min_prec(
                            scope,
                            parser,
                            next_min_prec,
                            terminator_a,
                            terminator_b,
                            false,
                            asi,
                            syntax,
                        )?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + parser[right].loc(),
                            Syntax::BinaryExpr {
                                parenthesised: false,
                                operator: operator.name,
                                left,
                                right,
                            },
                        )
                    }
                };
            }
        };
    }

    if parenthesised {
        match parser[left].stx_mut() {
            Syntax::BinaryExpr {
                ref mut parenthesised,
                ..
            }
            | Syntax::CallExpr {
                ref mut parenthesised,
                ..
            }
            | Syntax::ConditionalExpr {
                ref mut parenthesised,
                ..
            }
            | Syntax::FunctionExpr {
                ref mut parenthesised,
                ..
            }
            | Syntax::MemberExpr {
                ref mut parenthesised,
                ..
            }
            | Syntax::UnaryExpr {
                ref mut parenthesised,
                ..
            } => {
                *parenthesised = true;
            }
            _ => {}
        };
    };

    Ok(left)
}
