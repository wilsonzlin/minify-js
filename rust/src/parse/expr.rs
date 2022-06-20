use crate::ast::{
    ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, NodeId, ObjectMemberType,
    Syntax,
};
use crate::error::{SyntaxErrorType, TsResult};
use crate::lex::{LexMode, KEYWORDS_MAPPING};
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
        args.push(parse_expr_until_either(
            scope,
            parser,
            TokenType::Comma,
            TokenType::ParenthesisClose,
            syntax,
        )?);
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
            typ if is_valid_pattern_identifier(typ, syntax) => parser.create_node(
                scope,
                t.loc().clone(),
                Syntax::IdentifierExpr {
                    name: t.loc().clone(),
                },
            ),
            TokenType::KeywordFunction => {
                parser.restore_checkpoint(cp);
                parse_expr_function(scope, parser, syntax)?
            }
            TokenType::KeywordImport => {
                parser.restore_checkpoint(cp);
                parse_expr_import(scope, parser)?
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
                    OperatorName::Call => {
                        let arguments = parse_call_args(scope, parser, syntax)?;
                        let end = parser.require(TokenType::ParenthesisClose)?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + end.loc(),
                            Syntax::CallExpr {
                                parenthesised: false,
                                arguments,
                                callee: left,
                            },
                        )
                    }
                    OperatorName::ComputedMemberAccess => {
                        let member = parse_expr(scope, parser, TokenType::BracketClose, syntax)?;
                        let end = parser.require(TokenType::BracketClose)?;
                        parser.create_node(
                            scope,
                            parser[left].loc() + end.loc(),
                            Syntax::ComputedMemberExpr {
                                object: left,
                                member,
                            },
                        )
                    }
                    OperatorName::Conditional => {
                        let consequent = parse_expr(scope, parser, TokenType::Colon, syntax)?;
                        parser.require(TokenType::Colon)?;
                        let alternate = parse_expr_with_min_prec(
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
                            Syntax::MemberAccessExpr {
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
            | Syntax::MemberAccessExpr {
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
