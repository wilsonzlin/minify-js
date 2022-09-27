use crate::ast::{
    ExportName, ExportNames, ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, NodeId, Syntax,
};
use crate::error::{SyntaxErrorType, SyntaxResult};
use crate::parse::decl::{parse_decl_function, parse_decl_var};
use crate::parse::expr::parse_expr;
use crate::parse::literal::parse_and_normalise_literal_string;
use crate::parse::parser::Parser;
use crate::parse::pattern::{parse_pattern, ParsePatternAction};
use crate::source::SourceRange;
use crate::symbol::{ScopeId, ScopeType, Symbol};
use crate::token::TokenType;

use super::decl::{parse_decl_class, VarDeclParseMode};
use super::expr::{parse_expr_with_asi, Asi};
use super::pattern::{is_valid_pattern_identifier, ParsePatternSyntax};

// Parses `a`, `a as b`, `default as b`. Creates the symbol if importing.
fn parse_import_or_export_name(
    scope: ScopeId,
    parser: &mut Parser,
    add_to_scope: bool,
) -> SyntaxResult<ExportName> {
    let (target, alias) = match parser
        .consume_if(TokenType::KeywordDefault)?
        .match_loc_take()
    {
        Some(target) => {
            parser.require(TokenType::KeywordAs)?;
            let alias = parser.require(TokenType::Identifier)?.loc().clone();
            (target, alias)
        }
        None => {
            let target = parser.require(TokenType::Identifier)?.loc().clone();
            let alias = if parser.consume_if(TokenType::KeywordAs)?.is_match() {
                parser.require(TokenType::Identifier)?.loc().clone()
            } else {
                target.clone()
            };
            (target, alias)
        }
    };
    let alias_node = parser.create_node(
        scope,
        alias.clone(),
        Syntax::IdentifierPattern {
            name: alias.clone(),
        },
    );
    if add_to_scope {
        parser[scope].add_symbol(alias.clone(), Symbol::new(alias_node))?;
    };
    Ok(ExportName {
        target,
        alias: alias_node,
    })
}

pub fn parse_stmt(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    match parser.peek()?.typ() {
        TokenType::BraceOpen => parse_stmt_block(scope, parser, syntax),
        TokenType::KeywordBreak => parse_stmt_break(scope, parser, syntax),
        TokenType::KeywordClass => parse_decl_class(scope, parser, syntax),
        TokenType::KeywordConst | TokenType::KeywordLet | TokenType::KeywordVar => {
            parse_stmt_var(scope, parser, syntax)
        }
        TokenType::KeywordContinue => parse_stmt_continue(scope, parser, syntax),
        TokenType::KeywordDebugger => parse_stmt_debugger(scope, parser),
        TokenType::KeywordDo => parse_stmt_do_while(scope, parser, syntax),
        TokenType::KeywordExport => parse_stmt_export(scope, parser, syntax),
        TokenType::KeywordFor => parse_stmt_for(scope, parser, syntax),
        TokenType::KeywordAsync | TokenType::KeywordFunction => {
            parse_decl_function(scope, parser, syntax)
        }
        TokenType::KeywordIf => parse_stmt_if(scope, parser, syntax),
        TokenType::KeywordImport => parse_stmt_import_or_expr_import(scope, parser, syntax),
        TokenType::KeywordReturn => parse_stmt_return(scope, parser, syntax),
        TokenType::KeywordSwitch => parse_stmt_switch(scope, parser, syntax),
        TokenType::KeywordThrow => parse_stmt_throw(scope, parser, syntax),
        TokenType::KeywordTry => parse_stmt_try(scope, parser, syntax),
        TokenType::KeywordWhile => parse_stmt_while(scope, parser, syntax),
        TokenType::Semicolon => parse_stmt_empty(scope, parser),
        t if is_valid_pattern_identifier(t, syntax) => {
            let checkpoint = parser.checkpoint();
            let label_name = parser.next()?.loc_take();
            if parser.consume_if(TokenType::Colon)?.is_match() {
                let statement = parse_stmt(scope, parser, syntax)?;
                Ok(parser.create_node(
                    scope,
                    parser.since_checkpoint(checkpoint),
                    Syntax::LabelStmt {
                        name: label_name,
                        statement,
                    },
                ))
            } else {
                parser.restore_checkpoint(checkpoint);
                parse_stmt_expression(scope, parser, syntax)
            }
        }
        _ => parse_stmt_expression(scope, parser, syntax),
    }
}

pub fn parse_stmt_empty(scope: ScopeId, parser: &mut Parser) -> SyntaxResult<NodeId> {
    let loc = parser.require(TokenType::Semicolon)?.loc_take();
    Ok(parser.create_node(scope, loc, Syntax::EmptyStmt {}))
}

pub fn parse_stmt_block(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::BraceOpen)?;
    let block_scope = parser.create_child_scope(scope, ScopeType::Block);
    let mut body: Vec<NodeId> = Vec::new();
    loop {
        if let Some(end_loc) = parser.consume_if(TokenType::BraceClose)?.match_loc() {
            return Ok(parser.create_node(
                scope,
                start.loc() + end_loc,
                Syntax::BlockStmt { body },
            ));
        };
        body.push(parse_stmt(block_scope, parser, syntax)?);
    }
}

pub fn parse_stmt_var(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let declaration = parse_decl_var(scope, parser, VarDeclParseMode::Asi, syntax)?;
    Ok(parser.create_node(
        scope,
        parser[declaration].loc().clone(),
        Syntax::VarStmt { declaration },
    ))
}

struct BreakOrContinue {
    loc: SourceRange,
    label: Option<SourceRange>,
}

fn parse_stmt_break_or_continue(
    parser: &mut Parser,
    t: TokenType,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<BreakOrContinue> {
    let mut loc = parser.require(t)?.loc_take();
    let next = parser.peek()?;
    let label =
        if is_valid_pattern_identifier(next.typ(), syntax) && !next.preceded_by_line_terminator() {
            // Label.
            parser.consume_peeked();
            loc.extend(next.loc());
            Some(next.loc_take())
        } else if next.typ() == TokenType::Semicolon {
            parser.consume_peeked();
            None
        } else if next.preceded_by_line_terminator() || next.typ() == TokenType::BraceClose {
            // ASI.
            None
        } else {
            return Err(next.error(SyntaxErrorType::ExpectedSyntax("continue label")));
        };
    Ok(BreakOrContinue { loc, label })
}

pub fn parse_stmt_break(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let stmt = parse_stmt_break_or_continue(parser, TokenType::KeywordBreak, syntax)?;
    Ok(parser.create_node(scope, stmt.loc, Syntax::BreakStmt { label: stmt.label }))
}

pub fn parse_stmt_continue(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let stmt = parse_stmt_break_or_continue(parser, TokenType::KeywordContinue, syntax)?;
    Ok(parser.create_node(scope, stmt.loc, Syntax::ContinueStmt { label: stmt.label }))
}

pub fn parse_stmt_debugger(scope: ScopeId, parser: &mut Parser) -> SyntaxResult<NodeId> {
    let loc = parser.require(TokenType::KeywordDebugger)?.loc_take();
    Ok(parser.create_node(scope, loc, Syntax::DebuggerStmt {}))
}

// https://tc39.es/ecma262/#sec-exports
// https://jakearchibald.com/2021/export-default-thing-vs-thing-as-default/
pub fn parse_stmt_export(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    // TODO Ensure top-level.
    let start = parser.require(TokenType::KeywordExport)?;
    let cp = parser.checkpoint();
    let t = parser.next()?;
    Ok(match t.typ() {
        TokenType::BraceOpen => {
            let mut names = Vec::<ExportName>::new();
            loop {
                if parser.consume_if(TokenType::BraceClose)?.is_match() {
                    break;
                };
                let name = parse_import_or_export_name(scope, parser, false)?;
                names.push(name);
                if !parser.consume_if(TokenType::Comma)?.is_match() {
                    parser.require(TokenType::BraceClose)?;
                    break;
                };
            }
            let from = parser.consume_if(TokenType::KeywordFrom)?.and_then(|| {
                let from = parse_and_normalise_literal_string(parser)?;
                Ok(from)
            })?;
            // TODO Loc
            parser.create_node(
                scope,
                start.loc().clone(),
                Syntax::ExportListStmt {
                    names: ExportNames::Specific(names),
                    from,
                },
            )
        }
        TokenType::Asterisk => {
            let alias = if parser.consume_if(TokenType::KeywordAs)?.is_match() {
                let alias = parser.require(TokenType::Identifier)?.loc().clone();
                let alias_node = parser.create_node(
                    scope,
                    alias.clone(),
                    Syntax::IdentifierPattern {
                        name: alias.clone(),
                    },
                );
                Some(alias_node)
                // We don't need to add the symbol as it's not exposed within the module's scope.
            } else {
                None
            };
            parser.require(TokenType::KeywordFrom)?;
            let from = parse_and_normalise_literal_string(parser)?;
            // TODO Loc
            parser.create_node(
                scope,
                start.loc().clone(),
                Syntax::ExportListStmt {
                    names: ExportNames::All(alias),
                    from: Some(from),
                },
            )
        }
        TokenType::KeywordDefault => match parser.peek()?.typ() {
            // `class` and `function` are treated as statements that are hoisted, not expressions; however, they can be unnamed, which gives them the name `default`.
            TokenType::KeywordAsync | TokenType::KeywordClass | TokenType::KeywordFunction => {
                let declaration = parse_stmt(scope, parser, syntax)?;
                parser.create_node(
                    scope,
                    start.loc() + parser[declaration].loc(),
                    Syntax::ExportDeclStmt {
                        declaration,
                        default: true,
                    },
                )
            }
            _ => {
                let expression = parse_expr(scope, parser, TokenType::Semicolon, syntax)?;
                parser.create_node(
                    scope,
                    start.loc() + parser[expression].loc(),
                    Syntax::ExportDefaultExprStmt { expression },
                )
            }
        },
        TokenType::KeywordVar
        | TokenType::KeywordLet
        | TokenType::KeywordConst
        | TokenType::KeywordFunction
        | TokenType::KeywordClass => {
            // Reconsume declaration keyword.
            parser.restore_checkpoint(cp);
            let declaration = parse_stmt(scope, parser, syntax)?;
            parser.create_node(
                scope,
                start.loc() + parser[declaration].loc(),
                Syntax::ExportDeclStmt {
                    declaration,
                    default: false,
                },
            )
        }
        _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("exportable"))),
    })
}

// WARNING: Do not reuse this functions for other statements, as this will output a statement node, not an expression, which can lead to double semicolons that cause invalid code when outputting.
pub fn parse_stmt_expression(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let mut asi = Asi::can();
    let expression = parse_expr_with_asi(scope, parser, TokenType::Semicolon, &mut asi, syntax)?;
    if !asi.did_end_with_asi {
        parser.require(TokenType::Semicolon)?;
    };
    Ok(parser.create_node(
        scope,
        parser[expression].loc().clone(),
        Syntax::ExpressionStmt { expression },
    ))
}

pub fn parse_stmt_for(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let for_scope = parser.create_child_scope(scope, ScopeType::Block);

    let start = parser.require(TokenType::KeywordFor)?;
    parser.require(TokenType::ParenthesisOpen)?;
    enum LhsRaw {
        Declaration(NodeId),
        Expression(NodeId),
        Pattern(NodeId),
        Empty,
    }
    let lhs_raw = match parser.peek()?.typ() {
        TokenType::KeywordVar | TokenType::KeywordLet | TokenType::KeywordConst => {
            LhsRaw::Declaration(parse_decl_var(
                for_scope,
                parser,
                VarDeclParseMode::Leftmost,
                syntax,
            )?)
        }
        TokenType::Semicolon => LhsRaw::Empty,
        _ => {
            // A pattern could be reinterpreted as an expression (and vice versa), so we can only try parsing both.
            let checkpoint = parser.checkpoint();
            match if let Ok(node) =
                parse_pattern(for_scope, parser, ParsePatternAction::None, syntax)
            {
                match parser.peek()?.typ() {
                    TokenType::KeywordIn | TokenType::KeywordOf => Some(LhsRaw::Pattern(node)),
                    _ => {
                        // Mistakenly interpreted as pattern.
                        None
                    }
                }
            } else {
                None
            } {
                Some(p) => p,
                None => {
                    parser.restore_checkpoint(checkpoint);
                    LhsRaw::Expression(parse_expr(for_scope, parser, TokenType::Semicolon, syntax)?)
                }
            }
        }
    };
    let header = match parser.peek()?.typ() {
        TokenType::KeywordOf | TokenType::KeywordIn => {
            // for-of or for-in statement.
            let of = match parser.next()?.typ() {
                TokenType::KeywordOf => true,
                TokenType::KeywordIn => false,
                _ => unreachable!(),
            };
            let lhs = match lhs_raw {
                LhsRaw::Empty => return Err(start.error(SyntaxErrorType::ForLoopHeaderHasNoLhs)),
                LhsRaw::Declaration(node) => match parser[node].stx() {
                    Syntax::VarDecl { declarators, .. } => {
                        if declarators.len() != 1 {
                            return Err(
                                start.error(SyntaxErrorType::ForLoopHeaderHasMultipleDeclarators)
                            );
                        }
                        ForInOfStmtHeaderLhs::Declaration(node)
                    }
                    _ => unreachable!(),
                },
                LhsRaw::Pattern(pat) => ForInOfStmtHeaderLhs::Pattern(pat),
                LhsRaw::Expression(_) => {
                    return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs))
                }
            };
            let rhs = parse_expr(for_scope, parser, TokenType::ParenthesisClose, syntax)?;
            parser.require(TokenType::ParenthesisClose)?;
            ForStmtHeader::InOf { of, lhs, rhs }
        }
        _ => {
            // for statement.
            let init = match lhs_raw {
                LhsRaw::Declaration(decl) => {
                    parser.require(TokenType::Semicolon)?;
                    ForThreeInit::Declaration(decl)
                }
                LhsRaw::Expression(expr) => {
                    // We must check, due to possibility of illegal ASI (see previous).
                    parser.require(TokenType::Semicolon)?;
                    ForThreeInit::Expression(expr)
                }
                LhsRaw::Empty => {
                    parser.require(TokenType::Semicolon)?;
                    ForThreeInit::None
                }
                LhsRaw::Pattern(_) => {
                    return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs))
                }
            };
            let condition = if parser.consume_if(TokenType::Semicolon)?.is_match() {
                None
            } else {
                let expr = parse_expr(for_scope, parser, TokenType::Semicolon, syntax)?;
                parser.require(TokenType::Semicolon)?;
                Some(expr)
            };
            let post = if parser.consume_if(TokenType::ParenthesisClose)?.is_match() {
                None
            } else {
                let expr = parse_expr(for_scope, parser, TokenType::ParenthesisClose, syntax)?;
                parser.require(TokenType::ParenthesisClose)?;
                Some(expr)
            };
            ForStmtHeader::Three {
                init,
                condition,
                post,
            }
        }
    };
    let body = parse_stmt(for_scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        start.loc() + parser[body].loc(),
        Syntax::ForStmt { header, body },
    ))
}

pub fn parse_stmt_if(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordIf)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let test = parse_expr(scope, parser, TokenType::ParenthesisClose, syntax)?;
    parser.require(TokenType::ParenthesisClose)?;
    let consequent = parse_stmt(scope, parser, syntax)?;
    let alternate = if parser.consume_if(TokenType::KeywordElse)?.is_match() {
        Some(parse_stmt(scope, parser, syntax)?)
    } else {
        None
    };
    let end = alternate.as_ref().unwrap_or(&consequent);
    Ok(parser.create_node(
        scope,
        start.loc() + parser[*end].loc(),
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        },
    ))
}

pub fn parse_stmt_import_or_expr_import(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let cp = parser.checkpoint();
    let start = parser.require(TokenType::KeywordImport)?;
    if parser.consume_if(TokenType::ParenthesisOpen)?.is_match() {
        parser.restore_checkpoint(cp);
        return parse_stmt_expression(scope, parser, syntax);
    };

    // TODO Ensure top-level.

    let (default, can_have_names) =
        if let Some(alias) = parser.consume_if(TokenType::Identifier)?.match_loc() {
            let alias_node = parser.create_node(
                scope,
                alias.clone(),
                Syntax::IdentifierPattern {
                    name: alias.clone(),
                },
            );
            parser[scope].add_symbol(alias.clone(), Symbol::new(alias_node))?;
            (
                Some(alias_node),
                parser.consume_if(TokenType::Comma)?.is_match(),
            )
        } else {
            (None, true)
        };
    let names = if !can_have_names {
        None
    } else if parser.consume_if(TokenType::Asterisk)?.is_match() {
        parser.require(TokenType::KeywordAs)?;
        let alias = parser.require(TokenType::Identifier)?.loc().clone();
        let alias_node = parser.create_node(
            scope,
            alias.clone(),
            Syntax::IdentifierPattern {
                name: alias.clone(),
            },
        );
        parser[scope].add_symbol(alias.clone(), Symbol::new(alias_node))?;
        Some(ExportNames::All(Some(alias_node)))
    } else {
        parser.require(TokenType::BraceOpen)?;
        let mut names = Vec::<ExportName>::new();
        while !parser.consume_if(TokenType::BraceClose)?.is_match() {
            let name = parse_import_or_export_name(scope, parser, true)?;
            names.push(name);
            if !parser.consume_if(TokenType::Comma)?.is_match() {
                break;
            };
        }
        parser.require(TokenType::BraceClose)?;
        Some(ExportNames::Specific(names))
    };
    parser.require(TokenType::KeywordFrom)?;
    let module = parse_and_normalise_literal_string(parser)?;
    parser.require(TokenType::Semicolon)?;
    // TODO Loc
    Ok(parser.create_node(
        scope,
        start.loc().clone(),
        Syntax::ImportStmt {
            default,
            module,
            names,
        },
    ))
}

pub fn parse_stmt_return(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordReturn)?;
    let mut loc = start.loc().clone();
    let value = if parser.peek()?.preceded_by_line_terminator()
        || parser.peek()?.typ() == TokenType::BraceClose
    {
        // Automatic Semicolon Insertion.
        None
    } else if parser.consume_if(TokenType::Semicolon)?.is_match() {
        None
    } else {
        let mut asi = Asi::can();
        let value = parse_expr_with_asi(scope, parser, TokenType::Semicolon, &mut asi, syntax)?;
        if !asi.did_end_with_asi {
            parser.require(TokenType::Semicolon)?;
        };
        loc.extend(parser[value].loc());
        Some(value)
    };
    Ok(parser.create_node(scope, loc, Syntax::ReturnStmt { value }))
}

pub fn parse_stmt_throw(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordThrow)?;
    if parser.peek()?.preceded_by_line_terminator() {
        // Illegal under Automatic Semicolon Insertion rules.
        return Err(start.error(SyntaxErrorType::LineTerminatorAfterThrow));
    }
    let mut asi = Asi::can();
    let value = parse_expr_with_asi(scope, parser, TokenType::Semicolon, &mut asi, syntax)?;
    if !asi.did_end_with_asi {
        parser.require(TokenType::Semicolon)?;
    };
    Ok(parser.create_node(
        scope,
        start.loc() + parser[value].loc(),
        Syntax::ThrowStmt { value },
    ))
}

pub fn parse_stmt_try(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordTry)?;
    let mut loc = start.loc().clone();
    let wrapped = parse_stmt_block(scope, parser, syntax)?;
    let catch = if parser.consume_if(TokenType::KeywordCatch)?.is_match() {
        let catch_scope = parser.create_child_scope(scope, ScopeType::Block);
        let parameter = if parser.consume_if(TokenType::ParenthesisOpen)?.is_match() {
            let pattern = parse_pattern(
                catch_scope,
                parser,
                ParsePatternAction::AddToBlockScope,
                syntax,
            )?;
            parser.require(TokenType::ParenthesisClose)?;
            Some(pattern)
        } else {
            None
        };
        let body = parse_stmt_block(catch_scope, parser, syntax)?;
        loc.extend(parser[body].loc());
        Some(parser.create_node(
            scope,
            parser[body].loc().clone(),
            Syntax::CatchBlock { parameter, body },
        ))
    } else {
        None
    };
    let finally = if parser.consume_if(TokenType::KeywordFinally)?.is_match() {
        let body = parse_stmt_block(scope, parser, syntax)?;
        loc.extend(parser[body].loc());
        Some(body)
    } else {
        None
    };
    if catch.is_none() && finally.is_none() {
        return Err(start.error(SyntaxErrorType::TryStatementHasNoCatchOrFinally));
    }
    Ok(parser.create_node(
        scope,
        loc,
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        },
    ))
}

pub fn parse_stmt_while(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordWhile)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let condition = parse_expr(scope, parser, TokenType::ParenthesisClose, syntax)?;
    parser.require(TokenType::ParenthesisClose)?;
    let body = parse_stmt(scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        start.loc() + parser[body].loc(),
        Syntax::WhileStmt { condition, body },
    ))
}

pub fn parse_stmt_do_while(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordDo)?;
    let body = parse_stmt(scope, parser, syntax)?;
    parser.require(TokenType::KeywordWhile)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let condition = parse_expr(scope, parser, TokenType::ParenthesisClose, syntax)?;
    let end = parser.require(TokenType::ParenthesisClose)?;
    parser.consume_if(TokenType::Semicolon)?;
    Ok(parser.create_node(
        scope,
        start.loc() + end.loc(),
        Syntax::DoWhileStmt { condition, body },
    ))
}

pub fn parse_stmt_switch(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordSwitch)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let test = parse_expr(scope, parser, TokenType::ParenthesisClose, syntax)?;
    parser.require(TokenType::ParenthesisClose)?;
    parser.require(TokenType::BraceOpen)?;
    let mut branches = Vec::<NodeId>::new();
    while parser.peek()?.typ() != TokenType::BraceClose {
        let mut loc = parser.peek()?.loc_take();
        let case = if parser.consume_if(TokenType::KeywordCase)?.is_match() {
            Some(parse_expr(scope, parser, TokenType::Colon, syntax)?)
        } else {
            parser.require(TokenType::KeywordDefault)?;
            None
        };
        parser.require(TokenType::Colon)?;
        let mut body: Vec<NodeId> = Vec::new();
        loop {
            match parser.peek()?.typ() {
                TokenType::KeywordCase | TokenType::KeywordDefault | TokenType::BraceClose => break,
                _ => {
                    let stmt = parse_stmt(scope, parser, syntax)?;
                    body.push(stmt);
                    loc.extend(parser[stmt].loc());
                }
            }
        }
        branches.push(parser.create_node(scope, loc, Syntax::SwitchBranch { case, body }));
    }
    let end = parser.require(TokenType::BraceClose)?;
    Ok(parser.create_node(
        scope,
        start.loc() + end.loc(),
        Syntax::SwitchStmt { test, branches },
    ))
}
