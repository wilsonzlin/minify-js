use crate::ast::{
    ExportNames, ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, ImportNames,
    ImportOrExportName, Node, SwitchBranch, Syntax, TryCatch,
};
use crate::error::{TsErrorType, TsResult};
use crate::parse::decl::{parse_decl_function, parse_decl_var};
use crate::parse::expr::parse_expr;
use crate::parse::literal::parse_and_normalise_literal_string;
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::source::SourceRange;
use crate::token::TokenType;

use super::decl::VarDeclParseMode;
use super::expr::{parse_expr_with_asi, Asi};

fn parse_import_or_export_name(parser: &mut Parser) -> TsResult<ImportOrExportName> {
    Ok(
        if parser.consume_if(TokenType::KeywordDefault)?.is_match() {
            ImportOrExportName::Default
        } else {
            ImportOrExportName::Name(parser.require(TokenType::Identifier)?.loc().clone())
        },
    )
}

pub fn parse_stmt(parser: &mut Parser) -> TsResult<Node> {
    match parser.peek()?.typ() {
        TokenType::BraceOpen => parse_stmt_block(parser),
        TokenType::KeywordBreak => parse_stmt_break(parser),
        TokenType::KeywordConst | TokenType::KeywordLet | TokenType::KeywordVar => {
            parse_stmt_var(parser)
        }
        TokenType::KeywordContinue => parse_stmt_continue(parser),
        TokenType::KeywordDo => parse_stmt_do_while(parser),
        TokenType::KeywordExport => parse_stmt_export(parser),
        TokenType::KeywordFor => parse_stmt_for(parser),
        TokenType::KeywordFunction => parse_decl_function(parser),
        TokenType::KeywordIf => parse_stmt_if(parser),
        TokenType::KeywordImport => parse_stmt_import_or_expr_import(parser),
        TokenType::KeywordReturn => parse_stmt_return(parser),
        TokenType::KeywordSwitch => parse_stmt_switch(parser),
        TokenType::KeywordThrow => parse_stmt_throw(parser),
        TokenType::KeywordTry => parse_stmt_try(parser),
        TokenType::KeywordWhile => parse_stmt_while(parser),
        TokenType::Semicolon => Ok(Node::new(parser.next()?.loc_take(), Syntax::EmptyStmt {})),
        _ => parse_stmt_expression(parser),
    }
}

pub fn parse_stmt_block(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::BraceOpen)?;
    let mut body: Vec<Node> = Vec::new();
    loop {
        if let Some(end_loc) = parser.consume_if(TokenType::BraceClose)?.match_loc() {
            return Ok(Node::new(start.loc() + end_loc, Syntax::BlockStmt { body }));
        };
        body.push(parse_stmt(parser)?);
    }
}

pub fn parse_stmt_var(parser: &mut Parser) -> TsResult<Node> {
    let declaration = parse_decl_var(parser, VarDeclParseMode::Asi)?;
    Ok(Node::new(
        declaration.loc().clone(),
        Syntax::VarStmt { declaration },
    ))
}

struct BreakOrContinue {
    loc: SourceRange,
    label: Option<SourceRange>,
}

fn parse_stmt_break_or_continue(parser: &mut Parser, t: TokenType) -> TsResult<BreakOrContinue> {
    let mut loc = parser.require(t)?.loc_take();
    let next = parser.peek()?;
    let label = if next.typ() == TokenType::Identifier && !next.preceded_by_line_terminator() {
        // Label.
        parser.consume_peeked();
        loc.extend(next.loc());
        Some(next.loc_take())
    } else if next.typ() == TokenType::Semicolon {
        parser.consume_peeked();
        None
    } else if next.preceded_by_line_terminator() {
        None
    } else {
        return Err(next.error(TsErrorType::ExpectedSyntax("continue label")));
    };
    Ok(BreakOrContinue { loc, label })
}

pub fn parse_stmt_break(parser: &mut Parser) -> TsResult<Node> {
    let stmt = parse_stmt_break_or_continue(parser, TokenType::KeywordBreak)?;
    Ok(Node::new(stmt.loc, Syntax::BreakStmt { label: stmt.label }))
}

pub fn parse_stmt_continue(parser: &mut Parser) -> TsResult<Node> {
    let stmt = parse_stmt_break_or_continue(parser, TokenType::KeywordContinue)?;
    Ok(Node::new(
        stmt.loc,
        Syntax::ContinueStmt { label: stmt.label },
    ))
}

pub fn parse_stmt_export(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordExport)?;
    let cp = parser.checkpoint();
    let t = parser.next()?;
    Ok(match t.typ() {
        TokenType::BraceOpen => {
            let mut names = Vec::<(ImportOrExportName, Option<ImportOrExportName>)>::new();
            loop {
                if parser.consume_if(TokenType::BraceClose)?.is_match() {
                    break;
                };
                let name = parse_import_or_export_name(parser)?;
                let alias = parser
                    .consume_if(TokenType::KeywordAs)?
                    .and_then(|| parse_import_or_export_name(parser))?;
                names.push((name, alias));
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
            Node::new(
                start.loc().clone(),
                Syntax::ExportListStmt {
                    names: ExportNames::Specific(names),
                    from,
                },
            )
        }
        TokenType::Asterisk => {
            parser.require(TokenType::KeywordFrom)?;
            let from = parse_and_normalise_literal_string(parser)?;
            // TODO Loc
            Node::new(
                start.loc().clone(),
                Syntax::ExportListStmt {
                    names: ExportNames::All,
                    from: Some(from),
                },
            )
        }
        TokenType::KeywordDefault => {
            let expression = parse_expr(parser, TokenType::Semicolon)?;
            Node::new(
                start.loc() + expression.loc(),
                Syntax::ExportDefaultStmt { expression },
            )
        }
        TokenType::KeywordLet
        | TokenType::KeywordConst
        | TokenType::KeywordFunction
        | TokenType::KeywordClass => {
            // Reconsume declaration keyword.
            parser.restore_checkpoint(cp);
            let declaration = parse_stmt(parser)?;
            Node::new(
                start.loc() + declaration.loc(),
                Syntax::ExportDeclStmt { declaration },
            )
        }
        _ => return Err(t.error(TsErrorType::ExpectedSyntax("exportable"))),
    })
}

pub fn parse_stmt_expression(parser: &mut Parser) -> TsResult<Node> {
    let mut asi = Asi::can();
    let expression = parse_expr_with_asi(parser, TokenType::Semicolon, &mut asi)?;
    if !asi.did_end_with_asi {
        parser.require(TokenType::Semicolon)?;
    };
    Ok(Node::new(
        expression.loc().clone(),
        Syntax::ExpressionStmt { expression },
    ))
}

pub fn parse_stmt_for(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordFor)?;
    parser.require(TokenType::ParenthesisOpen)?;
    enum LhsRaw {
        Declaration(Node),
        Expression(Node),
        Pattern(Node),
        Empty,
    }
    let lhs_raw = match parser.peek()?.typ() {
        TokenType::KeywordVar | TokenType::KeywordLet | TokenType::KeywordConst => {
            LhsRaw::Declaration(parse_decl_var(parser, VarDeclParseMode::Leftmost)?)
        }
        TokenType::Semicolon => LhsRaw::Empty,
        _ => {
            // A pattern could be reinterpreted as an expression (and vice versa), so we can only try parsing both.
            let checkpoint = parser.checkpoint();
            match if let Ok(node) = parse_pattern(parser) {
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
                    LhsRaw::Expression(parse_expr(parser, TokenType::Semicolon)?)
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
                LhsRaw::Empty => return Err(start.error(TsErrorType::ForLoopHeaderHasNoLhs)),
                LhsRaw::Declaration(node) => match node.stx() {
                    Syntax::VarDecl { declarators, .. } => {
                        if declarators.len() != 1 {
                            return Err(
                                start.error(TsErrorType::ForLoopHeaderHasMultipleDeclarators)
                            );
                        }
                        ForInOfStmtHeaderLhs::Declaration(node)
                    }
                    _ => unreachable!(),
                },
                LhsRaw::Pattern(pat) => ForInOfStmtHeaderLhs::Pattern(pat),
                LhsRaw::Expression(_) => {
                    return Err(start.error(TsErrorType::ForLoopHeaderHasInvalidLhs))
                }
            };
            let rhs = parse_expr(parser, TokenType::ParenthesisClose)?;
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
                    return Err(start.error(TsErrorType::ForLoopHeaderHasInvalidLhs))
                }
            };
            let condition = if parser.consume_if(TokenType::Semicolon)?.is_match() {
                None
            } else {
                let expr = parse_expr(parser, TokenType::Semicolon)?;
                parser.require(TokenType::Semicolon)?;
                Some(expr)
            };
            let post = if parser.consume_if(TokenType::ParenthesisClose)?.is_match() {
                None
            } else {
                let expr = parse_expr(parser, TokenType::ParenthesisClose)?;
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
    let body = parse_stmt(parser)?;
    Ok(Node::new(
        start.loc() + body.loc(),
        Syntax::ForStmt { header, body },
    ))
}

pub fn parse_stmt_if(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordIf)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let test = parse_expr(parser, TokenType::ParenthesisClose)?;
    parser.require(TokenType::ParenthesisClose)?;
    let consequent = parse_stmt(parser)?;
    let alternate = if parser.consume_if(TokenType::KeywordElse)?.is_match() {
        Some(parse_stmt(parser)?)
    } else {
        None
    };
    let end = alternate.as_ref().unwrap_or(&consequent);
    Ok(Node::new(
        start.loc() + end.loc(),
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        },
    ))
}

pub fn parse_stmt_import_or_expr_import(parser: &mut Parser) -> TsResult<Node> {
    let cp = parser.checkpoint();
    let start = parser.require(TokenType::KeywordImport)?;
    if parser.consume_if(TokenType::ParenthesisOpen)?.is_match() {
        parser.restore_checkpoint(cp);
        return parse_stmt_expression(parser);
    };

    let (default, can_have_names) =
        if let Some(default) = parser.consume_if(TokenType::Identifier)?.match_loc() {
            (
                Some(default.clone()),
                parser.consume_if(TokenType::Comma)?.is_match(),
            )
        } else {
            (None, true)
        };
    let names = if !can_have_names {
        None
    } else if parser.consume_if(TokenType::Asterisk)?.is_match() {
        parser.require(TokenType::KeywordAs)?;
        Some(ImportNames::All(
            parser.require(TokenType::Identifier)?.loc().clone(),
        ))
    } else {
        parser.require(TokenType::BraceOpen)?;
        let mut names = Vec::<(ImportOrExportName, Option<SourceRange>)>::new();
        while !parser.consume_if(TokenType::BraceClose)?.is_match() {
            let name = parse_import_or_export_name(parser)?;
            let alias = if parser.consume_if(TokenType::KeywordAs)?.is_match() {
                Some(parser.require(TokenType::Identifier)?.loc().clone())
            } else {
                None
            };
            names.push((name, alias));
            if !parser.consume_if(TokenType::Comma)?.is_match() {
                break;
            };
        }
        parser.require(TokenType::BraceClose)?;
        Some(ImportNames::Specific(names))
    };
    parser.require(TokenType::KeywordFrom)?;
    let module = parse_and_normalise_literal_string(parser)?;
    parser.require(TokenType::Semicolon)?;
    // TODO Loc
    Ok(Node::new(
        start.loc().clone(),
        Syntax::ImportStmt {
            default,
            module,
            names,
        },
    ))
}

pub fn parse_stmt_return(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordReturn)?;
    let mut loc = start.loc().clone();
    let value = if parser.peek()?.preceded_by_line_terminator() {
        // Automatic Semicolon Insertion.
        None
    } else if parser.consume_if(TokenType::Semicolon)?.is_match() {
        None
    } else {
        // Use parse_stmt_expression to handle ASI.
        let value = parse_stmt_expression(parser)?;
        loc.extend(value.loc());
        Some(value)
    };
    Ok(Node::new(loc, Syntax::ReturnStmt { value }))
}

pub fn parse_stmt_throw(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordThrow)?;
    if parser.peek()?.preceded_by_line_terminator() {
        // Illegal under Automatic Semicolon Insertion rules.
        return Err(start.error(TsErrorType::LineTerminatorAfterThrow));
    }
    // Use parse_stmt_expression to handle ASI.
    let value = parse_stmt_expression(parser)?;
    Ok(Node::new(
        start.loc() + value.loc(),
        Syntax::ThrowStmt { value },
    ))
}

pub fn parse_stmt_try(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordTry)?;
    let mut loc = start.loc().clone();
    let wrapped = parse_stmt_block(parser)?;
    let catch = if parser.consume_if(TokenType::KeywordCatch)?.is_match() {
        let parameter = if parser.consume_if(TokenType::ParenthesisOpen)?.is_match() {
            let name = parser.require(TokenType::Identifier)?.loc_take();
            parser.require(TokenType::ParenthesisClose)?;
            Some(name)
        } else {
            None
        };
        let body = parse_stmt_block(parser)?;
        loc.extend(body.loc());
        Some(TryCatch { parameter, body })
    } else {
        None
    };
    let finally = if parser.consume_if(TokenType::KeywordFinally)?.is_match() {
        let body = parse_stmt_block(parser)?;
        loc.extend(body.loc());
        Some(body)
    } else {
        None
    };
    if catch.is_none() && finally.is_none() {
        return Err(start.error(TsErrorType::TryStatementHasNoCatchOrFinally));
    }
    Ok(Node::new(
        loc,
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        },
    ))
}

pub fn parse_stmt_while(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordWhile)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let condition = parse_expr(parser, TokenType::ParenthesisClose)?;
    parser.require(TokenType::ParenthesisClose)?;
    let body = parse_stmt_block(parser)?;
    Ok(Node::new(
        start.loc() + body.loc(),
        Syntax::WhileStmt { condition, body },
    ))
}

pub fn parse_stmt_do_while(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordDo)?;
    let body = parse_stmt_block(parser)?;
    parser.require(TokenType::KeywordWhile)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let condition = parse_expr(parser, TokenType::ParenthesisClose)?;
    let end = parser.require(TokenType::ParenthesisClose)?;
    parser.consume_if(TokenType::Semicolon)?;
    Ok(Node::new(
        start.loc() + end.loc(),
        Syntax::DoWhileStmt { condition, body },
    ))
}

pub fn parse_stmt_switch(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordSwitch)?;
    parser.require(TokenType::ParenthesisOpen)?;
    let test = parse_expr(parser, TokenType::ParenthesisClose)?;
    parser.require(TokenType::ParenthesisClose)?;
    parser.require(TokenType::BraceOpen)?;
    let mut branches = Vec::<SwitchBranch>::new();
    while parser.peek()?.typ() != TokenType::BraceClose {
        let case = if parser.consume_if(TokenType::KeywordCase)?.is_match() {
            Some(parse_expr(parser, TokenType::Colon)?)
        } else {
            parser.require(TokenType::KeywordDefault)?;
            None
        };
        parser.require(TokenType::Colon)?;
        let mut body: Vec<Node> = Vec::new();
        loop {
            match parser.peek()?.typ() {
                TokenType::KeywordCase | TokenType::KeywordDefault | TokenType::BraceClose => break,
                _ => {
                    body.push(parse_stmt(parser)?);
                }
            }
        }
        branches.push(SwitchBranch { case, body });
    }
    let end = parser.require(TokenType::BraceClose)?;
    Ok(Node::new(
        start.loc() + end.loc(),
        Syntax::SwitchStmt { test, branches },
    ))
}
