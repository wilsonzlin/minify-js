use crate::ast::{Node, Syntax, VarDeclMode, VariableDeclarator};
use crate::error::{TsErrorType, TsResult};
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::parse::signature::parse_signature_function;
use crate::parse::stmt::parse_stmt_block;
use crate::token::TokenType;

use super::expr::{parse_expr_until_either_with_asi, Asi};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDeclParseMode {
    // Standard parsing mode for var/let/const statement.
    Asi,
    // Parse as many valid declarators as possible, then break before the first invalid token (i.e. not a comma). Used by for-loop parser.
    Leftmost,
}

pub fn parse_decl_var(parser: &mut Parser, parse_mode: VarDeclParseMode) -> TsResult<Node> {
    let t = parser.next()?;
    let mode = match t.typ() {
        TokenType::KeywordLet => VarDeclMode::Let,
        TokenType::KeywordConst => VarDeclMode::Const,
        TokenType::KeywordVar => VarDeclMode::Var,
        _ => return Err(t.error(TsErrorType::ExpectedSyntax("variable declaration"))),
    };
    let mut declarators = vec![];
    let mut loc = t.loc().clone();
    loop {
        let pattern = parse_pattern(parser)?;
        loc.extend(pattern.loc());
        let mut asi = match parse_mode {
            VarDeclParseMode::Asi => Asi::can(),
            VarDeclParseMode::Leftmost => Asi::no(),
        };
        let initializer = if parser.consume_if(TokenType::Equals)?.is_match() {
            let expr = parse_expr_until_either_with_asi(
                parser,
                TokenType::Semicolon,
                TokenType::Comma,
                &mut asi,
            )?;
            loc.extend(expr.loc());
            Some(expr)
        } else {
            None
        };
        declarators.push(VariableDeclarator {
            pattern,
            initializer,
        });
        match parse_mode {
            VarDeclParseMode::Asi => {
                if parser.consume_if(TokenType::Semicolon)?.is_match() || asi.did_end_with_asi {
                    break;
                }
                parser.require(TokenType::Comma)?;
            }
            VarDeclParseMode::Leftmost => {
                if !parser.consume_if(TokenType::Comma)?.is_match() {
                    break;
                }
            }
        }
    }
    Ok(Node::new(loc, Syntax::VarDecl { mode, declarators }))
}

pub fn parse_decl_function(parser: &mut Parser) -> TsResult<Node> {
    let start = parser.require(TokenType::KeywordFunction)?.loc().clone();
    let name = parser.require(TokenType::Identifier)?.loc().clone();
    let signature = parse_signature_function(parser)?;
    let body = parse_stmt_block(parser)?;
    Ok(Node::new(
        &start + body.loc(),
        Syntax::FunctionDecl {
            name,
            signature,
            body,
        },
    ))
}
