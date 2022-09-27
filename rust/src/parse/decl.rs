use crate::ast::{NodeId, Syntax, VarDeclMode, VariableDeclarator};
use crate::error::{SyntaxErrorType, SyntaxResult};
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::parse::signature::parse_signature_function;
use crate::parse::stmt::parse_stmt_block;
use crate::symbol::{ScopeId, ScopeType, Symbol};
use crate::token::TokenType;

use super::class_or_object::{parse_class_body, ParseClassBodyResult};
use super::expr::{parse_expr, parse_expr_until_either_with_asi, Asi};
use super::pattern::{is_valid_pattern_identifier, ParsePatternAction, ParsePatternSyntax};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDeclParseMode {
    // Standard parsing mode for var/let/const statement.
    Asi,
    // Parse as many valid declarators as possible, then break before the first invalid token (i.e. not a comma). Used by for-loop parser.
    Leftmost,
}

pub fn parse_decl_var(
    scope: ScopeId,
    parser: &mut Parser,
    parse_mode: VarDeclParseMode,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let t = parser.next()?;
    let mode = match t.typ() {
        TokenType::KeywordLet => VarDeclMode::Let,
        TokenType::KeywordConst => VarDeclMode::Const,
        TokenType::KeywordVar => VarDeclMode::Var,
        _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("variable declaration"))),
    };
    let mut declarators = vec![];
    let mut loc = t.loc().clone();
    loop {
        let pattern = parse_pattern(
            scope,
            parser,
            match mode {
                VarDeclMode::Var => ParsePatternAction::AddToClosureScope,
                _ => ParsePatternAction::AddToBlockScope,
            },
            syntax,
        )?;
        loc.extend(parser[pattern].loc());
        let mut asi = match parse_mode {
            VarDeclParseMode::Asi => Asi::can(),
            VarDeclParseMode::Leftmost => Asi::no(),
        };
        let initializer = if parser.consume_if(TokenType::Equals)?.is_match() {
            let expr = parse_expr_until_either_with_asi(
                scope,
                parser,
                TokenType::Semicolon,
                TokenType::Comma,
                &mut asi,
                syntax,
            )?;
            loc.extend(parser[expr].loc());
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
                let t = parser.peek()?;
                if t.preceded_by_line_terminator() && t.typ() != TokenType::Comma {
                    break;
                };
                parser.require(TokenType::Comma)?;
            }
            VarDeclParseMode::Leftmost => {
                if !parser.consume_if(TokenType::Comma)?.is_match() {
                    break;
                }
            }
        }
    }
    Ok(parser.create_node(scope, loc, Syntax::VarDecl { mode, declarators }))
}

pub fn parse_decl_function(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let fn_scope = parser.create_child_scope(scope, ScopeType::Closure);
    let is_async = parser.consume_if(TokenType::KeywordAsync)?.is_match();
    let start = parser.require(TokenType::KeywordFunction)?.loc().clone();
    let generator = parser.consume_if(TokenType::Asterisk)?.is_match();
    // WARNING: The name belongs in the containing scope, not the function's scope.
    // For example, `function a() { let a = 1; }` is legal.
    // The name can only be omitted in default exports.
    let name = match parser
        .consume_if_pred(|t| is_valid_pattern_identifier(t.typ(), syntax))?
        .match_loc_take()
    {
        Some(name) => {
            let name_node = parser.create_node(
                scope,
                name.clone(),
                Syntax::ClassOrFunctionName { name: name.clone() },
            );
            if let Some(closure_id) = parser[scope].self_or_ancestor_closure() {
                parser[closure_id].add_symbol(name.clone(), Symbol::new(name_node))?;
            };
            Some(name_node)
        }
        _ => None,
    };
    let signature = parse_signature_function(fn_scope, parser, syntax)?;
    let body = parse_stmt_block(fn_scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        &start + parser[body].loc(),
        Syntax::FunctionDecl {
            is_async,
            generator,
            name,
            signature,
            body,
        },
    ))
}

pub fn parse_decl_class(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<NodeId> {
    let start = parser.require(TokenType::KeywordClass)?.loc().clone();
    // Names can be omitted only in default exports.
    let name = match parser
        .consume_if_pred(|t| is_valid_pattern_identifier(t.typ(), syntax))?
        .match_loc_take()
    {
        Some(name) => {
            let name_node = parser.create_node(
                scope,
                name.clone(),
                Syntax::ClassOrFunctionName { name: name.clone() },
            );
            parser[scope].add_block_symbol(name.clone(), Symbol::new(name_node))?;
            Some(name_node)
        }
        None => None,
    };
    // Unlike functions, classes are scoped to their block.
    let extends = if parser.consume_if(TokenType::KeywordExtends)?.is_match() {
        Some(parse_expr(scope, parser, TokenType::BraceOpen, syntax)?)
    } else {
        None
    };
    let ParseClassBodyResult { end, members } = parse_class_body(scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        &start + &end,
        Syntax::ClassDecl {
            name,
            extends,
            members,
        },
    ))
}
