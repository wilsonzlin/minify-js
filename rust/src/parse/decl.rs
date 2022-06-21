use crate::ast::{ClassMember, NodeId, Syntax, VarDeclMode, VariableDeclarator};
use crate::error::{SyntaxErrorType, SyntaxResult};
use crate::parse::parser::Parser;
use crate::parse::pattern::parse_pattern;
use crate::parse::signature::parse_signature_function;
use crate::parse::stmt::parse_stmt_block;
use crate::symbol::{ScopeId, ScopeType, Symbol};
use crate::token::TokenType;

use super::class_or_object::{parse_class_or_object_member, ParseClassOrObjectMemberResult};
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
    let start = parser.require(TokenType::KeywordFunction)?.loc().clone();
    let generator = parser.consume_if(TokenType::Asterisk)?.is_match();
    let name = match parser.next()? {
        t if is_valid_pattern_identifier(t.typ(), syntax) => t,
        t => return Err(t.error(SyntaxErrorType::ExpectedSyntax("function name"))),
    }
    .loc()
    .clone();
    let name_node = parser.create_node(
        fn_scope,
        name.clone(),
        Syntax::ClassOrFunctionName { name: name.clone() },
    );
    if let Some(closure_id) = parser[scope].self_or_ancestor_closure() {
        parser[closure_id].add_symbol(name.clone(), Symbol::new(name_node))?;
    }
    let signature = parse_signature_function(fn_scope, parser, syntax)?;
    let body = parse_stmt_block(fn_scope, parser, syntax)?;
    Ok(parser.create_node(
        scope,
        &start + parser[body].loc(),
        Syntax::FunctionDecl {
            generator,
            name: name_node,
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
    let name = parser.require(TokenType::Identifier)?.loc().clone();
    let name_node = parser.create_node(
        scope,
        name.clone(),
        Syntax::ClassOrFunctionName { name: name.clone() },
    );
    // Unlike functions, classes are scoped to their block.
    parser[scope].add_block_symbol(name.clone(), Symbol::new(name_node))?;
    let extends = if parser.consume_if(TokenType::KeywordExtends)?.is_match() {
        Some(parse_expr(scope, parser, TokenType::BraceOpen, syntax)?)
    } else {
        None
    };
    parser.require(TokenType::BraceOpen)?;
    let mut members = Vec::<ClassMember>::new();
    while parser.peek()?.typ() != TokenType::BraceClose {
        // `static` must always come first if present.
        let statik = parser.consume_if(TokenType::KeywordStatic)?.is_match();
        let ParseClassOrObjectMemberResult { key, value } = parse_class_or_object_member(
            scope,
            parser,
            TokenType::Equals,
            TokenType::Semicolon,
            &mut Asi::can(),
            syntax,
        )?;
        parser.consume_if(TokenType::Semicolon)?;
        members.push(ClassMember { key, statik, value });
    }
    let end = parser.require(TokenType::BraceClose)?;
    Ok(parser.create_node(
        scope,
        &start + end.loc(),
        Syntax::ClassDecl {
            name: name_node,
            extends,
            members,
        },
    ))
}
