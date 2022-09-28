use crate::{
    ast::{ClassMember, ClassOrObjectMemberKey, ClassOrObjectMemberValue},
    error::SyntaxResult,
    lex::KEYWORDS_MAPPING,
    source::SourceRange,
    symbol::{ScopeId, ScopeType},
    token::TokenType,
};

use super::{
    expr::{parse_expr, parse_expr_until_either_with_asi, Asi},
    parser::Parser,
    pattern::{is_valid_pattern_identifier, parse_pattern, ParsePatternAction, ParsePatternSyntax},
    signature::parse_signature_function,
    stmt::parse_stmt_block,
};

pub struct ParseClassBodyResult {
    pub members: Vec<ClassMember>,
    pub end: SourceRange,
}

pub fn parse_class_body(
    scope: ScopeId,
    parser: &mut Parser,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<ParseClassBodyResult> {
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
    let end = parser.require(TokenType::BraceClose)?.loc_take();
    Ok(ParseClassBodyResult { members, end })
}

pub struct ParseClassOrObjectMemberResult {
    pub key: ClassOrObjectMemberKey,
    pub value: ClassOrObjectMemberValue,
}

// It's strictly one of these:
// <key> [ '=' <expr> ]? [ <asi> | ';' ]
// async? '*'? <key> '(' ...
// [ get | set ] <key> '(' ...
// where <key> = <ident> | <keyword> | <str> | <num> | '[' <expr> ']'
pub fn parse_class_or_object_member(
    scope: ScopeId,
    parser: &mut Parser,
    value_delimiter: TokenType,
    statement_delimiter: TokenType,
    property_initialiser_asi: &mut Asi,
    syntax: &ParsePatternSyntax,
) -> SyntaxResult<ParseClassOrObjectMemberResult> {
    let checkpoint = parser.checkpoint();
    let mut is_getter = false;
    let mut is_setter = false;
    let mut is_async = false;
    if parser.consume_if(TokenType::KeywordGet)?.is_match() {
        is_getter = true;
    } else if parser.consume_if(TokenType::KeywordSet)?.is_match() {
        is_setter = true;
    } else if parser.consume_if(TokenType::KeywordAsync)?.is_match() {
        is_async = true;
    }
    if is_getter || is_setter || is_async {
        let next_tok = parser.peek()?.typ();
        if next_tok == value_delimiter || next_tok == TokenType::ParenthesisOpen {
            // Not actually getter/setter, just using `get`/`set` as property name.
            parser.restore_checkpoint(checkpoint);
            is_getter = false;
            is_setter = false;
            is_async = false;
        };
    }
    let is_generator = parser.consume_if(TokenType::Asterisk)?.is_match();
    let key = if parser.consume_if(TokenType::BracketOpen)?.is_match() {
        let key = ClassOrObjectMemberKey::Computed(parse_expr(
            scope,
            parser,
            TokenType::BracketClose,
            syntax,
        )?);
        parser.require(TokenType::BracketClose)?;
        key
    } else {
        let loc = if let Some(str) = parser
            .consume_if(TokenType::LiteralString)?
            .match_loc_take()
        {
            // TODO Do we need to remove quotes and/or decode?
            str
        } else if let Some(num) = parser
            .consume_if(TokenType::LiteralNumber)?
            .match_loc_take()
        {
            // TODO Do we need to normalise?
            num
        } else if let Some(loc) = parser
            .consume_if(TokenType::PrivateMember)?
            .match_loc_take()
        {
            loc
        } else if let Some(loc) = parser
            .consume_if_pred(|t| is_valid_pattern_identifier(t.typ(), syntax))?
            .match_loc_take()
        {
            loc
        } else {
            parser
                .require_predicate(
                    |t| KEYWORDS_MAPPING.contains_key(&t),
                    "keyword or identifier",
                )?
                .loc_take()
        };
        ClassOrObjectMemberKey::Direct(loc)
    };
    // Check is_generator/is_async first so that we don't have to check that they're false in every other branch.
    let value = if is_generator || is_async || parser.peek()?.typ() == TokenType::ParenthesisOpen {
        let signature = parse_signature_function(scope, parser, syntax)?;
        ClassOrObjectMemberValue::Method {
            is_async,
            generator: is_generator,
            signature,
            body: parse_stmt_block(scope, parser, syntax)?,
        }
    } else if is_getter {
        parser.require(TokenType::ParenthesisOpen)?;
        parser.require(TokenType::ParenthesisClose)?;
        ClassOrObjectMemberValue::Getter {
            body: parse_stmt_block(scope, parser, syntax)?,
        }
    } else if is_setter {
        let setter_scope = parser.create_child_scope(scope, ScopeType::Closure);
        parser.require(TokenType::ParenthesisOpen)?;
        let parameter = parse_pattern(
            setter_scope,
            parser,
            ParsePatternAction::AddToClosureScope,
            syntax,
        )?;
        parser.require(TokenType::ParenthesisClose)?;
        ClassOrObjectMemberValue::Setter {
            parameter,
            body: parse_stmt_block(setter_scope, parser, syntax)?,
        }
    } else if match key {
        ClassOrObjectMemberKey::Direct(_) => match parser.peek()? {
            // Given `class A {1}`, `"1" in new A`.
            t if t.typ() == TokenType::BraceClose => true,
            // Given `class A {1;}`, `"1" in new A`.
            t if t.typ() == statement_delimiter => true,
            // Given `class A {1\n2}`, `"2" in new A`.
            t if property_initialiser_asi.can_end_with_asi && t.preceded_by_line_terminator() => {
                true
            }
            _ => false,
        },
        _ => false,
    } {
        ClassOrObjectMemberValue::Property { initializer: None }
    } else {
        parser.require(value_delimiter)?;
        let value = parse_expr_until_either_with_asi(
            scope,
            parser,
            statement_delimiter,
            TokenType::BraceClose,
            property_initialiser_asi,
            syntax,
        )?;
        ClassOrObjectMemberValue::Property {
            initializer: Some(value),
        }
    };
    Ok(ParseClassOrObjectMemberResult { key, value })
}
