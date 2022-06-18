use crate::ast::Syntax::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_identifier() {
    let mut parser = p("abc;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(IdentifierExpr { name: r("abc") })),
    );
}
