use crate::ast::Syntax::*;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_regex() {
    let mut parser = p("/a/g0/ /a/;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: false,
            operator: Division,
            left: n(LiteralRegexExpr {}),
            right: n(LiteralRegexExpr {}),
        })),
    );
}
