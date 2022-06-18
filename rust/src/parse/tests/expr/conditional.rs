use crate::ast::Syntax::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_conditional() {
    let mut parser = p("a ? b : c;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(ConditionalExpr {
            parenthesised: false,
            test: n(IdentifierExpr { name: r("a") }),
            consequent: n(IdentifierExpr { name: r("b") }),
            alternate: n(IdentifierExpr { name: r("c") }),
        })),
    );
}
