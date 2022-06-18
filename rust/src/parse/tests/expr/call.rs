use crate::ast::Syntax::*;
use crate::num::JsNumber;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn parse_expr_call() {
    let mut parser = p("ab(1, 2, 3,);");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(CallExpr {
            parenthesised: false,
            callee: n(IdentifierExpr { name: r("ab") }),
            arguments: vec![
                n(LiteralNumberExpr {
                    value: JsNumber(1.0)
                }),
                n(LiteralNumberExpr {
                    value: JsNumber(2.0)
                }),
                n(LiteralNumberExpr {
                    value: JsNumber(3.0)
                }),
            ],
        })),
    );
}
