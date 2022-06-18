use crate::ast::Syntax::*;
use crate::num::JsNumber;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_comma() {
    let mut parser = p("1,true,'';");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: false,
            operator: Comma,
            left: n(BinaryExpr {
                parenthesised: false,
                operator: Comma,
                left: n(LiteralNumberExpr {
                    value: JsNumber(1.0)
                }),
                right: n(LiteralBooleanExpr { value: true }),
            }),
            right: n(LiteralStringExpr { value: s("") }),
        })),
    );
    let mut parser = p("a, b, c && d, e, f;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: false,
            operator: Comma,
            right: n(IdentifierExpr { name: r("f") }),
            left: n(BinaryExpr {
                parenthesised: false,
                operator: Comma,
                right: n(IdentifierExpr { name: r("e") }),
                left: n(BinaryExpr {
                    parenthesised: false,
                    operator: Comma,
                    right: n(BinaryExpr {
                        parenthesised: false,
                        operator: LogicalAnd,
                        left: n(IdentifierExpr { name: r("c") }),
                        right: n(IdentifierExpr { name: r("d") }),
                    }),
                    left: n(BinaryExpr {
                        parenthesised: false,
                        operator: Comma,
                        right: n(IdentifierExpr { name: r("b") }),
                        left: n(IdentifierExpr { name: r("a") }),
                    }),
                }),
            }),
        })),
    );
}
