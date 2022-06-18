use crate::ast::Syntax::*;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_new() {
    let mut parser = p("+new abc();");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(UnaryExpr {
            parenthesised: false,
            operator: UnaryPlus,
            argument: n(UnaryExpr {
                parenthesised: false,
                operator: New,
                argument: n(CallExpr {
                    parenthesised: false,
                    callee: n(IdentifierExpr { name: r("abc") }),
                    arguments: vec![],
                }),
            }),
        })),
    );
    let mut parser = p("+new abc?.def;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(UnaryExpr {
            parenthesised: false,
            operator: UnaryPlus,
            argument: n(UnaryExpr {
                parenthesised: false,
                operator: New,
                argument: n(BinaryExpr {
                    parenthesised: false,
                    operator: OptionalChaining,
                    left: n(IdentifierExpr { name: r("abc") }),
                    right: n(IdentifierExpr { name: r("def") }),
                }),
            }),
        })),
    );
    let mut parser = p("+new abc?.def();");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(UnaryExpr {
            parenthesised: false,
            operator: UnaryPlus,
            argument: n(UnaryExpr {
                parenthesised: false,
                operator: New,
                argument: n(CallExpr {
                    parenthesised: false,
                    callee: n(BinaryExpr {
                        parenthesised: false,
                        operator: OptionalChaining,
                        left: n(IdentifierExpr { name: r("abc") }),
                        right: n(IdentifierExpr { name: r("def") }),
                    }),
                    arguments: vec![],
                }),
            }),
        })),
    );
}
