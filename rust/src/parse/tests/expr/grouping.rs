use crate::ast::Syntax::*;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_grouping() {
    let mut parser = p("(a, b, c, d && d, e, f, g);");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: true,
            operator: Comma,
            right: n(IdentifierExpr { name: r("g") }),
            left: n(BinaryExpr {
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
                            left: n(IdentifierExpr { name: r("d") }),
                            right: n(IdentifierExpr { name: r("d") }),
                        }),
                        left: n(BinaryExpr {
                            parenthesised: false,
                            operator: Comma,
                            right: n(IdentifierExpr { name: r("c") }),
                            left: n(BinaryExpr {
                                parenthesised: false,
                                operator: Comma,
                                right: n(IdentifierExpr { name: r("b") }),
                                left: n(IdentifierExpr { name: r("a") }),
                            }),
                        }),
                    }),
                }),
            }),
        })),
    );

    let mut parser = p("(a, a ? b : c && d, d);");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: true,
            operator: Comma,
            right: n(IdentifierExpr { name: r("d") }),
            left: n(BinaryExpr {
                parenthesised: false,
                operator: Comma,
                right: n(ConditionalExpr {
                    parenthesised: false,
                    test: n(IdentifierExpr { name: r("a") }),
                    consequent: n(IdentifierExpr { name: r("b") }),
                    alternate: n(BinaryExpr {
                        parenthesised: false,
                        operator: LogicalAnd,
                        left: n(IdentifierExpr { name: r("c") }),
                        right: n(IdentifierExpr { name: r("d") }),
                    }),
                }),
                left: n(IdentifierExpr { name: r("a") }),
            }),
        })),
    );

    let mut parser = p("(a / b) / (c / d);");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: false,
            operator: Division,
            left: n(BinaryExpr {
                parenthesised: true,
                operator: Division,
                left: n(IdentifierExpr { name: r("a") }),
                right: n(IdentifierExpr { name: r("b") }),
            }),
            right: n(BinaryExpr {
                parenthesised: true,
                operator: Division,
                left: n(IdentifierExpr { name: r("c") }),
                right: n(IdentifierExpr { name: r("d") }),
            }),
        })),
    );
}
