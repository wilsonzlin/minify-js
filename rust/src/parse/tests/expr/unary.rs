use crate::ast::Syntax::*;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_unary() {
    let mut parser = p("!!++~abc?.(def).(!ghi);");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(UnaryExpr {
            parenthesised: false,
            operator: LogicalNot,
            argument: n(UnaryExpr {
                parenthesised: false,
                operator: LogicalNot,
                argument: n(UnaryExpr {
                    parenthesised: false,
                    operator: PrefixIncrement,
                    argument: n(UnaryExpr {
                        parenthesised: false,
                        operator: BitwiseNot,
                        argument: n(BinaryExpr {
                            parenthesised: false,
                            operator: MemberAccess,
                            right: n(UnaryExpr {
                                parenthesised: true,
                                operator: LogicalNot,
                                argument: n(IdentifierExpr { name: r("ghi") }),
                            }),
                            left: n(BinaryExpr {
                                parenthesised: false,
                                operator: OptionalChaining,
                                left: n(IdentifierExpr { name: r("abc") }),
                                right: n(IdentifierExpr { name: r("def") }),
                            }),
                        }),
                    }),
                }),
            }),
        })),
    );
}
