use crate::ast::Syntax::*;
use crate::num::JsNumber;
use crate::operator::OperatorName::*;
use crate::parse::expr::parse_expr;
use crate::token::TokenType;
use crate::util::test::*;

#[test]
fn test_parse_expression_arrow_function() {
    let mut parser = p("(a, b, c) => {};");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(ArrowFunctionExpr {
            signature: n(FunctionSignature {
                parameters: vec![
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("a"),
                        default_value: None
                    }),
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("b"),
                        default_value: None
                    }),
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("c"),
                        default_value: None
                    }),
                ],
            }),
            body: n(BlockStmt { body: vec![] }),
        })),
    );
    let mut parser = p("(a, b, c) => (x) => 1 ? ((y) => 2)() ? 3 : 4 : 5;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(ArrowFunctionExpr {
            signature: n(FunctionSignature {
                parameters: vec![
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("a"),
                        default_value: None
                    }),
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("b"),
                        default_value: None
                    }),
                    n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("c"),
                        default_value: None
                    }),
                ],
            }),
            body: n(ArrowFunctionExpr {
                signature: n(FunctionSignature {
                    parameters: vec![n(ParamDecl {
                        rest: false,
                        pattern: ident_pat("x"),
                        default_value: None
                    }),],
                }),
                body: n(ConditionalExpr {
                    parenthesised: false,
                    test: n(LiteralNumberExpr {
                        value: JsNumber(1.0)
                    }),
                    consequent: n(ConditionalExpr {
                        parenthesised: false,
                        test: n(CallExpr {
                            parenthesised: false,
                            callee: n(ArrowFunctionExpr {
                                signature: n(FunctionSignature {
                                    parameters: vec![n(ParamDecl {
                                        rest: false,
                                        pattern: ident_pat("y"),
                                        default_value: None
                                    }),],
                                }),
                                body: n(LiteralNumberExpr {
                                    value: JsNumber(2.0)
                                }),
                            }),
                            arguments: vec![],
                        }),
                        consequent: n(LiteralNumberExpr {
                            value: JsNumber(3.0)
                        }),
                        alternate: n(LiteralNumberExpr {
                            value: JsNumber(4.0)
                        }),
                    }),
                    alternate: n(LiteralNumberExpr {
                        value: JsNumber(5.0)
                    }),
                }),
            }),
        })),
    );
    let mut parser = p("x = true ? (x) => x ? x : x : 5;");
    assert_eq!(
        parse_expr(&mut parser, TokenType::Semicolon),
        Ok(n(BinaryExpr {
            parenthesised: false,
            operator: Assignment,
            left: n(IdentifierExpr { name: r("x") }),
            right: n(ConditionalExpr {
                parenthesised: false,
                test: n(LiteralBooleanExpr { value: true }),
                consequent: n(ArrowFunctionExpr {
                    signature: n(FunctionSignature {
                        parameters: vec![n(ParamDecl {
                            rest: false,
                            pattern: ident_pat("x"),
                            default_value: None
                        }),],
                    }),
                    body: n(ConditionalExpr {
                        parenthesised: false,
                        test: n(IdentifierExpr { name: r("x") }),
                        consequent: n(IdentifierExpr { name: r("x") }),
                        alternate: n(IdentifierExpr { name: r("x") }),
                    }),
                }),
                alternate: n(LiteralNumberExpr {
                    value: JsNumber(5.0)
                }),
            }),
        })),
    );
}
