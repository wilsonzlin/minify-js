use crate::{
    lex::Lexer,
    parse::{expr::parse_expr, parser::Parser},
    serialise::serialise_ast,
    token::TokenType,
    util::test::evaluate_test_input_files,
};
use serde_json::Value;

fn parse_expr_and_serialise(input: Vec<u8>) -> Value {
    let mut parser = Parser::new(Lexer::new(input));
    let scope = parser.create_global_scope();
    let node_id = parse_expr(scope, &mut parser, TokenType::Semicolon).unwrap();
    let (node_map, _) = parser.take();
    serialise_ast(&node_map, node_id)
}

#[test]
fn test_parse_expression() {
    evaluate_test_input_files("parse/tests/expr", |input| parse_expr_and_serialise(input));
}
