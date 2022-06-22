use crate::{
    lex::Lexer,
    parse::{parser::Parser, pattern::ParsePatternSyntax, stmt::parse_stmt},
    serialise::serialise_ast,
    util::test::evaluate_test_input_files,
};
use serde_json::Value;

fn parse_stmt_and_serialise(input: Vec<u8>) -> Value {
    let mut parser = Parser::new(Lexer::new(input));
    let scope = parser.create_global_scope();
    let node_id = parse_stmt(
        scope,
        &mut parser,
        &ParsePatternSyntax {
            await_allowed: true,
            yield_allowed: true,
        },
    )
    .unwrap();
    let (node_map, _) = parser.take();
    serialise_ast(&node_map, node_id)
}

#[test]
fn test_parse_expression() {
    evaluate_test_input_files("parse/tests/stmt", |input| parse_stmt_and_serialise(input));
}
