use crate::ast::VarDeclMode;
use crate::ast::{Syntax::*, VariableDeclarator};
use crate::num::JsNumber;
use crate::parse::decl::{parse_decl_var, VarDeclParseMode};
use crate::util::test::*;

#[test]
fn test_parse_variable_declaration() {
    let mut parser = p("let a = 1, b;");
    assert_eq!(
        parse_decl_var(&mut parser, VarDeclParseMode::Asi),
        Ok(n(VarDecl {
            mode: VarDeclMode::Let,
            declarators: vec![
                VariableDeclarator {
                    pattern: ident_pat("a"),
                    initializer: Some(n(LiteralNumberExpr {
                        value: JsNumber(1.0)
                    })),
                },
                VariableDeclarator {
                    pattern: ident_pat("b"),
                    initializer: None,
                },
            ]
        })),
    );
}
