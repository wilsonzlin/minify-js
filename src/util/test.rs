use std::path::PathBuf;

use crate::ast::{Node, Syntax};
use crate::lex::Lexer;
use crate::parse::parser::Parser;
use crate::source::{Source, SourceRange};

pub fn ident_pat(name: &str) -> Node {
    let range = r(name);
    Node::new(
        range.clone(),
        Syntax::IdentifierPattern {
            name: range.clone(),
        },
    )
}

pub fn n(stx: Syntax) -> Node {
    Node::new(
        SourceRange {
            source: Source::new(PathBuf::from("Dummy location"), vec![]),
            start: 0,
            end: 0,
        },
        stx,
    )
}

pub fn p(code: &str) -> Parser {
    Parser::new(Lexer::new(
        PathBuf::from("Test input"),
        code.as_bytes().to_vec(),
    ))
}

pub fn r(code: &str) -> SourceRange {
    SourceRange {
        source: Source::new(
            PathBuf::from("Standalone source range"),
            code.as_bytes().to_vec(),
        ),
        start: 0,
        end: code.len(),
    }
}

pub fn s(str: &str) -> String {
    str.to_string()
}
