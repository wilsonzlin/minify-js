use std::{
    io::{self, Read},
    path::PathBuf,
};

use emit::emit_js;
use lex::Lexer;
use parse::{parser::Parser, toplevel::parse_top_level};

mod ast;
mod char;
mod emit;
mod error;
mod lex;
mod num;
mod operator;
mod parse;
mod source;
mod symbol;
mod token;
mod util;

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).expect("read stdin");
    let lexer = Lexer::new(PathBuf::from("input"), input);
    let mut parser = Parser::new(lexer);
    let parsed = parse_top_level(&mut parser).expect("parse");
    emit_js(&mut io::stdout(), &parsed).expect("emit");
}
