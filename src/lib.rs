use std::io::{self, Write};

use emit::emit_js;
use error::SyntaxError;
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

pub enum MinifyError {
    Syntax(SyntaxError),
    IO(io::Error),
}

pub fn minify<T: Write>(source: Vec<u8>, output: &mut T) -> Result<(), MinifyError> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let parsed = parse_top_level(&mut parser).map_err(|err| MinifyError::Syntax(err))?;
    emit_js(output, &parsed).map_err(|err| MinifyError::IO(err))?;
    Ok(())
}
