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

#[derive(Debug)]
pub enum MinifyError {
    Syntax(SyntaxError),
    IO(io::Error),
}

/// Minifies UTF-8 JavaScript code, represented as an array of bytes.
///
/// # Arguments
///
/// * `source` - A vector of bytes representing the source code to minify.
/// * `output` - Destination to write minified output JavaScript code.
///
/// # Examples
///
/// ```
/// use std::io::BufWriter;
/// use minify_js::minify;
///
/// let mut code: &[u8] = b"let x = 1;";
/// let mut out = BufWriter::new(Vec::new());
/// minify(code.to_vec(), &mut out).unwrap();
/// assert_eq!(out.get_ref().as_slice(), b"let x=1;");
/// ```
pub fn minify<T: Write>(source: Vec<u8>, output: &mut T) -> Result<(), MinifyError> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let parsed = parse_top_level(&mut parser).map_err(|err| MinifyError::Syntax(err))?;
    emit_js(output, &parsed).map_err(|err| MinifyError::IO(err))?;
    Ok(())
}
