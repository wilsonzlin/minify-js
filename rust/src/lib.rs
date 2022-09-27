use emit::emit_js;
use error::SyntaxError;
use lex::Lexer;
use minify::minify_js;
use parse::{parser::Parser, toplevel::parse_top_level};
use std::io::{self, Write};

mod ast;
mod char;
mod emit;
mod error;
mod lex;
mod minify;
mod num;
mod operator;
mod parse;
#[cfg(test)]
mod serialise;
mod source;
mod symbol;
mod token;
mod update;
mod util;

pub use parse::toplevel::TopLevelMode;

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
/// use minify_js::minify;
///
/// let mut code: &[u8] = b"const main = () => { let my_first_variable = 1; };";
/// let mut out = Vec::new();
/// minify(code.to_vec(), &mut out).unwrap();
/// assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
/// ```
pub fn minify<T: Write>(
    top_level_mode: TopLevelMode,
    source: Vec<u8>,
    output: &mut T,
) -> Result<(), MinifyError> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let parsed =
        parse_top_level(&mut parser, top_level_mode).map_err(|err| MinifyError::Syntax(err))?;
    let (mut node_map, mut scope_map) = parser.take();
    minify_js(
        &mut scope_map,
        &mut node_map,
        parsed.top_level_scope_id,
        parsed.top_level_node_id,
    );
    emit_js(output, &node_map, parsed.top_level_node_id).map_err(|err| MinifyError::IO(err))?;
    Ok(())
}
