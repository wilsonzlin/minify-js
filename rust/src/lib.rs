use emit::emit_js;
use minify::minify_js;
use parse_js::ast::Node;
use parse_js::error::SyntaxError;
use parse_js::parse;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io::Write;
use std::io::{self};

mod emit;
mod minify;

pub use parse_js::parse::toplevel::TopLevelMode;
pub use parse_js::session::Session;

#[derive(Debug)]
pub enum MinifyError<'a> {
  Syntax(SyntaxError<'a>),
  IO(io::Error),
}

impl<'a> Display for MinifyError<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      MinifyError::Syntax(syntax) => {
        write!(f, "Syntax Error: {:?}", syntax)
      }
      MinifyError::IO(io) => {
        write!(f, "IO Error: {}", io)
      }
    }
  }
}

impl<'a> Error for MinifyError<'a> {}

/// Emits UTF-8 JavaScript code from a parsed AST in a minified way. This allows custom introspections and transforms on the tree before emitting it to code.
///
/// # Arguments
///
/// * `node` - The root node from the parsed AST.
/// * `output` - Destination to write output JavaScript code.
pub fn emit<'a, T: Write>(node: Node<'a>, output: &mut T) -> Result<(), MinifyError<'a>> {
  emit_js(output, node).map_err(|err| MinifyError::IO(err))
}

/// Minifies UTF-8 JavaScript code, represented as an array of bytes.
///
/// # Arguments
///
/// * `session` - Session to use as backing arena memory. Can be reused across calls and cleared at any time allowed by the Rust lifetime checker.
/// * `top_level_mode` - How to parse the provided code.
/// * `source` - A vector of bytes representing the source code to minify.
/// * `output` - Destination to write minified output JavaScript code.
///
/// # Examples
///
/// ```
/// use minify_js::{TopLevelMode, minify};
///
/// let mut code: &[u8] = b"const main = () => { let my_first_variable = 1; };";
/// let mut out = Vec::new();
/// minify(TopLevelMode::Global, code.to_vec(), &mut out).unwrap();
/// assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
/// ```
pub fn minify<'a, T: Write>(
  session: &'a Session,
  top_level_mode: TopLevelMode,
  source: &'a [u8],
  output: &mut T,
) -> Result<(), MinifyError<'a>> {
  let parsed = parse(session, source, top_level_mode).map_err(|err| MinifyError::Syntax(err))?;
  minify_js(session, parsed);
  emit(parsed, output)?;
  Ok(())
}
