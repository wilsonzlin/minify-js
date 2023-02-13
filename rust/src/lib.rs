use emit::emit_js;
use minify::minify_js;
use parse_js::ast::Node;
use parse_js::parse;

mod emit;
mod minify;

pub use parse_js::error::SyntaxError;
pub use parse_js::parse::toplevel::TopLevelMode;
pub use parse_js::session::Session;

/// Emits UTF-8 JavaScript code from a parsed AST in a minified way. This allows custom introspections and transforms on the tree before emitting it to code.
///
/// # Arguments
///
/// * `node` - The root node from the parsed AST.
/// * `output` - Destination to write output JavaScript code.
pub fn emit<'a>(node: Node<'a>, output: &mut Vec<u8>) -> () {
  emit_js(output, node);
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
/// use minify_js::{Session, TopLevelMode, minify};
///
/// let mut code: &[u8] = b"const main = () => { let my_first_variable = 1; };";
/// let session = Session::new();
/// let mut out = Vec::new();
/// minify(&session, TopLevelMode::Global, code, &mut out).unwrap();
/// assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
/// ```
pub fn minify<'a>(
  session: &'a Session,
  top_level_mode: TopLevelMode,
  source: &'a [u8],
  output: &mut Vec<u8>,
) -> Result<(), SyntaxError<'a>> {
  let parsed = parse(session, source, top_level_mode)?;
  minify_js(session, parsed);
  emit(parsed, output);
  Ok(())
}
