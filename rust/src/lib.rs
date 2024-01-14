use emit::emit_js;
use minify::minify_js;
use parse_js::ast::Node;
use parse_js::parse;

mod emit;
mod minify;

pub use symbol_js::TopLevelMode;
use symbol_js::compute_symbols;

/// Emits UTF-8 JavaScript code from a parsed AST in a minified way. This allows custom introspections and transforms on the tree before emitting it to code.
///
/// # Arguments
///
/// * `node` - The root node from the parsed AST.
/// * `output` - Destination to write output JavaScript code.
pub fn emit(node: &Node, output: &mut Vec<u8>) -> () {
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
/// let mut out = Vec::new();
/// minify(TopLevelMode::Global, code, &mut out).unwrap();
/// assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
/// ```
pub fn minify(
  top_level_mode: TopLevelMode,
  source: &[u8],
  output: &mut Vec<u8>,
) -> Result<(), SyntaxError> {
  let mut parsed = parse(source)?;
  compute_symbols(&mut parsed, top_level_mode);
  let minified = minify_js(&parsed);
  emit(&minified, output);
  Ok(())
}
