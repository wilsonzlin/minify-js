use emit::emit_js;
use minify::minify_js;
use parse_js::ast::{NodeId, NodeMap};
use parse_js::error::SyntaxError;
use parse_js::parse;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::{self, Write};

mod emit;
mod minify;

pub use parse_js::parse::toplevel::TopLevelMode;

#[derive(Debug)]
pub enum MinifyError {
    Syntax(SyntaxError),
    IO(io::Error),
}

impl Display for MinifyError {
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

impl Error for MinifyError {}

/// Emits UTF-8 JavaScript code from a parsed AST in a minified way. This allows custom introspections and transforms on the tree before emitting it to code.
///
/// # Arguments
///
/// * `node_map` - The NodeMap from the parsed AST.
/// * `top_level_node_id` - The ID of the root node from the parsed AST.
/// * `output` - Destination to write output JavaScript code.
pub fn emit<T: Write>(
    node_map: &NodeMap,
    top_level_node_id: NodeId,
    output: &mut T,
) -> Result<(), MinifyError> {
    emit_js(output, node_map, top_level_node_id).map_err(|err| MinifyError::IO(err))
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
/// use minify_js::{TopLevelMode, minify};
///
/// let mut code: &[u8] = b"const main = () => { let my_first_variable = 1; };";
/// let mut out = Vec::new();
/// minify(TopLevelMode::Global, code.to_vec(), &mut out).unwrap();
/// assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
/// ```
pub fn minify<T: Write>(
    top_level_mode: TopLevelMode,
    source: Vec<u8>,
    output: &mut T,
) -> Result<(), MinifyError> {
    let mut parsed = parse(source, top_level_mode).map_err(|err| MinifyError::Syntax(err))?;
    minify_js(
        &mut parsed.scope_map,
        &mut parsed.node_map,
        parsed.top_level_scope_id,
        parsed.top_level_node_id,
    );
    emit(&parsed.node_map, parsed.top_level_node_id, output)?;
    Ok(())
}
