use parse_js::{ast::{Node, Syntax, ClassOrObjectMemberValue}, visit::{Visitor, JourneyControls}};

mod function;

struct VarUsageVisitor;

impl Visitor for VarUsageVisitor {
  fn on_syntax_down(&mut self, node: &Node, ctl: &mut JourneyControls) {
    match node.stx.as_ref() {
      Syntax::IdentifierExpr { name } => {
        todo!();
      }
      _ => {}
    }
  }
}

struct TopLevelFunctionVisitor;

impl Visitor for TopLevelFunctionVisitor {
  fn on_syntax_down(&mut self, node: &Node, ctl: &mut JourneyControls) {
    match node.stx.as_ref() {
      Syntax::Function { parameters, body, .. } => {
        todo!();
      }
      _ => {}
    };
  }
}

pub(crate) fn minify_js(
  top_level_node: &Node,
) {
  // TODO Check for use before declaration to ensure strict SSA.
  // TODO Mark all symbols used across closures. (These must be loaded and stored using Foreign* insts.)
  todo!();
}
