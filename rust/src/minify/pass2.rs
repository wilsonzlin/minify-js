use super::ctx::Ctx;
use super::ctx::MinifyScope;
use parse_js::ast::NodeData;
use parse_js::ast::Syntax;
use parse_js::visit::Visitor;

// - Move function declarations into `hoisted_functions`, so we can then place them back in the tree at the top of a closure in the next pass.
pub struct Pass2<'a, 'b> {
  pub ctx: Ctx<'a, 'b>,
}

impl<'a, 'b> Visitor<'a> for Pass2<'a, 'b> {
  fn on_syntax_up(&mut self, n: &mut NodeData<'a>) -> () {
    let scope = n.scope;
    // This needs to be done when we iterate upwards and not downwards:
    // - If we do it while iterating down, we won't traverse the function declaration's subtree, which we still need to do for the other tasks (e.g. tracking inherited variables).
    // - It makes sense to cut out the pieces inside out (i.e. the nested parts that are function declarations), instead of removing the entire function declaration which itself may have some nested function declarations alongside other things.
    // TODO Consider `export` and `export default`.
    let named_fn_decl_name = match &n.stx {
      Syntax::FunctionDecl {
        export: false,
        name: Some(name),
        ..
      } => Some(name.loc),
      _ => None,
    };
    if let Some(name) = named_fn_decl_name {
      let decl_scope = scope
        .find_self_or_ancestor(|t| t.is_closure_or_global())
        .unwrap();
      self
        .ctx
        .scopes
        .entry(decl_scope)
        .or_insert_with(|| MinifyScope::new(self.ctx.session))
        .hoisted_functions
        .insert(name, n.replace(self.ctx.session, Syntax::EmptyStmt {}));
      return;
    };
  }
}
