pub mod advanced_if;
pub mod ctx;
pub mod name;
pub mod pass1;
pub mod pass2;
pub mod pass3;

use self::ctx::Ctx;
use self::ctx::MinifyScope;
use self::ctx::MinifySymbol;
use self::name::minify_names;
use self::pass1::Pass1;
use self::pass2::Pass2;
use self::pass3::Pass3;
use parse_js::ast::new_node;
use parse_js::ast::ExportName;
use parse_js::ast::ExportNames;
use parse_js::ast::NodeData;
use parse_js::ast::Syntax;
use parse_js::session::Session;
use parse_js::symbol::Scope;
use parse_js::symbol::Symbol;
use parse_js::visit::Visitor;

pub fn minify_js<'a>(session: &'a Session, top_level_node: &mut NodeData<'a>) -> () {
  let top_level_scope = top_level_node.scope;

  // Our custom data/state associated with a Symbol.
  let mut symbols = session.new_hashmap::<Symbol, MinifySymbol>();
  // Our custom data/state associated with a Scope.
  let mut scopes = session.new_hashmap::<Scope<'a>, MinifyScope<'a>>();
  // Exports: what they refer to and what they're named.
  let mut export_bindings = Vec::new();

  Pass1 {
    ctx: Ctx {
      scopes: &mut scopes,
      session,
      symbols: &mut symbols,
    },
  }
  .visit(top_level_node);

  Pass2 {
    ctx: Ctx {
      scopes: &mut scopes,
      session,
      symbols: &mut symbols,
    },
  }
  .visit(top_level_node);

  minify_names(session, top_level_scope, &mut scopes, &mut symbols);

  Pass3 {
    session,
    export_bindings: &mut export_bindings,
    symbols: &mut symbols,
    scopes: &mut scopes,
  }
  .visit(top_level_node);

  let mut export_names = session.new_vec();
  for e in export_bindings.iter() {
    let target_symbol = top_level_scope
      .find_symbol(e.target)
      .expect(format!("failed to find top-level export `{:?}`", e.target).as_str());
    export_names.push(ExportName {
      target: symbols[&target_symbol].minified_name.unwrap(),
      alias: new_node(
        session,
        top_level_scope,
        e.alias,
        Syntax::IdentifierPattern { name: e.alias },
      ),
    });
  }

  if !export_names.is_empty() {
    let final_export_stmt = new_node(
      session,
      top_level_scope,
      top_level_node.loc.at_end(),
      Syntax::ExportListStmt {
        names: ExportNames::Specific(export_names),
        from: None,
      },
    );
    match &mut top_level_node.stx {
      Syntax::TopLevel { body } => {
        body.push(final_export_stmt);
      }
      _ => unreachable!(),
    }
  }
}
