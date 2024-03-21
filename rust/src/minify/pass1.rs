use super::advanced_if::analyse_if_branch;
use super::advanced_if::process_if_branch;
use super::ctx::Ctx;
use super::ctx::MinifyScope;
use super::ctx::MinifySymbol;
use parse_js::ast::new_node;
use parse_js::ast::NodeData;
use parse_js::ast::Syntax;
use parse_js::operator::Operator;
use parse_js::operator::OperatorName;
use parse_js::session::Session;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;
use std::str::from_utf8_unchecked;

// - Detect all usages of JSX components, as React determines `<link>` to be the HTML tag and `<Link>` to be the variable `Link` as a component, so we cannot minify `Link` to `link` or `a0` or `bb` (i.e. make capitalised JSX elements uncapitalised).
// - Find all references of variables so we can determine inherited variables (see `MinifiedNameGenerator` and `MinifyScope`). This is because JS allows variables to be lexically referenced before they're used, so we cannot do this in the same pass. For example, `let b = 1; { let a = () => b; let b = 2; }`.
// - Find uses of `new <var>` and set `is_used_as_constructor`.
// - Find uses of `<var>.prototype` and set `has_prototype`.
// - Combine consecutive expression statements into one.
// - Convert `if (x) { expr; }` to `x && expr`.
// - Convert `if (x) { expr1; } else { expr2; }` to `x ? expr1 : expr2`.
// - Concatenate addition of two literal strings.
// - Unwrap unnecessary block statements.
// - Drop debugger statements.
// - Normalise `if-else` branches into block statements.
pub struct Pass1<'a, 'b> {
  pub ctx: Ctx<'a, 'b>,
}

fn stmt_has_return<'a>(stx: &Syntax<'a>) -> bool {
  match stx {
    Syntax::ReturnStmt { .. } => true,
    Syntax::BlockStmt { body } => body.iter().any(|n| stmt_has_return(&n.stx)),
    _ => false,
  }
}

// Decompose into functions for easier reasoning and testing, and more readable final top-level combined usage (i.e. `x(); y(); z()` instead of `match typ { A => { x(); y() } B => { x(); z() } C => { a() } D => { z() } }`).
// Hope that the compiler inlines into one giant `match` instead of executing sequentially and bailing out one-after-another on the first syntax type condition of each function.

// Wrap any consequent or alternate expression or non-block statement in a block (e.g. `if (x) a` => `if (x) { a; }`).
#[inline(always)]
fn maybe_ensure_if_statement_consequent_and_alternate_are_wrapped<'a, 'b>(
  ctx: &mut Ctx<'a, 'b>,
  n: &mut NodeData<'a>,
) {
  let Syntax::IfStmt {
    consequent,
    alternate,
    ..
  } = &mut n.stx
  else {
    return;
  };
  fn maybe_wrap<'a>(session: &'a Session, branch: &mut &mut NodeData<'a>) {
    match &branch.stx {
      // Already a block, don't need to do anything.
      Syntax::BlockStmt { .. } => {}
      _ => {
        let inner = branch.take(session);
        let wrapped = new_node(session, inner.scope, inner.loc, Syntax::BlockStmt {
          body: {
            let mut body = session.new_vec();
            body.push(inner);
            body
          },
        });
        // Update the original node's consequent/alternate to point to the new block we've just created.
        *branch = wrapped;
      }
    };
  }
  maybe_wrap(ctx.session, consequent);
  if let Some(alt) = alternate {
    maybe_wrap(ctx.session, alt);
  }
}

#[inline(always)]
fn maybe_combine_string_literals<'a, 'b>(ctx: &mut Ctx<'a, 'b>, n: &mut NodeData<'a>) {
  let Syntax::BinaryExpr {
    operator: OperatorName::Addition,
    left: NodeData {
      stx: Syntax::LiteralStringExpr { value: l },
      ..
    },
    right: NodeData {
      stx: Syntax::LiteralStringExpr { value: r },
      ..
    },
    ..
  } = &mut n.stx
  else {
    return;
  };
  let concat = ctx
    .session
    .get_allocator()
    .alloc_slice_fill_default(l.len() + r.len());
  concat[..l.len()].copy_from_slice(l.as_bytes());
  concat[l.len()..].copy_from_slice(r.as_bytes());
  n.stx = Syntax::LiteralStringExpr {
    value: unsafe { from_utf8_unchecked(concat) },
  };
}

#[cfg(test)]
mod tests {
  use super::Pass1;
  use crate::emit;
  use crate::minify::ctx::Ctx;
  use crate::minify::ctx::MinifyScope;
  use crate::minify::ctx::MinifySymbol;
  use crate::minify::pass1::maybe_combine_string_literals;
  use crate::minify::pass1::maybe_ensure_if_statement_consequent_and_alternate_are_wrapped;
  use parse_js::ast::NodeData;
  use parse_js::ast::Syntax;
  use parse_js::parse::toplevel::TopLevelMode;
  use parse_js::session::Session;
  use parse_js::symbol::Scope;
  use parse_js::symbol::Symbol;
  use parse_js::visit::JourneyControls;
  use parse_js::visit::Visitor;

  macro_rules! setup {
    // Rust won't expose declared vars in a macro unless we explicitly pass in their names.
    ($n:ident, $ctx:ident, $source:expr) => {
      let session = Session::new();
      let $n = parse_js::parse(&session, $source.as_bytes(), TopLevelMode::Global).unwrap();
      let mut symbols = session.new_hashmap::<Symbol, MinifySymbol>();
      let mut scopes = session.new_hashmap::<Scope<'_>, MinifyScope<'_>>();
      let $ctx = Ctx {
        scopes: &mut scopes,
        session: &session,
        symbols: &mut symbols,
      };
    };
  }

  macro_rules! check {
    ($root_node:expr, $expected:expr) => {
      let mut out = Vec::new();
      emit($root_node, &mut out);
      assert_eq!($expected, std::str::from_utf8(&out).unwrap());
    };
  }

  #[test]
  fn test_ensure_if_statement_consequent_and_alternate_are_wrapped() {
    struct P<'a, 'b> {
      pub ctx: Ctx<'a, 'b>,
    }
    impl<'a, 'b> Visitor<'a> for P<'a, 'b> {
      fn on_syntax_down(&mut self, n: &mut NodeData<'a>, _ctl: &mut JourneyControls) -> () {
        maybe_ensure_if_statement_consequent_and_alternate_are_wrapped(&mut self.ctx, n);
      }
    }

    setup!(n, ctx, "if (x) a;");
    P { ctx }.visit(n);
    check!(n, "if(x){a}");

    setup!(n, ctx, "if (x) {} else b;");
    P { ctx }.visit(n);
    check!(n, "if(x){}else{b}");

    setup!(n, ctx, "if (x) a; else {b}");
    P { ctx }.visit(n);
    check!(n, "if(x){a}else{b}");

    setup!(n, ctx, "if (x) a; else b");
    P { ctx }.visit(n);
    check!(n, "if(x){a}else{b}");
  }

  #[test]
  fn test_combine_string_literals() {
    struct P<'a, 'b> {
      pub ctx: Ctx<'a, 'b>,
    }
    impl<'a, 'b> Visitor<'a> for P<'a, 'b> {
      fn on_syntax_up(&mut self, n: &mut NodeData<'a>) -> () {
        maybe_combine_string_literals(&mut self.ctx, n);
      }
    }

    setup!(n, ctx, "'a' + 'b';");
    P { ctx }.visit(n);
    check!(n, "`ab`");

    setup!(n, ctx, "'a' + 'b' + 'c';");
    P { ctx }.visit(n);
    check!(n, "`abc`");
  }
}

impl<'a, 'b> Visitor<'a> for Pass1<'a, 'b> {
  fn on_syntax_down(&mut self, n: &mut NodeData<'a>, _ctl: &mut JourneyControls) -> () {
    let scope = n.scope;
    maybe_ensure_if_statement_consequent_and_alternate_are_wrapped(&mut self.ctx, n);
    match &mut n.stx {
      Syntax::BlockStmt { body } => {
        let mut i = 0;
        while i < body.len() {
          if let Syntax::IfStmt {
            test,
            consequent,
            alternate: maybe_alternate,
          } = &mut body[i].stx
          {
            if let Some(alternate) = maybe_alternate {
              // If `if` returns, unwrap `else`.
              // If `else` returns **AND** `if` does not return, swap `if` and `else`, and then unwrap `else` (post-swap).
              // Whatever is unwrapped becomes another statement in the current body and should be processed, not skipped.
              let cons_has_return = stmt_has_return(&consequent.stx);
              let swapped = if !cons_has_return && stmt_has_return(&alternate.stx) {
                core::mem::swap(consequent, alternate);
                let orig_test = test.take(self.ctx.session);
                test.stx = Syntax::UnaryExpr {
                  parenthesised: false,
                  operator: OperatorName::LogicalNot,
                  argument: orig_test,
                };
                true
              } else {
                false
              };
              if swapped || cons_has_return {
                // We can't always double-unwrap if it's a block as the block might be necessary (e.g. `let`). We have later optimisations that will unwrap it if possible.
                let alternate_branch = alternate.take(self.ctx.session);
                *maybe_alternate = None;
                body.insert(i + 1, alternate_branch);
              }
            }
          };
          i += 1;
        }
      }
      Syntax::IdentifierExpr { name } => {
        self.ctx.track_variable_usage(scope, *name);
      }
      // IdentifierPattern also appears in destructuring, not just declarations. It's safe either way; if it's a declaration, its scope will have its declaration, so there will be no inheritance.
      Syntax::IdentifierPattern { name } => {
        self.ctx.track_variable_usage(scope, *name);
      }
      Syntax::MemberExpr {
        right: p2,
        optional_chaining: false,
        left: NodeData {
          stx: Syntax::IdentifierExpr { name: p1 },
          ..
        },
        ..
      } if p2.as_slice() == b"prototype" => {
        if let Some(sym) = scope.find_symbol(*p1) {
          self
            .ctx
            .symbols
            .entry(sym)
            .or_insert_with(|| MinifySymbol::new(self.ctx.session))
            .has_prototype = true;
        };
      }
      Syntax::UnaryExpr {
        parenthesised: _,
        operator: OperatorName::New,
        argument,
      } => {
        let var_name = match &argument.stx {
          // e.g. `new Array()`.
          Syntax::CallExpr { callee, .. } => match &callee.stx {
            Syntax::IdentifierExpr { name } => Some(*name),
            _ => None,
          },
          // e.g. `new Array`.
          Syntax::IdentifierExpr { name } => Some(*name),
          _ => None,
        };
        if let Some(var_name) = var_name {
          if let Some(sym) = scope.find_symbol(var_name) {
            self
              .ctx
              .symbols
              .entry(sym)
              .or_insert_with(|| MinifySymbol::new(self.ctx.session))
              .is_used_as_constructor = true;
          };
        };
      }
      Syntax::JsxElement {
        name:
          Some(NodeData {
            stx: Syntax::IdentifierExpr { name },
            ..
          }),
        ..
      } => {
        if let Some(sym) = n.scope.find_symbol(*name) {
          self
            .ctx
            .symbols
            .entry(sym)
            .or_insert_with(|| MinifySymbol::new(self.ctx.session))
            .is_used_as_jsx_component = true;
        };
      }
      _ => {}
    }
  }

  fn on_syntax_up(&mut self, node: &mut NodeData<'a>) -> () {
    let loc = node.loc;
    let scope = node.scope;
    match &mut node.stx {
      // This is bottom-up as we could remove nested blocks recursively.
      Syntax::BlockStmt { body } => {
        let mut returned = false;
        // Next writable slot when shifting down due to gaps from deleting merged ExpressionStmt values.
        let mut w = 0;
        // Next readable slot to process.
        let mut r = 0;
        // We can't use a for loop or cache `body.len()` as it might change (e.g. unpacking redundant block statement).
        while r < body.len() {
          if returned {
            // Drop remaining unreachable code.
            // TODO There may be more code outside this block that's now unreachable and can be removed.
            break;
          };
          // Get `scope` before we borrow mutably for `stx`.
          let r_scope = body[r].scope;
          let keep = match &mut body[r].stx {
            // NOTE: We must match here as BlockStmt may not always be a block statement (e.g. `for`, `while`, function bodies).
            Syntax::BlockStmt { body: block_body } => {
              if block_body.is_empty() {
                false
              } else if r_scope.symbol_names().is_empty() {
                // This block statement doesn't have any block-scoped declarations, so it's unnecessary.
                let mut to_add = self.ctx.session.new_vec();
                for s in block_body {
                  to_add.push(s.take(self.ctx.session));
                }
                // Insert after current `r` so we process these next.
                body.splice(r + 1..r + 1, to_add);
                false
              } else {
                true
              }
            }
            Syntax::ExpressionStmt { expression: _ } => {
              // TODO Remove if pure.
              true
            }
            Syntax::ReturnStmt { .. } => {
              returned = true;
              true
            }
            _ => true,
          };
          if keep {
            body.swap(w, r);
            w += 1;
          };
          r += 1;
        }
        body.truncate(w);
      }
      Syntax::IfStmt {
        test,
        consequent,
        alternate,
      } => {
        // Note that we cannot process unless both branches can be processed, otherwise we'll be left with one branch mutated.
        let cons_ok = analyse_if_branch(&consequent.stx);
        let alt_ok = alternate.as_ref().map(|alt| analyse_if_branch(&alt.stx));

        match (cons_ok, alt_ok) {
          (true, None) => {
            let closure_scope = scope.find_self_or_ancestor(|t| t.is_closure()).unwrap_or(scope);
            let cons_expr = process_if_branch(self.ctx.session, scope, consequent);
            let min_scope = self
              .ctx
              .scopes
              .entry(closure_scope)
              .or_insert_with(|| MinifyScope::new(self.ctx.session));
            min_scope
              .hoisted_vars
              .extend_from_slice(&cons_expr.hoisted_vars);
            if cons_expr.returns {
              consequent.stx = Syntax::ReturnStmt {
                value: Some(cons_expr.expression),
              };
            } else {
              let right = cons_expr.expression;
              let test = test.take(self.ctx.session);
              node.stx = Syntax::ExpressionStmt {
                expression: new_node(self.ctx.session, scope, loc, Syntax::BinaryExpr {
                  parenthesised: false,
                  operator: OperatorName::LogicalAnd,
                  left: test,
                  right,
                }),
              };
            }
          }
          (true, Some(true)) => {
            let closure_scope = scope.find_self_or_ancestor(|t| t.is_closure()).unwrap_or(scope);
            let cons_expr = process_if_branch(self.ctx.session, scope, consequent);
            let alt_expr = process_if_branch(self.ctx.session, scope, alternate.as_mut().unwrap());
            let min_scope = self
              .ctx
              .scopes
              .entry(closure_scope)
              .or_insert_with(|| MinifyScope::new(self.ctx.session));
            min_scope
              .hoisted_vars
              .extend_from_slice(&cons_expr.hoisted_vars);
            min_scope
              .hoisted_vars
              .extend_from_slice(&alt_expr.hoisted_vars);
            // Due to normalisation, it's not possible for an `if-else` to return in either branch, because one branch would've been unwrapped.
            assert!(cons_expr.returns == alt_expr.returns);
            let test = test.take(self.ctx.session);
            let consequent = cons_expr.expression;
            let alternate = alt_expr.expression;
            node.stx = Syntax::ExpressionStmt {
              expression: new_node(self.ctx.session, scope, loc, Syntax::ConditionalExpr {
                parenthesised: false,
                test,
                consequent,
                alternate,
              }),
            };
          }
          _ => {}
        };
      }
      _ => {}
    };
  }
}
