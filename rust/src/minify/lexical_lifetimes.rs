use super::ctx::Ctx;
use super::ctx::MinifySymbol;
use core::mem::discriminant;
use parse_js::ast::NodeData;
use parse_js::ast::Syntax;
use parse_js::session::Session;
use parse_js::session::SessionVec;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;
use std::cmp::Ordering;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct LexicalLifetime<'a>(SessionVec<'a, u32>);

impl<'a> LexicalLifetime<'a> {
  pub fn new_zero(session: &'a Session) -> Self {
    let mut vec = session.new_vec();
    vec.push(0);
    Self(vec)
  }

  pub fn new_infinite(session: &'a Session) -> Self {
    let mut vec = session.new_vec();
    vec.push(u32::MAX);
    Self(vec)
  }

  pub fn bump(&mut self) {
    *self.0.last_mut().unwrap() += 1;
  }

  pub fn fork(&mut self) {
    self.0.push(0);
  }

  pub fn is_sibling_of(&self, other: &Self) -> bool {
    let lvl = self.0.len();
    lvl == other.0.len() && (lvl == 1 || self.0[lvl - 2] == other.0[lvl - 2])
  }
}

pub struct LexicalLifetimesPass<'a, 'b> {
  pub ctx: Ctx<'a, 'b>,
  pub stack: LexicalLifetime<'a>,
  pub in_loop: bool,
}

impl<'a, 'b> LexicalLifetimesPass<'a, 'b> {
  pub fn new(ctx: Ctx<'a, 'b>) -> Self {
    let stack = LexicalLifetime::new_zero(ctx.session);
    Self {
      ctx,
      stack,
      in_loop: false,
    }
  }

  fn fork_stack(&mut self) -> LexicalLifetime<'a> {
    let orig = self.stack.clone();
    self.stack.fork();
    orig
  }

  fn join_stack(&mut self, orig: &LexicalLifetime<'a>) {
    self.stack = orig.clone();
  }

  fn bump_stack(&mut self) -> &mut LexicalLifetime<'a> {
    self.stack.bump();
    &mut self.stack
  }

  fn visit_conditional(
    &mut self,
    test: &mut NodeData<'a>,
    consequent: &mut NodeData<'a>,
    alternate: Option<&mut NodeData<'a>>,
    ctl: &mut JourneyControls,
  ) {
    self.visit(test);

    // Conditionals get their own lifetime value, and then each branch is a fork (i.e. subelements).
    let if_stack = self.bump_stack().clone();
    self.fork_stack();
    self.visit(consequent);
    self.join_stack(&if_stack);
    if let Some(alternate) = alternate {
      self.fork_stack();
      self.visit(alternate);
      self.join_stack(&if_stack);
    };

    ctl.skip();
  }
}

impl<'a, 'b> Visitor<'a> for LexicalLifetimesPass<'a, 'b> {
  fn on_syntax_down(&mut self, node: &mut NodeData<'a>, ctl: &mut JourneyControls) -> () {
    let usage_scope = node.scope;
    let Some(usage_closure_scope) = usage_scope.find_self_or_ancestor(|s| s.is_closure_or_class()) else {
      // TODO Assert we're at the top level.
      return;
    };
    // We could reset the stack when entering a closure and restore on exit, but this isn't strictly necessary since values are still distinct even if we continue to use existing stack. It also avoids some complexity and performance costs.
    match &mut node.stx {
      Syntax::IdentifierExpr { name } => {
        if let Some((decl_scope, symbol)) = usage_scope.find_symbol_with_scope(*name) {
          let decl_closure_scope = decl_scope
            .find_self_or_ancestor(|s| s.is_closure())
            .unwrap();
          let lifetime = if decl_closure_scope == usage_closure_scope {
            self.bump_stack().clone()
          } else {
            // We're in a nested closure or class.
            LexicalLifetime::new_infinite(self.ctx.session)
          };
          self
            .ctx
            .symbols
            .entry(symbol)
            .or_insert_with(|| MinifySymbol::new(self.ctx.session))
            .update_lifetime(lifetime);
        }
      }
      Syntax::IdentifierPattern { name } => {
        // TODO
      }
      Syntax::IfStmt {
        test,
        consequent,
        alternate,
        ..
      } => {
        // This appears pointless but is to workaround the fact that Rust won't allow `*alternate` or `alternate.as_ref().map(|t| *t)` or `alternate.as_mut().map(|t| *t)`.
        match alternate {
          Some(alt) => self.visit_conditional(test, consequent, Some(alt), ctl),
          None => self.visit_conditional(test, consequent, None, ctl),
        };
      }
      Syntax::ConditionalExpr {
        test,
        consequent,
        alternate,
        ..
      } => {
        self.visit_conditional(test, consequent, Some(alternate), ctl);
      }
      Syntax::ForStmt { header, body } => {
        // TODO
      }
      Syntax::WhileStmt { condition, body } => {
        // TODO
      }
      Syntax::DoWhileStmt { condition, body } => {
        // TODO
      }
      _ => {}
    };
  }
}
