use super::lexical_lifetimes::LexicalLifetime;
use parse_js::ast::Node;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::session::SessionHashSet;
use parse_js::session::SessionVec;
use parse_js::source::SourceRange;
use parse_js::symbol::Identifier;
use parse_js::symbol::Scope;
use parse_js::symbol::Symbol;
use std::cmp::max;
use std::cmp::min;

// Our additional state that's associated with each Symbol.
pub struct MinifySymbol<'a> {
  pub minified_name: Option<SourceRange<'a>>,
  pub is_used_as_jsx_component: bool,
  pub has_usage: bool,
  // If this is true, and this symbol is associated with a function, don't transform the function into an arrow function, even if it doesn't use `this`.
  pub is_used_as_constructor: bool,
  // Similar to `is_used_as_constructor`, although a weaker signal, since the presence of `prototype` is highly likely to mean it's a constructor function, but not as certain as `new`.
  pub has_prototype: bool,
  pub lexical_lifetime_start: LexicalLifetime<'a>,
  pub lexical_lifetime_end: LexicalLifetime<'a>,
}

impl<'a> MinifySymbol<'a> {
  pub fn new(session: &'a Session) -> Self {
    Self {
      minified_name: None,
      is_used_as_jsx_component: false,
      has_usage: false,
      is_used_as_constructor: false,
      has_prototype: false,
      lexical_lifetime_start: LexicalLifetime::new_infinite(session),
      lexical_lifetime_end: LexicalLifetime::new_zero(session),
    }
  }

  pub fn update_lifetime(&mut self, lifetime: LexicalLifetime<'a>) {
    if lifetime < self.lexical_lifetime_start {
      self.lexical_lifetime_start = lifetime.clone();
    };
    if lifetime > self.lexical_lifetime_end {
      self.lexical_lifetime_end = lifetime.clone();
    };
  }
}

// Our additional state that's associated with each Scope.
pub struct MinifyScope<'a> {
  // Variables that are declared by an ancestor (not own) scope (or is not declared anywhere and assumed to be global), and used by code in own or any descendant scope.
  pub inherited_vars: SessionHashSet<'a, Identifier<'a>>,
  // Function declarations within this closure-like scope that must be hoisted to declarations at the very beginning of this closure's code (so we can transform them to `var` and still have them work correctly). There may be multiple closures with the same name, nested deep with many blocks and branches, which is why we use a map; the last visited (lexical) declaration wins. Note that this is only populated if this scope is a closure; function declarations don't hoist to blocks.
  // Since they could be deep and anywhere, we must take them and move them into this map; we can't just look at a BlockStmt's children as they may not always be there.
  pub hoisted_functions: SessionHashMap<'a, Identifier<'a>, Node<'a>>,
  // `var` declarations in this closure that need to be moved to allow for some optimisation.
  pub hoisted_vars: SessionVec<'a, Identifier<'a>>,
}

impl<'a> MinifyScope<'a> {
  pub fn new(session: &'a Session) -> MinifyScope<'a> {
    MinifyScope {
      inherited_vars: session.new_hashset(),
      hoisted_functions: session.new_hashmap(),
      hoisted_vars: session.new_vec(),
    }
  }
}

pub struct Ctx<'a, 'b> {
  pub session: &'a Session,
  pub symbols: &'b mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
  pub scopes: &'b mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
}

impl<'a, 'b> Ctx<'a, 'b> {
  // See [notes/Name minification.md] for the algorithm in more detail.
  pub fn track_variable_usage(&mut self, scope: Scope<'a>, name: Identifier<'a>) {
    let mut cur = Some(scope);
    while let Some(scope) = cur {
      if let Some(sym) = scope.get_symbol(name) {
        self
          .symbols
          .entry(sym)
          .or_insert_with(|| MinifySymbol::new(self.session))
          .has_usage = true;
        break;
      };
      self
        .scopes
        .entry(scope)
        .or_insert_with(|| MinifyScope::new(self.session))
        .inherited_vars
        .insert(name);
      cur = scope.parent();
    }
  }
}
