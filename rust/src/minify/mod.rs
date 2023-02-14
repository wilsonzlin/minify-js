use parse_js::ast::new_node;
use parse_js::ast::ClassOrObjectMemberKey;
use parse_js::ast::ClassOrObjectMemberValue;
use parse_js::ast::ExportName;
use parse_js::ast::ExportNames;
use parse_js::ast::Node;
use parse_js::ast::NodeData;
use parse_js::ast::ObjectMemberType;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::ast::VariableDeclarator;
use parse_js::builtin::Builtin;
use parse_js::builtin::BUILTINS;
use parse_js::char::ID_CONTINUE_CHARSTR;
use parse_js::char::ID_START_CHARSTR;
use parse_js::lex::KEYWORD_STRS;
use parse_js::num::JsNumber;
use parse_js::operator::OperatorName;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::session::SessionHashSet;
use parse_js::session::SessionString;
use parse_js::session::SessionVec;
use parse_js::source::SourceRange;
use parse_js::symbol::Identifier;
use parse_js::symbol::Scope;
use parse_js::symbol::ScopeFlag;
use parse_js::symbol::Symbol;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;
use std::fmt::Write;

// We don't minify booleans, as `!0` and `!1` are good enough,and in a sufficiently large codebase minified variable names will have lengths >= 2 anyway.
// TODO BigInt.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Constant<'a> {
  Number(JsNumber),
  String(SessionString<'a>),
  Null,
  Builtin(Builtin<'static>),
}

#[derive(Default)]
struct ConstantUsage<'a> {
  count: usize,
  replacement_var_name: Option<SourceRange<'a>>,
}

struct ExportBinding<'a> {
  target: SourceRange<'a>,
  alias: SourceRange<'a>,
}

// Generator of minified names. Works by generating the next smallest possible name (starting from `a`), and then repeats until it finds one that is not a keyword or would conflict with an inherited variable (a variable that is in scope **and** used by code that we would otherwise shadow).
struct MinifiedNameGenerator<'a> {
  session: &'a Session,
  // Index of each character of the last generated name, in reverse order (i.e. first character is last element) for optimised extension.
  state: SessionVec<'a, usize>,
}

impl<'a> MinifiedNameGenerator<'a> {
  pub fn new(session: &'a Session) -> MinifiedNameGenerator<'a> {
    MinifiedNameGenerator {
      session,
      state: session.new_vec(),
    }
  }

  fn transition_to_next_possible_minified_name(&mut self) -> SessionVec<'a, u8> {
    let n = &mut self.state;
    let mut overflow = true;
    for i in 0..n.len() {
      let charset = if i == n.len() - 1 {
        ID_START_CHARSTR
      } else {
        ID_CONTINUE_CHARSTR
      };
      if n[i] == charset.len() - 1 {
        n[i] = 0;
      } else {
        n[i] += 1;
        overflow = false;
        break;
      };
    }
    if overflow {
      n.push(0);
    };

    let mut name = self.session.new_vec(); // TODO Capacity
    for (i, idx) in n.iter().enumerate() {
      let charset = if i == n.len() - 1 {
        ID_START_CHARSTR
      } else {
        ID_CONTINUE_CHARSTR
      };
      name.push(charset[*idx]);
    }
    name.reverse();
    name
  }

  // TODO This needs optimisation, in case inherited_vars has a long sequence of used minified names (likely).
  pub fn generate_next_available_minified_name(
    &mut self,
    inherited_vars: &SessionHashSet<Identifier<'a>>,
  ) -> Identifier<'a> {
    loop {
      let name = self.transition_to_next_possible_minified_name();
      if KEYWORD_STRS.contains_key(name.as_slice()) {
        continue;
      };
      let name = self
        .session
        .get_allocator()
        .alloc_slice_copy(name.as_slice());
      let as_ident = SourceRange::new(name, 0, name.len());
      if inherited_vars.contains(&as_ident) {
        continue;
      };
      return as_ident;
    }
  }
}

// Our additional state that's associated with each Symbol.
#[derive(Default)]
struct MinifySymbol<'a> {
  minified_name: Option<SourceRange<'a>>,
  is_used_as_jsx_component: bool,
  has_usage: bool,
  // If this is true, and this symbol is associated with a function, don't transform the function into an arrow function, even if it doesn't use `this`.
  is_used_as_constructor: bool,
  // Similar to `is_used_as_constructor`, although a weaker signal, since the presence of `prototype` is highly likely to mean it's a constructor function, but not as certain as `new`.
  has_prototype: bool,
}

// Our additional state that's associated with each Scope.
struct MinifyScope<'a> {
  // Variables that are declared by an ancestor (not own) scope (or is not declared anywhere and assumed to be global), and used by code in own or any descendant scope.
  inherited_vars: SessionHashSet<'a, Identifier<'a>>,
  // Function declarations within this closure-like scope that must be hoisted to declarations at the very beginning of this closure's code (so we can transform them to `var` and still have them work correctly). There may be multiple closures with the same name, nested deep with many blocks and branches, which is why we use a map; the last visited (lexical) declaration wins. Note that this is only populated if this scope is a closure; function declarations don't hoist to blocks.
  // Since they could be deep and anywhere, we must take them and move them into this map; we can't just look at a BlockStmt's children as they may not always be there.
  hoisted_functions: SessionHashMap<'a, Identifier<'a>, Node<'a>>,
  constant_usages: SessionHashMap<'a, Constant<'a>, ConstantUsage<'a>>,
}

impl<'a> MinifyScope<'a> {
  pub fn new(session: &'a Session) -> MinifyScope<'a> {
    MinifyScope {
      inherited_vars: session.new_hashset(),
      hoisted_functions: session.new_hashmap(),
      constant_usages: session.new_hashmap(),
    }
  }
}

struct Ctx<'a, 'b> {
  session: &'a Session,
  symbols: &'b mut SessionHashMap<'a, Symbol<'a>, MinifySymbol<'a>>,
  scopes: &'b mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
}

impl<'a, 'b> Ctx<'a, 'b> {
  // See [notes/Name minification.md] for the algorithm in more detail.
  fn track_variable_usage(&mut self, scope: Scope<'a>, name: Identifier<'a>) {
    let mut cur = Some(scope);
    while let Some(scope) = cur {
      if let Some(sym) = scope.get_symbol(name) {
        self.symbols.entry(sym).or_default().has_usage = true;
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

  fn track_constant_usage(&mut self, scope: Scope<'a>, constant: Constant<'a>) {
    if let Some(ancestor_scope) = scope.find_furthest_self_or_ancestor(|t| t.is_closure_or_block())
    {
      self
        .scopes
        .entry(ancestor_scope)
        .or_insert_with(|| MinifyScope::new(self.session))
        .constant_usages
        .entry(constant)
        .or_default()
        .count += 1;
    }
  }
}

// This should be run after `PretransformPass` visitor has run and before the `MinifyPass` visitor runs.
// The IdentifierPass pass collects all usages of variables to determine inherited variables for each scope, so we can know what minified names can be safely used (see `MinifiedNameGenerator`). This function will then go through each declaration in each scope and generate and update their corresponding `MinifySymbol.minified_name`.
// Some pecularities to note: globals aren't minified (whether declared or not), so when blacklisting minified names, they are directly disallowed. However, all other variables will be minified, so we need to blacklist their minified name, not their original name. This is why this function processes scopes top-down (from the root), as we need to know the minified names of ancestor variables first before we can blacklist them.
fn minify_names<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  minify_scopes: &mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
  minify_symbols: &mut SessionHashMap<'a, Symbol<'a>, MinifySymbol<'a>>,
) {
  // It's possible that the entry doesn't exist, if there were no inherited variables during the first pass.
  let minify_scope = minify_scopes
    .entry(scope)
    .or_insert_with(|| MinifyScope::new(session));
  // Our `inherited_vars` contains original names; we need to retrieve their minified names.
  let mut minified_inherited_vars = session.new_hashset();
  for &original_inherited_var in minify_scope.inherited_vars.iter() {
    match scope.find_symbol(original_inherited_var) {
      None => {
        // Global (undeclared or declared).
        minified_inherited_vars.insert(original_inherited_var);
      }
      Some(sym) => {
        let min_sym = minify_symbols.get(&sym).unwrap();
        let min_name = min_sym.minified_name.unwrap();
        minified_inherited_vars.insert(min_name);
      }
    };
  }
  // Yes, we start from the very beginning in case there are possible gaps/opportunities due to inherited variables on ancestors.
  let mut next_min_name = MinifiedNameGenerator::new(session);
  for &sym_name in scope.symbol_names().iter() {
    let sym = scope.get_symbol(sym_name).unwrap();
    let min_sym = minify_symbols.entry(sym).or_default();
    assert!(min_sym.minified_name.is_none());
    if min_sym.is_used_as_jsx_component {
      // We'll process these in another iteration, as there's fewer characters allowed for the identifier start, and we don't want to skip past valid identifiers for non-JSX-component names.
      continue;
    };
    min_sym.minified_name =
      Some(next_min_name.generate_next_available_minified_name(&minified_inherited_vars))
  }
  for &sym_name in scope.symbol_names().iter() {
    let sym = scope.get_symbol(sym_name).unwrap();
    let min_sym = minify_symbols.get_mut(&sym).unwrap();
    if !min_sym.is_used_as_jsx_component {
      continue;
    };
    // TODO This is very slow and dumb.
    let mut min_name;
    loop {
      min_name = next_min_name.generate_next_available_minified_name(&minified_inherited_vars);
      if !min_name.as_slice()[0].is_ascii_lowercase() {
        break;
      };
    }
    min_sym.minified_name = Some(min_name)
  }
  for &c in scope.children().iter() {
    minify_names(session, c, minify_scopes, minify_symbols);
  }
}

fn get_builtin<'a>(
  scope: Scope<'a>,
  p1: SourceRange<'a>,
  p2: Option<SourceRange<'a>>,
  p3: Option<SourceRange<'a>>,
) -> Option<Constant<'a>> {
  debug_assert!(!p1.is_empty());
  if scope.find_symbol(p1).is_some() {
    return None;
  };
  let Some(builtin) = BUILTINS.get(&Builtin(p1.as_slice(), p2.as_ref().map(|r| r.as_slice()).unwrap_or(b""), p3.as_ref().map(|r| r.as_slice()).unwrap_or(b""))) else {
    return None;
  };
  Some(Constant::Builtin(builtin.get()))
}

// - Detect all usages of JSX components, as React determines `<link>` to be the HTML tag and `<Link>` to be the variable `Link` as a component, so we cannot minify `Link` to `link` or `a0` or `bb` (i.e. make capitalised JSX elements uncapitalised).
// - Find all references of variables so we can determine inherited variables (see `MinifiedNameGenerator` and `MinifyScope`). This is because JS allows variables to be lexically references before they're used, so we cannot do this in the same pass. For example, `let b = 1; { let a = () => b; let b = 2; }`.
// - Find uses of `new <var>` and set `is_used_as_constructor`.
// - Find uses of `<var>.prototype` and set `has_prototype`.
// - Count uses of constants. If there's more than one, create a new shared variable for it.
struct IdentifierPass<'a, 'b> {
  ctx: Ctx<'a, 'b>,
}

impl<'a, 'b> Visitor<'a> for IdentifierPass<'a, 'b> {
  fn on_syntax_down(&mut self, n: &mut NodeData<'a>, _ctl: &mut JourneyControls) -> () {
    let scope = n.scope;
    match &n.stx {
      Syntax::LiteralNumberExpr { value } => {
        self
          .ctx
          .track_constant_usage(scope, Constant::Number(*value));
      }
      Syntax::LiteralStringExpr { value } => {
        // We must clone the string. Consider that it's owned, so a mutator of Syntax can technically mutate it, even though we'd never do such a thing. We can change this by instead making the parser use a immutable value, which could be as simple as a non-mutable reference.
        self
          .ctx
          .track_constant_usage(scope, Constant::String(value.clone()));
      }
      Syntax::LiteralNull {} => {
        self.ctx.track_constant_usage(scope, Constant::Null);
      }
      Syntax::IdentifierExpr { name } => {
        // TODO Don't do this if operator to `typeof`.
        if let Some(builtin) = get_builtin(scope, *name, None, None) {
          self.ctx.track_constant_usage(scope, builtin)
        };
        self.ctx.track_variable_usage(scope, *name);
      }
      // Matches `p1.p2.p3` but not if it's an assignment target.
      Syntax::MemberExpr {
        assignment_target: false,
        right: p3,
        optional_chaining: false,
        left:
          NodeData {
            stx:
              Syntax::MemberExpr {
                right: p2,
                optional_chaining: false,
                left:
                  NodeData {
                    stx: Syntax::IdentifierExpr { name: p1 },
                    ..
                  },
                ..
              },
            ..
          },
        ..
      } => {
        if let Some(builtin) = get_builtin(scope, *p1, Some(*p2), Some(*p3)) {
          self.ctx.track_constant_usage(scope, builtin)
        };
      }
      // Matches `p1.p2`.
      Syntax::MemberExpr {
        assignment_target,
        right: p2,
        optional_chaining: false,
        left: NodeData {
          stx: Syntax::IdentifierExpr { name: p1 },
          ..
        },
        ..
      } => {
        if !assignment_target {
          if let Some(builtin) = get_builtin(scope, *p1, Some(*p2), None) {
            self.ctx.track_constant_usage(scope, builtin)
          };
        };
        if p2.as_slice() == b"prototype" {
          if let Some(sym) = scope.find_symbol(*p1) {
            self.ctx.symbols.entry(sym).or_default().has_prototype = true;
          };
        }
      }
      Syntax::UnaryExpr {
        parenthesised,
        operator,
        argument,
      } if *operator == OperatorName::New => {
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
              .or_default()
              .is_used_as_constructor = true;
          };
        };
      }
      Syntax::JsxMember { base: name, .. } => {
        self.ctx.track_variable_usage(scope, *name);
      }
      Syntax::JsxName {
        name,
        namespace: None,
      } if !name.as_slice()[0].is_ascii_lowercase() => {
        if let Some(sym) = n.scope.find_symbol(*name) {
          self
            .ctx
            .symbols
            .entry(sym)
            .or_default()
            .is_used_as_jsx_component = true;
        };
        self.ctx.track_variable_usage(scope, *name);
      }
      // IdentifierPattern also appears in destructuring, not just declarations. It's safe either way; if it's a declaration, its scope will have its declaration, so there will be no inheritance.
      Syntax::IdentifierPattern { name } => {
        self.ctx.track_variable_usage(scope, *name);
      }
      // Same rationale as IdentifierPattern. Note that ClassOrObjectMemberKey::Direct doesn't use an IdentifierPattern, so we must visit it explicitly.
      Syntax::ObjectPatternProperty {
        key: ClassOrObjectMemberKey::Direct(name),
        target: None,
        ..
      } => {
        // TODO What if `name` is a keyword, number, string, etc.?
        self.ctx.track_variable_usage(scope, *name);
      }
      Syntax::ObjectMember {
        typ: ObjectMemberType::Shorthand { name },
      } => {
        self.ctx.track_variable_usage(scope, *name);
      }
      _ => {}
    }
  }
}

// The second pass.
struct PretransformPass<'a, 'b> {
  ctx: Ctx<'a, 'b>,
}

impl<'a, 'b> PretransformPass<'a, 'b> {
  fn maybe_replace_constant_usage(
    &mut self,
    scope: Scope<'a>,
    constant: Constant<'a>,
  ) -> Option<SourceRange<'a>> {
    let Some(ancestor_scope) = scope.find_furthest_self_or_ancestor(|t| t.is_closure_or_block()) else {
      return None;
    };
    let usage = self
      .ctx
      .scopes
      .entry(ancestor_scope)
      .or_insert_with(|| MinifyScope::new(self.ctx.session))
      .constant_usages
      .entry(constant.clone())
      .or_default();
    if usage.count <= 1 {
      return None;
    };
    if usage.replacement_var_name.is_none() {
      let mut name = self.ctx.session.new_string();
      name.push_str("__MINIFY_JS_CONST_REPLACEMENT_VAR_");

      let mut repr = self.ctx.session.new_string();
      match constant {
        Constant::Number(n) => {
          write!(repr, "Number{}", n)
        }
        Constant::String(s) => {
          write!(repr, "String{}", s)
        }
        Constant::Null => {
          write!(repr, "Null")
        }
        Constant::Builtin(b) => {
          write!(repr, "Builtin{}", b)
        }
      }
      .unwrap();
      for byte in repr.as_bytes() {
        write!(name, "{:X}", byte).unwrap();
      }

      let name = SourceRange::from_slice(
        self
          .ctx
          .session
          .get_allocator()
          .alloc_slice_copy(name.as_bytes()),
      );
      usage.replacement_var_name = Some(name);
      ancestor_scope.add_symbol(name).unwrap();
    };
    usage.replacement_var_name
  }
}

// - Replace all shared constant usages to reference the newly-created shared variable instead.
// - Move function declarations into `hoisted_functions`, so we can then place them back in the tree at the top of a closure in the next pass.
impl<'a, 'b> Visitor<'a> for PretransformPass<'a, 'b> {
  fn on_syntax_down(&mut self, n: &mut NodeData<'a>, _: &mut JourneyControls) {
    // We can't do this in the IdentifierPass::on_syntax_up as counts are only valid after going through the entire code, and on_syntax_up only ensures the current subtree has been visited.
    let scope = n.scope;
    let new_name = match &n.stx {
      Syntax::LiteralNumberExpr { value } => {
        self.maybe_replace_constant_usage(scope, Constant::Number(*value))
      }
      Syntax::LiteralStringExpr { value } => {
        // We must clone the string. Consider that it's owned, so a mutator of Syntax can technically mutate it, even though we'd never do such a thing. We can change this by instead making the parser use a immutable value, which could be as simple as a non-mutable reference.
        self.maybe_replace_constant_usage(scope, Constant::String(value.clone()))
      }
      Syntax::LiteralNull {} => self.maybe_replace_constant_usage(scope, Constant::Null),
      Syntax::IdentifierExpr { name } => {
        if let Some(builtin) = get_builtin(scope, *name, None, None) {
          self.maybe_replace_constant_usage(scope, builtin)
        } else {
          None
        }
      }
      // Matches `p1.p2.p3` but not if it's an assignment target.
      Syntax::MemberExpr {
        assignment_target: false,
        right: p3,
        optional_chaining: false,
        left:
          NodeData {
            stx:
              Syntax::MemberExpr {
                right: p2,
                optional_chaining: false,
                left:
                  NodeData {
                    stx: Syntax::IdentifierExpr { name: p1 },
                    ..
                  },
                ..
              },
            ..
          },
        ..
      } => {
        if let Some(builtin) = get_builtin(scope, *p1, Some(*p2), Some(*p3)) {
          self.maybe_replace_constant_usage(scope, builtin)
        } else {
          None
        }
      }
      // Matches `p1.p2` but not if it's an assignment target.
      Syntax::MemberExpr {
        assignment_target: false,
        right: p2,
        optional_chaining: false,
        left: NodeData {
          stx: Syntax::IdentifierExpr { name: p1 },
          ..
        },
        ..
      } => {
        if let Some(builtin) = get_builtin(scope, *p1, Some(*p2), None) {
          self.maybe_replace_constant_usage(scope, builtin)
        } else {
          None
        }
      }
      _ => None,
    };
    if let Some(new_name) = new_name {
      n.replace(self.ctx.session, Syntax::IdentifierExpr { name: new_name });
      // We need to call `process_variable_usage` because even if generated name is unlikely to conflict, minified name might.
      self.ctx.track_variable_usage(scope, new_name);
    };
  }

  fn on_syntax_up(&mut self, n: &mut NodeData<'a>) -> () {
    let scope = n.scope;
    // This needs to be done when we iterate upwards and not downwards:
    // - If we do it while iterating down, we won't traverse the function declaration's subtree, which we still need to do for the other tasks (e.g. tracking inherited variables).
    // - It makes sense to cut out the pieces inside out (i.e. the nested parts that are function declarations), instead of removing the entire function declaration which itself may have some nested function declarations alongside other things.
    // TODO Consider `export` and `export default`.
    let named_fn_decl_name = match &n.stx {
      Syntax::FunctionDecl {
        export,
        name: Some(name),
        ..
      } if !*export => Some(name.loc),
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

// The third and main pass, that does most of the work. This should be run after the `minify_names` function.
struct MinifyPass<'a, 'b> {
  session: &'a Session,
  // Exports with the same exported name (including multiple default exports) are illegal, so we don't have to worry about/handle that case.
  export_bindings: &'b mut Vec<ExportBinding<'a>>,
  symbols: &'b mut SessionHashMap<'a, Symbol<'a>, MinifySymbol<'a>>,
  scopes: &'b mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
}

impl<'a, 'b> MinifyPass<'a, 'b> {
  fn visit_exported_pattern(&mut self, n: &mut NodeData<'a>) -> () {
    match &mut n.stx {
      Syntax::ArrayPattern { elements, rest } => {
        for e in elements {
          if let Some(e) = e {
            self.visit_exported_pattern(e.target);
          }
        }
        if let Some(rest) = rest {
          self.visit_exported_pattern(*rest);
        }
      }
      Syntax::ObjectPattern { properties, rest } => {
        for p in properties {
          self.visit_exported_pattern(*p);
        }
        if let Some(rest) = rest {
          self.visit_exported_pattern(*rest);
        }
      }
      Syntax::ObjectPatternProperty { key, target, .. } => {
        match target {
          Some(target) => self.visit_exported_pattern(*target),
          // Shorthand.
          None => match key {
            ClassOrObjectMemberKey::Direct(key) => self.export_bindings.push(ExportBinding {
              target: *key,
              alias: *key,
            }),
            _ => unreachable!(),
          },
        }
      }
      Syntax::IdentifierPattern { name } => self.export_bindings.push(ExportBinding {
        target: *name,
        alias: *name,
      }),
      _ => unreachable!(),
    }
  }
}

impl<'a, 'b> Visitor<'a> for MinifyPass<'a, 'b> {
  fn on_syntax_down(&mut self, node: &mut NodeData<'a>, ctl: &mut JourneyControls) -> () {
    // We must not use `node.` after this point, as we're now borrowing it as mut.
    let loc = node.loc;
    let scope = node.scope;
    let mut new_stx: Option<Syntax<'_>> = None;
    match &mut node.stx {
      Syntax::TopLevel { body } | Syntax::BlockStmt { body } => {
        // TODO Are all global/closure scopes associated with exactly one BlockStmt or TopLevel?
        if scope.typ().is_closure_or_global() {
          if let Some(min_scope) = self.scopes.get_mut(&scope) {
            for fn_decl in min_scope.hoisted_functions.values_mut() {
              // TODO Batch prepend to avoid repeated Vec shifting.
              body.insert(0, fn_decl.take(self.session));
            }
          };
        };
        // TODO Are all non-global non-class scopes associated with exactly one BlockStmt or TopLevel?
        if scope.typ().is_closure_or_block() {
          if let Some(min_scope) = self.scopes.get(&scope) {
            let new_loc = loc.at_start();
            // Declarations are ordered for builtin aliases e.g. the alias for `Array` must come before `Array.from`.
            let mut new_decls = self.session.new_vec();
            for (con, con_usg) in min_scope.constant_usages.iter() {
              // This is only Some if we've made replacements (i.e. count > 1).
              if let Some(new_name) = con_usg.replacement_var_name {
                let ordinal = match con {
                  Constant::Number(_) | Constant::String(_) | Constant::Null => {
                    (b"".as_slice(), b"".as_slice(), b"".as_slice())
                  }
                  Constant::Builtin(b) => (b.0, b.1, b.2),
                };
                let init = new_node(self.session, scope, new_loc, match con {
                  Constant::Number(n) => Syntax::LiteralNumberExpr { value: *n },
                  Constant::String(v) => Syntax::LiteralStringExpr { value: v.clone() },
                  Constant::Null => Syntax::LiteralNull {},
                  Constant::Builtin(b) => {
                    let mut expr = Syntax::IdentifierExpr {
                      name: SourceRange::from_slice(b.0),
                    };
                    for p in &[b.1, b.2] {
                      if p == b"" {
                        break;
                      };
                      expr = Syntax::MemberExpr {
                        parenthesised: false,
                        optional_chaining: false,
                        assignment_target: false,
                        left: new_node(self.session, scope, new_loc, expr),
                        right: SourceRange::from_slice(p),
                      };
                    }
                    expr
                  }
                });
                new_decls.push((ordinal, VariableDeclarator {
                  pattern: new_node(self.session, scope, new_loc, Syntax::IdentifierPattern {
                    name: new_name,
                  }),
                  initializer: Some(init),
                }));
              };
            }
            if !new_decls.is_empty() {
              new_decls.sort_unstable_by_key(|(ord, _)| *ord);
              let mut new_decls_sorted = self.session.new_vec();
              for (_, decl) in new_decls.into_iter() {
                new_decls_sorted.push(decl);
              }
              let decl = new_node(self.session, scope, new_loc, Syntax::VarDecl {
                export: false,
                mode: VarDeclMode::Let,
                declarators: new_decls_sorted,
              });
              body.insert(
                0,
                new_node(self.session, scope, new_loc, Syntax::VarStmt {
                  declaration: decl,
                }),
              );
            };
          }
        };
      }
      Syntax::ArrowFunctionExpr {
        parenthesised,
        is_async,
        signature,
        body,
      } => {
        if let Syntax::BlockStmt { body } = &mut body.stx {
          if body.len() == 1 {
            if let Syntax::ReturnStmt { value } = &mut body[0].stx {
              if let Some(return_value) = value {
                new_stx = Some(Syntax::ArrowFunctionExpr {
                  parenthesised: *parenthesised,
                  is_async: *is_async,
                  signature: signature.take(self.session),
                  body: return_value.take(self.session),
                });
              }
            };
          };
        };
      }
      Syntax::FunctionExpr {
        parenthesised,
        is_async,
        generator,
        name,
        signature,
        body,
      } => {
        let fn_scope = body.scope;
        // TODO This will still work for named functions as long as that name isn't used (including if it's shadowed).
        // TODO Detect property access of "prototype" on variable referencing function to reduce (but not remove) false negatives.
        // TODO Can this work sometimes even when `arguments` is used?
        // TODO This is still not risk-free, as the function's prototype could still be used even if there is no `this`.
        // TODO Detect `function(){}.bind(this)`, which is pretty much risk free unless somehow Function.prototype.bind has been overridden. However, any other value for the first argument of `.bind` means that it is no longer safe.
        if name.is_none()
          && !*generator
          && !fn_scope.has_flag(ScopeFlag::UsesArguments)
          && !fn_scope.has_flag(ScopeFlag::UsesThis)
        {
          new_stx = Some(Syntax::ArrowFunctionExpr {
            // TODO
            parenthesised: true,
            is_async: *is_async,
            signature: signature.take(self.session),
            body: body.take(self.session),
          });
        };
      }
      Syntax::FunctionDecl {
        export,
        body,
        generator,
        is_async,
        name,
        signature,
        ..
      } if !*export => {
        let fn_scope = body.scope;
        // TODO Consider `export function` and `export default function`.
        // TODO Detect property access of "prototype" on variable referencing function to reduce (but not remove) false negatives.
        // TODO Can this work sometimes even when `arguments` is used?
        // TODO This is still not risk-free, as the function's prototype could still be used even if there is no `this`.
        // TODO Detect `function(){}.bind(this)`, which is pretty much risk free unless somehow Function.prototype.bind has been overridden. However, any other value for the first argument of `.bind` means that it is no longer safe.
        if !*export
          && name.is_some()
          && !*generator
          && !fn_scope.has_flag(ScopeFlag::UsesArguments)
          && !fn_scope.has_flag(ScopeFlag::UsesThis)
          // Use `find_symbol` as we might not be in a closure scope and the function declaration's symbol would've been added to an ancestor.
          // If no symbol is found (e.g. global), or it exists but is not `is_used_as_constructor` and not `has_prototype`, then we can safely proceed.
          && scope.find_symbol(name.as_ref().unwrap().loc).and_then(|sym| self.symbols.get(&sym)).filter(|sym| sym.is_used_as_constructor || sym.has_prototype).is_none()
        {
          let var_decl_pat = new_node(
            self.session,
            // TODO Is this scope correct?
            scope,
            name.as_ref().unwrap().loc,
            Syntax::IdentifierPattern {
              name: name.as_ref().unwrap().loc,
            },
          );
          let var_decl_init = new_node(
            self.session,
            // TODO Is this scope correct?
            scope,
            loc,
            Syntax::ArrowFunctionExpr {
              // TODO
              parenthesised: true,
              is_async: *is_async,
              signature: signature.take(self.session),
              body: body.take(self.session),
            },
          );
          let var_decl = new_node(self.session, scope, loc, Syntax::VarDecl {
            export: false,
            // We must use `var` to have the same hoisting and shadowing semantics.
            // TODO Are there some differences e.g. reassignment, shadowing, hoisting, redeclaration, and use-before-assignment/declaration?
            mode: VarDeclMode::Var,
            declarators: {
              let mut vec = self.session.new_vec();
              vec.push(VariableDeclarator {
                pattern: var_decl_pat,
                initializer: Some(var_decl_init),
              });
              vec
            },
          });

          new_stx = Some(Syntax::VarStmt {
            declaration: var_decl,
          });
        }
      }
      Syntax::IdentifierPattern { name } => {
        let sym = scope.find_symbol(*name);
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].minified_name.unwrap();
          new_stx = Some(Syntax::IdentifierPattern { name: minified });
        };
      }
      Syntax::IdentifierExpr { name } => {
        let sym = scope.find_symbol(*name);
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].minified_name.unwrap();
          new_stx = Some(Syntax::IdentifierExpr { name: minified });
        };
      }
      Syntax::ClassOrFunctionName { name } => {
        let sym = scope.find_symbol(*name);
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].minified_name.unwrap();
          new_stx = Some(Syntax::ClassOrFunctionName { name: minified });
        };
      }
      Syntax::JsxMember {
        base: name, path, ..
      } => {
        let sym = scope.find_symbol(*name);
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].minified_name.unwrap();
          new_stx = Some(Syntax::JsxMember {
            base: minified,
            path: path.clone(),
          });
        };
      }
      Syntax::JsxName {
        name,
        namespace: None,
      } => {
        let sym = scope.find_symbol(*name);
        // TODO JsxName must be capitalised to be interpreted as a component.
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].minified_name.unwrap();
          new_stx = Some(Syntax::JsxName {
            namespace: None,
            name: minified,
          });
        };
      }
      Syntax::ClassDecl {
        export,
        export_default,
        name,
        ..
      }
      | Syntax::FunctionDecl {
        export,
        export_default,
        name,
        ..
      } if *export => {
        if let Some(name) = name {
          match &name.stx {
            Syntax::ClassOrFunctionName { name } => {
              self.export_bindings.push(ExportBinding {
                target: *name,
                alias: if *export_default {
                  SourceRange::from_slice(b"default")
                } else {
                  *name
                },
              });
            }
            _ => unreachable!(),
          };
        }
      }
      Syntax::VarDecl {
        export,
        declarators,
        ..
      } if *export => {
        for decl in declarators.iter_mut() {
          self.visit_exported_pattern(decl.pattern);
        }
      }
      Syntax::ExportListStmt { names, from } => {
        ctl.skip();
        match from {
          None => match names {
            ExportNames::Specific(names) => {
              for e in names {
                self.export_bindings.push(ExportBinding {
                  target: e.target.clone(),
                  alias: match &e.alias.stx {
                    Syntax::IdentifierPattern { name } => name.clone(),
                    _ => unreachable!(),
                  },
                });
                new_stx = Some(Syntax::EmptyStmt {});
              }
            }
            ExportNames::All(_) => unreachable!(),
          },
          // `export ... from ...` do not touch/alter the module's scope, so we can ignore completely.
          _ => {}
        }
      }
      Syntax::ObjectPatternProperty {
        key: ClassOrObjectMemberKey::Direct(name),
        target: None,
        default_value,
      } => {
        if scope.find_symbol(*name).is_some() {
          // If the symbol declaration exists, we know it definitely has a minified name. However, because the parser recurses into changed subtrees, we must simply expand this property with a IdentifierPattern target referencing the original name, so that the visitor for it will then change it to the minified name. Otherwise, we'll retrieve the minified name for a minified name, which is incorrect. Note that we can't simply skip the subtree entirely as there are still other parts.
          let replacement_target_node =
            new_node(self.session, scope, loc, Syntax::IdentifierPattern {
              name: *name,
            });
          new_stx = Some(Syntax::ObjectPatternProperty {
            key: ClassOrObjectMemberKey::Direct(*name),
            target: Some(replacement_target_node),
            default_value: default_value.take(),
          });
        };
      }
      Syntax::ObjectMember {
        typ: ObjectMemberType::Shorthand { name },
      } => {
        if scope.find_symbol(*name).is_some() {
          // See Syntax::ObjectPatternProperty match branch.
          let replacement_initializer_node =
            new_node(self.session, scope, loc, Syntax::IdentifierExpr {
              name: *name,
            });
          new_stx = Some(Syntax::ObjectMember {
            typ: ObjectMemberType::Valued {
              key: ClassOrObjectMemberKey::Direct(*name),
              value: ClassOrObjectMemberValue::Property {
                initializer: Some(replacement_initializer_node),
              },
            },
          });
        };
      }
      _ => {}
    };

    if let Some(new_stx) = new_stx {
      node.stx = new_stx;
    }
  }
}

pub fn minify_js<'a>(session: &'a Session, top_level_node: &mut NodeData<'a>) -> () {
  let top_level_scope = top_level_node.scope;

  // Our custom data/state associated with a Symbol.
  let mut symbols = session.new_hashmap::<Symbol<'a>, MinifySymbol>();
  // Our custom data/state associated with a Scope.
  let mut scopes = session.new_hashmap::<Scope<'a>, MinifyScope<'a>>();
  // Exports: what they refer to and what they're named.
  let mut export_bindings = Vec::new();

  IdentifierPass {
    ctx: Ctx {
      scopes: &mut scopes,
      session,
      symbols: &mut symbols,
    },
  }
  .visit(top_level_node);

  PretransformPass {
    ctx: Ctx {
      scopes: &mut scopes,
      session,
      symbols: &mut symbols,
    },
  }
  .visit(top_level_node);

  minify_names(session, top_level_scope, &mut scopes, &mut symbols);

  MinifyPass {
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
