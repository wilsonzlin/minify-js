use parse_js::ast::ClassOrObjectMemberKey;
use parse_js::ast::ClassOrObjectMemberValue;
use parse_js::ast::ExportName;
use parse_js::ast::ExportNames;
use parse_js::ast::Node;
use parse_js::ast::ObjectMemberType;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::ast::VariableDeclarator;
use parse_js::char::ID_CONTINUE_CHARSTR;
use parse_js::char::ID_START_CHARSTR;
use parse_js::lex::KEYWORD_STRS;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::source::SourceRange;
use parse_js::symbol::Identifier;
use parse_js::symbol::ScopeFlag;
use parse_js::symbol::Symbol;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;

const JSX_COMPONENT_NAME_PREFIX: char = '\u{01BC}';
const ALT_MINIFIED_NAMES: &'static [char] = &[
  '\u{01BB}', '\u{02B0}', '\u{02B1}', '\u{02B2}', '\u{02B3}', '\u{02B4}', '\u{02B5}', '\u{02B6}',
  '\u{02B7}', '\u{02B8}', '\u{02B9}', '\u{02BA}', '\u{02BB}', '\u{02BC}', '\u{02BD}', '\u{02BE}',
  '\u{02BF}', '\u{02C0}', '\u{02C1}', '\u{02C6}', '\u{02C7}', '\u{02C8}', '\u{02C9}', '\u{02CA}',
  '\u{02CB}', '\u{02CC}', '\u{02CD}', '\u{02CE}', '\u{02CF}', '\u{02D0}', '\u{02D1}', '\u{02E0}',
  '\u{02E1}', '\u{02E2}', '\u{02E3}', '\u{02E4}', '\u{02EC}', '\u{02EE}', '\u{0374}', '\u{037A}',
  '\u{0559}', '\u{0640}', '\u{06E5}', '\u{06E6}', '\u{07F4}', '\u{07F5}', '\u{07FA}',
];

struct ExportBinding<'a> {
  target: SourceRange<'a>,
  alias: SourceRange<'a>,
}

#[derive(Default)]
struct MinifySymbol {
  // Set to 0 initially, before minification pass. WARNING: 0 is still a valid value, so do not use before setting.
  minified_name_id: usize,
  is_used_as_jsx_component: bool,
}

impl MinifySymbol {
  fn set_minified_name_id(&mut self, id: usize) {
    self.minified_name_id = id;
  }

  fn mark_as_used_as_jsx_component(&mut self) {
    self.is_used_as_jsx_component = true;
  }

  fn generate_minified_name_with_edit<'a>(
    &self,
    session: &'a Session,
    original: Identifier<'a>,
  ) -> SourceRange<'a> {
    let mut id = self.minified_name_id;
    let mut name = session.new_vec();
    // See main function at bottom for why we do this.
    if self.is_used_as_jsx_component {
      name.extend(b"  ");
      JSX_COMPONENT_NAME_PREFIX.encode_utf8(&mut name[0..2]);
    }
    name.push(ID_START_CHARSTR[id % ID_START_CHARSTR.len()]);
    if id >= ID_START_CHARSTR.len() {
      id /= ID_START_CHARSTR.len();
      while id >= ID_CONTINUE_CHARSTR.len() {
        name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
        id /= ID_CONTINUE_CHARSTR.len();
      }
      name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
    };
    if let Some(alt_id) = KEYWORD_STRS.get(name.as_slice()) {
      // This name is a keyword, so we replace it with a Unicode character instead.
      // This Unicode character is 2 bytes when encoded in UTF-8, so it's more than minimal enough. UTF-8 encodes U+0080 to U+07FF in 2 bytes.
      // There should be exactly one ALT_MINIFIED_NAMES element for each KEYWORD_STRS entry.
      // Using a Unicode name will ensure no chance of clashing with keywords, well-knowns, and almost all variables.
      // Clashes can appear quickly e.g. `in`, `of`, `if`.
      // TODO This could still clash with an untracked or unminified variable; however, that's technically true of any minified name.
      let s = ALT_MINIFIED_NAMES[*alt_id].encode_utf8(&mut name).len();
      name.truncate(s);
    };
    let name = session.get_allocator().alloc_slice_copy(&name);
    original.with_edit(name)
  }
}

// See main function at bottom for why we need this.
struct JsxVisitor<'a, 'b> {
  symbols: &'b mut SessionHashMap<'a, Symbol<'a>, MinifySymbol>,
}

impl<'a, 'b> Visitor<'a> for JsxVisitor<'a, 'b> {
  fn on_syntax(&mut self, n: Node<'a>, _ctl: &mut JourneyControls) -> () {
    match &*n.stx() {
      Syntax::JsxName {
        namespace: None,
        name,
      } if !name.as_slice()[0].is_ascii_lowercase() => {
        match n.scope().find_symbol(*name) {
          Some(sym) => self
            .symbols
            .entry(sym)
            .or_default()
            .mark_as_used_as_jsx_component(),
          None => {
            // TODO Warn if symbol not found.
          }
        };
      }
      _ => {}
    }
  }
}

struct MinifyVisitor<'a, 'b> {
  session: &'a Session,
  // Exports with the same exported name (including multiple default exports) are illegal, so we don't have to worry about/handle that case.
  export_bindings: &'b mut Vec<ExportBinding<'a>>,
  symbols: &'b mut SessionHashMap<'a, Symbol<'a>, MinifySymbol>,
}

impl<'a, 'b> MinifyVisitor<'a, 'b> {
  fn visit_exported_pattern(&mut self, n: Node<'a>) -> () {
    match &*n.stx() {
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
              target: key.clone(),
              alias: key.clone(),
            }),
            _ => unreachable!(),
          },
        }
      }
      Syntax::IdentifierPattern { name } => self.export_bindings.push(ExportBinding {
        target: name.clone(),
        alias: name.clone(),
      }),
      _ => unreachable!(),
    }
  }
}

impl<'a, 'b> Visitor<'a> for MinifyVisitor<'a, 'b> {
  fn on_syntax(&mut self, node: Node<'a>, ctl: &mut JourneyControls) -> () {
    let mut node_data = node.get_mut();
    // We must not use `node.` after this point, as we're now borrowing it as mut.
    let loc = node_data.loc;
    let scope = node_data.scope;
    let mut new_stx: Option<Syntax<'a>> = None;
    match &node_data.stx {
      Syntax::ArrowFunctionExpr {
        parenthesised,
        is_async,
        signature,
        body,
      } => {
        if let Syntax::BlockStmt { body } = &*body.stx() {
          if body.len() == 1 {
            if let Syntax::ReturnStmt { value } = &*body[0].stx() {
              if let Some(return_value) = value {
                new_stx = Some(Syntax::ArrowFunctionExpr {
                  parenthesised: *parenthesised,
                  is_async: *is_async,
                  signature: *signature,
                  body: *return_value,
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
        let fn_scope = body.scope();
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
            signature: *signature,
            body: *body,
          });
        };
      }
      Syntax::FunctionDecl {
        body,
        generator,
        is_async,
        name,
        signature,
      } => {
        let fn_scope = body.scope();
        // TODO Consider `export function` and `export default function`.
        // TODO Detect property access of "prototype" on variable referencing function to reduce (but not remove) false negatives.
        // TODO Can this work sometimes even when `arguments` is used?
        // TODO This is still not risk-free, as the function's prototype could still be used even if there is no `this`.
        // TODO Detect `function(){}.bind(this)`, which is pretty much risk free unless somehow Function.prototype.bind has been overridden. However, any other value for the first argument of `.bind` means that it is no longer safe.
        if name.is_some()
          && !*generator
          && !fn_scope.has_flag(ScopeFlag::UsesArguments)
          && !fn_scope.has_flag(ScopeFlag::UsesThis)
        {
          let var_decl_pat = Node::new(
            self.session,
            // TODO Is this scope correct?
            scope,
            name.unwrap().loc(),
            Syntax::IdentifierPattern {
              name: name.unwrap().loc(),
            },
          );
          let var_decl_init = Node::new(
            self.session,
            // TODO Is this scope correct?
            scope,
            loc,
            Syntax::ArrowFunctionExpr {
              // TODO
              parenthesised: true,
              is_async: *is_async,
              signature: *signature,
              body: *body,
            },
          );
          let var_decl = Node::new(self.session, scope, loc, Syntax::VarDecl {
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
      stx @ (Syntax::IdentifierPattern { name }
      | Syntax::IdentifierExpr { name }
      | Syntax::ClassOrFunctionName { name }
      | Syntax::JsxMember { base: name, .. }
      | Syntax::JsxName {
        name,
        namespace: None,
      }) => {
        let sym = scope.find_symbol(*name);
        // TODO JsxMember and JsxNamespacedName must be capitalised to be interpreted as a component.
        if let Some(sym) = sym {
          let minified = self.symbols[&sym].generate_minified_name_with_edit(self.session, *name);
          new_stx = Some(match stx {
            Syntax::IdentifierPattern { .. } => Syntax::IdentifierPattern { name: minified },
            Syntax::IdentifierExpr { .. } => Syntax::IdentifierExpr { name: minified },
            Syntax::ClassOrFunctionName { .. } => Syntax::ClassOrFunctionName { name: minified },
            Syntax::JsxMember { path, .. } => Syntax::JsxMember {
              base: minified,
              path: path.clone(),
            },
            Syntax::JsxName { namespace, .. } => Syntax::JsxName {
              namespace: namespace.clone(),
              name: minified,
            },
            _ => unreachable!(),
          });
        };
      }
      Syntax::ExportDeclStmt {
        declaration,
        default,
      } => match &*declaration.stx() {
        Syntax::ClassDecl { name, .. } | Syntax::FunctionDecl { name, .. } => {
          match name {
            Some(name) => match &*name.stx() {
              Syntax::ClassOrFunctionName { name } => {
                self.export_bindings.push(ExportBinding {
                  target: name.clone(),
                  alias: if *default {
                    name.with_edit(b"default")
                  } else {
                    name.clone()
                  },
                });
              }
              _ => unreachable!(),
            },
            _ => {}
          };
        }
        Syntax::VarStmt { declaration } => match &*declaration.stx() {
          Syntax::VarDecl { declarators, .. } => {
            for decl in declarators {
              self.visit_exported_pattern(decl.pattern);
            }
          }
          _ => unreachable!(),
        },
        _ => unreachable!(),
      },
      Syntax::ExportListStmt { names, from } => {
        ctl.skip();
        match from {
          None => match names {
            ExportNames::Specific(names) => {
              for e in names {
                self.export_bindings.push(ExportBinding {
                  target: e.target.clone(),
                  alias: match &*e.alias.stx() {
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
        key,
        target,
        default_value,
      } => {
        match key {
          ClassOrObjectMemberKey::Direct(name) => {
            if target.is_none() {
              let sym = scope.find_symbol(*name);
              if let Some(sym) = sym {
                let minified =
                  self.symbols[&sym].generate_minified_name_with_edit(self.session, *name);
                let replacement_target_node =
                  Node::new(self.session, scope, minified, Syntax::IdentifierPattern {
                    name: minified,
                  });
                new_stx = Some(Syntax::ObjectPatternProperty {
                  key: *key,
                  target: Some(replacement_target_node),
                  default_value: *default_value,
                });
              };
            }
          }
          _ => {}
        };
      }
      Syntax::ObjectMember { typ } => {
        match typ {
          ObjectMemberType::Shorthand { name } => {
            let sym = scope.find_symbol(*name);
            if let Some(sym) = sym {
              let minified =
                self.symbols[&sym].generate_minified_name_with_edit(self.session, *name);
              let replacement_initializer_node =
                Node::new(self.session, scope, minified, Syntax::IdentifierExpr {
                  name: minified,
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
      }
      _ => {}
    };

    if let Some(new_stx) = new_stx {
      node_data.stx = new_stx;
    }
  }
}

pub fn minify_js<'a>(session: &'a Session, top_level_node: Node<'a>) -> () {
  let top_level_scope = top_level_node.scope();

  let mut symbols = session.new_hashmap::<Symbol, MinifySymbol>();
  let mut scopes = session.new_vec();
  scopes.push(top_level_scope);
  let mut next_scope_to_inspect = 0;
  while next_scope_to_inspect < scopes.len() {
    let scope = scopes[next_scope_to_inspect];
    scopes.extend_from_slice(&scope.children());
    next_scope_to_inspect += 1;
  }
  // We need these since we cannot look up in scope_map while mutably borrowed by iter_mut().
  let symbol_counts: Vec<usize> = scopes.iter().map(|s| s.symbol_count()).collect();
  let mut minified_name_starts: Vec<usize> = vec![0; scopes.len()];
  // Iterating like this assumes that any parent of an element is always present before it in the list.
  for (id, &scope) in scopes.iter().enumerate() {
    // TODO Opportunity for optimisation: not all variables are used by descendant scopes, so some of their names can be reused by descendants.
    let minified_name_start = match scope.parent() {
      Some(p) => symbol_counts[id] + minified_name_starts[id],
      None => 0,
    };
    for (symbol_idx, &symbol_name) in scope.symbol_names().iter().enumerate() {
      let symbol = scope.get_symbol(symbol_name).unwrap();
      symbols
        .entry(symbol)
        .or_default()
        .set_minified_name_id(minified_name_start + symbol_idx);
    }
    minified_name_starts[id] = minified_name_start;
  }

  // Since React requires that non-namespaced JSX elements that refer to a JS component must never start with a lowercase letter (otherwise it will always be interpreted as an HTML tag, even if a same-named variable is in scope), we must detect all usages and prefix them with a unique non-lowercase character JSX_COMPONENT_NAME_PREFIX (this is the simplest solution given how we currently minify identifiers). Since we can't know which variables are used as such (for the same reason we cannot minify while parsing in the same step i.e. due to forward references across scopes), we must run a separate pass after parsing and before minifying.
  let mut jsx_visitor = JsxVisitor {
    symbols: &mut symbols,
  };
  jsx_visitor.visit_top_level(top_level_node);

  let mut export_bindings = Vec::new();
  let mut visitor = MinifyVisitor {
    session,
    export_bindings: &mut export_bindings,
    symbols: &mut symbols,
  };
  visitor.visit_top_level(top_level_node);
  let mut export_names = session.new_vec();
  for e in export_bindings.iter() {
    let target_symbol = top_level_scope
      .find_symbol(e.target)
      .expect(format!("failed to find top-level export `{:?}`", e.target).as_str());
    export_names.push(ExportName {
      target: symbols[&target_symbol].generate_minified_name_with_edit(session, e.target),
      alias: Node::new(
        session,
        top_level_scope,
        e.alias,
        Syntax::IdentifierPattern { name: e.alias },
      ),
    });
  }

  if !export_names.is_empty() {
    let final_export_stmt = Node::new(
      session,
      top_level_scope,
      top_level_node.loc().get_end_of_source(),
      Syntax::ExportListStmt {
        names: ExportNames::Specific(export_names),
        from: None,
      },
    );
    match &mut *top_level_node.stx_mut() {
      Syntax::TopLevel { body } => {
        body.push(final_export_stmt);
      }
      _ => unreachable!(),
    }
  }
}
