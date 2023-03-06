use super::ctx::MinifyScope;
use super::ctx::MinifySymbol;
use parse_js::ast::new_node;
use parse_js::ast::ClassOrObjectMemberKey;
use parse_js::ast::ClassOrObjectMemberValue;
use parse_js::ast::ExportNames;
use parse_js::ast::NodeData;
use parse_js::ast::ObjectMemberType;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::ast::VariableDeclarator;
use parse_js::flag::Flags;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::source::SourceRange;
use parse_js::symbol::Scope;
use parse_js::symbol::ScopeFlag;
use parse_js::symbol::Symbol;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;

pub struct ExportBinding<'a> {
  pub target: SourceRange<'a>,
  pub alias: SourceRange<'a>,
}

fn unwrap_block_statement_if_possible<'a>(session: &'a Session, node: &mut NodeData<'a>) {
  if let Syntax::BlockStmt { body } = &mut node.stx {
    if body.len() == 1 {
      let stmt = body[0].take(session);
      core::mem::swap(node, stmt);
    };
  };
}

// This should be run after the `minify_names` function.
pub struct Pass3<'a, 'b> {
  pub session: &'a Session,
  // Exports with the same exported name (including multiple default exports) are illegal, so we don't have to worry about/handle that case.
  pub export_bindings: &'b mut Vec<ExportBinding<'a>>,
  pub symbols: &'b mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
  pub scopes: &'b mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
}

impl<'a, 'b> Pass3<'a, 'b> {
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

impl<'a, 'b> Visitor<'a> for Pass3<'a, 'b> {
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
            if !min_scope.hoisted_vars.is_empty() {
              body.insert(
                0,
                new_node(self.session, scope, loc, Syntax::VarDecl {
                  export: false,
                  mode: VarDeclMode::Var,
                  declarators: {
                    let mut decls = self.session.new_vec();
                    for v in min_scope.hoisted_vars.iter() {
                      decls.push(VariableDeclarator {
                        pattern: new_node(self.session, scope, loc, Syntax::IdentifierPattern {
                          name: *v,
                        }),
                        initializer: None,
                      })
                    }
                    decls
                  },
                }),
              );
            }
            for fn_decl in min_scope.hoisted_functions.values_mut() {
              // TODO Batch prepend to avoid repeated Vec shifting.
              body.insert(0, fn_decl.take(self.session));
            }
          };
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
        parenthesised: _,
        is_async,
        generator: false,
        name: None,
        signature,
        body,
      } => {
        let fn_scope = body.scope;
        // TODO This will still work for named functions as long as that name isn't used (including if it's shadowed).
        // TODO Detect property access of "prototype" on variable referencing function to reduce (but not remove) false negatives.
        // TODO Can this work sometimes even when `arguments` is used?
        // TODO This is still not risk-free, as the function's prototype could still be used even if there is no `this`.
        // TODO Detect `function(){}.bind(this)`, which is pretty much risk free unless somehow Function.prototype.bind has been overridden. However, any other value for the first argument of `.bind` means that it is no longer safe.
        if !fn_scope
          .flags()
          .has_any(Flags::new() | ScopeFlag::UsesArguments | ScopeFlag::UsesThis)
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
        export: false,
        body,
        generator: false,
        is_async,
        name: Some(name),
        signature,
        ..
      } => {
        let fn_scope = body.scope;
        // TODO Consider `export function` and `export default function`.
        // TODO Detect property access of "prototype" on variable referencing function to reduce (but not remove) false negatives.
        // TODO Can this work sometimes even when `arguments` is used?
        // TODO This is still not risk-free, as the function's prototype could still be used even if there is no `this`.
        // TODO Detect `function(){}.bind(this)`, which is pretty much risk free unless somehow Function.prototype.bind has been overridden. However, any other value for the first argument of `.bind` means that it is no longer safe.
        if !fn_scope.flags().has_any(Flags::new() | ScopeFlag::UsesArguments | ScopeFlag::UsesThis)
          // Use `find_symbol` as we might not be in a closure scope and the function declaration's symbol would've been added to an ancestor.
          // If no symbol is found (e.g. global), or it exists but is not `is_used_as_constructor` and not `has_prototype`, then we can safely proceed.
          && scope.find_symbol(name.loc).and_then(|sym| self.symbols.get(&sym)).filter(|sym| sym.is_used_as_constructor || sym.has_prototype).is_none()
        {
          let var_decl_pat = new_node(
            self.session,
            // TODO Is this scope correct?
            scope,
            name.loc,
            Syntax::IdentifierPattern { name: name.loc },
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

          new_stx = Some(Syntax::VarDecl {
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
      Syntax::ClassDecl {
        export: true,
        export_default,
        name,
        ..
      }
      | Syntax::FunctionDecl {
        export: true,
        export_default,
        name,
        ..
      } => {
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
        export: true,
        declarators,
        ..
      } => {
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
                  target: e.target,
                  alias: match &e.alias.stx {
                    Syntax::IdentifierPattern { name } => *name,
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
        typ: ObjectMemberType::Shorthand { identifier },
      } => {
        let name = identifier.loc;
        if scope.find_symbol(name).is_some() {
          // See Syntax::ObjectPatternProperty match branch.
          let replacement_initializer_node = identifier.take(self.session);
          new_stx = Some(Syntax::ObjectMember {
            typ: ObjectMemberType::Valued {
              key: ClassOrObjectMemberKey::Direct(name),
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

  fn on_syntax_up(&mut self, node: &mut NodeData<'a>) -> () {
    match &mut node.stx {
      Syntax::IfStmt {
        consequent,
        alternate,
        ..
      } => {
        unwrap_block_statement_if_possible(self.session, consequent);
        if let Some(alt) = alternate {
          unwrap_block_statement_if_possible(self.session, alt);
        }
      }
      Syntax::WhileStmt { body, .. }
      | Syntax::DoWhileStmt { body, .. }
      | Syntax::ForStmt { body, .. } => {
        unwrap_block_statement_if_possible(self.session, body);
      }
      _ => {}
    };
  }
}
