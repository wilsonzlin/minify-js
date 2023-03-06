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
use parse_js::char::ID_CONTINUE_CHARSTR;
use parse_js::char::ID_START_CHARSTR;
use parse_js::flag::Flags;
use parse_js::lex::KEYWORD_STRS;
use parse_js::operator::OperatorName;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::session::SessionHashSet;
use parse_js::session::SessionVec;
use parse_js::source::SourceRange;
use parse_js::symbol::Identifier;
use parse_js::symbol::Scope;
use parse_js::symbol::ScopeFlag;
use parse_js::symbol::Symbol;
use parse_js::visit::JourneyControls;
use parse_js::visit::Visitor;
use std::str::from_utf8_unchecked;

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

// If statement optimisation:
// - `if (a) { b }`            => `a && b`             if `b` can be reduced to a single expression.
// - `if (a) { b } else { c }` => `a ? b : c`          if `b` and `c` can be reduced to a single expression.
// - `if (a) { b; return c }`  => `if (a) return b, c` if `b` can be reduced to a single expression.
// The last form is more for normalisation: it doesn't minify much by itself (it still remains a statement), but allows a containing `if` to optimise `if (a) { b; return c } d; return e` into `return a ? (b, c) : (d, e)` if `b` and `d` can be reduced to a single expression. Otherwise, we wouldn't be able to minify the containing `if`.
// Note that it's not possible for both branches to return, as a previous pass should have already unwrapped the unnecessary block. We also normalise it such that if only `else` returns, it's flipped, and then the `else` can be unwrapped.

// We only perform advanced statement analysis and transformation to expression in `if` and `else` blocks as that will allow opportunities to transform `if` into logical expressions. This isn't useful elsewhere, as a sequence of expression statements is the same size as a sequence of expressions separated by commas, so the fact it's an expression is not being leveraged.

// We first perform some analysis to see if it's even worthwhile to perform this optimisation.
fn analyse_if_branch<'a>(stx: &Syntax<'a>) -> bool {
  let Syntax::BlockStmt { body } = stx else {
    // We should have already normalised all `if` branches into a block if they were single statements, so this should not be possible.
    unreachable!();
  };
  let mut block_returned = false;
  let mut if_returned = false;
  for stmt in body.iter() {
    match &stmt.stx {
      Syntax::VarDecl {
        mode, declarators, ..
      } => {
        match mode {
          // We can make `var` declarations into expressions by hoisting the declaration part and leaving behind an assignment expression (if an initialiser exists).
          // TODO Support non-identifier patterns, although they may not be worth minifying if we have to hoist and therefore duplicate the variable names.
          VarDeclMode::Var => {
            if declarators.iter().any(|d| match d.pattern.stx {
              Syntax::IdentifierPattern { .. } => false,
              _ => true,
            }) {
              return false;
            }
          }
          // TODO We currently disallow if `let` or `const`, however there is a complex approach we could consider in the future: they're scoped to the block, so we can either wrap our optimised expression in a block or create a unique variable in the nearest closure and hoist it like `var`. Since the former means we're still left with a (block) statement, we choose the latter, but that means we can't do this if we're in the global scope as we're not allowed to introduce global variables (even if they're very unlikely to collide in reality). We'd have to replace all usages of these variables, however.
          VarDeclMode::Const | VarDeclMode::Let => return false,
        };
      }
      Syntax::ExpressionStmt { .. } => {}
      Syntax::ReturnStmt { .. } => block_returned = true,
      // Since we perform this optimisation bottom-up, any IfStmt should already be optimised, so if they were optimised and still exist as a statement, they should only have exactly one statement of `return` in `if` and `else`.
      Syntax::IfStmt {
        consequent: NodeData {
          stx: Syntax::ReturnStmt { .. },
          ..
        },
        alternate: None,
        ..
      } => if_returned = true,
      // Debugger and empty statements should already be removed.
      _ => return false,
    };
  }
  // We must only be left with at most one return statement (i.e. unconditional, although value can be conditional). Essentially, this means that if we have an `if (x) return`, we must have a block-level return, as otherwise we cannot represent it as a single `return`.
  !if_returned || block_returned
}

struct ProcessedIfBranch<'a> {
  expression: Node<'a>,
  hoisted_vars: SessionVec<'a, SourceRange<'a>>,
  // If true, it means that it's not an expression, but a single return statement with the expression as the return value.
  returns: bool,
}

fn process_if_branch_block<'a, 'b>(
  session: &'a Session,
  scope: Scope<'a>,
  body: &'b mut [Node<'a>],
) -> ProcessedIfBranch<'a> {
  let mut returns = false;
  let mut hoisted_vars: SessionVec<'a, SourceRange<'a>> = session.new_vec();
  let mut expressions: SessionVec<'a, Node<'a>> = session.new_vec();
  let mut i = 0;
  while i < body.len() {
    let loc = body[i].loc;
    let scope = body[i].scope;
    match &mut body[i].stx {
      Syntax::ExpressionStmt { expression } => {
        expressions.push(expression.take(session));
      }
      Syntax::ReturnStmt { value } => {
        returns = true;
        expressions.push(match value {
          Some(value) => value.take(session),
          None => new_node(session, scope, loc, Syntax::IdentifierExpr {
            name: SourceRange::from_slice(b"undefined"),
          }),
        });
      }
      Syntax::VarDecl {
        declarators,
        mode: VarDeclMode::Var,
        ..
      } => {
        for decl in declarators.iter_mut() {
          let target = decl.pattern.take(session);
          let Syntax::IdentifierPattern { name } = target.stx else {
            unreachable!();
          };
          hoisted_vars.push(name);
          if let Some(init) = &mut decl.initializer {
            let right = init.take(session);
            expressions.push(new_node(
              session,
              scope,
              name + init.loc,
              Syntax::BinaryExpr {
                parenthesised: false,
                operator: OperatorName::Assignment,
                left: target,
                right,
              },
            ));
          }
        }
      }
      Syntax::IfStmt {
        test,
        consequent:
          NodeData {
            stx: Syntax::ReturnStmt { value },
            loc: ret_loc,
            ..
          },
        alternate: None,
      } => {
        returns = true;

        // Take before we reborrow mutably for process_if_branch_block.
        let test = test.take(session);
        let consequent = value.as_mut().map(|v| v.take(session)).unwrap_or(new_node(
          session,
          scope,
          *ret_loc,
          Syntax::IdentifierExpr {
            name: SourceRange::from_slice(b"undefined"),
          },
        ));

        let mut remaining = process_if_branch_block(session, scope, &mut body[i + 1..]);
        assert!(remaining.returns);
        hoisted_vars.append(&mut remaining.hoisted_vars);
        let alternate = remaining.expression;
        expressions.push(new_node(session, scope, loc, Syntax::ConditionalExpr {
          parenthesised: false,
          test,
          consequent,
          alternate,
        }));
        break;
      }
      _ => unreachable!(),
    };
    i += 1;
  }

  ProcessedIfBranch {
    expression: expressions
      .into_iter()
      .reduce(|left, right| {
        new_node(session, scope, left.loc + right.loc, Syntax::BinaryExpr {
          parenthesised: false,
          operator: OperatorName::Comma,
          left,
          right,
        })
      })
      .unwrap(),
    hoisted_vars,
    returns,
  }
}

fn process_if_branch<'a, 'b>(
  session: &'a Session,
  scope: Scope<'a>,
  branch: &'b mut NodeData<'a>,
) -> ProcessedIfBranch<'a> {
  let Syntax::BlockStmt { body } = &mut branch.stx else {
    // We should have already normalised all `if` branches into a block if they were single statements, so this should not be possible.
    unreachable!()
  };
  process_if_branch_block(session, scope, body)
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
  // `var` declarations in this closure that need to be moved to allow for some optimisation.
  hoisted_vars: SessionVec<'a, Identifier<'a>>,
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

struct Ctx<'a, 'b> {
  session: &'a Session,
  symbols: &'b mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
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
}

// This should be run after `PretransformPass` visitor has run and before the `MinifyPass` visitor runs.
// The IdentifierPass pass collects all usages of variables to determine inherited variables for each scope, so we can know what minified names can be safely used (see `MinifiedNameGenerator`). This function will then go through each declaration in each scope and generate and update their corresponding `MinifySymbol.minified_name`.
// Some pecularities to note: globals aren't minified (whether declared or not), so when blacklisting minified names, they are directly disallowed. However, all other variables will be minified, so we need to blacklist their minified name, not their original name. This is why this function processes scopes top-down (from the root), as we need to know the minified names of ancestor variables first before we can blacklist them.
fn minify_names<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  minify_scopes: &mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
  minify_symbols: &mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
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
      Some(next_min_name.generate_next_available_minified_name(&minified_inherited_vars));
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

// - Detect all usages of JSX components, as React determines `<link>` to be the HTML tag and `<Link>` to be the variable `Link` as a component, so we cannot minify `Link` to `link` or `a0` or `bb` (i.e. make capitalised JSX elements uncapitalised).
// - Find all references of variables so we can determine inherited variables (see `MinifiedNameGenerator` and `MinifyScope`). This is because JS allows variables to be lexically references before they're used, so we cannot do this in the same pass. For example, `let b = 1; { let a = () => b; let b = 2; }`.
// - Find uses of `new <var>` and set `is_used_as_constructor`.
// - Find uses of `<var>.prototype` and set `has_prototype`.
// - Combine consecutive expression statements into one.
// - Convert `if (x) { expr; }` to `x && expr`.
// - Convert `if (x) { expr1; } else { expr2; }` to `x ? expr1 ; expr2`.
// - Concatenate addition of two literal strings.
// - Unwrap unnecessary block statements.
// - Drop debugger statements.
// - Normalise `if-else` branches into block statements.
struct IdentifierPass<'a, 'b> {
  ctx: Ctx<'a, 'b>,
}

fn stmt_has_return<'a>(stx: &Syntax<'a>) -> bool {
  match stx {
    Syntax::ReturnStmt { .. } => true,
    Syntax::BlockStmt { body } => body.iter().any(|n| stmt_has_return(&n.stx)),
    _ => false,
  }
}

impl<'a, 'b> Visitor<'a> for IdentifierPass<'a, 'b> {
  fn on_syntax_down(&mut self, n: &mut NodeData<'a>, _ctl: &mut JourneyControls) -> () {
    let scope = n.scope;
    match &mut n.stx {
      Syntax::IfStmt {
        consequent,
        alternate,
        ..
      } => {
        match &consequent.stx {
          Syntax::BlockStmt { .. } => {}
          _ => {
            let inner = consequent.take(self.ctx.session);
            let wrapped = new_node(
              self.ctx.session,
              inner.scope,
              inner.loc,
              Syntax::BlockStmt {
                body: {
                  let mut body = self.ctx.session.new_vec();
                  body.push(inner);
                  body
                },
              },
            );
            *consequent = wrapped;
          }
        };
        if let Some(alt) = alternate {
          match &alt.stx {
            Syntax::BlockStmt { .. } => {}
            _ => {
              let inner = alt.take(self.ctx.session);
              let wrapped = new_node(
                self.ctx.session,
                inner.scope,
                inner.loc,
                Syntax::BlockStmt {
                  body: {
                    let mut body = self.ctx.session.new_vec();
                    body.push(inner);
                    body
                  },
                },
              );
              *alt = wrapped;
            }
          }
        }
      }
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
          self.ctx.symbols.entry(sym).or_default().has_prototype = true;
        };
      }
      Syntax::UnaryExpr {
        parenthesised,
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
              .or_default()
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
            .or_default()
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
      Syntax::BinaryExpr {
        operator: OperatorName::Addition,
        left:
          NodeData {
            stx: Syntax::LiteralStringExpr { value: l },
            ..
          },
        right:
          NodeData {
            stx: Syntax::LiteralStringExpr { value: r },
            ..
          },
        ..
      } => {
        let concat = self
          .ctx
          .session
          .get_allocator()
          .alloc_slice_fill_default(l.len() + r.len());
        concat[..l.len()].copy_from_slice(l.as_bytes());
        concat[l.len()..].copy_from_slice(r.as_bytes());
        node.stx = Syntax::LiteralStringExpr {
          value: unsafe { from_utf8_unchecked(concat) },
        };
      }
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
            Syntax::ExpressionStmt { expression } => {
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
            let closure_scope = scope.find_self_or_ancestor(|t| t.is_closure()).unwrap();
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
            let closure_scope = scope.find_self_or_ancestor(|t| t.is_closure()).unwrap();
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
            assert!(cons_expr.returns && alt_expr.returns);
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

// - Move function declarations into `hoisted_functions`, so we can then place them back in the tree at the top of a closure in the next pass.
struct PretransformPass<'a, 'b> {
  ctx: Ctx<'a, 'b>,
}

impl<'a, 'b> Visitor<'a> for PretransformPass<'a, 'b> {
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

fn unwrap_block_statement_if_possible<'a>(session: &'a Session, node: &mut NodeData<'a>) {
  if let Syntax::BlockStmt { body } = &mut node.stx {
    if body.len() == 1 {
      let stmt = body[0].take(session);
      core::mem::swap(node, stmt);
    };
  };
}

// The third and main pass, that does most of the work. This should be run after the `minify_names` function.
struct MinifyPass<'a, 'b> {
  session: &'a Session,
  // Exports with the same exported name (including multiple default exports) are illegal, so we don't have to worry about/handle that case.
  export_bindings: &'b mut Vec<ExportBinding<'a>>,
  symbols: &'b mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
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
        parenthesised,
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

pub fn minify_js<'a>(session: &'a Session, top_level_node: &mut NodeData<'a>) -> () {
  let top_level_scope = top_level_node.scope;

  // Our custom data/state associated with a Symbol.
  let mut symbols = session.new_hashmap::<Symbol, MinifySymbol>();
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
