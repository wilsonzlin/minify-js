use parse_js::ast::new_node;
use parse_js::ast::Node;
use parse_js::ast::NodeData;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::operator::OperatorName;
use parse_js::session::Session;
use parse_js::session::SessionVec;
use parse_js::source::SourceRange;
use parse_js::symbol::Scope;

// If statement optimisation:
// - `if (a) { b }`            => `a && b`             if `b` can be reduced to a single expression.
// - `if (a) { b } else { c }` => `a ? b : c`          if `b` and `c` can be reduced to a single expression.
// - `if (a) { b; return c }`  => `if (a) return b, c` if `b` can be reduced to a single expression.
// The last form is more for normalisation: it doesn't minify much by itself (it still remains a statement), but allows a containing `if` to optimise `if (a) { b; return c } d; return e` into `return a ? (b, c) : (d, e)` if `b` and `d` can be reduced to a single expression. Otherwise, we wouldn't be able to minify the containing `if`.
// Note that it's not possible for both branches to return, as a previous pass should have already unwrapped the unnecessary block. We also normalise it such that if only `else` returns, it's flipped, and then the `else` can be unwrapped.

// We only perform advanced statement analysis and transformation to expression in `if` and `else` blocks as that will allow opportunities to transform `if` into logical expressions. This isn't useful elsewhere, as a sequence of expression statements is the same size as a sequence of expressions separated by commas, so the fact it's an expression is not being leveraged.

// We first perform some analysis to see if it's even worthwhile to perform this optimisation.
pub fn analyse_if_branch<'a>(stx: &Syntax<'a>) -> bool {
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

pub struct ProcessedIfBranch<'a> {
  pub expression: Node<'a>,
  pub hoisted_vars: SessionVec<'a, SourceRange<'a>>,
  // If true, it means that it's not an expression, but a single return statement with the expression as the return value.
  pub returns: bool,
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

pub fn process_if_branch<'a, 'b>(
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
