use super::builtin::BUILTINS;
use super::counter::Counter;
use super::inst::Arg;
use super::inst::BinOp;
use super::inst::CallArg;
use super::inst::Const;
use super::inst::Inst;
use super::inst::UnOp;
use ahash::AHashMap;
use parse_js::ast::ClassOrObjectMemberKey;
use parse_js::ast::ForInit;
use parse_js::ast::Node;
use parse_js::ast::Syntax;
use parse_js::ast::VariableDeclarator;
use parse_js::num::JsNumber;
use parse_js::operator::OperatorName;
use symbol_js::symbol::Scope;
use symbol_js::symbol::Symbol;
use std::collections::VecDeque;

struct SourceToInst<'c_temp, 'c_label> {
  out: Vec<Inst>,
  c_temp: &'c_temp mut Counter,
  c_label: &'c_label mut Counter,
  symbol_to_temp: AHashMap<Symbol, u32>,
  break_stack: Vec<u32>, // Upon `break`, generate Inst::Goto to the label at the top of this stack.
}

enum VarType {
  Local(Symbol),
  Foreign(Symbol),
  Unknown(String),
  Builtin(String),
}

impl VarType {
  pub fn from_scope_name(scope: &Scope, name: String) -> VarType {
    match scope.find_symbol_up_to(name, |s| s.is_closure()) {
      Some(local) => VarType::Local(local),
      None => match scope.find_symbol(name) {
        Some(foreign) => VarType::Foreign(foreign),
        None => match BUILTINS.get(name.as_str()) {
          Some(_) => VarType::Builtin(name),
          None => VarType::Unknown(name),
        },
      },
    }
  }
}

impl<'c_temp, 'c_label> SourceToInst<'c_temp, 'c_label> {
  fn symbol_to_temp(&mut self, sym: Symbol) -> u32 {
    *self
      .symbol_to_temp
      .entry(sym)
      .or_insert_with(|| self.c_temp.bump())
  }

  fn compile_arg(&mut self, n: &Node) -> Arg {
    match n.stx.as_ref() {
      Syntax::LiteralBooleanExpr { value } => Arg::Const(Const::Bool(*value)),
      Syntax::LiteralNumberExpr { value } => Arg::Const(Const::Num(*value)),
      Syntax::LiteralStringExpr { value } => Arg::Const(Const::Str(value.to_string())),
      // Recurse into expression.
      Syntax::BinaryExpr { .. }
      | Syntax::CallExpr { .. }
      | Syntax::ConditionalExpr { .. }
      | Syntax::IdentifierExpr { .. }
      | Syntax::MemberExpr { .. }
      | Syntax::UnaryExpr { .. }
      | Syntax::UnaryPostfixExpr { .. } => {
        self.compile_expr(n);
        match self.out.last().unwrap() {
          Inst::Bin { tgt, .. }
          | Inst::Un { tgt, .. }
          | Inst::VarAssign { tgt, .. }
          | Inst::Call { tgt: Some(tgt), .. }
          | Inst::ForeignLoad { to: tgt, .. }
          | Inst::UnknownLoad { to: tgt, .. } => Arg::Var(*tgt),
          inst => panic!("do not know how to get target from {inst:?}"),
        }
      }
      n => panic!("unknown arg: {n:?}"),
    }
  }

  fn compile_expr(&mut self, n: &Node) {
    match n.stx.as_ref() {
      // Due to conditional chaining requiring us to know the location after the entire chain (and not just a single node/subexpression), upon entry into any node that is possibly part of a chain, we run an inner algorithm that flattens the chain nodes and then compiles left to right.
      // TODO `(a?.b).c` is not the same as `a?.b.c`.
      Syntax::CallExpr { .. } | Syntax::MemberExpr { .. } | Syntax::ComputedMemberExpr { .. } => {
        let mut chain = Vec::new();
        let mut q = VecDeque::new();
        q.push_back(n);
        while let Some(n) = q.pop_front() {
          chain.push(n);
          match n.stx.as_ref() {
            Syntax::CallExpr { callee, .. } => {
              q.push_back(&*callee);
            }
            Syntax::MemberExpr { left, .. } => {
              q.push_back(&*left);
            }
            Syntax::ComputedMemberExpr { object, .. } => {
              q.push_back(&*object);
            }
            _ => {}
          };
        }
        chain.reverse();

        let after_chain_label_id = self.c_label.bump();
        // We must repeatedly assign to this target instead of creating new temporaries. Otherwise, consider `b?.c?.d?.e`. What's the target that the subsequent code can use for the result of this chain?
        let res_tmp_var = self.c_temp.bump();
        let mut did_optional_chaining = false;
        let mut last_last_arg = None;
        let mut last_arg = None;
        let mut prev_was_member = false;
        for n in chain {
          match n.stx.as_ref() {
            Syntax::MemberExpr {
              optional_chaining, ..
            }
            | Syntax::ComputedMemberExpr {
              optional_chaining, ..
            }
            | Syntax::CallExpr {
              optional_chaining, ..
            } if *optional_chaining => {
              did_optional_chaining = true;
              let is_undefined_tmp_var = self.c_temp.bump();
              self.out.push(Inst::Bin {
                tgt: is_undefined_tmp_var,
                left: last_arg.clone().unwrap(),
                op: BinOp::LooseEq,
                right: Arg::Const(Const::Null),
              });
              self.out.push(Inst::CondGoto {
                cond: Arg::Var(is_undefined_tmp_var),
                label: after_chain_label_id,
              });
            }
            _ => {}
          };
          let next_arg = match n.stx.as_ref() {
            Syntax::MemberExpr { .. } | Syntax::ComputedMemberExpr { .. } => {
              let next_arg_tmp_var = self.c_temp.bump();
              let right_arg = match n.stx.as_ref() {
                Syntax::MemberExpr { right, .. } => Arg::Const(Const::Str(right.to_string())),
                Syntax::ComputedMemberExpr { member, .. } => self.compile_arg(&member),
                _ => unreachable!(),
              };
              self.out.push(Inst::Bin {
                tgt: next_arg_tmp_var,
                left: last_arg.clone().unwrap(),
                op: BinOp::GetProp,
                right: right_arg,
              });
              Arg::Var(next_arg_tmp_var)
            }
            Syntax::CallExpr { arguments, .. } => {
              let next_arg_tmp_var = self.c_temp.bump();
              let mut args = Vec::new();
              for a in arguments {
                let Syntax::CallArg { spread: s, value } = a.stx.as_ref() else {
                  unreachable!();
                };
                args.push(if *s {
                  CallArg::Spread(self.compile_arg(&value))
                } else {
                  CallArg::Arg(self.compile_arg(&value))
                });
              }
              self.out.push(Inst::Call {
                tgt: Some(next_arg_tmp_var),
                func: last_arg.clone().unwrap(),
                this: last_last_arg
                  .clone()
                  .filter(|_| prev_was_member)
                  .unwrap_or_else(|| Arg::Const(Const::Undefined)),
                args,
              });
              Arg::Var(next_arg_tmp_var)
            }
            _ => self.compile_arg(n),
          };
          self.out.push(Inst::VarAssign {
            tgt: res_tmp_var,
            value: next_arg.clone(),
          });
          prev_was_member = match n.stx.as_ref() {
            Syntax::MemberExpr { .. } | Syntax::ComputedMemberExpr { .. } => true,
            _ => false,
          };
          last_last_arg = last_arg;
          last_arg = Some(next_arg);
        }
        if did_optional_chaining {
          self.out.push(Inst::Label {
            label: after_chain_label_id,
          });
          // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
          self.out.push(Inst::VarAssign {
            tgt: res_tmp_var,
            value: Arg::Var(res_tmp_var),
          });
        };
      }
      // This branch is rare, but can happen (e.g. redundant no-op statement like `a;`).
      Syntax::IdentifierExpr { name } => {
        let inst = match VarType::from_scope_name(n.assoc.get::<Scope>().unwrap(), name.clone()) {
          // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
          VarType::Local(local) => Inst::VarAssign {
            tgt: self.symbol_to_temp(local),
            value: Arg::Var(self.symbol_to_temp(local)),
          },
          // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
          VarType::Builtin(builtin) => Inst::VarAssign {
            tgt: self.c_temp.bump(),
            value: Arg::Builtin(builtin),
          },
          VarType::Foreign(foreign) => Inst::ForeignLoad {
            from: foreign,
            to: self.c_temp.bump(),
          },
          VarType::Unknown(name) => Inst::UnknownLoad {
            from: name,
            to: self.c_temp.bump(),
          },
        };
        self.out.push(inst);
      }
      Syntax::ConditionalExpr {
        test,
        consequent,
        alternate,
        ..
      } => {
        let res_tmp_var = self.c_temp.bump();
        let test_arg = self.compile_arg(&test);
        let cons_label_id = self.c_label.bump();
        let after_label_id = self.c_label.bump();
        self.out.push(Inst::CondGoto {
          cond: test_arg,
          label: cons_label_id,
        });
        let alt_res = self.compile_arg(&alternate);
        self.out.push(Inst::VarAssign {
          tgt: res_tmp_var,
          value: alt_res,
        });
        self.out.push(Inst::Goto {
          label: after_label_id,
        });
        self.out.push(Inst::Label {
          label: cons_label_id,
        });
        let cons_res = self.compile_arg(&consequent);
        self.out.push(Inst::VarAssign {
          tgt: res_tmp_var,
          value: cons_res,
        });
        self.out.push(Inst::Label {
          label: after_label_id,
        });
        // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
        self.out.push(Inst::VarAssign {
          tgt: res_tmp_var,
          value: Arg::Var(res_tmp_var),
        });
      }
      Syntax::UnaryPostfixExpr {
        operator, argument, ..
      } => {
        let arg = self.compile_arg(&argument);
        let tmp_var = self.c_temp.bump();
        self.out.push(Inst::VarAssign {
          tgt: tmp_var,
          value: arg.clone(),
        });
        self.out.push(Inst::Bin {
          tgt: arg.clone().to_var(),
          left: arg,
          right: Arg::Const(Const::Num(JsNumber(1.0))),
          op: match operator {
            OperatorName::PostfixDecrement => BinOp::Sub,
            OperatorName::PostfixIncrement => BinOp::Add,
            _ => unreachable!(),
          },
        });
        // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
        self.out.push(Inst::VarAssign {
          tgt: tmp_var,
          value: Arg::Var(tmp_var),
        });
      }
      Syntax::UnaryExpr {
        operator, argument, ..
      } => {
        match operator {
          // Prefix increment/decrement.
          OperatorName::PrefixDecrement | OperatorName::PrefixIncrement => {
            let arg = self.compile_arg(&argument);
            self.out.push(Inst::Bin {
              tgt: arg.to_var(),
              left: arg,
              right: Arg::Const(Const::Num(JsNumber(1.0))),
              op: match operator {
                OperatorName::PrefixDecrement => BinOp::Sub,
                OperatorName::PrefixIncrement => BinOp::Add,
                _ => unreachable!(),
              },
            });
          }
          // Other expressions.
          _ => {
            let op = match operator {
              OperatorName::UnaryNegation => UnOp::Neg,
              _ => unimplemented!(),
            };
            let arg = self.compile_arg(&argument);
            self.out.push(Inst::Un {
              tgt: self.c_temp.bump(),
              op,
              arg,
            });
          }
        }
      }
      Syntax::BinaryExpr {
        operator,
        left,
        right,
        ..
      } => {
        // TODO Shorthand logic for `&&=` and `||=`.
        if operator.is_assignment()
          && !matches!(
            operator,
            OperatorName::AssignmentLogicalAnd | OperatorName::AssignmentLogicalOr
          )
        {
          // WARNING: We assume that the LHS of an assignment will never contain a conditional chaining anywhere in the chain, and that this is enforced at a previous stage (e.g. parsing).
          let dummy_val = Arg::Const(Const::Num(JsNumber(0xdeadbeefu32 as f64)));
          let mut ass_inst = match left.stx.as_ref() {
            Syntax::IdentifierPattern { name } => {
              let vartype = VarType::from_scope_name(n.assoc.get::<Scope>().unwrap(), name.clone());
              match vartype {
                VarType::Local(l) => Inst::VarAssign {
                  tgt: self.symbol_to_temp(l),
                  value: dummy_val, // This will be replaced later.
                },
                VarType::Foreign(f) => Inst::ForeignStore {
                  from: dummy_val, // This will be replaced later.
                  to: f,
                },
                VarType::Unknown(n) => Inst::UnknownStore {
                  from: dummy_val, // This will be replaced later.
                  to: n,
                },
                VarType::Builtin(builtin) => panic!("assignment to builtin {builtin}"),
              }
            }
            Syntax::MemberExpr { left, right, .. } => {
              let left_arg = self.compile_arg(&left);
              Inst::PropAssign {
                obj: left_arg,
                prop: Arg::Const(Const::Str(right.to_string())),
                value: dummy_val, // This will be replaced later.
              }
            }
            Syntax::ComputedMemberExpr { object, member, .. } => {
              let left_arg = self.compile_arg(&object);
              let member_arg = self.compile_arg(&member);
              Inst::PropAssign {
                obj: left_arg,
                prop: member_arg,
                value: dummy_val, // This will be replaced later.
              }
            }
            _ => unreachable!(),
          };
          let mut value = self.compile_arg(&right);
          if *operator != OperatorName::Assignment {
            let op = match operator {
              OperatorName::AssignmentAddition => BinOp::Add,
              OperatorName::AssignmentSubtraction => BinOp::Sub,
              OperatorName::AssignmentMultiplication => BinOp::Mul,
              OperatorName::AssignmentDivision => BinOp::Div,
              _ => unimplemented!(),
            };
            let left_arg = match &ass_inst {
              Inst::VarAssign { tgt, .. } => Arg::Var(*tgt),
              Inst::ForeignStore { to, .. } => {
                let left_tmp_var = self.c_temp.bump();
                self.out.push(Inst::ForeignLoad {
                  from: *to,
                  to: left_tmp_var,
                });
                Arg::Var(left_tmp_var)
              }
              Inst::UnknownStore { to, .. } => {
                let left_tmp_var = self.c_temp.bump();
                self.out.push(Inst::UnknownLoad {
                  from: to.clone(),
                  to: left_tmp_var,
                });
                Arg::Var(left_tmp_var)
              }
              Inst::PropAssign { obj, prop, .. } => {
                let left_tmp_var = self.c_temp.bump();
                self.out.push(Inst::Bin {
                  left: obj.clone(),
                  op: BinOp::GetProp,
                  right: prop.clone(),
                  tgt: left_tmp_var,
                });
                Arg::Var(left_tmp_var)
              }
              _ => unreachable!(),
            };
            let rhs_tmp_var = self.c_temp.bump();
            let rhs_inst = Inst::Bin {
              tgt: rhs_tmp_var,
              left: left_arg,
              op,
              right: value,
            };
            self.out.push(rhs_inst);
            value = Arg::Var(rhs_tmp_var);
          };
          match &mut ass_inst {
            Inst::VarAssign { value: v, .. }
            | Inst::ForeignStore { from: v, .. }
            | Inst::UnknownStore { from: v, .. }
            | Inst::PropAssign { value: v, .. } => *v = value,
            _ => unreachable!(),
          };
          self.out.push(ass_inst);
        } else if matches!(operator, OperatorName::LogicalAnd | OperatorName::LogicalOr) {
          let converge_label_id = self.c_label.bump();
          let res_tmp_var = self.c_temp.bump();
          let left = self.compile_arg(&left);
          self.out.push(Inst::VarAssign {
            tgt: res_tmp_var,
            value: left.clone(),
          });
          self.out.push(match operator {
            // Given `a && b`, skip `b` only if NOT `a`.
            OperatorName::LogicalAnd => Inst::NotCondGoto {
              cond: left,
              label: converge_label_id,
            },
            // Given `a || b`, skip `b` only IF `a`.
            OperatorName::LogicalOr => Inst::CondGoto {
              cond: left,
              label: converge_label_id,
            },
            _ => unreachable!(),
          });
          let right = self.compile_arg(&right);
          self.out.push(Inst::VarAssign {
            tgt: res_tmp_var,
            value: right,
          });
          self.out.push(Inst::Label {
            label: converge_label_id,
          });
          // TODO This seems hacky, but our implementation requires that the last Inst in the `out` stack represents the value/target to use.
          self.out.push(Inst::VarAssign {
            tgt: res_tmp_var,
            value: Arg::Var(res_tmp_var),
          });
        } else {
          let op = match operator {
            OperatorName::Addition => BinOp::Add,
            OperatorName::Division => BinOp::Div,
            OperatorName::LessThan => BinOp::Lt,
            OperatorName::Multiplication => BinOp::Mul,
            OperatorName::StrictEquality => BinOp::StrictEq,
            OperatorName::Subtraction => BinOp::Sub,
            _ => unimplemented!(),
          };
          let left = self.compile_arg(&left);
          let right = self.compile_arg(&right);
          self.out.push(Inst::Bin {
            tgt: self.c_temp.bump(),
            left,
            op,
            right,
          });
        }
      }
      n => panic!("not yet implemented: {n:?}"),
    };
  }

  // Handle `[a = 1] = x;` or `{b: c = 2} = y;`.
  fn compile_destructuring_via_prop(
    &mut self,
    obj: Arg,
    prop: Arg,
    target: &Node, // Pattern.
    default_value: Option<&Node>,
  ) {
    let tmp_var = self.c_temp.bump();
    self.out.push(Inst::Bin {
      tgt: tmp_var,
      left: obj,
      op: BinOp::GetProp,
      right: prop,
    });
    if let Some(dv) = default_value {
      // Compile default value. If `%tmp` is undefined, we need to assign `e.default_value` to it.
      let after_label_id = self.c_label.bump();
      let is_undefined_tmp_var = self.c_temp.bump();
      self.out.push(Inst::Bin {
        tgt: is_undefined_tmp_var,
        left: Arg::Var(tmp_var),
        op: BinOp::StrictEq,
        right: Arg::Const(Const::Undefined),
      });
      self.out.push(Inst::NotCondGoto {
        cond: Arg::Var(is_undefined_tmp_var),
        label: after_label_id,
      });
      let dv_arg = self.compile_arg(&dv);
      self.out.push(Inst::VarAssign {
        tgt: tmp_var,
        value: dv_arg,
      });
      self.out.push(Inst::Label {
        label: after_label_id,
      });
    };
    self.compile_destructuring(target, Arg::Var(tmp_var));
  }

  fn compile_destructuring(&mut self, pat: &Node, rval: Arg) {
    match pat.stx.as_ref() {
      Syntax::ArrayPattern { elements, rest } => {
        for (i, e) in elements.iter().enumerate() {
          let Some(e) = e else {
            continue;
          };
          self.compile_destructuring_via_prop(
            rval.clone(),
            Arg::Const(Const::Num(JsNumber(i as f64))),
            &e.target,
            e.default_value.as_ref(),
          );
        }
        // TODO `rest`.
      }
      Syntax::ObjectPattern { properties, rest } => {
        for p in properties {
          let Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
            ..
          } = p.stx.as_ref()
          else {
            unreachable!();
          };
          let prop = match key {
            ClassOrObjectMemberKey::Direct(d) => Arg::Const(Const::Str(d.to_string())),
            ClassOrObjectMemberKey::Computed(c) => self.compile_arg(c),
          };
          self.compile_destructuring_via_prop(rval.clone(), prop, target, default_value.as_ref());
        }
        // TODO `rest`.
      }
      Syntax::IdentifierPattern { name } => {
        // NOTE: It's possible to destructure-assign to ancestor scope vars (including globals), so just because this is a pattern doesn't mean it's for a local var.
        let inst = match VarType::from_scope_name(pat.assoc.get::<Scope>().unwrap(), name.clone()) {
          VarType::Local(local) => Inst::VarAssign {
            tgt: self.symbol_to_temp(local),
            value: rval.clone(),
          },
          VarType::Foreign(foreign) => Inst::ForeignStore {
            from: rval.clone(),
            to: foreign,
          },
          VarType::Unknown(unknown) => Inst::UnknownStore {
            from: rval.clone(),
            to: unknown,
          },
          VarType::Builtin(builtin) => panic!("assignment to builtin {builtin}"),
        };
        self.out.push(inst);
      }
      _ => unreachable!(),
    };
  }

  fn compile_stmt(&mut self, n: &Node) {
    match n.stx.as_ref() {
      Syntax::BlockStmt { body } => {
        for stmt in body {
          self.compile_stmt(stmt);
        }
      }
      Syntax::BreakStmt { label } => {
        // TODO Label.
        self.out.push(Inst::Goto {
          label: *self.break_stack.last().unwrap(),
        });
      }
      Syntax::ExpressionStmt { expression } => {
        self.compile_expr(expression);
      }
      Syntax::ForStmt {
        init,
        condition,
        post,
        body,
      } => {
        match init {
          ForInit::None => {}
          ForInit::Expression(e) => self.compile_expr(e),
          ForInit::Declaration(d) => self.compile_stmt(d),
        };
        let loop_entry_label = self.c_label.bump();
        let after_loop_label = self.c_label.bump();
        self.out.push(Inst::Label {
          label: loop_entry_label,
        });
        if let Some(condition) = condition {
          let cond_arg = self.compile_arg(&condition);
          self.out.push(Inst::NotCondGoto {
            cond: cond_arg,
            label: after_loop_label,
          });
        };
        self.break_stack.push(after_loop_label);
        self.compile_stmt(&body);
        self.break_stack.pop().unwrap();
        if let Some(post) = post {
          self.compile_expr(post);
        };
        self.out.push(Inst::Goto {
          label: loop_entry_label,
        });
        self.out.push(Inst::Label {
          label: after_loop_label,
        });
      }
      Syntax::IfStmt {
        test,
        consequent,
        alternate,
      } => {
        let test_arg = self.compile_arg(&test);
        match alternate {
          Some(alternate) => {
            let cons_label_id = self.c_label.bump();
            let after_label_id = self.c_label.bump();
            self.out.push(Inst::CondGoto {
              cond: test_arg,
              label: cons_label_id,
            });
            self.compile_stmt(&alternate);
            self.out.push(Inst::Goto {
              label: after_label_id,
            });
            self.out.push(Inst::Label {
              label: cons_label_id,
            });
            self.compile_stmt(&consequent);
            self.out.push(Inst::Label {
              label: after_label_id,
            });
          }
          None => {
            let after_label_id = self.c_label.bump();
            self.out.push(Inst::NotCondGoto {
              cond: test_arg,
              label: after_label_id,
            });
            self.compile_stmt(&consequent);
            self.out.push(Inst::Label {
              label: after_label_id,
            });
          }
        }
      }
      // TODO export.
      Syntax::VarDecl { declarators, .. } => {
        for VariableDeclarator {
          initializer,
          pattern,
        } in declarators
        {
          // TODO `initializer` must exist if `pattern` isn't IdentifierPattern (e.g. `var [a]; var {b};`).
          let Some(init) = initializer else {
            continue;
          };
          let tmp = self.c_temp.bump();
          let rval = self.compile_arg(&init);
          self.out.push(Inst::VarAssign {
            tgt: tmp,
            value: rval,
          });
          self.compile_destructuring(&pattern, Arg::Var(tmp));
        }
      }
      Syntax::WhileStmt { condition, body } => {
        let before_test_label = self.c_label.bump();
        let after_loop_label = self.c_label.bump();
        self.out.push(Inst::Label {
          label: before_test_label,
        });
        let test_arg = self.compile_arg(&condition);
        self.out.push(Inst::NotCondGoto {
          cond: test_arg,
          label: after_loop_label,
        });
        self.break_stack.push(after_loop_label);
        self.compile_stmt(body);
        self.break_stack.pop();
        self.out.push(Inst::Goto {
          label: before_test_label,
        });
        self.out.push(Inst::Label {
          label: after_loop_label,
        });
      }
      _ => unreachable!(),
    };
  }
}

pub(crate) fn translate_source_to_inst(
  top_level: &Node,
  c_label: &mut Counter,
  c_temp: &mut Counter,
) -> Vec<Inst> {
  let Syntax::TopLevel { body } = top_level.stx.as_ref() else {
    panic!("not top level node");
  };
  let mut compiler = SourceToInst {
    c_label,
    c_temp,
    out: Vec::new(),
    symbol_to_temp: AHashMap::new(),
    break_stack: Vec::new(),
  };
  for stmt in body {
    compiler.compile_stmt(&stmt)
  }
  compiler.out
}
