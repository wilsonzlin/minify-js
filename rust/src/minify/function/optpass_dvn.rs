use super::consteval::maybe_eval_const_bin_expr;
use super::consteval::maybe_eval_const_builtin_call;
use super::consteval::maybe_eval_const_builtin_val;
use super::inst::Arg;
use super::inst::BinOp;
use super::inst::CallArg;
use super::inst::Const;
use super::inst::Inst;
use super::inst::UnOp;
use ahash::AHashMap;
use croaring::Bitmap;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::mem::swap;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Val {
  Bin { left: Arg, op: BinOp, right: Arg },
  Un { op: UnOp, arg: Arg },
}

#[derive(Clone, Default)]
struct State {
  val_to_coc: AHashMap<Val, Arg>,
  tgt_to_coc: AHashMap<u32, Arg>,
}

fn inner(
  changed: &mut bool,
  state: &mut State,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
  domtree: &AHashMap<u32, Bitmap>,
  label: u32,
) {
  macro_rules! upsert_val {
    // If this is the first instance/computation of $val, all future uses will use $available_at_tgt instead.
    ($val:expr, $available_at_tgt:expr) => {
      match state.val_to_coc.entry($val.clone()) {
        Entry::Occupied(o) => (o.get().clone(), Some(o.get().clone())),
        Entry::Vacant(v) => {
          v.insert(Arg::Var($available_at_tgt));
          (Arg::Var($available_at_tgt), None)
        }
      }
    };
  }
  macro_rules! canon_arg {
    ($arg:expr) => {
      match $arg {
        Arg::Builtin(p) => match maybe_eval_const_builtin_val(p) {
          Some(value) => Arg::Const(value),
          None => Arg::Builtin(p.clone()),
        },
        Arg::Const(c) => Arg::Const(c.clone()),
        Arg::Var(tgt) => match state.tgt_to_coc.entry(*tgt) {
          Entry::Occupied(o) => o.get().clone(),
          // We haven't seen this variable before, so it must be from a back edge. Therefore, we must leave it as is.
          Entry::Vacant(_) => Arg::Var(*tgt),
        }
      }
    };
  }
  let orig_state = state.clone();
  for inst in bblocks.get_mut(&label).unwrap().iter_mut() {
    // We still need to normalise args in instructions in case we don't rewrite entire instruction to a VarAssign.
    // Do a `&*` to ensure we don't mutate `inst` within the `match`. (Important as changes must be marked, so we should have one place for changing.)
    let new_inst = match &*inst {
      Inst::Bin {
        tgt,
        left,
        op,
        right,
      } => {
        let mut left_coc = canon_arg!(left);
        let mut right_coc = canon_arg!(right);
        // Remember: `a + b` isn't commutative if either is a string.
        let commutative = match (op, &left_coc, &right_coc) {
          (BinOp::Add, Arg::Const(Const::Num(_)), Arg::Const(Const::Num(_))) => true,
          (BinOp::Mul, Arg::Const(Const::Num(_)), Arg::Const(Const::Num(_))) => true,
          _ => false,
        };
        if commutative && left_coc > right_coc {
          swap(&mut left_coc, &mut right_coc);
        };
        let consteval = match (*op, &left_coc, &right_coc) {
          (op, Arg::Const(l), Arg::Const(r)) => maybe_eval_const_bin_expr(op, l, r).map(Arg::Const),
          (BinOp::GetProp, Arg::Builtin(o), Arg::Const(Const::Str(p))) => {
            Some(Arg::Builtin(format!("{o}.{p}")))
          }
          _ => None,
        };
        match consteval {
          Some(value) => {
            assert!(state.tgt_to_coc.insert(*tgt, value.clone()).is_none());
            Some(Inst::VarAssign { tgt: *tgt, value })
          }
          None => {
            let val = Val::Bin {
              left: left_coc.clone(),
              op: *op,
              right: right_coc.clone(),
            };
            let (row, existing) = upsert_val!(val, *tgt);
            assert!(state.tgt_to_coc.insert(*tgt, row).is_none());
            Some(match existing {
              Some(value) => Inst::VarAssign { tgt: *tgt, value },
              None => Inst::Bin {
                tgt: *tgt,
                left: left_coc,
                op: *op,
                right: right_coc,
              },
            })
          }
        }
      }
      Inst::Un { tgt, arg, op } => {
        let arg_coc = canon_arg!(arg);
        let val = Val::Un {
          op: *op,
          arg: arg_coc.clone(),
        };
        let (row, existing) = upsert_val!(val, *tgt);
        assert!(state.tgt_to_coc.insert(*tgt, row).is_none());
        Some(match existing {
          Some(value) => Inst::VarAssign { tgt: *tgt, value },
          None => Inst::Un {
            tgt: *tgt,
            op: *op,
            arg: arg_coc,
          },
        })
      }
      Inst::VarAssign { tgt, value } => {
        let coc = canon_arg!(value);
        assert!(state.tgt_to_coc.insert(*tgt, coc.clone()).is_none());
        Some(Inst::VarAssign {
          tgt: *tgt,
          value: coc,
        })
      }
      Inst::ForeignLoad { .. } => None,
      Inst::ForeignStore { from, to } => Some(Inst::ForeignStore {
        from: canon_arg!(from),
        to: *to,
      }),
      Inst::UnknownLoad { .. } => None,
      Inst::UnknownStore { from, to } => Some(Inst::UnknownStore {
        from: canon_arg!(from),
        to: to.clone(),
      }),
      Inst::Call {
        tgt,
        func,
        this,
        args,
      } => {
        let canon_args = args
          .iter()
          .map(|a| match a {
            CallArg::Arg(a) => CallArg::Arg(canon_arg!(a)),
            CallArg::Spread(a) => CallArg::Spread(canon_arg!(a)),
          })
          .collect_vec();
        // TODO If constevalable and `tgt` is None.
        let maybe_consteval = match (tgt, func, args) {
          (Some(_), Arg::Builtin(func), args)
            if args
              .iter()
              .all(|a| matches!(a, CallArg::Arg(Arg::Const(_)))) =>
          {
            maybe_eval_const_builtin_call(
              &func,
              &args.iter().map(|a| a.as_arg().to_const()).collect_vec(),
            )
          }
          _ => None,
        };
        Some(match maybe_consteval {
          Some(value) => Inst::VarAssign {
            tgt: tgt.unwrap(),
            value: Arg::Const(value),
          },
          // Only pure functions can be reused, and we pessimistically assume all functions are non-pure.
          None => Inst::Call {
            tgt: *tgt,
            func: canon_arg!(func),
            this: canon_arg!(this),
            args: canon_args,
          },
        })
      }
      Inst::PropAssign { obj, prop, value } => Some(Inst::PropAssign {
        obj: canon_arg!(obj),
        prop: canon_arg!(prop),
        value: canon_arg!(value),
      }),
      // In the original Keith-Cooper paper, phis are processed first. This is still the case here, as Phi instructions are always first.
      Inst::Phi { tgt, from_blocks } => Some(Inst::Phi {
        tgt: *tgt,
        from_blocks: from_blocks
          .iter()
          .map(|(label, arg)| (*label, canon_arg!(arg)))
          .collect(),
      }),
      Inst::CondGoto { cond, label } => Some(Inst::CondGoto {
        cond: canon_arg!(cond),
        label: *label,
      }),
      Inst::NotCondGoto { cond, label } => Some(Inst::NotCondGoto {
        cond: canon_arg!(cond),
        label: *label,
      }),
      Inst::Goto { .. } | Inst::Label { .. } => None,
    };
    if let Some(new_inst) = new_inst {
      if inst != &new_inst {
        *changed = true;
        *inst = new_inst;
      }
    };
  }

  for s in cfg_children[&label].iter() {
    for inst in bblocks.get_mut(&s).unwrap().iter_mut() {
      let Inst::Phi { from_blocks, .. } = inst else {
        // No more phi nodes.
        break;
      };
      // TODO Is the following algorithm correct?
      let Some(ex) = from_blocks.get_mut(&label) else {
        continue;
      };
      let coc = canon_arg!(ex);
      if ex != &coc {
        *changed = true;
        *ex = coc;
      };
    }
  }

  if let Some(children) = domtree.get(&label) {
    for c in children.iter() {
      inner(changed, state, bblocks, cfg_children, domtree, c);
    }
  }

  *state = orig_state;
}

/// Dominator-based value numbering.
/// - https://www.cs.tufts.edu/~nr/cs257/archive/keith-cooper/value-numbering.pdf
/// - https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/global-value-numbering/
///
/// This performs:
///
/// - Common subexpression elimination
/// - Copy propagation
/// - Const propagation
/// - Const evaluation
pub(crate) fn optpass_dvn(
  changed: &mut bool,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
  domtree: &AHashMap<u32, Bitmap>,
) {
  let mut state = State::default();
  inner(changed, &mut state, bblocks, cfg_children, domtree, 0);
}
