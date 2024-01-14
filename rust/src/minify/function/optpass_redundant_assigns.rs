use super::inst::Arg;
use super::inst::CallArg;
use super::inst::Inst;
use super::visit::visit_inst_args;
use ahash::AHashMap;

// VarAssigns are always useless in strict SSA. However, dominator-based value numbering doesn't manage to detect and remove all such insts, with one reason being that DVNT only traverses domtree children.
// My theory for correctness:
// - Strict SSA requires all defs to dominate all their uses.
// - Targets are only assigned in one place globally.
pub(crate) fn optpass_redundant_assigns(changed: &mut bool, bblocks: &mut AHashMap<u32, Vec<Inst>>) {
  let mut tgt_to_arg = AHashMap::new();
  for bblock in bblocks.values_mut() {
    let mut to_delete = Vec::new();
    for (i, inst) in bblock.iter().enumerate() {
      let Inst::VarAssign { tgt, value } = inst else {
        continue;
      };
      to_delete.push(i);
      assert!(tgt_to_arg.insert(*tgt, value.clone()).is_none());
    }
    for i in to_delete.into_iter().rev() {
      bblock.remove(i);
      *changed = true;
    }
  }
  for bblock in bblocks.values_mut() {
    for inst in bblock.iter_mut() {
      visit_inst_args!(inst, |arg| match arg {
        Arg::Var(t) => match tgt_to_arg.get(t) {
          None => {}
          Some(new_arg) => *arg = new_arg.clone(),
        },
        _ => {}
      });
    }
  }
}
