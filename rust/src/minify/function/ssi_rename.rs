use super::counter::Counter;
use super::inst::Arg;
use super::inst::CallArg;
use super::inst::Inst;
use super::visit::visit_inst_args;
use super::visit::visit_inst_tgts;
use ahash::AHashMap;
use croaring::Bitmap;

fn inner(
  rename_stacks: &mut AHashMap<u32, Vec<u32>>,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
  phi_orig_tgts: &AHashMap<(u32, usize), u32>,
  domtree: &AHashMap<u32, Bitmap>,
  label: u32,
  c_temp: &mut Counter,
) {
  let mut to_pop = AHashMap::<u32, usize>::new();

  // Replace arguments and targets in instructions.
  for mut inst in bblocks.get_mut(&label).unwrap().iter_mut() {
    if !matches!(inst, Inst::Phi { .. }) {
      visit_inst_args!(&mut inst, |arg| if let Some(tgt) = arg.maybe_var() {
        let new_arg = Arg::Var(*rename_stacks.get(&tgt).unwrap().last().unwrap());
        *arg = new_arg;
      });
    };
    visit_inst_tgts!(&mut inst, |var| {
      let new_var = c_temp.bump();
      rename_stacks.entry(*var).or_default().push(new_var);
      *to_pop.entry(*var).or_default() += 1;
      *var = new_var;
    });
  }

  for s in cfg_children[&label].iter() {
    for (inst_no, inst) in bblocks.get_mut(&s).unwrap().iter_mut().enumerate() {
      let Inst::Phi { from_blocks, .. } = inst else {
        // No more phi nodes.
        break;
      };
      let orig_tgt = phi_orig_tgts[&(s, inst_no)];
      let Some(&new_tgt) = rename_stacks.get(&orig_tgt).and_then(|s| s.last()) else {
        // TODO Delete phi now?
        continue;
      };
      assert!(from_blocks.insert(label, Arg::Var(new_tgt)).is_none());
    }
  }

  if let Some(children) = domtree.get(&label) {
    for c in children.iter() {
      inner(
        rename_stacks,
        bblocks,
        cfg_children,
        phi_orig_tgts,
        domtree,
        c,
        c_temp,
      );
    }
  }

  for (tgt, cnt) in to_pop {
    let s = rename_stacks.get_mut(&tgt).unwrap();
    for _ in 0..cnt {
      s.pop().unwrap();
    }
  }
}

pub(crate) fn rename_targets_for_ssi_construction(
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
  domtree: &AHashMap<u32, Bitmap>,
  c_temp: &mut Counter,
) {
  // Store the original `tgt` field values from all Inst::Phi.
  let mut phi_orig_tgts = AHashMap::<(u32, usize), u32>::new();
  for (label, insts) in bblocks.iter() {
    for (inst_no, inst) in insts.iter().enumerate() {
      let Inst::Phi { tgt, .. } = inst else {
        // No more Phi insts.
        break;
      };
      assert!(phi_orig_tgts.insert((*label, inst_no), *tgt).is_none());
    }
  }

  let mut rename_stacks = AHashMap::<u32, Vec<u32>>::new();
  inner(
    &mut rename_stacks,
    bblocks,
    cfg_children,
    &phi_orig_tgts,
    domtree,
    0,
    c_temp,
  );
  // Prune phi nodes.
  // WARNING: It's not logically correct to do this during the previous rename processing at any time.
  // TODO Do we need to replace usages of these pruned phi nodes?
  for bblock in bblocks.values_mut() {
    let mut to_delete = Vec::new();
    for (i, inst) in bblock.iter().enumerate() {
      let Inst::Phi { from_blocks, .. } = inst else {
        // No more phi nodes.
        break;
      };
      // TODO Why is it possible for there to be exactly one entry? What does that mean? How does that happen?
      if from_blocks.len() <= 1 {
        to_delete.push(i);
      };
    }
    for &i in to_delete.iter().rev() {
      bblock.remove(i);
    }
  }
}
