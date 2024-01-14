use super::inst::Arg;
use super::inst::CallArg;
use super::inst::Inst;
use super::visit::visit_inst_args;
use super::visit::visit_inst_tgts;
use ahash::AHashMap;
use croaring::Bitmap;

pub(crate) fn analyse_single_use_defs(
  bblocks: &AHashMap<u32, Vec<Inst>>,
) -> (AHashMap<(u32, usize), (u32, usize)>, Bitmap) {
  let mut use_locs = AHashMap::<u32, Vec<(u32, usize)>>::new();
  let mut def_locs = AHashMap::<u32, (u32, usize)>::new();
  for (&label, insts) in bblocks {
    for (inst_no, inst) in insts.iter().enumerate() {
      visit_inst_args!(inst, |arg| match arg {
        Arg::Var(t) => use_locs.entry(*t).or_default().push((label, inst_no)),
        _ => {}
      });
      visit_inst_tgts!(inst, |var| assert!(def_locs
        .insert(*var, (label, inst_no))
        .is_none()));
    }
  }

  // K => V, where the inst at location K has been inlined into location V. Location V may itself be inlined into somewhere else.
  let mut inlines = AHashMap::<(u32, usize), (u32, usize)>::new();
  let mut inlined_vars = Bitmap::new();
  for (var, uses) in use_locs {
    if uses.len() != 1 {
      continue;
    };
    let dest_loc = uses.first().unwrap();
    let src_loc = def_locs.remove(&var).unwrap();
    // Inlining is only supported within the same bblock.
    if src_loc.0 != dest_loc.0 {
      continue;
    };
    assert!(src_loc.1 < dest_loc.1);
    assert!(inlines.insert(src_loc, dest_loc.clone()).is_none());
    inlined_vars.add(var);
  }

  (inlines, inlined_vars)
}
