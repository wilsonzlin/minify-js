use super::inst::Arg;
use super::inst::CallArg;
use super::inst::Inst;
use super::visit::visit_inst_args;
use super::visit::visit_inst_tgts;
use ahash::AHashMap;
use croaring::Bitmap;
use std::collections::VecDeque;

pub(crate) fn calculate_live_ins(
  bblocks: &AHashMap<u32, Vec<Inst>>,
  cfg_parents: &AHashMap<u32, Bitmap>,
  cfg_children: &AHashMap<u32, Bitmap>,
  inlines: &AHashMap<(u32, usize), (u32, usize)>,
  inlined_vars: &Bitmap,
) -> AHashMap<(u32, usize), Bitmap> {
  // Some preparation of inlines is required. If an inst is inlined, then that inst no longer has any lifetime at all, and its args' lifetimes get extended (because they move to a later inst). Insts may inline recursively, so follow the "chain" of inlining to find the ultimate inst that the arg lifetimes will be extended to.
  let mut additional_uses_at = AHashMap::<(u32, usize), Bitmap>::new();
  for (src, next) in inlines {
    let mut dest = next;
    while let Some(next) = inlines.get(dest) {
      dest = next;
    }
    visit_inst_args!(&bblocks[&src.0][src.1], |arg| match arg {
      Arg::Var(t) if !inlined_vars.contains(*t) => {
        additional_uses_at.entry(dest.clone()).or_default().add(*t);
      }
      _ => {}
    });
  }

  // Worklist algorithm.
  let mut live_in_at = AHashMap::<(u32, usize), Bitmap>::new();
  // This is necessary as inst 0 may be inlined so won't exist in `live_in_at`.
  let mut live_in_at_top_of_bblock = AHashMap::<u32, Bitmap>::new();
  let mut worklist = bblocks.keys().cloned().collect::<VecDeque<_>>();
  while let Some(label) = worklist.pop_front() {
    let mut cur = cfg_children[&label]
      .iter()
      .filter_map(|c| live_in_at_top_of_bblock.get(&c))
      .cloned()
      .reduce(|r, p| r.or(&p))
      .unwrap_or_default();
    // TODO We go inst by inst to avoid another loop to calculate each inst, but also because it may be possible to use before def if self-loop. Is the latter true?
    // WARNING: Remove def first, then add usages, in case args contains the same var (although this isn't possible when using SSA).
    let mut did_change = false;
    for (inst_no, inst) in bblocks[&label].iter().enumerate().rev() {
      let loc = (label, inst_no);
      if inlines.contains_key(&loc) {
        // This inst has been inlined, pretend it doesn't exist (and don't add to `live_in_at`).
        continue;
      };
      visit_inst_tgts!(inst, |var| cur.remove(*var));
      visit_inst_args!(inst, |arg| match arg {
        Arg::Var(t) => cur.add(*t),
        _ => {}
      });
      if let Some(add_uses) = additional_uses_at.get(&loc) {
        // `add_uses` contains all other vars that will be in this inst after inlining.
        cur |= add_uses;
      };
      let ex = live_in_at.entry((label, inst_no)).or_default();
      if ex != &cur {
        *ex = cur.clone();
        did_change = true;
      };
    }
    live_in_at_top_of_bblock.insert(label, cur);
    if did_change {
      for p in cfg_parents[&label].iter() {
        worklist.push_back(p);
      }
    };
  }
  live_in_at
}
