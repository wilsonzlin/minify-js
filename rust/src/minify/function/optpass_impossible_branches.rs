use super::cfg::delete_bblocks_from_cfg;
use super::cfg::mark_unreachable_cfg_bblocks;
use super::consteval::coerce_to_bool;
use super::inst::Arg;
use super::inst::Inst;
use ahash::AHashMap;
use croaring::Bitmap;
use itertools::Itertools;

// Correctness:
// - When we detach bblocks A and B (because A can never branch to B in reality e.g. const eval is always true/false), we move all bblocks in subgraph G, which contains all bblocks only reachable from B.
// - We must then detach all bblocks within G i.e. remove all edges to bblocks outside of G. This isn't recursive, as the bblocks only reachable from B doesn't change as we remove these bblocks or their edges.
// - We must clean up any usages of defs within G outside of G. Outside of G, these uses can only appear in Phi nodes.
pub(crate) fn optpass_impossible_branches(
  changed: &mut bool,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_parents: &mut AHashMap<u32, Bitmap>,
  cfg_children: &mut AHashMap<u32, Bitmap>,
) {
  loop {
    for label in bblocks.keys().cloned().collect_vec() {
      let Some(inst) = bblocks[&label].last() else {
        continue;
      };
      let children = &cfg_children[&label];
      if children.cardinality() != 2 {
        continue;
      };
      let (always_bool, eval_bool, explicit_child) = match inst {
        Inst::CondGoto {
          cond: Arg::Const(c),
          label,
        } => (true, coerce_to_bool(c), *label),
        Inst::NotCondGoto {
          cond: Arg::Const(c),
          label,
        } => (false, coerce_to_bool(c), *label),
        _ => continue,
      };
      let child = if always_bool != eval_bool {
        explicit_child
      } else {
        let Ok(other_child) = children
          .iter()
          .filter(|&c| c != explicit_child)
          .exactly_one()
        else {
          unreachable!();
        };
        other_child
      };
      println!("Branch from @{label} to @{child} will never be taken: {inst:?}");
      // Remove instruction.
      bblocks.get_mut(&label).unwrap().pop().unwrap();
      // Detach from child.
      cfg_children.get_mut(&label).unwrap().remove(child);
      cfg_parents.get_mut(&child).unwrap().remove(label);
    }

    // Detaching from bblocks means that we may have removed entire subgraphs (i.e. other bblocks). Therefore, we must recalculate again the accessible bblocks.
    let mut to_delete = Bitmap::new();
    mark_unreachable_cfg_bblocks(&mut to_delete, bblocks, cfg_children);
    // All defs in now-deleted bblocks must be cleared. Since we are in strict SSA, they should only ever appear outside of the deleted bblocks in Phi insts.
    for n in to_delete.iter() {
      // Update Phi insts in children.
      for c in cfg_children[&n].iter() {
        for inst in bblocks.get_mut(&c).unwrap().iter_mut() {
          let Inst::Phi { from_blocks, .. } = inst else {
            // No more Phi insts.
            break;
          };
          // NOTE: We don't try to remove the Phi insts or transform into a VarAssign (if it only has one entry in `from_blocks`) right now out of abundance of caution for correctness, since `from_blocks` could still be modified during these loops.
          from_blocks.remove(&n);
        }
      }
      println!("Removed unreachable @{n}");
    }

    // Delete bblocks now so that only valid bblocks remain, which is the set of bblocks to iterate for pruning Phi insts.
    delete_bblocks_from_cfg(&to_delete, bblocks, cfg_parents, cfg_children);

    // Prune Phi insts in remaining bblocks.
    for bblock in bblocks.values_mut() {
      let mut phis_to_delete = Vec::new();
      for (i, inst) in bblock.iter_mut().enumerate() {
        let Inst::Phi {
          tgt, from_blocks, ..
        } = inst
        else {
          // No more Phi insts.
          break;
        };
        if from_blocks.is_empty() {
          // TODO Is this always safe?
          phis_to_delete.push(i);
        }
        if from_blocks.len() == 1 {
          let arg = from_blocks.iter().next().unwrap().1.clone();
          *inst = Inst::VarAssign {
            tgt: *tgt,
            value: arg,
          };
        };
      }
      for i in phis_to_delete.into_iter().rev() {
        bblock.remove(i);
      }
    }

    if to_delete.is_empty() {
      break;
    }
    *changed = true;
  }
}
