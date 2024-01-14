use super::inst::Inst;
use ahash::AHashMap;
use croaring::Bitmap;
use itertools::Itertools;

/**
 * WARNING: Read comment in cfg.rs.
 */

pub(crate) fn optpass_cfg_prune(
  changed: &mut bool,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_parents: &mut AHashMap<u32, Bitmap>,
  cfg_children: &mut AHashMap<u32, Bitmap>,
) {
  // Iterate until convergence, instead of waiting for another optimisation pass.
  loop {
    // WARNING: We must update graph within this loop, instead of simply marking and then removing afterwards, as we possibly pop instructions which could make a non-empty bblock empty, but if we don't then immediately update the graph some invariants won't hold (e.g. empty bblocks have <= 1 children). This means we can't use common utility graph functions.
    let mut converged = true;
    for label in bblocks.keys().cloned().collect_vec() {
      // TODO Figure out how to delete node 0 (i.e. re-root).
      if label == 0 {
        continue;
      };
      // Always have an exit node.
      if label == u32::MAX {
        continue;
      };
      let Ok(parent) = cfg_parents[&label].iter().exactly_one() else {
        continue;
      };
      if cfg_children[&parent].cardinality() != 1 {
        continue;
      };
      // Clone so we can update other entries in `cfg_*`.
      let children = cfg_children[&label].clone();

      // Connect parent to children.
      for c in children.iter() {
        cfg_parents.get_mut(&c).unwrap().add(parent);
        cfg_children.get_mut(&parent).unwrap().add(c);
      }
      // Detach from children.
      for c in children.iter() {
        cfg_parents.get_mut(&c).unwrap().remove(label);
      }
      // Detach from parents.
      cfg_children.get_mut(&parent).unwrap().remove(label);

      // Remove any goto (cond. or otherwise) instruction in the parent block.
      let p_bblocks = bblocks.get_mut(&parent).unwrap();
      let should_pop = match p_bblocks.last() {
        Some(Inst::Goto { label: l })
        | Some(Inst::CondGoto { label: l, .. })
        | Some(Inst::NotCondGoto { label: l, .. }) => {
          assert_eq!(*l, label);
          true
        }
        _ => false,
      };
      if should_pop {
        p_bblocks.pop();
      };
      // Detach.
      let mut insts = bblocks.remove(&label).unwrap();
      cfg_parents.remove(&label).unwrap();
      cfg_children.remove(&label).unwrap();
      // Move insts to parent.
      bblocks.get_mut(&parent).unwrap().append(&mut insts);
      // Update phi nodes in children.
      for c in children.iter() {
        for c_inst in bblocks.get_mut(&c).unwrap() {
          let Inst::Phi { from_blocks, .. } = c_inst else {
            // No more phi nodes.
            break;
          };
          if let Some(ex) = from_blocks.remove(&label) {
            from_blocks.insert(parent, ex);
          };
        }
      }
      println!("Removed redundant @{label}");
      *changed = true;
      converged = false;
    }

    if converged {
      break;
    }
  }
}
