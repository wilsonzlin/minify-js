use super::inst::Inst;
use ahash::AHashMap;
use croaring::Bitmap;
use std::collections::VecDeque;

/**
 * WARNING: It is dangerous to remove empty bblocks, as they can have significance despite being empty:
 * - Phi insts still need to distinguish between different branches for different values. This can happen quite often with const propagation, where an empty block merely determines which const value in a Phi is picked.
 * When is it safe to remove a bblock (and move insts and fix up parent-child relationships and Phi insts)?
 * - When a bblock has exactly one parent and that parent has exactly one child.
 * - When an empty bblock has no children.
 * - When an empty bblock has no children with Phi insts.
 */

pub(crate) fn mark_unreachable_cfg_bblocks(
  to_delete: &mut Bitmap,
  bblocks: &AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
) {
  let mut seen = Bitmap::of(&[0]);
  let mut to_visit = [0].into_iter().collect::<VecDeque<_>>();
  while let Some(n) = to_visit.pop_front() {
    for c in cfg_children[&n].iter() {
      if !seen.contains(c) {
        seen.add(c);
        to_visit.push_back(c);
      };
    }
  }
  // Find unreachable bblocks.
  for &n in bblocks.keys() {
    if !seen.contains(n) {
      to_delete.add(n);
    };
  }
}

pub(crate) fn delete_bblocks_from_cfg(
  to_delete: &Bitmap,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  cfg_parents: &mut AHashMap<u32, Bitmap>,
  cfg_children: &mut AHashMap<u32, Bitmap>,
) {
  // Connect parents and children.
  for label in to_delete.iter() {
    let parents = cfg_parents[&label].clone();
    let children = cfg_children[&label].clone();
    for p in parents.iter() {
      for c in children.iter() {
        cfg_children.get_mut(&p).unwrap().add(c);
        cfg_parents.get_mut(&c).unwrap().add(p);
      }
    }
  }
  // Remove nonexistent links.
  for v in cfg_parents.values_mut() {
    *v -= to_delete.clone();
  }
  for v in cfg_children.values_mut() {
    *v -= to_delete.clone();
  }
  // Detach.
  for label in to_delete.iter() {
    bblocks.remove(&label).unwrap();
    cfg_parents.remove(&label).unwrap();
    cfg_children.remove(&label).unwrap();
  }
}

pub(crate) fn calculate_cfg(
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  // We consume this because all subsequent analysis operations should use a well-defined order (e.g. reverse postorder) for safety/correctness, and not this rather arbitrary ordering.
  mut bblock_order: Vec<u32>,
) -> (AHashMap<u32, Bitmap>, AHashMap<u32, Bitmap>) {
  // Adj. list from label to labels.
  let mut cfg_children = AHashMap::<u32, Bitmap>::new();
  let mut cfg_parents = AHashMap::<u32, Bitmap>::new();
  for i in 0..bblocks.len() {
    let label = bblock_order[i];
    let mut children = Bitmap::new();
    let last = &bblocks[&label].last();
    match last {
      Some(Inst::Goto { label })
      | Some(Inst::NotCondGoto { label, .. })
      | Some(Inst::CondGoto { label, .. }) => children.add(*label),
      _ => {}
    };
    match last {
      Some(Inst::Goto { .. }) => {}
      _ if i == bblocks.len() - 1 => {}
      _ => children.add(bblock_order[i + 1]),
    };
    for c in children.iter() {
      cfg_parents.entry(c).or_default().add(label);
    }
    // Ensure that every node exists in `cfg_parents` even if they have none.
    cfg_parents.entry(label).or_default();
    cfg_children.insert(label, children);
  }
  // Prune unreachable blocks from 0. This is necessary for dominance calculation to be correct (basic example: every block should be dominated by 0, but if there's an unreachable block it'll make all its descendants not dominated by 0).
  // This can happen due to user code (unreachable code) or by us, because we split after a `goto` which makes the new other-split-half block unreachable (this block is usually empty).
  {
    let mut to_delete = Bitmap::new();
    mark_unreachable_cfg_bblocks(&mut to_delete, bblocks, &cfg_children);
    delete_bblocks_from_cfg(&to_delete, bblocks, &mut cfg_parents, &mut cfg_children);
    #[allow(unused)] // For correctness in case we do use `bblock_order` in the future.
    {
      bblock_order = bblock_order
        .into_iter()
        .filter(|&n| !to_delete.contains(n))
        .collect();
    }
  };

  (cfg_parents, cfg_children)
}
