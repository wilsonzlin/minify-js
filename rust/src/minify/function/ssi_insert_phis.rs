use super::inst::Inst;
use ahash::AHashMap;
use ahash::AHashSet;
use croaring::Bitmap;
use itertools::Itertools;
use std::collections::VecDeque;

pub(crate) fn insert_phis_for_ssi_construction(
  defs: &mut AHashMap<u32, Bitmap>,
  bblocks: &mut AHashMap<u32, Vec<Inst>>,
  domfront: &AHashMap<u32, Bitmap>,
) {
  for v in defs.keys().cloned().collect_vec() {
    let mut already_inserted = Bitmap::new();
    // We'll start with these blocks but add more as we process, so we can't just use `defs[v].iter()`.
    let mut q = defs[&v].iter().collect::<VecDeque<_>>();
    let mut seen = AHashSet::from_iter(q.iter().cloned());
    while let Some(d) = q.pop_front() {
      // Look at the blocks in the dominance frontier for block `d`.
      let Some(blocks) = domfront.get(&d) else {
        continue;
      };
      for label in blocks.iter() {
        if already_inserted.contains(label) {
          continue;
        };
        already_inserted.add(label);
        bblocks.get_mut(&label).unwrap().insert(0, Inst::Phi {
          tgt: v,
          // We'll populate this later.
          from_blocks: AHashMap::new(),
        });
        defs.get_mut(&v).unwrap().add(label);
        if seen.insert(label) {
          q.push_back(label);
        };
      }
    }
  }
}
