use ahash::AHashMap;
use croaring::Bitmap;

// We must use DFS, as it's only cycles within a path, not revisited nodes across the entire walk, that count as backedges.
fn inner(
  stack: &mut Vec<u32>,
  backedges: &mut AHashMap<u32, u32>, // Map from A -> B where B -> A is a backedge.
  cfg_children: &AHashMap<u32, Bitmap>,
  label: u32,
) {
  for c in cfg_children[&label].iter() {
    if stack.contains(&c) {
      // This is a backedge.
      backedges.insert(c, label);
    } else {
      stack.push(c);
      inner(stack, backedges, cfg_children, c);
      stack.pop();
    };
  }
}

pub(crate) fn find_backedges(cfg_children: &AHashMap<u32, Bitmap>) -> AHashMap<u32, u32> {
  let mut backedges = AHashMap::<u32, u32>::new();
  inner(&mut vec![0], &mut backedges, cfg_children, 0);
  backedges
}
