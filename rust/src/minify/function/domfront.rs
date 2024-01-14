use ahash::AHashMap;
use croaring::Bitmap;

// A's dominance frontier contains B if A doesn't dominate B, but does dominate a parent of B.
// One way of thinking about it: it's the nodes immediately adjacent to the subgraph of A's domination.
// Another way to think about it: B is dominated by A if all of B's parents are dominated by A (see comments further up). On the other hand, if it's in the fringe, at least one parent is dominated by A, but not all of them.
// https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
// Other implementations: https://github.com/sampsyo/bril/blob/34133101a68bb50ae0fc8083857a3e3bd6bae260/bril-llvm/dom.py#L69
pub(crate) fn calculate_domfront(
  cfg_parents: &AHashMap<u32, Bitmap>,
  idom_by: &AHashMap<u32, u32>,
  postorder: &[u32],
) -> AHashMap<u32, Bitmap> {
  let mut domfront = AHashMap::<u32, Bitmap>::new();
  for &b in postorder.iter().rev() {
    let parents = &cfg_parents[&b];
    if parents.cardinality() < 2 {
      continue;
    };
    for p in parents.iter() {
      let mut runner = p;
      while runner != idom_by[&b] {
        domfront.entry(runner).or_default().add(b);
        runner = idom_by[&runner];
      }
    }
  }
  domfront
}
