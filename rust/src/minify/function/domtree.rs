use ahash::AHashMap;
use croaring::Bitmap;

// A dominates B if A will **always** execute some time at or before B. (All paths to B go through A.)
// B is dominated by A if A also dominates **all** of B's parents. (Think about it.)
// Dominance tree: edges represent only "immediate" dominations. A immediately dominates B iff A dominates B and doesn't strictly dominate any other node that strictly dominates B. (Strictly dominates means A dominates B and A != B.)
// NOTE: Every node, except the entry node, has exactly one immediate dominator: https://en.wikipedia.org/wiki/Dominator_(graph_theory).
//
// https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
// - This paper also contains an explanation on how to calculate what a node dominates given `idom_by`, which is much faster than other dominance calculation algorithms.
// Other implementations:
// - https://github.com/sampsyo/bril/blob/34133101a68bb50ae0fc8083857a3e3bd6bae260/bril-llvm/dom.py#L47
pub(crate) fn calculate_domtree(
  cfg_parents: &AHashMap<u32, Bitmap>,
  postorder: &[u32],
  label_to_postorder: &AHashMap<u32, usize>,
  entry: u32,
) -> (AHashMap<u32, u32>, AHashMap<u32, Bitmap>) {
  let mut idom_by = AHashMap::<u32, u32>::new();
  let mut domtree = AHashMap::<u32, Bitmap>::new();
  {
    macro_rules! intersect {
      ($b1:expr, $b2:expr) => {{
        // WARNING: We're getting the position in postorder, NOT reverse postorder.
        let mut finger1 = label_to_postorder[&$b1];
        let mut finger2 = label_to_postorder[&$b2];
        while finger1 != finger2 {
          while finger1 < finger2 {
            finger1 = label_to_postorder[&idom_by[&postorder[finger1]]];
          }
          while finger2 < finger1 {
            finger2 = label_to_postorder[&idom_by[&postorder[finger2]]];
          }
        }
        postorder[finger1]
      }};
    }
    idom_by.insert(entry, entry);
    loop {
      let mut changed = false;
      for &b in postorder.iter().rev().filter(|b| **b != entry) {
        let parents = &cfg_parents[&b];
        let Some(mut new_idom) = parents.iter().find(|n| idom_by.contains_key(n)) else {
          continue;
        };
        let to_skip = new_idom;
        for p in parents.iter().filter(|p| *p != to_skip) {
          if idom_by.get(&p).is_some() {
            new_idom = intersect!(p, new_idom);
          };
        }
        if idom_by.get(&b) != Some(&new_idom) {
          idom_by.insert(b, new_idom);
          changed = true;
        }
      }
      if !changed {
        break;
      };
    }
    idom_by.remove(&entry);
    for (&c, &p) in idom_by.iter() {
      domtree.entry(p).or_default().add(c);
    }
  };
  (idom_by, domtree)
}
