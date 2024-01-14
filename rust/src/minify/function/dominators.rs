use ahash::AHashMap;
use croaring::Bitmap;

// https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
pub(crate) fn calculate_dominated_by(idom_by: &AHashMap<u32, u32>, root: u32) -> AHashMap<u32, Bitmap> {
  let mut dom = AHashMap::<u32, Bitmap>::new();
  for label in idom_by.keys().cloned() {
    let e = dom.entry(label).or_default();
    let mut n = label;
    loop {
      e.add(n);
      if n == root {
        break;
      };
      n = idom_by[&n];
    }
  }
  dom
}
