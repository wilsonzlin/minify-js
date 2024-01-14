use ahash::AHashMap;
use croaring::Bitmap;

struct PostOrderVisitor<'a> {
  graph: &'a AHashMap<u32, Bitmap>,
  seen: Bitmap,
  order: Vec<u32>,
}

impl<'a> PostOrderVisitor<'a> {
  fn visit(&mut self, n: u32) {
    if self.seen.contains(n) {
      return;
    };
    self.seen.add(n);
    for c in self.graph[&n].iter() {
      self.visit(c);
    }
    self.order.push(n);
  }
}

pub(crate) fn calculate_postorder(
  adj_list: &AHashMap<u32, Bitmap>,
  entry: u32,
) -> (Vec<u32>, AHashMap<u32, usize>) {
  let mut order_po_v = PostOrderVisitor {
    graph: adj_list,
    order: Vec::new(),
    seen: Bitmap::new(),
  };
  order_po_v.visit(entry);
  // Order of blocks to visit in order to visit by postorder. Elements are labels. This can also be used to map from postorder number (i.e. number each node would be assigned if sequentially visited and assigned in postorder) to label.
  let order_po = order_po_v.order;
  // Map from postorder number to label (see above).
  let label_to_po = order_po
    .iter()
    .enumerate()
    .map(|(i, l)| (*l, i))
    .collect::<AHashMap<_, _>>();
  (order_po, label_to_po)
}
