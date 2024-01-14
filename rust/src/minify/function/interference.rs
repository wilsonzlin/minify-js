use ahash::AHashMap;
use croaring::Bitmap;
use itertools::Itertools;

pub(crate) fn calculate_interference_graph(
  liveness: &AHashMap<(u32, usize), Bitmap>,
) -> AHashMap<u32, Bitmap> {
  // Undirected graph, so two pairs of directed edges for each connection.
  let mut intgraph = AHashMap::<u32, Bitmap>::new();
  for lives in liveness.values() {
    let lives = lives.iter().collect_vec();
    for i in 0..lives.len() {
      // Ensure the node exists in the graph even if no connections.
      intgraph.entry(lives[i]).or_default();
      for j in (i + 1)..lives.len() {
        let (a, b) = (lives[i], lives[j]);
        intgraph.entry(a).or_default().add(b);
        intgraph.entry(b).or_default().add(a);
      }
    }
  }
  intgraph
}
