use ahash::AHashMap;
use croaring::Bitmap;

pub(crate) fn allocate_registers(intgraph: &AHashMap<u32, Bitmap>) -> AHashMap<u32, u32> {
  let all_colours = Bitmap::from_range(0..u32::try_from(intgraph.len()).unwrap());
  let mut rem = intgraph.clone();
  let mut stack = Vec::new();
  while let Some(t) = rem
    .iter()
    .min_by_key(|(_, c)| c.cardinality())
    .map(|(t, _)| *t)
  {
    stack.push(t);
    let children = rem.remove(&t).unwrap();
    for c in children.iter() {
      rem.get_mut(&c).unwrap().remove(t);
    }
  }
  let mut allocated = AHashMap::<u32, u32>::new();
  for t in stack.into_iter().rev() {
    let mut avail = all_colours.clone();
    for neighbour in intgraph[&t].iter() {
      if let Some(&neighbour_colour) = allocated.get(&neighbour) {
        avail.remove(neighbour_colour);
      };
    }
    let pick = avail.minimum().unwrap();
    assert!(allocated.insert(t, pick).is_none());
  }
  allocated
}
