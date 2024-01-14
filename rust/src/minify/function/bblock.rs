use super::counter::Counter;
use super::inst::Inst;
use ahash::AHashMap;

// We may need to create some new blocks (which require labels), which is why we need `c_label`.
pub(crate) fn convert_insts_to_bblocks(
  insts: Vec<Inst>,
  c_label: &mut Counter,
) -> (AHashMap<u32, Vec<Inst>>, Vec<u32>) {
  let mut bblocks = AHashMap::<u32, Vec<Inst>>::new();
  // The order is required for implicit "fallthrough" jumps to the lexically "next" block.
  let mut bblock_order = vec![0];
  let mut bblock = bblocks.entry(0).or_default();
  for inst in insts {
    match inst {
      Inst::Label { label } => {
        bblock_order.push(label);
        bblock = bblocks.entry(label).or_default();
      }
      Inst::Goto { .. } | Inst::CondGoto { .. } | Inst::NotCondGoto { .. } => {
        bblock.push(inst);
        let l = c_label.bump();
        bblock_order.push(l);
        bblock = bblocks.entry(l).or_default();
      }
      inst => {
        bblock.push(inst);
      }
    };
  }
  assert!(bblocks.insert(u32::MAX, Vec::new()).is_none());
  bblock_order.push(u32::MAX);
  (bblocks, bblock_order)
}
