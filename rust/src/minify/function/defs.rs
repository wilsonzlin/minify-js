use super::inst::Inst;
use super::visit::visit_inst_tgts;
use ahash::AHashMap;
use croaring::Bitmap;

// Find which bblocks assign to what vars.
pub(crate) fn calculate_defs(bblocks: &AHashMap<u32, Vec<Inst>>) -> AHashMap<u32, Bitmap> {
  let mut defs = AHashMap::<u32, Bitmap>::new();
  for (label, insts) in bblocks.iter() {
    for inst in insts.iter() {
      visit_inst_tgts!(inst, |var| defs.entry(*var).or_default().add(*label));
    }
  }
  defs
}
