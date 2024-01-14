mod backedge;
mod bblock;
mod builtin;
mod cfg;
mod consteval;
mod counter;
mod deconstruct_ssa;
mod defs;
mod domfront;
mod dominators;
mod domtree;
mod dot;
mod inst;
mod interference;
mod liveness;
mod optpass_cfg_prune;
mod optpass_dvn;
mod optpass_impossible_branches;
mod optpass_redundant_assigns;
mod optpass_trivial_dce;
mod postorder;
mod register_alloc;
mod single_use_insts;
mod source_to_inst;
mod ssi_insert_phis;
mod ssi_rename;
mod visit;
mod reconstruct_source;

use bblock::convert_insts_to_bblocks;
use cfg::calculate_cfg;
use counter::Counter;
use defs::calculate_defs;
use domfront::calculate_domfront;
use domtree::calculate_domtree;
use optpass_dvn::optpass_dvn;
use optpass_impossible_branches::optpass_impossible_branches;
use optpass_redundant_assigns::optpass_redundant_assigns;
use optpass_trivial_dce::optpass_trivial_dce;
use parse_js::ast::Node;
use postorder::calculate_postorder;
use source_to_inst::translate_source_to_inst;
use ssi_insert_phis::insert_phis_for_ssi_construction;
use ssi_rename::rename_targets_for_ssi_construction;
use backedge::find_backedges;
use deconstruct_ssa::deconstruct_ssa;
use interference::calculate_interference_graph;
use liveness::calculate_live_ins;
use optpass_cfg_prune::optpass_cfg_prune;
use register_alloc::allocate_registers;
use single_use_insts::analyse_single_use_defs;

pub(crate) fn minify_js_function(
  body_node: &Node,
) {
  // Label 0 is for entry.
  let mut c_label = Counter::new(1);
  let mut c_temp = Counter::new(0);
  let insts = translate_source_to_inst(body_node, &mut c_label, &mut c_temp);
  let (mut bblocks, bblock_order) = convert_insts_to_bblocks(insts, &mut c_label);
  let (mut cfg_parents, mut cfg_children) = calculate_cfg(&mut bblocks, bblock_order);

  let (postorder, label_to_postorder) = calculate_postorder(&cfg_children, 0);
  let (idom_by, domtree) = calculate_domtree(&cfg_parents, &postorder, &label_to_postorder, 0);
  let domfront = calculate_domfront(&cfg_parents, &idom_by, &postorder);
  let mut defs = calculate_defs(&bblocks);

  // Construct SSA.
  insert_phis_for_ssi_construction(&mut defs, &mut bblocks, &domfront);
  rename_targets_for_ssi_construction(&mut bblocks, &cfg_children, &domtree, &mut c_temp);

  // Optimisation passes.
  // - Dominator-based value numbering.
  // - Trivial dead code elimination.
  // Drop defs as it likely will be invalid after even one pass.
  drop(defs);
  loop {
    let mut changed = false;

    // TODO Can we avoid recalculating these on every iteration i.e. mutate in-place when changing the CFG?
    let (postorder, label_to_postorder) = calculate_postorder(&cfg_children, 0);
    let (_, domtree) = calculate_domtree(&cfg_parents, &postorder, &label_to_postorder, 0);

    optpass_dvn(&mut changed, &mut bblocks, &cfg_children, &domtree);
    optpass_trivial_dce(&mut changed, &mut bblocks);
    // TODO Isn't this really const/copy propagation to child Phi insts?
    optpass_redundant_assigns(&mut changed, &mut bblocks);
    optpass_impossible_branches(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );
    optpass_cfg_prune(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );

    if !changed {
      break;
    }
  }

  let (inlines, inlined_tgts) = analyse_single_use_defs(&bblocks);
  let liveness = calculate_live_ins(
    &bblocks,
    &cfg_parents,
    &cfg_children,
    &inlines,
    &inlined_tgts,
  );
  let intgraph = calculate_interference_graph(&liveness);
  let var_alloc = allocate_registers(&intgraph);

  // It's safe to calculate liveliness before removing Phi insts; after deconstructing, they always lie exactly between all parent bblocks and the head of the bblock, so their lifetimes are identical.
  deconstruct_ssa(
    &mut bblocks,
    &mut cfg_parents,
    &mut cfg_children,
    &mut c_label,
  );

  // To calculate the post dominators, reverse the edges and run any dominator algorithm.
  let ipostdom_by = {
    let (rpo, rpo_label) = calculate_postorder(&cfg_parents, u32::MAX);
    calculate_domtree(&cfg_children, &rpo, &rpo_label, u32::MAX).0
  };
  let backedges = find_backedges(&cfg_children);

  // TODO Reconstruct source.
  todo!();
}
