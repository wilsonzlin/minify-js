// Rust won't let us iterate the tree, matching and exploring, while also making the occasional mutation of nodes or subtrees during this iterating. Therefore, we use an update list that we'll apply after the iteration finishes.

use crate::{
    ast::{NodeData, NodeId, NodeMap, Syntax},
    source::SourceRange,
    symbol::ScopeId,
};

struct NodeUpdate {
    // If this is outside the range of NodeMap, it's a new node that needs to be inserted into the NodeMap.
    node_id: NodeId,
    new_data: NodeData,
}

pub struct NodeUpdates {
    updates: Vec<NodeUpdate>,
    next_new_node_id: usize,
}

impl NodeUpdates {
    pub fn new(node_map: &NodeMap) -> NodeUpdates {
        NodeUpdates {
            updates: vec![],
            next_new_node_id: node_map.len(),
        }
    }

    // To delete a node, replace it with something blank (varies by context). We cannot delete nodes as we don't have backlinks into parents that reference it.
    pub fn replace_node(
        &mut self,
        node_id: NodeId,
        scope: ScopeId,
        loc: SourceRange,
        stx: Syntax,
    ) -> () {
        self.updates.push(NodeUpdate {
            node_id,
            new_data: NodeData::new(scope, loc, stx),
        })
    }

    pub fn create_node(&mut self, scope: ScopeId, loc: SourceRange, stx: Syntax) -> NodeId {
        let node_id = NodeId::new(self.next_new_node_id);
        self.next_new_node_id += 1;
        self.updates.push(NodeUpdate {
            node_id,
            new_data: NodeData::new(scope, loc, stx),
        });
        node_id
    }

    pub fn apply_updates(self, node_map: &mut NodeMap) -> () {
        for u in self.updates {
            if u.node_id.id() == node_map.len() {
                node_map.push(u.new_data)
            } else {
                node_map[u.node_id] = u.new_data;
            }
        }
    }
}
