use crate::extract;

use super::{
    visitor::{HlirVisitor, VisitAction},
    Node, NodeKind,
};

pub struct Simplifier;

impl HlirVisitor for Simplifier {
    fn visit_block(&mut self, node: &mut Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Block(nodes));

        if nodes.len() == 1 {
            let child = nodes.pop().unwrap();
            *node = child;
        }

        VisitAction::Recurse
    }
}
