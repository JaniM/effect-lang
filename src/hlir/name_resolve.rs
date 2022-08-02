use lasso::Spur;

use super::{
    visitor::{HlirVisitor, VisitAction},
    Builtins, NodeKind,
};

pub struct NameResolver<'a> {
    builtins: &'a Builtins,
    names: Vec<Spur>,
}

impl<'a> NameResolver<'a> {
    pub fn new(builtins: &'a Builtins) -> Self {
        Self {
            builtins,
            names: Vec::new(),
        }
    }

    fn name_in_scope(&self, name: &Spur) -> bool {
        self.names.contains(name)
    }
}

impl HlirVisitor for NameResolver<'_> {
    fn visit_node(&mut self, node: &mut super::Node) -> VisitAction {
        match &mut node.kind {
            NodeKind::Let { name, value, expr } => {
                self.walk_node(value);

                self.names.push(*name);
                self.walk_node(expr);
                self.names.pop();

                VisitAction::Nothing
            }
            NodeKind::Name(name) => {
                if !self.name_in_scope(name) && let Some(b) = self.builtins.builtins.get(name) {
                    node.kind = NodeKind::Builtin(b.idx);
                    node.ty = b.ty.clone();
                }
                VisitAction::Recurse
            }
            _ => VisitAction::Recurse,
        }
    }
}
