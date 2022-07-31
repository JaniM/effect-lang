use lasso::Spur;

use crate::extract;

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
    fn visit_name(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Name(name));

        if !self.name_in_scope(name) && let Some(b) = self.builtins.builtins.get(name) {
            node.kind = NodeKind::Builtin(*name);
            node.ty = b.ty.clone();
        }

        VisitAction::Nothing
    }

    fn visit_let(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Let { name });

        self.names.push(*name);

        VisitAction::on_exit(|s: &mut Self, _| {
            s.names.pop();
        })
    }
}
