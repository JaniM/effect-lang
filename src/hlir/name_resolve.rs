use std::collections::HashMap;

use lasso::Spur;

use super::{
    visitor::{HlirVisitor, VisitAction},
    Builtins, FunctionId, NodeKind, Type,
};

#[derive(Debug)]
pub struct FnHeader {
    pub id: FunctionId,
    pub name: Option<Spur>,
    pub ty: Type,
}

pub struct NameResolver<'a> {
    builtins: &'a Builtins,
    global: HashMap<Spur, FnHeader>,
    names: Vec<Spur>,
}

impl<'a> NameResolver<'a> {
    pub fn new(builtins: &'a Builtins) -> Self {
        Self {
            builtins,
            global: Default::default(),
            names: Vec::new(),
        }
    }

    fn name_in_scope(&self, name: &Spur) -> bool {
        self.names.contains(name)
    }
}

impl HlirVisitor for NameResolver<'_> {
    fn visit_module(&mut self, module: &mut super::Module) -> VisitAction {
        self.global = module
            .functions
            .values()
            .filter_map(|v| {
                Some((
                    v.name?,
                    FnHeader {
                        id: v.id,
                        name: v.name,
                        ty: Type::Function {
                            inputs: vec![],
                            output: Type::Unit.into(),
                        },
                    },
                ))
            })
            .collect();

        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &mut super::Node) -> VisitAction {
        match &mut node.kind {
            NodeKind::Let { name, value, expr } => {
                self.walk_node(value);

                self.names.push(*name);
                self.walk_node(expr);
                self.names.pop();

                VisitAction::Nothing
            }
            &mut NodeKind::Name(name) => {
                if self.name_in_scope(&name) {
                    return VisitAction::Recurse;
                }
                if let Some(header) = self.global.get(&name) {
                    node.kind = NodeKind::Function(header.id);
                    node.ty = header.ty.clone();
                }
                if let Some(b) = self.builtins.builtins.get(&name) {
                    node.kind = NodeKind::Builtin(b.idx);
                    node.ty = b.ty.clone();
                }
                VisitAction::Recurse
            }
            _ => VisitAction::Recurse,
        }
    }
}
