use std::collections::HashMap;

use lasso::Spur;

use crate::{
    intern::{resolve_symbol, INTERNER},
    typecheck::TypeStore,
};

use super::{
    visitor::{HlirVisitor, VisitAction},
    Builtins, FnHeader, Literal, NodeKind, Type, TypeId,
};

pub struct NameResolver<'a> {
    builtins: &'a Builtins,
    global: HashMap<Spur, FnHeader>,
    names: Vec<Spur>,
    types: TypeStore,
}

impl<'a> NameResolver<'a> {
    pub fn new(builtins: &'a Builtins) -> Self {
        Self {
            builtins,
            global: Default::default(),
            names: Vec::new(),
            types: Default::default(),
        }
    }

    fn name_in_scope(&self, name: &Spur) -> bool {
        self.names.contains(name)
    }

    fn resolve_type_names(&self, id: TypeId) -> TypeId {
        match self.types.get(id) {
            Type::Function { inputs, output } => {
                let new_inputs = inputs
                    .iter()
                    .map(|x| self.resolve_type_names(*x))
                    .collect::<Vec<_>>();
                if &new_inputs != &inputs {
                    self.types.insert(Type::Function {
                        inputs: new_inputs,
                        output,
                    })
                } else {
                    id
                }
            }
            Type::Name(key) => {
                let name = resolve_symbol(key);
                match name {
                    "int" => self.types.int(),
                    "string" => self.types.string(),
                    _ => id,
                }
            }
            _ => id,
        }
    }
}

impl HlirVisitor for NameResolver<'_> {
    fn visit_hlir(&mut self, hlir: &mut super::Hlir) -> VisitAction {
        self.types = hlir.types.clone();
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &mut super::Module) -> VisitAction {
        self.global = module
            .functions
            .values()
            .filter_map(|v| Some((v.header.name?, v.header.clone())))
            .collect();

        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &mut super::FnDef) -> VisitAction {
        function.header.ty = self.resolve_type_names(function.header.ty);
        match self.types.get(function.header.ty) {
            Type::Function { inputs, output } => {
                for (arg, ty) in std::iter::zip(&mut function.arguments, inputs) {
                    arg.ty = ty;
                }
                function.return_ty = output;
            }
            _ => unreachable!(),
        }
        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &mut super::Node) -> VisitAction {
        let out = match &mut node.kind {
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
                } else if let Some(b) = self.builtins.builtins.get(&name) {
                    node.kind = NodeKind::Builtin(b.idx);
                    node.ty = b.ty;
                } else if name == INTERNER.get_or_intern_static("true") {
                    node.kind = NodeKind::Literal(Literal::Bool(true));
                    node.ty = self.types.bool();
                }
                VisitAction::Recurse
            }
            _ => VisitAction::Recurse,
        };

        node.ty = self.resolve_type_names(node.ty);

        out
    }
}
