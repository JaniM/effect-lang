use lasso::Spur;

use crate::{
    intern::{resolve_symbol, INTERNER},
    typecheck::TypeStore,
};

use super::{
    index::{Header, Index, Item},
    visitor::{HlirVisitor, VisitAction},
    Literal, ModuleId, NodeKind, Type, TypeId,
};

#[derive(Default)]
pub struct NameResolver {
    module: ModuleId,
    index: Index,
    names: Vec<Spur>,
    types: TypeStore,
}

impl NameResolver {
    pub fn new() -> Self {
        Self::default()
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
                let new_output = self.resolve_type_names(output);
                if &new_inputs != &inputs || new_output != output {
                    self.types.insert(Type::Function {
                        inputs: new_inputs,
                        output: new_output,
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

impl HlirVisitor for NameResolver {
    fn visit_hlir(&mut self, hlir: &mut super::Hlir) -> VisitAction {
        self.types = hlir.types.clone();
        self.index = hlir.construct_index();
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &mut super::Module) -> VisitAction {
        self.module = module.id;

        for group in module.effect_groups.values_mut() {
            for effect in &mut group.effects {
                effect.header.ty = self.resolve_type_names(effect.header.ty);
                match self.types.get(effect.header.ty) {
                    Type::Function { inputs, output } => {
                        for (arg, ty) in std::iter::zip(&mut effect.arguments, inputs) {
                            arg.ty = ty;
                        }
                        effect.return_ty = output;
                    }
                    _ => unreachable!(),
                }
            }
        }

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
                let module = self.index.modules.get(&self.module).unwrap();
                match module.names.get(&name) {
                    Some(Item::Fn(id)) => {
                        let header = self.index.functions.get(id).unwrap();
                        match header {
                            Header::Fn(header) => {
                                node.kind = NodeKind::Function(header.id);
                                node.ty = header.ty.clone();
                            }
                            Header::Effect(header) => {
                                node.kind = NodeKind::Function(header.id);
                                node.ty = header.ty.clone();
                            }
                        }
                    }
                    Some(Item::EffectGroup(_header)) => {
                        todo!()
                    }
                    None => {}
                }
                if let Some(b) = self.index.builtins.builtins.get(&name) {
                    node.kind = NodeKind::Builtin(b.idx);
                    node.ty = b.ty;
                } else if name == INTERNER.get_or_intern_static("true") {
                    node.kind = NodeKind::Literal(Literal::Bool(true));
                    node.ty = self.types.bool();
                }
                VisitAction::Recurse
            }
            NodeKind::Call { callee, args } => {
                match &callee.kind {
                    &NodeKind::Name(name) if name == INTERNER.get_or_intern_static("resume") => {
                        assert_eq!(args.len(), 1);
                        let arg = args.pop().unwrap().into();
                        node.kind = NodeKind::Resume { arg };
                        node.ty = self.types.unit();
                    }
                    _ => {}
                }
                VisitAction::Recurse
            }
            _ => VisitAction::Recurse,
        };

        node.ty = self.resolve_type_names(node.ty);

        out
    }
}
