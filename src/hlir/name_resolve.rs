use lasso::Spur;

use crate::{
    intern::{resolve_symbol, INTERNER},
    typecheck::{EffectSet, Type, TypeId, TypeStore},
};

use super::{
    index::{Index, Item},
    visitor::{HlirVisitor, VisitAction},
    Literal, ModuleId, NodeKind,
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

    fn resolve_type_names(&self, id: TypeId) {
        match self.types.get(id) {
            Type::Function {
                inputs,
                output,
                effects,
            } => {
                let module = self.index.modules.get(&self.module).unwrap();
                let new_effects = match effects {
                    EffectSet::Unsolved { names, open } => {
                        let mut effects = vec![];
                        for name in names {
                            match module.names.get(&name) {
                                Some(Item::EffectGroup(id)) => effects.push(*id),
                                _ => todo!(),
                            }
                        }
                        EffectSet::new(effects, open)
                    }
                    x => x,
                };
                for input in &inputs {
                    self.resolve_type_names(*input);
                }
                self.resolve_type_names(output);
                self.types.rewrite(
                    id,
                    Type::Function {
                        inputs,
                        output,
                        effects: new_effects,
                    },
                );
            }
            Type::Name(key) => {
                let span = self.types.get_span(id);
                let name = resolve_symbol(key);
                let ty = match name {
                    "int" => self.types.int(span),
                    "string" => self.types.string(span),
                    "unit" => self.types.unit(span),
                    "_" => self.types.unknown_type(span),
                    _ => id,
                };
                self.types.replace(id, ty);
            }
            Type::Forall(_, ty) => self.resolve_type_names(ty),
            _ => {}
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
                self.resolve_type_names(effect.header.ty);
                match self.types.get(effect.header.ty) {
                    Type::Function { inputs, output, .. } => {
                        for (arg, ty) in std::iter::zip(&mut effect.arguments, inputs) {
                            arg.ty = ty;
                        }
                        effect.return_ty = output;
                    }
                    _ => unreachable!(),
                }
            }
        }

        for function in module.functions.values_mut() {
            self.resolve_type_names(function.header.ty);
            let mut ty = self.types.get(function.header.ty);
            loop {
                match ty {
                    Type::Forall(_, t) => ty = self.types.get(t),
                    _ => break,
                }
            }
            match ty {
                Type::Function { inputs, output, .. } => {
                    for (arg, ty) in std::iter::zip(&mut function.arguments, inputs) {
                        arg.ty = ty;
                    }
                    function.return_ty = output;
                }
                _ => unreachable!(),
            }
        }

        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &mut super::Node) -> VisitAction {
        let out = match &mut node.kind {
            NodeKind::Let {
                name, value, expr, ..
            } => {
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
                        node.kind = NodeKind::Function(*id);
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
                    node.ty = self.types.bool(node.source_span);
                }
                VisitAction::Recurse
            }
            NodeKind::Call { callee, args } => {
                match &callee.kind {
                    &NodeKind::Name(name) if name == INTERNER.get_or_intern_static("resume") => {
                        assert!(args.len() <= 1);
                        let arg = args.pop().map(Box::new);
                        let span = callee.source_span;
                        node.kind = NodeKind::Resume { arg };
                        node.ty = self.types.unit(span);
                    }
                    _ => {}
                }
                VisitAction::Recurse
            }
            NodeKind::Handle {
                group_id,
                name,
                handlers,
                ..
            } => {
                let module = self.index.modules.get(&self.module).unwrap();
                let group = match module.names.get(&name) {
                    Some(Item::EffectGroup(group)) => group,
                    Some(Item::Fn(_)) => todo!(),
                    None => todo!(),
                };
                let group = self.index.effect_groups.get(group).unwrap();
                *group_id = group.id;

                for (handler, effect) in std::iter::zip(handlers, &group.effects) {
                    handler.effect_id = effect.header.id;
                }

                VisitAction::Recurse
            }
            NodeKind::ApplyType { ty, .. } => {
                self.resolve_type_names(*ty);
                VisitAction::Recurse
            }
            _ => VisitAction::Recurse,
        };

        self.resolve_type_names(node.ty);

        out
    }
}
