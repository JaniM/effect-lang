mod test;
pub mod typestore;

use std::{cell::RefCell, collections::HashSet, iter::zip};

use lasso::Spur;

use crate::{
    hlir::{
        index::{Header, Index},
        visitor::{HlirVisitorImmut, VisitAction},
        EffectGroupId, FnDef, Hlir, Literal, Module, ModuleId, Node, NodeKind, Span,
    },
    parser::BinopKind,
};

pub use typestore::*;

/// Represents a type constraint. Used by [`TypecheckContext::apply_constraints()`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// The two types must be equal.
    Equal(TypeId, TypeId),
    /// Propagate equality only to the left parameter.
    LeftEqual(TypeId, TypeId),
    /// function, argument, index
    /// The argument is used as a parameter to function at index.
    ArgumentOf(TypeId, TypeId, usize),
    /// function, result
    /// The return type of function is result.
    ResultOf(TypeId, TypeId),
    /// The type can be called with N arguments..
    /// Provides negative effect space, ie. effect groups this call is allowed to use in addition
    /// to the surrounding function.
    Call(TypeId, usize, Vec<EffectGroupId>),
    Apply(TypeId, TypeId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub constraints: Vec<Constraint>,
}

#[derive(Default)]
pub struct TypecheckContext {
    types: TypeStore,
    index: Index,
    module: ModuleId,
    constraints: Vec<Constraint>,
    names: Vec<(Spur, TypeId)>,
    resume_type: Option<TypeId>,
    return_type: TypeId,
    extend: RefCell<Vec<Constraint>>,
    late_instantiate: bool,
    function_ty: TypeId,

    negative_effect_space: Vec<EffectGroupId>,

    pub errors: Vec<Error>,
}

impl TypecheckContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Apply all known constraints.
    pub fn apply_constraints(&mut self) {
        while !self.constraints.is_empty() {
            self.constraints.sort();
            let mut applied = HashSet::new();
            for constraint in &self.constraints {
                if self.solve_constraint(constraint) {
                    applied.insert(constraint.clone());
                }
            }

            self.constraints.retain(|x| !applied.contains(x));

            let mut extend = self.extend.borrow_mut();
            self.constraints.extend_from_slice(&extend);
            extend.clear();

            if applied.is_empty() {
                break;
            }
        }

        if !self.constraints.is_empty() {
            self.errors.push(Error {
                constraints: std::mem::take(&mut self.constraints),
            });
        }
    }

    /// Attempts to solve a constraint. Returns true if the constraint was solved.
    fn solve_constraint(&self, constraint: &Constraint) -> bool {
        match constraint {
            &Constraint::Equal(a, b) => self.unify(a, b),
            &Constraint::LeftEqual(a, b) => {
                match (self.types.get_concrete(a), self.types.get_concrete(b)) {
                    (ConcreteType::Unknown(_), x) => {
                        self.types
                            .replace(a, self.types.insert_concrete(x, self.types.get_span(b)));
                        true
                    }
                    (a, b) if a == b => true,
                    (_, _) => false,
                }
            }
            &Constraint::ArgumentOf(func, arg, index) => self.solve_argumentof(func, arg, index),
            &Constraint::ResultOf(func, res) => {
                match (self.types.get(func), self.types.get(res)) {
                    // If we don't know either type yet, solving is impossible.
                    (Type::Unknown(_), Type::Unknown(_)) => false,
                    (Type::Function { output, .. }, _) => self.unify(res, output),
                    (_a, _b) => false,
                }
            }
            Constraint::Call(func, count, negative) => self.solve_call(*func, *count, negative),
            &Constraint::Apply(forall, replace_with) => match self.types.get(forall) {
                Type::Unknown(_) => false,
                Type::Forall(param, _) => {
                    self.rewrite_type_parameter(param, replace_with, forall);
                    true
                }
                Type::Parameter(_) => todo!(),
                _ => false,
            },
        }
    }

    fn solve_call(&self, func: TypeId, count: usize, negative: &[EffectGroupId]) -> bool {
        match self.types.get(func) {
            Type::Forall(_, _) => unreachable!(),
            Type::Function {
                inputs, effects, ..
            } => {
                if inputs.len() != count {
                    return false;
                }

                let out_eff = match self.types.walk_forall(self.function_ty) {
                    Type::Function { effects, .. } => effects,
                    _ => unreachable!(),
                };

                out_eff.is_superset(&effects.remove(negative))
            }
            _ => false,
        }
    }

    fn solve_argumentof(&self, func: TypeId, arg: TypeId, index: usize) -> bool {
        match (self.types.get(func), self.types.get(arg)) {
            // If we don't know either type yet, solving is impossible.
            (Type::Unknown(_), Type::Unknown(_)) => false,
            // If the function has too few inputs, solving is impossible.
            (Type::Function { inputs, .. }, _) if inputs.len() <= index => false,
            (Type::Function { inputs, .. }, _) => self.unify(inputs[index], arg),
            (_a, _b) => false,
        }
    }

    fn unify(&self, a: TypeId, b: TypeId) -> bool {
        match (self.types.get(a), self.types.get(b)) {
            (Type::Unknown(_), Type::Unknown(_)) => false,
            (Type::Unknown(_), _) => {
                self.types.replace_deep(a, b);
                true
            }
            (_, Type::Unknown(_)) => {
                self.types.replace_deep(b, a);
                true
            }
            (
                Type::Function {
                    inputs: inputs1,
                    output: output1,
                    effects: effects1,
                },
                Type::Function {
                    inputs: inputs2,
                    output: output2,
                    effects: effects2,
                },
            ) => {
                if inputs1.len() != inputs2.len() {
                    return false;
                }

                if effects1 != effects2 {
                    return false;
                }

                // TODO: See if this can bw done without creating new constraints.
                let mut extend = self.extend.borrow_mut();
                for (a, b) in zip(inputs1, inputs2) {
                    extend.push(Constraint::Equal(a, b));
                }
                extend.push(Constraint::Equal(output1, output2));
                true
            }
            (a, b) if a == b => true,
            (_a, _b) => false,
        }
    }

    fn instantiate_fn(&self, id: TypeId, span: Span) {
        loop {
            match self.types.get(id) {
                Type::Forall(param, _) => {
                    self.rewrite_type_parameter(param, self.types.unknown_type(span), id);
                }
                _ => break,
            }
        }
    }

    fn rewrite_type_parameter(&self, param: u32, replace_with: TypeId, ty: TypeId) {
        match self.types.get(ty) {
            Type::Function { inputs, output, .. } => {
                for input in inputs {
                    self.rewrite_type_parameter(param, replace_with, input);
                }
                self.rewrite_type_parameter(param, replace_with, output);
            }
            Type::Forall(p, x) if p == param => {
                self.rewrite_type_parameter(param, replace_with, x);
                self.types.replace(ty, x);
            }
            Type::Forall(_p, x) => {
                self.rewrite_type_parameter(param, replace_with, x);
            }
            Type::Parameter(p) if p == param => {
                self.types.replace(ty, replace_with);
            }
            Type::Parameter(_) => {}
            _ => {}
        }
    }
}

impl HlirVisitorImmut for TypecheckContext {
    fn visit_hlir(&mut self, hlir: &Hlir) -> VisitAction {
        self.types = hlir.types.clone();
        self.index = hlir.construct_index();
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &Module) -> VisitAction {
        self.module = module.id;

        // We need to solve functions in a specific order. Take for example:
        //
        // ```ignore
        // fn foo(x: _) { takes_int(x); }
        // fn bar() { foo(0); }
        // ```
        //
        // This is a completely valid program, but if we attempt to solve `bar` before `foo`, we
        // won't yet know the type foo takes.
        let mut function_order = vec![];
        for (&id, func) in &module.functions {
            if self.types.type_is_solvwd(func.header.ty) {
                function_order.push(id);
            } else {
                function_order.insert(0, id);
            }
        }
        for id in function_order {
            self.visit_function(module.functions.get(&id).unwrap());
        }
        VisitAction::Nothing
    }

    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        self.return_type = function.return_ty;
        self.function_ty = function.header.ty;
        for arg in &function.arguments {
            self.names.push((arg.name, arg.ty));
        }
        self.walk_node(&function.body);
        for _ in &function.arguments {
            self.names.pop();
        }
        self.apply_constraints();
        VisitAction::Nothing
    }

    fn visit_node(&mut self, node: &Node) -> VisitAction {
        use Constraint::*;
        match &node.kind {
            NodeKind::Handle {
                group_id,
                expr,
                handlers,
                ..
            } => {
                // Clone to satisfy borrow checker.
                let group = self.index.effect_groups.get(group_id).unwrap().clone();
                for handler in handlers {
                    let effect = group
                        .effects
                        .iter()
                        .find(|e| e.header.id == handler.effect_id)
                        .unwrap();
                    self.types.replace(handler.ty, effect.header.ty);

                    for (name, arg) in zip(&handler.arguments, &effect.arguments) {
                        self.names.push((*name, arg.ty));
                    }

                    let old_resume =
                        std::mem::replace(&mut self.resume_type, Some(effect.return_ty));

                    self.walk_node(&handler.body);

                    self.resume_type = old_resume;

                    for _ in &handler.arguments {
                        self.names.pop();
                    }
                }
                self.negative_effect_space.push(*group_id);
                self.walk_node(expr);
                self.negative_effect_space.pop();
                VisitAction::Nothing
            }
            NodeKind::Let {
                name,
                name_span,
                value,
                expr,
            } => {
                self.walk_node(value);
                let lhs_ty = self.types.insert(Type::Ref(value.ty), *name_span);
                self.names.push((*name, lhs_ty));
                self.walk_node(expr);
                self.names.pop();

                VisitAction::Nothing
            }
            NodeKind::Assign { name, value } => {
                let ty = self
                    .names
                    .iter()
                    .rev()
                    .find(|(k, _)| k == name)
                    .map(|(_, t)| *t);
                if let Some(ty) = ty {
                    self.constraints.push(Equal(value.ty, ty));
                }

                VisitAction::Recurse
            }
            NodeKind::Binop { op, left, right } => {
                self.constraints.push(Equal(left.ty, right.ty));
                match op {
                    BinopKind::Equals | BinopKind::Less | BinopKind::Greater => {
                        self.constraints
                            .push(Equal(node.ty, self.types.bool(node.source_span)));
                    }
                    BinopKind::Add => {
                        self.constraints.push(Equal(node.ty, left.ty));
                    }
                }
                VisitAction::Recurse
            }
            NodeKind::Call { callee, args } => {
                for (idx, arg) in args.iter().enumerate() {
                    self.constraints.push(ArgumentOf(callee.ty, arg.ty, idx));
                }
                self.constraints.push(ResultOf(callee.ty, node.ty));
                self.constraints.push(Call(
                    callee.ty,
                    args.len(),
                    self.negative_effect_space.clone(),
                ));
                VisitAction::Recurse
            }
            NodeKind::Resume { arg } => {
                match (self.resume_type, arg) {
                    (Some(ty), Some(arg)) => self.constraints.push(Equal(arg.ty, ty)),
                    (Some(ty), None) => self
                        .constraints
                        .push(Equal(self.types.unit(node.source_span), ty)),
                    (None, _) => unreachable!(),
                }
                VisitAction::Recurse
            }
            NodeKind::If { cond, .. } => {
                self.constraints
                    .push(Equal(cond.ty, self.types.bool(node.source_span)));
                VisitAction::Recurse
            }
            NodeKind::While { cond, .. } => {
                self.constraints
                    .push(Equal(cond.ty, self.types.bool(node.source_span)));
                VisitAction::Recurse
            }
            NodeKind::Block(_) => VisitAction::Recurse,
            NodeKind::Literal(lit) => {
                let ty = match lit {
                    Literal::String(_) => self.types.string(node.source_span),
                    Literal::Int(_) => self.types.int(node.source_span),
                    Literal::Bool(_) => self.types.bool(node.source_span),
                };
                self.constraints.push(Equal(node.ty, ty));
                VisitAction::Recurse
            }
            NodeKind::Name(key) => {
                let ty = self
                    .names
                    .iter()
                    .rev()
                    .find(|(k, _)| k == key)
                    .map(|(_, t)| *t);
                if let Some(ty) = ty {
                    self.constraints.push(Equal(node.ty, ty));
                }
                VisitAction::Recurse
            }
            NodeKind::Builtin(_) => VisitAction::Recurse,
            NodeKind::Function(id) => {
                let header = self.index.functions.get(id).unwrap();
                let ty = match header {
                    Header::Fn(header) => header.ty,
                    Header::Effect(header) => header.ty,
                };
                let span = self.types.get_span(ty);
                self.types.replace(
                    node.ty,
                    self.types
                        .insert_concrete(self.types.get_concrete(ty), span),
                );
                if !self.late_instantiate {
                    self.instantiate_fn(node.ty, node.source_span);
                }
                VisitAction::Recurse
            }
            NodeKind::Return(value) => {
                if let Some(value) = value {
                    self.constraints
                        .push(Constraint::Equal(value.ty, self.return_type));
                }
                VisitAction::Recurse
            }
            NodeKind::ApplyType { expr, ty, .. } => {
                let late_instantiate = self.late_instantiate;
                self.late_instantiate = true;

                self.walk_node(expr);
                self.late_instantiate = late_instantiate;

                self.constraints
                    .push(Constraint::LeftEqual(node.ty, expr.ty));
                self.constraints.push(Constraint::Apply(node.ty, *ty));

                VisitAction::Nothing
            }
        }
    }
}

pub fn report_unknown_types(hlir: &Hlir) {
    struct Visitor(TypeStore);

    impl HlirVisitorImmut for Visitor {
        fn visit_node(&mut self, node: &Node) -> VisitAction {
            match self.0.get(node.ty) {
                Type::Unknown(id) => {
                    println!("Unknown type: ?{}", id);
                    println!("{:?}", node);
                }
                _ => {}
            }

            VisitAction::Recurse
        }
    }

    Visitor(hlir.types.clone()).walk_hlir(hlir);
}
