mod test;

use std::{cell::RefCell, collections::HashSet, iter::zip, rc::Rc};

use lasso::Spur;
use tinyvec::TinyVec;

use crate::{
    hlir::{
        index::{Header, Index},
        visitor::{HlirVisitorImmut, VisitAction},
        EffectGroupId, FnDef, Hlir, Literal, Module, ModuleId, Node, NodeKind,
    },
    inc,
    parser::BinopKind,
};

/// An opaque id used with [TypeStore].
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EffectSet {
    Solved {
        effects: Rc<Vec<EffectGroupId>>,
        open: bool,
    },
    Unsolved {
        names: Vec<Spur>,
        open: bool,
    },
}

/// A type representing all possible types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Ref(TypeId),
    /// The type of this node is currently unknown. Used as a type inference point.
    Unknown(u32),
    /// A currently unresolved type name.
    Name(Spur),
    Function {
        inputs: TinyVec<[TypeId; 4]>,
        output: TypeId,
        effects: EffectSet,
    },
    /// The type is valid for all types represented by the parameter.
    Forall(u32, TypeId),
    /// A type parameter referring to some Forall
    Parameter(u32),
    String,
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConcreteType {
    /// The type of this node is currently unknown. Used as a type inference point.
    Unknown(u32),
    /// A currently unresolved type name.
    Name(Spur),
    Function {
        inputs: Vec<ConcreteType>,
        output: Box<ConcreteType>,
        effects: EffectSet,
    },
    /// The type is valid for all types represented by the parameter.
    Forall(u32, Box<ConcreteType>),
    /// A type parameter referring to some Forall
    Parameter(u32),
    String,
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
struct TypeStoreInternal {
    types: Vec<Type>,
    unknowns: usize,
}

impl TypeStoreInternal {
    const UNIT: TypeId = TypeId(0);
    const INT: TypeId = TypeId(1);
    const STRING: TypeId = TypeId(2);
    const BOOL: TypeId = TypeId(3);
}

impl Default for TypeStoreInternal {
    fn default() -> Self {
        Self {
            types: vec![Type::Unit, Type::Int, Type::String, Type::Bool],
            unknowns: Default::default(),
        }
    }
}

/// Stores types in an opaque, easily modifiable/queryable way. Produces a [TypeId] for each
/// inserted type.
///
/// It is generally expected that a [Node]'s [TypeId] is *never* changed.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct TypeStore(Rc<RefCell<TypeStoreInternal>>);

impl Default for EffectSet {
    fn default() -> Self {
        Self::Solved {
            effects: Default::default(),
            open: true,
        }
    }
}

impl EffectSet {
    /// Produce a new EffectSet out of known effects.
    pub fn new(mut effects: Vec<EffectGroupId>, open: bool) -> Self {
        effects.sort_unstable();
        effects.dedup();
        Self::Solved {
            effects: effects.into(),
            open,
        }
    }

    /// Produce a new EffectSet as a set join of two EffectSets.
    /// This will return `None` if self is closed and the joined set isn't equal to self.
    fn join(&self, other: &EffectSet) -> Option<Self> {
        let (vec1, open1) = self.assume_solved();
        let (vec2, _) = other.assume_solved();

        let mut vec = vec1.clone();
        vec.extend_from_slice(&vec2);
        vec.sort_unstable();
        vec.dedup();

        if !open1 && &vec != vec1 {
            return None;
        }

        Some(Self::Solved {
            effects: vec.into(),
            open: open1,
        })
    }

    fn remove(&self, items: &[EffectGroupId]) -> Self {
        let (vec1, open1) = self.assume_solved();

        let vec: Vec<_> = vec1
            .iter()
            .copied()
            .filter(|x| !items.contains(x))
            .collect();

        Self::Solved {
            effects: vec.into(),
            open: open1,
        }
    }

    /// Return true if `self` is a strict superset of `other`.
    fn is_superset(&self, other: &EffectSet) -> bool {
        let (vec1, _) = self.assume_solved();
        let (vec2, _) = other.assume_solved();

        for item in vec2 {
            if !vec1.contains(item) {
                return false;
            }
        }
        true
    }

    fn assume_solved(&self) -> (&Vec<EffectGroupId>, bool) {
        match self {
            EffectSet::Solved { effects, open } => (effects, *open),
            EffectSet::Unsolved { .. } => unreachable!("EffectSet::assume_solved not solved"),
        }
    }
}

impl TypeStore {
    /// Get a deduplicated unit type.
    pub fn unit(&self) -> TypeId {
        TypeStoreInternal::UNIT
    }

    /// Get a deduplicated boolean type.
    pub fn bool(&self) -> TypeId {
        TypeStoreInternal::BOOL
    }

    /// Get a deduplicated integer type.
    pub fn int(&self) -> TypeId {
        TypeStoreInternal::INT
    }

    /// Get a deduplicated string type.
    pub fn string(&self) -> TypeId {
        TypeStoreInternal::STRING
    }

    /// Get a new unknown type. This will always be unique. Used to produce a type inference point
    /// in the [Hlir] tree.
    pub fn unknown_type(&self) -> TypeId {
        let id = {
            let mut store = self.0.borrow_mut();
            inc!(store.unknowns) as u32
        };
        self.insert(Type::Unknown(id))
    }

    /// Inserts a new type to the store.
    pub fn insert(&self, ty: Type) -> TypeId {
        let mut store = self.0.borrow_mut();

        let id = TypeId(store.types.len());
        store.types.push(ty);
        id
    }

    /// Get a type from the store. Clones the type, so modifying it directly will have no effect.
    /// Use [`Self::replace()`] to update it.
    pub fn get(&self, id: TypeId) -> Type {
        let store = self.0.borrow();
        let mut ty = &Type::Ref(id);
        while let Type::Ref(id) = ty {
            ty = store
                .types
                .get(id.0)
                .expect("TypeStore::get on unknown TypeId");
        }
        ty.clone()
    }

    pub fn walk_forall(&self, id: TypeId) -> Type {
        let mut ty = self.get(id);
        loop {
            match ty {
                Type::Forall(_, id) => ty = self.get(id),
                x => return x,
            }
        }
    }

    pub fn get_concrete(&self, id: TypeId) -> ConcreteType {
        match self.get(id) {
            Type::Ref(id) => self.get_concrete(id),
            Type::Function {
                inputs,
                output,
                effects,
            } => ConcreteType::Function {
                inputs: inputs.into_iter().map(|x| self.get_concrete(x)).collect(),
                output: self.get_concrete(output).into(),
                effects,
            },
            Type::Forall(x, id) => ConcreteType::Forall(x, self.get_concrete(id).into()),
            Type::Parameter(x) => ConcreteType::Parameter(x),
            Type::Unknown(x) => ConcreteType::Unknown(x),
            Type::Name(x) => ConcreteType::Name(x),
            Type::String => ConcreteType::String,
            Type::Int => ConcreteType::Int,
            Type::Bool => ConcreteType::Bool,
            Type::Unit => ConcreteType::Unit,
        }
    }

    #[allow(unused)]
    pub fn insert_concrete(&self, ty: ConcreteType) -> TypeId {
        match ty {
            ConcreteType::Function {
                inputs,
                output,
                effects,
            } => self.insert(Type::Function {
                inputs: inputs
                    .into_iter()
                    .map(|x| self.insert_concrete(x))
                    .collect(),
                output: self.insert_concrete(*output),
                effects,
            }),
            ConcreteType::Forall(x, t) => self.insert(Type::Forall(x, self.insert_concrete(*t))),
            ConcreteType::Parameter(x) => self.insert(Type::Parameter(x)),
            ConcreteType::Unknown(x) => self.insert(Type::Unknown(x)),
            ConcreteType::Name(x) => self.insert(Type::Name(x)),
            ConcreteType::String => self.insert(Type::String),
            ConcreteType::Int => self.insert(Type::Int),
            ConcreteType::Bool => self.insert(Type::Bool),
            ConcreteType::Unit => self.insert(Type::Unit),
        }
    }

    /// Replaces the type `id` refers to.
    pub fn replace(&self, id: TypeId, ty: TypeId) {
        if id == ty {
            return;
        }
        let mut store = self.0.borrow_mut();
        store.types[id.0] = Type::Ref(ty);
    }

    /// Rewrites the referred type.
    pub fn rewrite(&self, mut id: TypeId, new_ty: Type) {
        let mut store = self.0.borrow_mut();
        loop {
            match store.types[id.0] {
                Type::Ref(new_id) => id = new_id,
                _ => break,
            }
        }
        store.types[id.0] = new_ty;
    }

    fn type_is_solvwd(&self, id: TypeId) -> bool {
        match self.get(id) {
            Type::Unknown(_) => false,
            Type::Name(_) => false,
            Type::Function { inputs, output, .. } => {
                !(inputs.into_iter().any(|x| !self.type_is_solvwd(x))
                    || !self.type_is_solvwd(output))
            }
            Type::Forall(_, x) => self.type_is_solvwd(x),
            _ => true,
        }
    }
}

/// Represents a type constraint. Used by [`TypecheckContext::apply_constraints()`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
                        self.types.replace(a, self.types.insert_concrete(x));
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
            (Type::Unknown(_), ty) => {
                self.types.rewrite(a, ty);
                true
            }
            (ty, Type::Unknown(_)) => {
                self.types.rewrite(b, ty);
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

    fn instantiate_fn(&self, id: TypeId) {
        loop {
            match self.types.get(id) {
                Type::Forall(param, _) => {
                    self.rewrite_type_parameter(param, self.types.unknown_type(), id);
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
            NodeKind::Let { name, value, expr } => {
                self.walk_node(value);
                self.names.push((*name, value.ty));
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
                        self.constraints.push(Equal(node.ty, self.types.bool()));
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
                    (Some(ty), None) => self.constraints.push(Equal(self.types.unit(), ty)),
                    (None, _) => unreachable!(),
                }
                VisitAction::Recurse
            }
            NodeKind::If { cond, .. } => {
                self.constraints.push(Equal(cond.ty, self.types.bool()));
                VisitAction::Recurse
            }
            NodeKind::While { cond, .. } => {
                self.constraints.push(Equal(cond.ty, self.types.bool()));
                VisitAction::Recurse
            }
            NodeKind::Block(_) => VisitAction::Recurse,
            NodeKind::Literal(lit) => {
                let ty = match lit {
                    Literal::String(_) => self.types.string(),
                    Literal::Int(_) => self.types.int(),
                    Literal::Bool(_) => self.types.bool(),
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
                self.types.replace(
                    node.ty,
                    self.types.insert_concrete(self.types.get_concrete(ty)),
                );
                if !self.late_instantiate {
                    self.instantiate_fn(node.ty);
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
