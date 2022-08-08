use std::{cell::RefCell, collections::HashSet, rc::Rc};

use lasso::Spur;

use crate::{
    hlir::{
        index::Index,
        visitor::{HlirVisitorImmut, VisitAction},
        FnDef, Hlir, Literal, Module, ModuleId, Node, NodeKind,
    },
    inc,
    parser::BinopKind,
};

/// An opaque id used with [TypeStore].
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

/// A type representing all possible types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    /// The type of this node is currently unknown. Used as a type inference point.
    Unknown(u32),
    /// A currently unresolved type name.
    Name(Spur),
    Function {
        inputs: Vec<TypeId>,
        output: TypeId,
    },
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
    },
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
        store
            .types
            .get(id.0)
            .expect("TypeStore::get on unknown TypeId")
            .clone()
    }

    pub fn get_concrete(&self, id: TypeId) -> ConcreteType {
        match self.get(id) {
            Type::Function { inputs, output } => ConcreteType::Function {
                inputs: inputs.into_iter().map(|x| self.get_concrete(x)).collect(),
                output: self.get_concrete(output).into(),
            },
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
            ConcreteType::Function { inputs, output } => self.insert(Type::Function {
                inputs: inputs
                    .into_iter()
                    .map(|x| self.insert_concrete(x))
                    .collect(),
                output: self.insert_concrete(*output),
            }),
            ConcreteType::Unknown(x) => self.insert(Type::Unknown(x)),
            ConcreteType::Name(x) => self.insert(Type::Name(x)),
            ConcreteType::String => self.insert(Type::String),
            ConcreteType::Int => self.insert(Type::Int),
            ConcreteType::Bool => self.insert(Type::Bool),
            ConcreteType::Unit => self.insert(Type::Unit),
        }
    }

    /// Replaces the type `id` refers to.
    fn replace(&self, id: TypeId, ty: Type) {
        let mut store = self.0.borrow_mut();
        store.types[id.0] = ty;
    }
}

/// Represents a type constraint. Used by [`TypecheckContext::apply_constraints()`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Constraint {
    /// The two types must be equal.
    Equal(TypeId, TypeId),
    /// function, argument, index
    /// The argument is used as a parameter to function at index.
    ArgumentOf(TypeId, TypeId, usize),
    /// function, result
    /// The return type of function is result.
    ResultOf(TypeId, TypeId),
}

#[derive(Default)]
pub struct TypecheckContext {
    types: TypeStore,
    index: Index,
    module: ModuleId,
    constraints: Vec<Constraint>,
    names: Vec<(Spur, TypeId)>,
    resume_type: Option<TypeId>,
}

impl TypecheckContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn apply_constraints(&mut self) {
        while !self.constraints.is_empty() {
            let mut extend = Vec::new();
            let mut applied = HashSet::new();
            for &constraint in &self.constraints {
                match constraint {
                    Constraint::Equal(a, b) => match (self.types.get(a), self.types.get(b)) {
                        (Type::Unknown(_), Type::Unknown(_)) => continue,
                        (Type::Unknown(_), ty) => self.types.replace(a, ty),
                        (ty, Type::Unknown(_)) => self.types.replace(b, ty),
                        (a, b) if a == b => continue,
                        (a, b) => panic!("Type mismatch {a:?}, {b:?}"),
                    },
                    Constraint::ArgumentOf(func, arg, index) => {
                        match (self.types.get(func), self.types.get_concrete(arg)) {
                            (Type::Function { .. }, ConcreteType::Unknown(_)) => continue,
                            (Type::Unknown(_), ConcreteType::Unknown(_)) => continue,
                            (Type::Function { inputs, .. }, arg_ty) => {
                                let in_ty = self.types.get_concrete(inputs[index]);
                                if matches!(in_ty, ConcreteType::Unknown(_)) {
                                    extend.push(Constraint::Equal(inputs[index], arg));
                                } else if in_ty != arg_ty {
                                    panic!("ArgumentOf mismatch {func:?} {in_ty:?}, {arg:?} {arg_ty:?}");
                                }
                            }
                            (a, b) => panic!("ArgumentOf unify error {a:?}, {b:?}"),
                        }
                    }
                    Constraint::ResultOf(func, res) => {
                        match (self.types.get(func), self.types.get(res)) {
                            (Type::Unknown(_), Type::Unknown(_)) => continue,
                            (Type::Function { output, .. }, Type::Unknown(_)) => {
                                self.types.replace(res, self.types.get(output))
                            }
                            (Type::Function { output, .. }, x) if self.types.get(output) == x => {
                                continue
                            }
                            (a, b) => panic!("ResultOf unify error {a:?}, {b:?}"),
                        }
                    }
                }
                applied.insert(constraint);
            }
            self.constraints.retain(|x| !applied.contains(x));
            self.constraints.extend_from_slice(&extend);
            if applied.is_empty() {
                break;
            }
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
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        for arg in &function.arguments {
            self.names.push((arg.name, arg.ty));
        }
        self.walk_node(&function.body);
        for _ in &function.arguments {
            self.names.pop();
        }
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
                    let effect_type = self.types.get(effect.header.ty);
                    self.types.replace(handler.ty, effect_type);

                    for (name, arg) in std::iter::zip(&handler.arguments, &effect.arguments) {
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
                self.walk_node(expr);
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
                VisitAction::Recurse
            }
            NodeKind::Resume { arg } => {
                match self.resume_type {
                    Some(ty) => self.constraints.push(Equal(arg.ty, ty)),
                    None => unreachable!(),
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
            NodeKind::Function(_) => VisitAction::Recurse,
            NodeKind::Return(_value) => {
                // TODO
                VisitAction::Recurse
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
