use std::{cell::RefCell, collections::HashMap, rc::Rc};

use lasso::Spur;

use crate::{
    hlir::{
        visitor::{HlirVisitorImmut, VisitAction},
        FnDef, Hlir, Literal, Node, NodeKind, Type, TypeId,
    },
    parser::BinopKind,
};

/// TODO: Figure out a way to do deduplication without duplicating the items.
#[derive(Clone, Debug, Default, PartialEq)]
struct TypeStoreInternal {
    types: Vec<Type>,
    dedup: HashMap<Type, TypeId>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct TypeStore(Rc<RefCell<TypeStoreInternal>>);

impl TypeStore {
    pub fn unit(&self) -> TypeId {
        self.insert(Type::Unit)
    }

    pub fn bool(&self) -> TypeId {
        self.insert(Type::Bool)
    }

    pub fn int(&self) -> TypeId {
        self.insert(Type::Int)
    }

    pub fn string(&self) -> TypeId {
        self.insert(Type::String)
    }

    pub fn unknown_type(&self) -> TypeId {
        self.insert_nodedup(Type::Unknown)
    }

    pub fn insert(&self, ty: Type) -> TypeId {
        assert_ne!(ty, Type::Unknown);
        let mut store = self.0.borrow_mut();

        if let Some(id) = store.dedup.get(&ty) {
            return *id;
        }

        let id = TypeId(store.types.len());
        store.types.push(ty.clone());
        store.dedup.insert(ty, id);
        id
    }

    pub fn insert_nodedup(&self, ty: Type) -> TypeId {
        let mut store = self.0.borrow_mut();

        let id = TypeId(store.types.len());
        store.types.push(ty);
        id
    }

    pub fn get(&self, id: TypeId) -> Type {
        let store = self.0.borrow();
        store
            .types
            .get(id.0)
            .expect("TypeStore::get on unknown TypeId")
            .clone()
    }

    fn replace(&self, id: TypeId, ty: Type) {
        let mut store = self.0.borrow_mut();
        store.types[id.0] = ty;
    }
}

#[derive(Debug)]
enum Constraint {
    Equal(TypeId, TypeId),
    ArgumentOf(TypeId, TypeId, usize),
    ResultOf(TypeId, TypeId),
}

pub struct TypecheckContext {
    types: TypeStore,
    constraints: Vec<Constraint>,
    names: Vec<(Spur, TypeId)>,
}

impl TypecheckContext {
    pub fn new(types: &TypeStore) -> Self {
        Self {
            types: types.clone(),
            constraints: Default::default(),
            names: Default::default(),
        }
    }

    pub fn apply_constraints(&self) {
        loop {
            let mut applied = false;
            for constraint in &self.constraints {
                match constraint {
                    &Constraint::Equal(a, b) => match (self.types.get(a), self.types.get(b)) {
                        (Type::Unknown, Type::Unknown) => continue,
                        (Type::Unknown, ty) => self.types.replace(a, ty),
                        (ty, Type::Unknown) => self.types.replace(b, ty),
                        (a, b) if a == b => continue,
                        (a, b) => panic!("Type mismatch {a:?}, {b:?}"),
                    },
                    &Constraint::ArgumentOf(func, arg, index) => {
                        match (self.types.get(func), self.types.get(arg)) {
                            (Type::Function { .. }, Type::Unknown) => continue,
                            (Type::Unknown, Type::Unknown) => continue,
                            (Type::Function { inputs, .. }, arg_ty) => {
                                let in_ty = self.types.get(inputs[index]);
                                if in_ty != arg_ty {
                                    panic!("ArgumentOf mismatch {func:?} {in_ty:?}, {arg:?} {arg_ty:?}");
                                }
                                continue;
                            }
                            (a, b) => panic!("ArgumentOf unify error {a:?}, {b:?}"),
                        }
                    }
                    &Constraint::ResultOf(func, res) => {
                        match (self.types.get(func), self.types.get(res)) {
                            (Type::Unknown, Type::Unknown) => continue,
                            (Type::Function { output, .. }, Type::Unknown) => {
                                self.types.replace(res, self.types.get(output))
                            }
                            (Type::Function { output, .. }, x) if self.types.get(output) == x => {
                                continue
                            }
                            (a, b) => panic!("ResultOf unify error {a:?}, {b:?}"),
                        }
                    }
                }
                applied = true;
            }
            if !applied {
                break;
            }
        }
    }
}

impl HlirVisitorImmut for TypecheckContext {
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
            NodeKind::Let { name, value, expr } => {
                self.walk_node(value);
                self.names.push((*name, value.ty));
                self.walk_node(expr);
                self.names.pop();

                VisitAction::Nothing
            }
            NodeKind::Binop { op, left, right } => {
                self.constraints.push(Equal(left.ty, right.ty));
                match op {
                    BinopKind::Equals => {
                        self.constraints.push(Equal(node.ty, self.types.bool()));
                    }
                    BinopKind::Less => {
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
            NodeKind::If {
                cond,
                if_true,
                if_false,
            } => {
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
        }
    }
}

pub fn report_unknown_types(hlir: &Hlir) {
    struct Visitor(TypeStore);

    impl HlirVisitorImmut for Visitor {
        fn visit_node(&mut self, node: &Node) -> VisitAction {
            match self.0.get(node.ty) {
                Type::Unknown => {
                    println!("Unknown type: ?{}", node.ty.0);
                    println!("{:?}", node);
                }
                _ => {}
            }

            VisitAction::Recurse
        }
    }

    Visitor(hlir.types.clone()).walk_hlir(hlir);
}
