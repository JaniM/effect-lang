use std::{cell::RefCell, rc::Rc};

use lasso::Spur;
use tinyvec::TinyVec;

use crate::{
    hlir::{EffectGroupId, Span, Spanned},
    inc,
};

/// An opaque id used with [TypeStore].
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub usize);

/// Describes the set of effects a function uses. A set is open if it can be extended to allow
/// unspecified effects.
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

#[derive(Clone, Debug, Default, PartialEq)]
struct TypeStoreInternal {
    types: Vec<Spanned<Type>>,
    unknowns: usize,
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
    pub(super) fn join(&self, other: &EffectSet) -> Option<Self> {
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

    pub(super) fn remove(&self, items: &[EffectGroupId]) -> Self {
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
    pub(super) fn is_superset(&self, other: &EffectSet) -> bool {
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
    pub fn unit(&self, span: Span) -> TypeId {
        self.insert(Type::Unit, span)
    }

    /// Get a deduplicated boolean type.
    pub fn bool(&self, span: Span) -> TypeId {
        self.insert(Type::Bool, span)
    }

    /// Get a deduplicated integer type.
    pub fn int(&self, span: Span) -> TypeId {
        self.insert(Type::Int, span)
    }

    /// Get a deduplicated string type.
    pub fn string(&self, span: Span) -> TypeId {
        self.insert(Type::String, span)
    }

    /// Get a new unknown type. This will always be unique. Used to produce a type inference point
    /// in the [Hlir] tree.
    pub fn unknown_type(&self, span: Span) -> TypeId {
        let id = {
            let mut store = self.0.borrow_mut();
            inc!(store.unknowns) as u32
        };
        self.insert(Type::Unknown(id), span)
    }

    /// Inserts a new type to the store.
    pub fn insert(&self, ty: Type, span: Span) -> TypeId {
        let mut store = self.0.borrow_mut();

        let id = TypeId(store.types.len());
        store.types.push(Spanned(ty, span));
        id
    }

    pub fn get_spanned_noderef(&self, id: TypeId) -> Spanned<Type> {
        let store = self.0.borrow();
        store
            .types
            .get(id.0)
            .expect("TypeStore::get on unknown TypeId")
            .clone()
    }

    /// Get a type from the store. Clones the type, so modifying it directly will have no effect.
    /// Use [`Self::replace()`] to update it.
    pub fn get_spanned(&self, id: TypeId) -> Spanned<Type> {
        let store = self.0.borrow();
        let mut ty = &Spanned(Type::Ref(id), Span::default());
        while let Type::Ref(id) = &ty.0 {
            ty = store
                .types
                .get(id.0)
                .expect("TypeStore::get on unknown TypeId");
        }
        ty.clone()
    }

    /// Get a type from the store. Clones the type, so modifying it directly will have no effect.
    /// Use [`Self::replace()`] to update it.
    pub fn get(&self, id: TypeId) -> Type {
        self.get_spanned(id).0
    }

    /// Get a type's span from the store.
    pub fn get_span(&self, id: TypeId) -> Span {
        self.get_spanned(id).1
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
    pub fn insert_concrete(&self, ty: ConcreteType, span: Span) -> TypeId {
        match ty {
            ConcreteType::Function {
                inputs,
                output,
                effects,
            } => self.insert(
                Type::Function {
                    inputs: inputs
                        .into_iter()
                        .map(|x| self.insert_concrete(x, span))
                        .collect(),
                    output: self.insert_concrete(*output, span),
                    effects,
                },
                span,
            ),
            ConcreteType::Forall(x, t) => {
                self.insert(Type::Forall(x, self.insert_concrete(*t, span)), span)
            }
            ConcreteType::Parameter(x) => self.insert(Type::Parameter(x), span),
            ConcreteType::Unknown(x) => self.insert(Type::Unknown(x), span),
            ConcreteType::Name(x) => self.insert(Type::Name(x), span),
            ConcreteType::String => self.insert(Type::String, span),
            ConcreteType::Int => self.insert(Type::Int, span),
            ConcreteType::Bool => self.insert(Type::Bool, span),
            ConcreteType::Unit => self.insert(Type::Unit, span),
        }
    }

    /// Replaces the type `id` refers to.
    pub fn replace(&self, id: TypeId, ty: TypeId) {
        if id == ty {
            return;
        }
        let mut store = self.0.borrow_mut();
        store.types[id.0].0 = Type::Ref(ty);
    }

    /// Replaces the referred type.
    pub fn replace_deep(&self, mut id: TypeId, new_ty: TypeId) {
        let mut store = self.0.borrow_mut();
        loop {
            match store.types[id.0].0 {
                Type::Ref(new_id) => id = new_id,
                _ => break,
            }
        }
        store.types[id.0].0 = Type::Ref(new_ty);
    }

    /// Rewrites the referred type.
    pub fn rewrite(&self, mut id: TypeId, new_ty: Type) {
        let mut store = self.0.borrow_mut();
        loop {
            match store.types[id.0].0 {
                Type::Ref(new_id) => id = new_id,
                _ => break,
            }
        }
        store.types[id.0].0 = new_ty;
    }

    pub(super) fn type_is_solvwd(&self, id: TypeId) -> bool {
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
