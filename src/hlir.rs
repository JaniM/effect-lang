pub mod index;
pub mod name_resolve;
pub mod pretty;
pub mod simplify;
pub mod visitor;

use std::collections::HashMap;
use std::rc::Rc;

use lasso::Spur;

use crate::inc;
use crate::intern::INTERNER;
use crate::parser::{self as ast, EffectKind, Generic};
use crate::parser::{BinopKind, TypeProto};
use crate::typecheck::{EffectSet, Type, TypeId, TypeStore};

use self::index::Index;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct FileId(pub Spur);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Span(pub FileId, pub usize, pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EffectGroupId(pub usize);

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub Span);

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", INTERNER.resolve(&self.0))
    }
}

/// Block is a flat sequence of expressions, evaluated in order.
#[derive(Clone, Debug, PartialEq)]
pub struct Block(Vec<Node>);

/// A literal value, known directly from parsing.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
    Int(i64),
    Bool(bool),
}

/// A definition of an effect handler.
#[derive(Clone, Debug, PartialEq)]
pub struct Handler {
    pub effect_id: FunctionId,
    /// Name of the handled effect.
    pub name: Spur,
    /// Names of the arguments passed to this effect handler.
    pub arguments: Vec<Spur>,
    /// The expression evaluated when this handler is executed.
    pub body: Node,
    /// Type of this effect handler. Should resolve to [`Type::Function`].
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Let {
        name: Spur,
        name_span: Span,
        value: Box<Node>,
        expr: Box<Node>,
    },
    Assign {
        name: Spur,
        value: Box<Node>,
    },
    Binop {
        op: BinopKind,
        left: Box<Node>,
        right: Box<Node>,
    },
    Call {
        callee: Box<Node>,
        args: Vec<Node>,
    },
    Resume {
        arg: Option<Box<Node>>,
    },
    If {
        cond: Box<Node>,
        if_true: Box<Node>,
        if_false: Option<Box<Node>>,
    },
    While {
        cond: Box<Node>,
        body: Box<Node>,
    },
    Handle {
        group_id: EffectGroupId,
        name: Spur,
        handlers: Vec<Handler>,
        expr: Box<Node>,
    },
    Return(Option<Box<Node>>),
    Block(Vec<Node>),
    Literal(Literal),
    Name(Spur),
    Builtin(usize),
    Function(FunctionId),
    ApplyType {
        expr: Box<Node>,
        ty: TypeId,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: TypeId,
    pub source_span: Span,
}

/// The "header" of a function. Represents all useful static information about it for index/query
/// purposes.
#[derive(Clone, Debug, PartialEq)]
pub struct FnHeader {
    pub id: FunctionId,
    pub name: Option<Spur>,
    pub generics: Vec<Generic>,
    pub ty: TypeId,
}

/// Represents a single argument to a function.
#[derive(Clone, Debug, PartialEq)]
pub struct FnArgument {
    pub name: Spur,
    pub ty: TypeId,
}

/// Represents a full function definition, including its header and the actual implementation.
///
/// TODO: Currently duplicates argument and the return types from the actual type of the function.
/// This seems.. less than great, leading to data mangling to keep the state consistent.
#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    pub header: FnHeader,
    pub arguments: Vec<FnArgument>,
    pub return_ty: TypeId,
    pub body: Node,
}

/// The "header" of an effect. Represents all useful static information about it for index/query
/// purposes.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectHeader {
    pub kind: EffectKind,
    pub id: FunctionId,
    pub name: Spur,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectDef {
    pub header: EffectHeader,
    pub arguments: Vec<FnArgument>,
    pub return_ty: TypeId,
}

/// Represents a group of effects, ie. an `effect` item in the language.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectGroup {
    pub id: EffectGroupId,
    pub name: Spur,
    pub effects: Vec<EffectDef>,
}

/// Represents everything there is to know about a module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub file: FileId,
    pub id: ModuleId,
    pub name: Option<Spur>,
    pub functions: HashMap<FunctionId, FnDef>,
    pub effect_groups: HashMap<EffectGroupId, EffectGroup>,
}

/// The top-level structure representing the entire program.
#[derive(Default, Debug)]
pub struct Hlir {
    pub modules: HashMap<ModuleId, Module>,
    pub types: TypeStore,
    pub builtins: Rc<Builtins>,
}

/// A helper structure for building an [`Hlir`].
#[derive(Default)]
pub struct HlirBuilder {
    pub hlir: Hlir,
    module_id_counter: usize,
    func_id_counter: usize,
    effect_group_id_counter: usize,
}

#[derive(Debug)]
pub enum HlirBuildError {
    InvalidTopLevel(Span),
}

impl Hlir {
    /// Construct an [`Index`] for this Hlir.
    pub fn construct_index(&self) -> Index {
        Index::from_hlir(self)
    }
}

impl HlirBuilder {
    /// Load builtin type and name information to the structure. You want to call this before
    /// analyzing the Hlir.
    pub fn load_builtins(&mut self, f: impl FnOnce(&mut BuiltinLoader)) {
        let builtins = Builtins::load(&self.hlir.types, f);
        self.hlir.builtins = builtins.into();
    }

    /// Reads a single module, aka a file. Use this to convert the [AST][ast::Node] of a single
    /// file into the Hlir structure.
    pub fn read_module(
        &mut self,
        file: FileId,
        node: Vec<ast::Node>,
    ) -> Result<(), HlirBuildError> {
        let mut module = Module {
            file,
            id: ModuleId(inc!(self.module_id_counter)),
            name: None,
            functions: HashMap::new(),
            effect_groups: HashMap::new(),
        };

        self.read_ast(&mut module, node)?;

        self.hlir.modules.insert(module.id, module);

        Ok(())
    }

    fn read_ast(
        &mut self,
        module: &mut Module,
        nodes: Vec<ast::Node>,
    ) -> Result<(), HlirBuildError> {
        for node in nodes {
            self.read_top_level(module, node)?;
        }
        Ok(())
    }

    fn read_top_level(
        &mut self,
        module: &mut Module,
        ast::Spanned(node, span): ast::Node,
    ) -> Result<(), HlirBuildError> {
        let span = Span(module.file, span.start, span.end);
        match node {
            ast::RawNode::FnDef(def) => {
                self.read_top_level_function(def, span, module)?;
            }
            ast::RawNode::Effect(group) => {
                self.read_top_level_effect_group(group, span, module);
            }
            _ => return Err(HlirBuildError::InvalidTopLevel(span)),
        }
        Ok(())
    }

    fn read_top_level_effect_group(
        &mut self,
        group: ast::EffectGroup,
        span: Span,
        module: &mut Module,
    ) {
        let group_id = EffectGroupId(inc!(self.effect_group_id_counter));
        let mut effects = Vec::new();
        for ast::Spanned(def, span) in group.effects {
            let span = Span(module.file, span.start, span.end);
            let arguments: Vec<_> = def
                .args
                .into_iter()
                .map(|x| FnArgument {
                    name: x.name,
                    ty: self.typeproto_to_type(&x.ty, &[], module, false),
                })
                .collect();

            let output = self.typeproto_to_type(&def.return_ty, &[], module, true);

            let ty = self.hlir.types.insert(
                Type::Function {
                    inputs: arguments.iter().map(|x| x.ty).collect(),
                    output,
                    effects: EffectSet::new(vec![group_id], false),
                },
                span,
            );

            let header = EffectHeader {
                kind: def.kind,
                id: FunctionId(inc!(self.func_id_counter)),
                name: def.name,
                ty,
            };

            let effect = EffectDef {
                header,
                arguments,
                return_ty: output,
            };
            effects.push(effect);
        }
        let group = EffectGroup {
            id: group_id,
            name: group.name,
            effects,
        };
        module.effect_groups.insert(group.id, group);
    }

    fn typeproto_to_type(
        &self,
        ty: &ast::Spanned<ast::TypeProto>,
        generics: &[Generic],
        module: &Module,
        unknown_is_unit: bool,
    ) -> TypeId {
        let type_store = &self.hlir.types;
        let span_it = |s: ast::Span| Span(module.file, s.start, s.end);
        let source_span = span_it(ty.1.clone());
        match &ty.0 {
            TypeProto::Function {
                arguments,
                return_ty,
                effects,
            } => type_store.insert(
                Type::Function {
                    inputs: arguments
                        .iter()
                        .map(|x| self.typeproto_to_type(x, generics, module, unknown_is_unit))
                        .collect(),
                    output: self.typeproto_to_type(return_ty, generics, module, unknown_is_unit),
                    effects: EffectSet::Unsolved {
                        names: effects.effects.clone(),
                        open: false,
                    },
                },
                source_span,
            ),
            TypeProto::Name(key) => match generics.iter().position(|x| x.name == *key) {
                Some(x) => type_store.insert(Type::Parameter(x as u32), source_span),
                None => type_store.insert(Type::Name(*key), source_span),
            },
            TypeProto::Unknown if unknown_is_unit => type_store.unit(source_span),
            TypeProto::Unknown => self.unknown_type(source_span),
        }
    }

    fn read_top_level_function(
        &mut self,
        def: ast::FnDef,
        span: Span,
        module: &mut Module,
    ) -> Result<(), HlirBuildError> {
        let header_span = Span(module.file, def.header_span.start, def.header_span.end);
        let arguments: Vec<_> = def
            .args
            .into_iter()
            .map(|x| FnArgument {
                name: x.name,
                ty: self.typeproto_to_type(&x.ty, &def.generics, module, false),
            })
            .collect();
        let output = self.typeproto_to_type(&def.return_ty, &def.generics, module, true);
        let mut ty = self.hlir.types.insert(
            Type::Function {
                inputs: arguments.iter().map(|x| x.ty).collect(),
                output,
                effects: EffectSet::Unsolved {
                    names: def.effects.effects,
                    open: false,
                },
            },
            header_span,
        );
        for (id, _generic) in def.generics.iter().enumerate().rev() {
            ty = self
                .hlir
                .types
                .insert(Type::Forall(id as u32, ty), header_span);
        }

        let header = FnHeader {
            id: FunctionId(inc!(self.func_id_counter)),
            generics: def.generics,
            name: def.name,
            ty,
        };
        let func = FnDef {
            header,
            arguments,
            return_ty: output,
            body: self.read_block(module, def.body)?,
        };
        module.functions.insert(func.header.id, func);
        Ok(())
    }

    fn read_block(
        &mut self,
        module: &mut Module,
        ast::Spanned(block, span): ast::Spanned<ast::Block>,
    ) -> Result<Node, HlirBuildError> {
        let nodes = block
            .stmts
            .into_iter()
            .map(|x| self.read_node(module, x))
            .collect::<Result<_, _>>()?;

        let nodes = self.fold_nodes(module, nodes)?;

        let source_span = Span(module.file, span.start, span.end);
        let node = Node {
            kind: NodeKind::Block(nodes),
            ty: self.hlir.types.unit(source_span),
            source_span,
        };

        Ok(node)
    }

    /// Folds lets into a tree structure.
    /// For example, `let a = 0; use(a); use(1);` becomes `let a = 0 in { use(a); use(b); }`
    ///
    /// TODO: This should absolutely be done in the parser.
    fn fold_nodes(
        &mut self,
        module: &mut Module,
        mut nodes: Vec<Node>,
    ) -> Result<Vec<Node>, HlirBuildError> {
        if let Some(idx) = nodes
            .iter()
            .position(|x| matches!(&x.kind, NodeKind::Let { .. }))
        {
            let scope = nodes.drain(idx + 1..).collect::<Vec<_>>();
            let scope = self.fold_nodes(module, scope)?;

            let mut expr_node = self.unit_node();

            if let Some(first) = scope.first() {
                expr_node.source_span.1 = first.source_span.1;
            }

            if let Some(last) = scope.last() {
                expr_node.ty = last.ty.clone();
                expr_node.source_span.0 = last.source_span.0;
                expr_node.source_span.2 = last.source_span.2;
            }

            expr_node.kind = NodeKind::Block(scope);

            let letd = nodes.last_mut().unwrap();
            match &mut letd.kind {
                NodeKind::Let { expr, .. } => *expr = expr_node.into(),
                _ => unreachable!(),
            }
        }
        Ok(nodes)
    }

    fn read_node(
        &mut self,
        module: &mut Module,
        ast::Spanned(node, span): ast::Spanned<ast::RawNode>,
    ) -> Result<Node, HlirBuildError> {
        let source_span = Span(module.file, span.start, span.end);

        let kind = match node {
            ast::RawNode::Effect(_) => todo!(),
            ast::RawNode::Handle(hdl) => self.read_handle_expr(hdl, source_span, module)?,
            ast::RawNode::FnDef(_) => todo!(),
            ast::RawNode::Call(call) => NodeKind::Call {
                callee: self.read_node(module, *call.callee)?.into(),
                args: call
                    .args
                    .into_iter()
                    .map(|x| self.read_node(module, x))
                    .collect::<Result<_, _>>()?,
            },
            ast::RawNode::Name(key) => NodeKind::Name(key),
            ast::RawNode::String(key) => NodeKind::Literal(Literal::String(key)),
            ast::RawNode::Number(key) => NodeKind::Literal(Literal::Int(key)),
            ast::RawNode::If(ifd) => NodeKind::If {
                cond: self.read_node(module, *ifd.cond)?.into(),
                if_true: self.read_block(module, ifd.if_true)?.into(),
                if_false: ifd
                    .if_false
                    .map(|x| self.read_block(module, x))
                    .transpose()?
                    .map(Box::new),
            },
            ast::RawNode::While(d) => NodeKind::While {
                cond: self.read_node(module, *d.cond)?.into(),
                body: self.read_block(module, d.body)?.into(),
            },
            ast::RawNode::Binop(op) => NodeKind::Binop {
                op: op.kind,
                left: self.read_node(module, *op.left)?.into(),
                right: self.read_node(module, *op.right)?.into(),
            },
            ast::RawNode::Let(letd) => NodeKind::Let {
                name: letd.name.0,
                name_span: Span(module.file, letd.name.1.start, letd.name.1.end),
                value: self.read_node(module, *letd.value)?.into(),
                expr: self.unit_node().into(),
            },
            ast::RawNode::Assign(letd) => NodeKind::Assign {
                name: letd.name.0,
                value: self.read_node(module, *letd.value)?.into(),
            },
            ast::RawNode::Return(value) => NodeKind::Return(
                value
                    .map(|x| self.read_node(module, *x))
                    .transpose()?
                    .map(Box::new),
            ),
            ast::RawNode::ApplyType { name, ty } => NodeKind::ApplyType {
                expr: self
                    .read_node(module, ast::Spanned(ast::RawNode::Name(name), span.clone()))?
                    .into(),
                ty: self.hlir.types.insert(Type::Name(ty), source_span),
            },
        };

        let ty = match &kind {
            NodeKind::Handle { .. }
            | NodeKind::Let { .. }
            | NodeKind::Assign { .. }
            | NodeKind::If { .. }
            | NodeKind::While { .. }
            | NodeKind::Block(_)
            | NodeKind::Return(_) => self.hlir.types.unit(source_span),
            _ => self.unknown_type(source_span),
        };

        Ok(Node {
            kind,
            ty,
            source_span,
        })
    }

    fn read_handle_expr(
        &mut self,
        hdl: ast::Handle,
        span: Span,
        module: &mut Module,
    ) -> Result<NodeKind, HlirBuildError> {
        let handlers = hdl
            .effects
            .into_iter()
            .map(|x| {
                Ok(Handler {
                    effect_id: FunctionId(0),
                    name: x.name,
                    arguments: x.args.into_iter().map(|x| x.name).collect(),
                    body: self.read_block(module, x.body)?,
                    ty: self.unknown_type(span),
                })
            })
            .collect::<Result<_, _>>()?;
        Ok(NodeKind::Handle {
            group_id: EffectGroupId(0),
            name: hdl.name,
            handlers,
            expr: self.read_block(module, hdl.expr)?.into(),
        })
    }

    fn unknown_type(&self, span: Span) -> TypeId {
        self.hlir.types.unknown_type(span)
    }

    fn unit_node(&self) -> Node {
        Node {
            kind: NodeKind::Block(vec![]),
            ty: self.hlir.types.insert(Type::Unit, Span::default()),
            source_span: Span(FileId(Spur::default()), 0, 0),
        }
    }
}

/// Represents the static information known about a builtin.
#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinIR {
    pub name: Spur,
    pub ty: TypeId,
    pub idx: usize,
}

#[derive(Debug, Default, PartialEq)]
pub struct Builtins {
    pub builtins: HashMap<Spur, BuiltinIR>,
    pub builtins_ord: Vec<BuiltinIR>,
}

impl Builtins {
    pub fn load(types: &TypeStore, f: impl FnOnce(&mut BuiltinLoader)) -> Self {
        let mut builtins = Builtins::default();
        let mut loader = BuiltinLoader {
            idx: 0,
            types: types.clone(),
            builtins: &mut builtins,
        };
        f(&mut loader);
        builtins
    }
}

pub struct BuiltinLoader<'a> {
    idx: usize,
    types: TypeStore,
    pub builtins: &'a mut Builtins,
}

impl<P: crate::interpreter::Ports> crate::interpreter::builtin::LoadBuiltin<P>
    for BuiltinLoader<'_>
{
    fn load_builtin<F, I>(&mut self, name: &str, f: F)
    where
        F: crate::interpreter::builtin::BuiltinFunction<P, I> + 'static,
        P: 'static,
        I: 'static,
    {
        let key = INTERNER.get_or_intern(name);
        let ir = BuiltinIR {
            name: key,
            ty: f.extract_type(&self.types),
            idx: inc!(self.idx),
        };
        self.builtins.builtins.insert(key, ir.clone());
        self.builtins.builtins_ord.push(ir);
    }
}
