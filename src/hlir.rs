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
use crate::parser::BinopKind;
use crate::parser::{self as ast, EffectKind};
use crate::typecheck::TypeStore;

use self::index::Index;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct FileId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Span(pub FileId, pub usize, pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct EffectGroupId(pub usize);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unknown,
    Function { inputs: Vec<TypeId>, output: TypeId },
    String,
    Int,
    Bool,
    Unit,
    Name(Spur),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(Vec<Node>);

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
    Int(i64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Handler {
    pub name: Spur,
    pub arguments: Vec<Spur>,
    pub body: Node,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Let {
        name: Spur,
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
        arg: Box<Node>,
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: TypeId,
    pub source_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnHeader {
    pub id: FunctionId,
    pub name: Option<Spur>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnArgument {
    pub name: Spur,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    pub header: FnHeader,
    pub arguments: Vec<FnArgument>,
    pub return_ty: TypeId,
    pub body: Node,
}

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

#[derive(Clone, Debug, PartialEq)]
pub struct EffectGroup {
    pub id: EffectGroupId,
    pub name: Spur,
    pub effects: Vec<EffectDef>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub file: FileId,
    pub id: ModuleId,
    pub name: Option<Spur>,
    pub functions: HashMap<FunctionId, FnDef>,
    pub effect_groups: HashMap<EffectGroupId, EffectGroup>,
}

#[derive(Default, Debug)]
pub struct Hlir {
    pub modules: HashMap<ModuleId, Module>,
    pub types: TypeStore,
    pub builtins: Rc<Builtins>,
}

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
    pub fn construct_index(&self) -> Index {
        Index::from_hlir(self)
    }
}

impl HlirBuilder {
    pub fn load_builtins(&mut self, f: impl FnOnce(&mut BuiltinLoader)) {
        let builtins = Builtins::load(&self.hlir.types, f);
        self.hlir.builtins = builtins.into();
    }

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
        match node {
            ast::RawNode::FnDef(def) => {
                let arguments: Vec<_> = def
                    .args
                    .into_iter()
                    .map(|x| FnArgument {
                        name: x.name,
                        ty: match x.ty {
                            Some(key) => self.hlir.types.insert(Type::Name(key)),
                            None => self.unknown_type(),
                        },
                    })
                    .collect();

                let output = match def.return_ty {
                    Some(key) => self.hlir.types.insert(Type::Name(key)),
                    None => self.hlir.types.unit(),
                };

                let ty = self.hlir.types.insert(Type::Function {
                    inputs: arguments.iter().map(|x| x.ty).collect(),
                    output,
                });

                let header = FnHeader {
                    id: FunctionId(inc!(self.func_id_counter)),
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
            }
            ast::RawNode::Effect(group) => {
                let mut effects = Vec::new();
                for def in group.effects {
                    let arguments: Vec<_> = def
                        .args
                        .into_iter()
                        .map(|x| FnArgument {
                            name: x.name,
                            ty: match x.ty {
                                Some(key) => self.hlir.types.insert(Type::Name(key)),
                                None => self.unknown_type(),
                            },
                        })
                        .collect();

                    let output = match def.return_ty {
                        Some(key) => self.hlir.types.insert(Type::Name(key)),
                        None => self.hlir.types.unit(),
                    };

                    let ty = self.hlir.types.insert(Type::Function {
                        inputs: arguments.iter().map(|x| x.ty).collect(),
                        output,
                    });

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
                    id: EffectGroupId(inc!(self.effect_group_id_counter)),
                    name: group.name,
                    effects,
                };
                module.effect_groups.insert(group.id, group);
            }
            _ => {
                return Err(HlirBuildError::InvalidTopLevel(Span(
                    module.file,
                    span.start,
                    span.end,
                )))
            }
        }
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

        let node = Node {
            kind: NodeKind::Block(nodes),
            ty: self.hlir.types.unit(),
            source_span: Span(module.file, span.start, span.end),
        };

        Ok(node)
    }

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
        let kind = match node {
            ast::RawNode::Effect(_) => todo!(),
            ast::RawNode::Handle(hdl) => {
                let handlers = hdl
                    .effects
                    .into_iter()
                    .map(|x| {
                        Ok(Handler {
                            name: x.name,
                            arguments: x.args.into_iter().map(|x| x.name).collect(),
                            body: self.read_block(module, x.body)?,
                            ty: self.unknown_type(),
                        })
                    })
                    .collect::<Result<_, _>>()?;

                NodeKind::Handle {
                    name: hdl.name,
                    handlers,
                    expr: self.read_block(module, hdl.expr)?.into(),
                }
            }
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
        };

        let ty = match &kind {
            NodeKind::Handle { .. }
            | NodeKind::Let { .. }
            | NodeKind::Assign { .. }
            | NodeKind::If { .. }
            | NodeKind::While { .. }
            | NodeKind::Block(_)
            | NodeKind::Return(_) => self.hlir.types.unit(),
            _ => self.unknown_type(),
        };

        Ok(Node {
            kind,
            ty,
            source_span: Span(module.file, span.start, span.end),
        })
    }

    fn unknown_type(&mut self) -> TypeId {
        self.hlir.types.unknown_type()
    }

    fn unit_node(&self) -> Node {
        Node {
            kind: NodeKind::Block(vec![]),
            ty: self.hlir.types.insert(Type::Unit),
            source_span: Span(FileId(usize::MAX), 0, 0),
        }
    }
}

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        hlir::{name_resolve::NameResolver, simplify::Simplifier, visitor::HlirVisitor},
        interpreter::standard::{load_standard_builtins, StandardPorts},
        parser::parse,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let ast = parse(source).unwrap();

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();

        builder.load_builtins(|l| load_standard_builtins::<StandardPorts>(l));

        Simplifier.walk_hlir(&mut builder.hlir);
        NameResolver::new().walk_hlir(&mut builder.hlir);

        let key = |v| INTERNER.get(v).unwrap();

        let types = builder.hlir.types;
        let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();

        use NodeKind::*;

        let unit = types.unit();

        let func = FnDef {
            header: FnHeader {
                id: FunctionId(0),
                name: Some(key("main")),
                ty: types.insert(Type::Function {
                    inputs: vec![],
                    output: unit,
                }),
            },
            arguments: vec![],
            return_ty: unit,
            body: Node {
                kind: Call {
                    callee: Node {
                        kind: Builtin(0),
                        ty: types.insert(Type::Function {
                            inputs: vec![types.string()],
                            output: unit,
                        }),
                        source_span: Span(FileId(0), 12, 17),
                    }
                    .into(),
                    args: vec![Node {
                        kind: Literal(self::Literal::String(key("hello"))),
                        ty: TypeId(3),
                        source_span: Span(FileId(0), 18, 25),
                    }],
                },
                ty: TypeId(4),
                source_span: Span(FileId(0), 12, 26),
            },
        };
        let functions = HashMap::from([(FunctionId(0), func)]);
        assert_eq!(
            module,
            &Module {
                file: FileId(0),
                id: ModuleId(0),
                name: None,
                functions,
                effect_groups: Default::default(),
            }
        );
    }

    #[test]
    fn let_fold() {
        let source = r#"fn main() { let a = 1; a; }"#;
        let ast = parse(source).unwrap();

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();

        let builtins = Builtins::load(&builder.hlir.types, |l| {
            load_standard_builtins::<StandardPorts>(l)
        });

        Simplifier.walk_hlir(&mut builder.hlir);
        NameResolver::new().walk_hlir(&mut builder.hlir);

        let key = |v: &str| INTERNER.get(v).unwrap();

        let types = &builder.hlir.types;
        let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();

        use NodeKind::*;

        let unit = types.unit();

        let func = FnDef {
            header: FnHeader {
                id: FunctionId(0),
                name: Some(key("main")),
                ty: types.insert(Type::Function {
                    inputs: vec![],
                    output: unit,
                }),
            },
            arguments: vec![],
            return_ty: unit,
            body: Node {
                kind: Let {
                    name: key("a"),
                    value: Node {
                        kind: Literal(self::Literal::Int(1)),
                        ty: TypeId(2),
                        source_span: Span(FileId(0), 20, 21),
                    }
                    .into(),
                    expr: Node {
                        kind: Name(key("a")),
                        ty: TypeId(3),
                        source_span: Span(FileId(0), 23, 24),
                    }
                    .into(),
                },
                ty: unit,
                source_span: Span(FileId(0), 12, 22),
            },
        };

        println!("{:?}", builder.hlir);
        let functions = HashMap::from([(FunctionId(0), func)]);
        assert_eq!(
            module,
            &Module {
                file: FileId(0),
                id: ModuleId(0),
                name: None,
                functions,
                effect_groups: Default::default(),
            }
        );
    }
}
