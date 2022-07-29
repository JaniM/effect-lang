use std::collections::HashMap;

use lasso::Spur;

use crate::parser as ast;
use crate::parser::BinopKind;

macro_rules! inc {
    ($x:expr) => {{
        let v = $x;
        $x += 1;
        v
    }};
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span(pub FileId, pub usize, pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unknown(usize),
    Function {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
    String,
    Int,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnArgument {
    name: Spur,
    ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(Vec<Node>);

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
    Int(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Let {
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
    If {
        cond: Box<Node>,
        if_true: Box<Node>,
        if_false: Option<Box<Node>>,
    },
    Block(Vec<Node>),
    Literal(Literal),
    Name(Spur),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    kind: NodeKind,
    ty: Type,
    source_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    id: FunctionId,
    name: Option<Spur>,
    arguments: Vec<FnArgument>,
    return_ty: Type,
    body: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    file: FileId,
    id: ModuleId,
    name: Option<Spur>,
    functions: HashMap<FunctionId, FnDef>,
}

#[derive(Default, Debug)]
pub struct Hlir {
    modules: HashMap<ModuleId, Module>,
}

#[derive(Default)]
pub struct HlirBuilder {
    pub hlir: Hlir,
    module_id_counter: usize,
    func_id_counter: usize,
    type_counter: usize,
}

#[derive(Debug)]
pub enum HlirBuildError {
    InvalidTopLevel(Span),
}

impl HlirBuilder {
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
                let func = FnDef {
                    id: FunctionId(inc!(self.func_id_counter)),
                    name: def.name,
                    arguments: vec![],
                    return_ty: Type::Unit,
                    body: self.read_block(module, def.body)?,
                };

                module.functions.insert(func.id, func);
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
        let node = Node {
            kind: NodeKind::Block(nodes),
            ty: self.unknown_type(),
            source_span: Span(module.file, span.start, span.end),
        };
        Ok(node)
    }

    fn read_node(
        &mut self,
        module: &mut Module,
        ast::Spanned(node, span): ast::Spanned<ast::RawNode>,
    ) -> Result<Node, HlirBuildError> {
        let kind = match node {
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
            ast::RawNode::Binop(op) => NodeKind::Binop {
                op: op.kind,
                left: self.read_node(module, *op.left)?.into(),
                right: self.read_node(module, *op.right)?.into(),
            },
            ast::RawNode::Let(letd) => NodeKind::Let {
                name: letd.name.0,
                value: self.read_node(module, *letd.value)?.into(),
            },
        };

        Ok(Node {
            kind,
            ty: self.unknown_type(),
            source_span: Span(module.file, span.start, span.end),
        })
    }

    fn unknown_type(&mut self) -> Type {
        Type::Unknown(inc!(self.type_counter))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;
    use lasso::Rodeo;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let mut interner = Rodeo::default();
        let ast = parse(source, &mut interner).unwrap();

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();

        let key = |v| interner.get(v).unwrap();

        let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();

        use NodeKind::*;
        use Type::*;

        let func = FnDef {
            id: FunctionId(0),
            name: Some(key("main")),
            arguments: vec![],
            return_ty: Unit,
            body: Node {
                kind: Block(vec![Node {
                    kind: Call {
                        callee: Node {
                            kind: Name(key("print")),
                            ty: Unknown(0),
                            source_span: Span(FileId(0), 12, 17),
                        }
                        .into(),
                        args: vec![Node {
                            kind: Literal(self::Literal::String(key("hello"))),
                            ty: Unknown(1),
                            source_span: Span(FileId(0), 18, 25),
                        }],
                    },
                    ty: Unknown(2),
                    source_span: Span(FileId(0), 12, 26),
                }]),
                ty: Unknown(3),
                source_span: Span(FileId(0), 10, 29),
            },
        };
        let functions = HashMap::from([(FunctionId(0), func)]);
        println!("{:?}", builder.hlir);
        assert_eq!(
            module,
            &Module {
                file: FileId(0),
                id: ModuleId(0),
                name: None,
                functions
            }
        );
    }
}
