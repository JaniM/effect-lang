// Example:
//
// ```
// fn foo() {
//   print("hello");
// }
// ```
//
// b&ecomes
//
// fn foo -> unit
//   block 0 ()
//     %0 = print : (String) -> unit
//     %1 = "hello" : String
//     %2 = call %0 (%1) : unit

mod name_resolve;
pub use crate::hlir::Type;
use colored::{ColoredString, Colorize};
pub use name_resolve::resolve_names;

macro_rules! inc {
    ($x:expr) => {{
        let v = $x;
        $x += 1;
        v
    }};
}

use std::collections::HashMap;

use lasso::{Rodeo, Spur};

use crate::{
    interpreter::{
        builtin::{BuiltinFunction, LoadBuiltin},
        Ports,
    },
    parser::{Call, FnDef, Node as AstNode, RawNode, Span},
};

pub struct IRWriteCtx<'a> {
    interner: &'a Rodeo,
    source: &'a str,
}

impl<'a> IRWriteCtx<'a> {
    pub fn new(interner: &'a Rodeo, source: &'a str) -> Self {
        IRWriteCtx { interner, source }
    }
}

fn format_var(param: usize) -> ColoredString {
    format!("%{}", param).blue()
}

impl Type {
    fn write(&self, ctx: &IRWriteCtx) -> usize {
        match self {
            Type::Unknown(id) => {
                print!("?{}", id);
                (*id as f32).log10() as usize + 2
            }
            Type::Function { inputs, output } => {
                let mut len = 1;
                print!("(");
                for (i, input) in inputs.iter().enumerate() {
                    len += input.write(ctx);
                    if i != inputs.len() - 1 {
                        print!(", ");
                        len += 2;
                    }
                }
                print!(") -> ");
                len += ") -> ".len();
                len + output.write(ctx)
            }
            Type::String => {
                print!("string");
                6
            }
            Type::Unit => {
                print!("unit");
                4
            }
            _ => 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
}

impl Literal {
    fn write(&self, ctx: &IRWriteCtx) -> usize {
        match self {
            Literal::String(key) => {
                let n = ctx.interner.resolve(key);
                print!("{}", format!(r#""{}""#, n).yellow());
                n.len() + 2
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Symbol(Spur),
    Builtin { name: Spur, idx: usize },
    Literal(Literal),
    Call(usize, Vec<usize>),
}

impl NodeKind {
    fn write(&self, ctx: &IRWriteCtx) -> usize {
        match self {
            NodeKind::Symbol(key) => {
                let n = ctx.interner.resolve(key);
                print!("{} (?)", n);
                n.len() + " (?)".len()
            }
            NodeKind::Builtin { name, .. } => {
                let n = ctx.interner.resolve(name);
                print!("@{}", n);
                n.len() + 1
            }
            NodeKind::Literal(lit) => lit.write(ctx),
            NodeKind::Call(fn_var, args) => {
                print!("call {} (", format_var(*fn_var));
                let mut len = (*fn_var as f32).log10() as usize + 2 + "call  (".len();
                for (i, arg) in args.iter().enumerate() {
                    print!("{}", format_var(*arg));
                    len += (*arg as f32).log10() as usize + 2;
                    if i != args.len() - 1 {
                        print!(", ");
                        len += 2;
                    }
                }
                print!(")");
                len += 1;
                len
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: Type,
    pub span: Span,
}

impl Node {
    fn write(&self, ctx: &IRWriteCtx) {
        let ty_align = 20usize;
        let source_align = 50usize;

        let mut len = 0;
        len += self.kind.write(ctx);
        print!("{}: ", " ".repeat(ty_align.saturating_sub(len)));
        len = ty_align + 2;
        len += self.ty.write(ctx);
        print!(
            "{}| {}",
            " ".repeat(source_align.saturating_sub(len)),
            &ctx.source[self.span.clone()].purple()
        );
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub idx: usize,
    pub nodes: Vec<Node>,
    pub inputs: Vec<Type>,
}

impl Block {
    fn write(&self, ctx: &IRWriteCtx) {
        print!("block {} (", self.idx);
        for (i, arg) in self.inputs.iter().enumerate() {
            print!("#{} : ", i);
            arg.write(ctx);
            if i != self.inputs.len() - 1 {
                print!(", ");
            }
        }
        print!(")\n");

        for (i, node) in self.nodes.iter().enumerate() {
            print!("  {} = ", format_var(i));
            node.write(ctx);
            print!("\n");
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Option<Spur>,
    pub blocks: Vec<Block>,
    pub ty: Type,
}

impl Function {
    fn write(&self, ctx: &IRWriteCtx) {
        print!(
            "fn {}\n",
            self.name
                .map(|k| ctx.interner.resolve(&k))
                .unwrap_or("<unnamed>")
        );

        for block in self.blocks.iter() {
            block.write(ctx);
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Builder {
    current: Option<Function>,
    type_counter: usize,
    functions: Vec<Function>,
}

#[derive(Debug, Default, PartialEq)]
pub struct IR {
    pub functions: Vec<Function>,
}

impl IR {
    pub fn write(&self, ctx: &IRWriteCtx) {
        for func in &self.functions {
            func.write(ctx);
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    InvalidTopLevel,
    NestedFn,
}

impl Function {
    fn new(def: &FnDef) -> Self {
        Function {
            name: def.name,
            blocks: Vec::new(),
            ty: Type::Unit,
        }
    }
}

type BuildResult<T = (), E = CompileError> = Result<T, E>;

impl Builder {
    pub fn to_ir(self) -> IR {
        IR {
            functions: self.functions,
        }
    }

    pub fn read_top_nodes(&mut self, nodes: &[AstNode]) -> BuildResult {
        for item in nodes {
            self.read_top_node(item)?;
        }
        Ok(())
    }

    fn read_top_node(&mut self, node: &AstNode) -> BuildResult {
        match &node.0 {
            RawNode::FnDef(def) => self.read_function(def),
            _ => Err(CompileError::InvalidTopLevel),
        }
    }

    fn read_function(&mut self, def: &FnDef) -> BuildResult {
        self.begin_function(Function::new(def))?;
        self.begin_block(Vec::new())?;

        for item in def.body.0.stmts.iter() {
            self.read_expression(item)?;
        }

        self.end_function()?;
        Ok(())
    }

    fn read_expression(&mut self, node: &AstNode) -> BuildResult<usize> {
        match &node.0 {
            RawNode::FnDef(_) => todo!(),
            RawNode::If(_) => todo!(),
            RawNode::Call(call) => self.read_call(call, node.1.clone()),
            RawNode::Name(name) => self.read_name(*name, node.1.clone()),
            RawNode::String(string) => self.read_string(*string, node.1.clone()),
            RawNode::Binop(_) => todo!(),
            RawNode::Let(_) => todo!(),
            RawNode::Number(_) => todo!(),
        }
    }

    fn read_call(&mut self, call: &Call, span: Span) -> BuildResult<usize> {
        let fn_var = self.read_expression(&call.callee)?;

        let args = call
            .args
            .iter()
            .map(|item| self.read_expression(item))
            .collect::<Result<_, _>>()?;

        let ty = self.unknown_type();
        let var = self.push(Node {
            kind: NodeKind::Call(fn_var, args),
            ty,
            span,
        });
        Ok(var)
    }

    fn read_name(&mut self, name: Spur, span: Span) -> BuildResult<usize> {
        let ty = self.unknown_type();
        let var = self.push(Node {
            kind: NodeKind::Symbol(name),
            ty,
            span,
        });
        Ok(var)
    }

    fn read_string(&mut self, string: Spur, span: Span) -> BuildResult<usize> {
        let ty = Type::String;
        let var = self.push(Node {
            kind: NodeKind::Literal(Literal::String(string)),
            ty,
            span,
        });
        Ok(var)
    }

    fn begin_function(&mut self, function: Function) -> BuildResult {
        if self.current.is_some() {
            return Err(CompileError::NestedFn);
        }
        self.current = Some(function);
        Ok(())
    }

    fn end_function(&mut self) -> BuildResult {
        let func = self.current.take().unwrap();
        self.functions.push(func);
        Ok(())
    }

    fn begin_block(&mut self, inputs: Vec<Type>) -> BuildResult {
        let function = self.current();
        let idx = function.blocks.len();
        function.blocks.push(Block {
            nodes: Vec::new(),
            inputs,
            idx,
        });
        Ok(())
    }

    fn current(&mut self) -> &mut Function {
        self.current.as_mut().unwrap()
    }

    fn current_block(&mut self) -> &mut Block {
        self.current().blocks.last_mut().unwrap()
    }

    fn unknown_type(&mut self) -> Type {
        Type::Unknown(inc!(self.type_counter))
    }

    fn push(&mut self, inst: Node) -> usize {
        let block = self.current_block();
        block.nodes.push(inst);
        block.nodes.len() - 1
    }
}

#[cfg(test)]
mod test {
    use super::{name_resolve::resolve_names, *};
    use crate::{
        hlir::Builtins,
        interpreter::standard::{load_standard_builtins, StandardPorts},
        parser::parse,
        typecheck::Typechecker,
    };
    use lasso::Rodeo;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;

        let mut interner = Rodeo::default();
        let ast = parse(source, &mut interner).unwrap();
        let mut builder = Builder::default();
        builder.read_top_nodes(&ast).unwrap();

        let mut ir = builder.to_ir();

        ir.write(&IRWriteCtx::new(&interner, source));

        let builtins = Builtins::load(&mut interner, |l| {
            load_standard_builtins::<StandardPorts>(l)
        });

        resolve_names(&builtins, &mut ir).unwrap();
        ir.write(&IRWriteCtx::new(&interner, source));

        let mut typecheck = Typechecker::new(&mut interner, &builtins);
        typecheck.infer(&mut ir).unwrap();

        ir.write(&IRWriteCtx::new(&interner, source));

        let key = |v: &str| interner.get(v).unwrap();

        assert_eq!(
            ir,
            IR {
                functions: vec![Function {
                    name: Some(key("main")),
                    blocks: vec![Block {
                        idx: 0,
                        nodes: vec![
                            Node {
                                kind: NodeKind::Builtin {
                                    name: key("print"),
                                    idx: 0,
                                },
                                ty: Type::Function {
                                    inputs: vec![Type::String],
                                    output: Box::new(Type::Unit),
                                },
                                span: 12..17,
                            },
                            Node {
                                kind: NodeKind::Literal(Literal::String(key("hello"))),
                                ty: Type::String,
                                span: 18..25,
                            },
                            Node {
                                kind: NodeKind::Call(0, vec![1]),
                                ty: Type::Unit,
                                span: 12..26,
                            },
                        ],
                        inputs: vec![],
                    }],
                    ty: Type::Unit,
                }],
            }
        )
    }
}
