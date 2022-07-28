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
use colored::{Colorize, ColoredString};
pub use name_resolve::resolve_names;

macro_rules! inc {
    ($x:expr) => {{
        let v = $x;
        $x += 1;
        v
    }};
}

use std::{collections::HashMap, fmt::Write as _, io::Write};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unknown(usize),
    Name(Spur),
    Function {
        inputs: Vec<Type>,
        output: Box<Type>,
    },
    String,
    Unit,
}

impl Type {
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<usize> {
        let len = match self {
            Type::Unknown(id) => {
                write!(f, "?{}", id)?;
                (*id as f32).log10() as usize + 2
            }
            Type::Name(key) => {
                let n = ctx.interner.resolve(key);
                write!(f, "{} (unresolved)", n)?;
                n.len() + " (unresolved)".len()
            }
            Type::Function { inputs, output } => {
                let mut len = 1;
                write!(f, "(")?;
                for (i, input) in inputs.iter().enumerate() {
                    len += input.write(ctx, f)?;
                    if i != inputs.len() - 1 {
                        write!(f, ", ")?;
                        len += 2;
                    }
                }
                write!(f, ") -> ")?;
                len += ") -> ".len();
                len + output.write(ctx, f)?
            }
            Type::String => {
                write!(f, "string")?;
                6
            }
            Type::Unit => {
                write!(f, "unit")?;
                4
            }
        };
        Ok(len)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
}

impl Literal {
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<usize> {
        let len = match self {
            Literal::String(key) => {
                let n = ctx.interner.resolve(key);
                write!(f, "{}", format!(r#""{}""#, n).yellow())?;
                n.len() + 2
            }
        };
        Ok(len)
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
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<usize> {
        let len = match self {
            NodeKind::Symbol(key) => {
                let n = ctx.interner.resolve(key);
                write!(f, "{} (?)", n)?;
                n.len() + " (?)".len()
            }
            NodeKind::Builtin { name, .. } => {
                let n = ctx.interner.resolve(name);
                write!(f, "@{}", n)?;
                n.len() + 1
            }
            NodeKind::Literal(lit) => lit.write(ctx, f)?,
            NodeKind::Call(fn_var, args) => {
                write!(f, "call {} (", format_var(*fn_var)).unwrap();
                let mut len = (*fn_var as f32).log10() as usize + 2 + "call  (".len();
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", format_var(*arg)).unwrap();
                    len += (*arg as f32).log10() as usize + 2;
                    if i != args.len() - 1 {
                        write!(f, ", ").unwrap();
                        len += 2;
                    }
                }
                write!(f, ")").unwrap();
                len += 1;
                len
            }
        };
        Ok(len)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: Type,
    pub span: Span,
}

impl Node {
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<()> {
        let ty_align = 20usize;
        let source_align = 50usize;

        let mut len = 0;
        len += self.kind.write(ctx, f)?;
        write!(f, "{}: ", " ".repeat(ty_align.saturating_sub(len)))?;
        len = ty_align + 2;
        len += self.ty.write(ctx, f)?;
        write!(
            f,
            "{}| {}",
            " ".repeat(source_align.saturating_sub(len)),
            &ctx.source[self.span.clone()].purple()
        )?;
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub idx: usize,
    pub nodes: Vec<Node>,
    pub inputs: Vec<Type>,
}

impl Block {
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<()> {
        write!(f, "block {} (", self.idx)?;
        for (i, arg) in self.inputs.iter().enumerate() {
            write!(f, "#{} : ", i)?;
            arg.write(ctx, f)?;
            if i != self.inputs.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")\n")?;

        for (i, node) in self.nodes.iter().enumerate() {
            write!(f, "  {} = ", format_var(i))?;
            node.write(ctx, f)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Option<Spur>,
    pub blocks: Vec<Block>,
    pub ty: Type,
}

impl Function {
    fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<()> {
        write!(
            f,
            "fn {}\n",
            self.name
                .map(|k| ctx.interner.resolve(&k))
                .unwrap_or("<unnamed>")
        )?;

        for block in self.blocks.iter() {
            block.write(ctx, f)?;
        }

        Ok(())
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
    pub fn write(&self, ctx: &IRWriteCtx, f: &mut impl Write) -> std::io::Result<()> {
        for func in &self.functions {
            func.write(ctx, f)?;
        }
        Ok(())
    }

    pub fn to_string(&self, ctx: &IRWriteCtx) -> String {
        let mut s = Vec::new();
        self.write(ctx, &mut std::io::Cursor::new(&mut s)).unwrap();
        String::from_utf8_lossy(&s).into_owned()
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

        for item in def.body.iter() {
            self.read_expression(item)?;
        }

        self.end_function()?;
        Ok(())
    }

    fn read_expression(&mut self, node: &AstNode) -> BuildResult<usize> {
        match &node.0 {
            RawNode::FnDef(_) => todo!(),
            RawNode::Call(call) => self.read_call(call, node.1.clone()),
            RawNode::Name(name) => self.read_name(*name, node.1.clone()),
            RawNode::String(string) => self.read_string(*string, node.1.clone()),
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

pub struct BuiltinIR {
    pub name: Spur,
    pub ty: Type,
    pub idx: usize,
}

pub struct Builtins {
    pub builtins: HashMap<Spur, BuiltinIR>,
}

impl Builtins {
    pub fn load(interner: &mut Rodeo, f: impl FnOnce(&mut BuiltinLoader)) -> Self {
        let mut loader = BuiltinLoader {
            interner,
            idx: 0,
            builtins: HashMap::new(),
        };
        f(&mut loader);
        Builtins {
            builtins: loader.builtins,
        }
    }
}

pub struct BuiltinLoader<'a> {
    interner: &'a mut Rodeo,
    idx: usize,
    pub builtins: HashMap<Spur, BuiltinIR>,
}

impl<P: Ports> LoadBuiltin<P> for BuiltinLoader<'_> {
    fn load_builtin<F, I>(&mut self, name: &str, f: F)
    where
        F: BuiltinFunction<P, I> + 'static,
        P: 'static,
        I: 'static,
    {
        let key = self.interner.get_or_intern(name);
        let ir = BuiltinIR {
            name: key,
            ty: f.extract_type(),
            idx: inc!(self.idx),
        };
        self.builtins.insert(key, ir);
    }
}

#[cfg(test)]
mod test {
    use super::{name_resolve::resolve_names, *};
    use crate::{
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

        println!("{}", ir.to_string(&IRWriteCtx::new(&interner, source)));

        let builtins = Builtins::load(&mut interner, |l| {
            load_standard_builtins::<StandardPorts>(l)
        });

        resolve_names(&builtins, &mut ir).unwrap();
        println!("{}", ir.to_string(&IRWriteCtx::new(&interner, source)));

        let mut typecheck = Typechecker::new(&mut interner, &builtins);
        typecheck.infer(&mut ir).unwrap();

        println!("{}", ir.to_string(&IRWriteCtx::new(&interner, source)));

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
