// Example:
//
// ```
// fn foo() {
//   print("hello");
// }
// ```
//
// becomes
//
// fn foo -> unit
//   block 0 ()
//     %0 = print : (String) -> unit
//     %1 = "hello" : String
//     %2 = call %0 (%1) : unit

macro_rules! inc {
    ($x:expr) => {{
        let v = $x;
        $x += 1;
        v
    }};
}

use std::{collections::HashMap, io::Write};

use lasso::{Rodeo, Spur};

use crate::{
    interpreter::{
        builtin::{BuiltinFunction, LoadBuiltin},
        Ports,
    },
    parser::{Call, FnDef, Node as AstNode, RawNode},
};

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
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        match self {
            Type::Unknown(id) => write!(f, "?{}", id),
            Type::Name(key) => write!(f, "{} (unresolved)", interner.resolve(key)),
            Type::Function { inputs, output } => {
                write!(f, "(")?;
                for (i, input) in inputs.iter().enumerate() {
                    input.write(interner, f)?;
                    if i != inputs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> ")?;
                output.write(interner, f)
            }
            Type::String => write!(f, "string"),
            Type::Unit => write!(f, "unit"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(Spur),
}

impl Literal {
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        match self {
            Literal::String(key) => write!(f, r#""{}""#, interner.resolve(key)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Symbol(Spur),
    Literal(Literal),
    Call(usize, Vec<usize>),
}

impl NodeKind {
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        match self {
            NodeKind::Symbol(key) => write!(f, "{} (unresolved)", interner.resolve(key)),
            NodeKind::Literal(lit) => lit.write(interner, f),
            NodeKind::Call(fn_var, args) => {
                write!(f, "call %{} (", fn_var)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "%{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub ty: Type,
}

impl Node {
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        self.kind.write(interner, f)?;
        write!(f, " : ")?;
        self.ty.write(interner, f)
    }
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub idx: usize,
    pub nodes: Vec<Node>,
    pub inputs: Vec<Type>,
}

impl Block {
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        write!(f, "block {} (", self.idx)?;
        for (i, arg) in self.inputs.iter().enumerate() {
            write!(f, "#{} : ", i)?;
            arg.write(interner, f)?;
            if i != self.inputs.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")\n")?;

        for (i, node) in self.nodes.iter().enumerate() {
            write!(f, "  %{} = ", i)?;
            node.write(interner, f)?;
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
    fn write(&self, interner: &Rodeo, f: &mut impl Write) -> std::io::Result<()> {
        write!(
            f,
            "fn {}\n",
            self.name
                .map(|k| interner.resolve(&k))
                .unwrap_or("<unnamed>")
        )?;

        for (i, block) in self.blocks.iter().enumerate() {
            block.write(interner, f)?;
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
            RawNode::Call(call) => self.read_call(call),
            RawNode::Name(name) => self.read_name(*name),
            RawNode::String(string) => self.read_string(*string),
        }
    }

    fn read_call(&mut self, call: &Call) -> BuildResult<usize> {
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
        });
        Ok(var)
    }

    fn read_name(&mut self, name: Spur) -> BuildResult<usize> {
        let ty = self.unknown_type();
        let var = self.push(Node {
            kind: NodeKind::Symbol(name),
            ty,
        });
        Ok(var)
    }

    fn read_string(&mut self, string: Spur) -> BuildResult<usize> {
        let ty = Type::String;
        let var = self.push(Node {
            kind: NodeKind::Literal(Literal::String(string)),
            ty,
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
    use super::*;
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

        let key = |v: &str| interner.get(v).unwrap();

        let mut ir = builder.to_ir();

        for func in &ir.functions {
            func.write(&interner, &mut std::io::stderr()).unwrap();
        }

        let builtins = Builtins::load(&mut interner, |l| load_standard_builtins::<StandardPorts>(l));

        let mut typecheck = Typechecker::new(&mut interner, &builtins);
        typecheck.infer(&mut ir).unwrap();

        for func in &ir.functions {
            func.write(&interner, &mut std::io::stderr()).unwrap();
        }

        panic!()
    }
}
