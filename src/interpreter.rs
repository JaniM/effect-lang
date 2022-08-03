pub mod builtin;
pub mod standard;
mod test;

use core::panic;
use std::{cmp::Ordering, io::Write, rc::Rc};

use chumsky::prelude::Simple;

use derive_more::From;

use crate::{
    bytecode::{program::Program, Instruction, Register, Value},
    hlir::{
        self,
        name_resolve::NameResolver,
        simplify::Simplifier,
        typecheck::{report_unknown_types, Typechecker},
        visitor::HlirVisitor,
        FileId, HlirBuilder,
    },
    interpreter::standard::{load_standard_builtins, StandardPorts},
    lexer::{LexError, Lexer, Token},
    parser::parse_tokens,
};

use self::builtin::{BuiltinAdapter, BuiltinFunction, ErasedBuiltin, LoadBuiltin};

pub trait Ports {
    type Stdin: 'static;
    type Stdout: Write + 'static;
}

pub struct Interpreter<P: Ports> {
    program: Program,
    ip: usize,
    stdout: Option<P::Stdout>,
    executing: bool,
    stack: Vec<Value>,
    registers: [Value; 256],
    builtins: Vec<Rc<dyn ErasedBuiltin<P>>>,
    compare: Ordering,
}

#[derive(Debug, From)]
pub enum BuildError {
    Lex(LexError),
    Parse(Vec<Simple<Token>>),
}

#[derive(Debug)]
pub enum ExecuteError {}

type ExecuteResult<T = (), E = ExecuteError> = Result<T, E>;

impl<P: Ports> Interpreter<P> {
    pub fn new(program: Program) -> Self {
        Interpreter {
            program,
            ip: 0,
            stdout: None,
            executing: false,
            stack: Vec::new(),
            registers: std::array::from_fn(|_| Default::default()),
            builtins: Vec::new(),
            compare: Ordering::Equal,
        }
    }

    pub fn with_stdout(mut self, stdout: P::Stdout) -> Self {
        self.stdout = Some(stdout);
        self
    }

    pub fn load_source(source: &str) -> Result<Self, BuildError> {
        let tokens = Lexer::new(source).collect()?;
        let ast = parse_tokens(tokens)?;

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();
        let mut hlir = builder.hlir;

        let builtins = hlir::Builtins::load(|l| load_standard_builtins::<StandardPorts>(l));

        Simplifier.walk_hlir(&mut hlir);
        NameResolver::new(&builtins).walk_hlir(&mut hlir);
        Typechecker::default().walk_hlir(&mut hlir);

        report_unknown_types(&mut hlir);

        let program = Program::from_hlir(&hlir);

        let interpreter = Self::new(program);
        Ok(interpreter)
    }

    fn begin(&mut self) {
        self.ip = self.program.entrypoint;
        self.executing = true;
    }

    pub fn run(&mut self) -> ExecuteResult {
        self.begin();

        while self.executing {
            self.step()?;
        }

        Ok(())
    }

    fn step(&mut self) -> ExecuteResult {
        let inst = &self.program.insts[self.ip];
        self.ip += 1;

        match *inst {
            Instruction::Copy(Register(from), Register(to)) => {
                self.registers[to as usize] = self.registers[from as usize].clone();
            },
            Instruction::LoadConstant(idx, Register(reg)) => {
                self.registers[reg as usize] = self.program.constants[idx as usize].clone();
            }
            Instruction::LoadLocal(idx, Register(reg)) => {
                // FIXME: This is a temporary hack.
                let idx = idx as usize + 128;
                self.registers[reg as usize] = self.registers[idx].clone();
            }
            Instruction::StoreLocal(idx, Register(reg)) => {
                // FIXME: This is a temporary hack.
                let idx = idx as usize + 128;
                self.registers[idx] = self.registers[reg as usize].clone();
            }
            Instruction::LoadBuiltin(idx, Register(reg)) => {
                self.registers[reg as usize] = Value::Builtin(idx);
            }
            Instruction::IntCmp(Register(a), Register(b)) => {
                match (&self.registers[a as usize], &self.registers[b as usize]) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.compare = a.cmp(b);
                    }
                    (a, b) => panic!("Compare between {a:?} and {b:?} unimplemented"),
                }
            }
            Instruction::Equals => {
                self.registers[0] = Value::Bool(self.compare == Ordering::Equal);
            }
            Instruction::Jump(_) => todo!(),
            Instruction::Branch(if_true, if_false, Register(reg)) => {
                match &self.registers[reg as usize] {
                    Value::Bool(true) => self.ip = if_true as usize,
                    Value::Bool(false) => self.ip = if_false as usize,
                    x => panic!("Can't branxh on {x:?}"),
                }
            }
            Instruction::Call(Register(reg)) => match &self.registers[reg as usize] {
                Value::Builtin(idx) => {
                    let builtin = self.builtins[*idx as usize].clone();
                    builtin.call(self);
                }
                Value::Function(_) => todo!(),
                x => panic!("Can't call {:?}", x),
            },
            Instruction::Return => {
                self.executing = false;
            }
            Instruction::Push(Register(reg)) => {
                self.stack.push(self.registers[reg as usize].clone());
            }
            Instruction::Pop(_) => todo!(),
        }

        Ok(())
    }
}

impl<P: Ports> LoadBuiltin<P> for Interpreter<P> {
    fn load_builtin<F, I>(&mut self, _name: &str, f: F)
    where
        F: BuiltinFunction<P, I> + 'static,
        P: 'static,
        I: 'static,
    {
        self.builtins.push(Rc::new(BuiltinAdapter::new(f)));
    }
}
