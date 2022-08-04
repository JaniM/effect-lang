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

#[derive(Default)]
struct Frame {
    registers: [Value; 32],
    return_addr: usize,
}

pub struct Interpreter<P: Ports> {
    program: Program,
    ip: usize,
    stdout: Option<P::Stdout>,
    executing: bool,
    stack: Vec<Value>,
    builtins: Vec<Rc<dyn ErasedBuiltin<P>>>,
    compare: Ordering,
    frame: Frame,
    frames: Vec<Frame>,
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
            builtins: Vec::new(),
            compare: Ordering::Equal,
            frame: Default::default(),
            frames: Default::default(),
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
                self.frame.registers[to as usize] = self.frame.registers[from as usize].clone();
            }
            Instruction::LoadConstant(idx, Register(reg)) => {
                self.frame.registers[reg as usize] = self.program.constants[idx as usize].clone();
            }
            Instruction::LoadLocal(idx, Register(reg)) => {
                // FIXME: This is a temporary hack.
                let idx = idx as usize + 128;
                self.frame.registers[reg as usize] = self.frame.registers[idx].clone();
            }
            Instruction::StoreLocal(idx, Register(reg)) => {
                // FIXME: This is a temporary hack.
                let idx = idx as usize + 128;
                self.frame.registers[idx] = self.frame.registers[reg as usize].clone();
            }
            Instruction::LoadBuiltin(idx, Register(reg)) => {
                self.frame.registers[reg as usize] = Value::Builtin(idx);
            }
            Instruction::Add(Register(a), Register(b), Register(out)) => {
                match (
                    &self.frame.registers[a as usize],
                    &self.frame.registers[b as usize],
                ) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.frame.registers[out as usize] = Value::Int(a + b);
                    }
                    (a, b) => panic!("Addition between {a:?} and {b:?} unimplemented"),
                }
            }
            Instruction::IntCmp(Register(a), Register(b)) => {
                match (
                    &self.frame.registers[a as usize],
                    &self.frame.registers[b as usize],
                ) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.compare = a.cmp(b);
                    }
                    (a, b) => panic!("Compare between {a:?} and {b:?} unimplemented"),
                }
            }
            Instruction::Equals(Register(reg)) => {
                self.frame.registers[reg as usize] = Value::Bool(self.compare == Ordering::Equal);
            }
            Instruction::Jump(addr) => {
                self.ip = addr as usize;
            }
            Instruction::Branch(if_true, if_false, Register(reg)) => {
                match &self.frame.registers[reg as usize] {
                    Value::Bool(true) => self.ip = if_true as usize,
                    Value::Bool(false) => self.ip = if_false as usize,
                    x => panic!("Can't branxh on {x:?}"),
                }
            }
            Instruction::Call(Register(reg)) => match &self.frame.registers[reg as usize] {
                Value::Builtin(idx) => {
                    let builtin = self.builtins[*idx as usize].clone();
                    builtin.call(self);
                }
                &Value::Function(idx) => {
                    let old_frame = std::mem::take(&mut self.frame);
                    self.frames.push(old_frame);

                    self.frame.return_addr = self.ip;
                    self.ip = idx as usize;
                }
                x => panic!("Can't call {:?}", x),
            },
            Instruction::Return => match self.frames.pop() {
                Some(frame) => {
                    self.ip = self.frame.return_addr;
                    self.frame = frame;
                }
                None => self.executing = false,
            },
            Instruction::Push(Register(reg)) => {
                self.stack.push(self.frame.registers[reg as usize].clone());
            }
            Instruction::Pop(Register(reg)) => {
                let value = self.stack.pop().expect("Pop on empty stack");
                self.frame.registers[reg as usize] = value;
            }
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
