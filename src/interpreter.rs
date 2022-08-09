pub mod builtin;
pub mod standard;
mod test;

use core::panic;
use std::{cmp::Ordering, collections::HashMap, io::Write, rc::Rc};

use chumsky::prelude::Simple;

use derive_more::From;
use tinyvec::TinyVec;

use crate::{
    bytecode::{program::Program, Instruction, Register, Value},
    hlir::{
        name_resolve::NameResolver, simplify::Simplifier, visitor::HlirVisitor, FileId, HlirBuilder,
    },
    interpreter::standard::{load_standard_builtins, StandardPorts},
    lexer::{LexError, Lexer, Token},
    parser::parse_tokens,
    typecheck::{report_unknown_types, TypecheckContext},
};

use self::builtin::{BuiltinAdapter, BuiltinFunction, ErasedBuiltin, LoadBuiltin};

pub trait Ports {
    type Stdin: 'static;
    type Stdout: Write + 'static;
}

#[derive(Clone, Default)]
struct Frame {
    registers: [Value; 32],
    locals: TinyVec<[Value; 8]>,
    return_addr: usize,
    replace_frame: Option<usize>,
}

enum Handler {
    Fn {
        idx: usize,
        frame_level: usize,
    },
    #[allow(unused)]
    Effect {
        idx: usize,
    },
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
    handlers: HashMap<u32, Vec<Handler>>,
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
            handlers: Default::default(),
        }
    }

    pub fn with_stdout(mut self, stdout: P::Stdout) -> Self {
        self.stdout = Some(stdout);
        self
    }

    #[allow(unused)]
    pub fn load_source(source: &str) -> Result<Self, BuildError> {
        let tokens = Lexer::new(source).collect()?;
        let ast = parse_tokens(tokens)?;

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();
        builder.load_builtins(|l| load_standard_builtins::<StandardPorts>(l));
        let mut hlir = builder.hlir;

        Simplifier.walk_hlir(&mut hlir);
        NameResolver::new().walk_hlir(&mut hlir);
        let mut typecheck = TypecheckContext::new();
        typecheck.walk_hlir(&mut hlir);
        typecheck.apply_constraints();

        for error in typecheck.errors {
            println!("{:?}", error);
            panic!();
        }

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
        // println!("#{:<3} {inst:?}", self.ip);
        self.ip += 1;

        match *inst {
            Instruction::Copy(Register(from), Register(to)) => {
                self.frame.registers[to as usize] = self.frame.registers[from as usize].clone();
            }
            Instruction::LoadConstant(idx, Register(reg)) => {
                self.frame.registers[reg as usize] = self.program.constants[idx as usize].clone();
            }
            Instruction::LoadLocal(idx, Register(reg)) => {
                self.frame.registers[reg as usize] = self.frame.locals[idx as usize].clone();
            }
            Instruction::StoreLocal(idx, Register(reg)) => {
                let idx = idx as usize;
                if idx <= self.frame.locals.len() {
                    self.frame.locals.resize_with(idx + 1, Default::default);
                }
                self.frame.locals[idx] = self.frame.registers[reg as usize].clone();
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
            Instruction::Less(Register(reg)) => {
                self.frame.registers[reg as usize] = Value::Bool(self.compare == Ordering::Less);
            }
            Instruction::Greater(Register(reg)) => {
                self.frame.registers[reg as usize] = Value::Bool(self.compare == Ordering::Greater);
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
                Value::Effect(id) => {
                    let handler = self.handlers.get(id).unwrap();
                    match handler.last() {
                        Some(Handler::Fn { idx, frame_level }) => {
                            let frame = if *frame_level == self.frames.len() {
                                self.frame.clone()
                            } else {
                                self.frames.get(*frame_level).unwrap().clone()
                            };

                            let old_frame = std::mem::replace(&mut self.frame, frame);
                            self.frames.push(old_frame);
                            self.frame.replace_frame = Some(*frame_level);
                            self.frame.return_addr = self.ip;
                            self.ip = *idx;
                        }
                        Some(Handler::Effect { .. }) => todo!(),
                        None => todo!(),
                    }
                }
                x => panic!("Can't call {:?}", x),
            },
            Instruction::Return => {
                if let Some(idx) = self.frame.replace_frame {
                    self.frame.replace_frame = None;
                    let frame = std::mem::take(&mut self.frame);
                    let old = &mut self.frames[idx];
                    old.locals = frame.locals;
                    self.frames.drain(idx + 1..);
                    self.frame = self.frames.pop().unwrap();
                }
                match self.frames.pop() {
                    Some(frame) => {
                        self.ip = self.frame.return_addr;
                        self.frame = frame;
                    }
                    None => self.executing = false,
                }
            }
            Instruction::Resume => {
                let return_addr = self.frame.return_addr;
                if let Some(idx) = self.frame.replace_frame {
                    self.frame.replace_frame = None;
                    let frame = std::mem::take(&mut self.frame);
                    let old = &mut self.frames[idx];
                    old.locals = frame.locals;
                }
                match self.frames.pop() {
                    Some(frame) => {
                        self.ip = return_addr;
                        self.frame = frame;
                    }
                    None => self.executing = false,
                }
            }
            Instruction::Push(Register(reg)) => {
                self.stack.push(self.frame.registers[reg as usize].clone());
            }
            Instruction::Pop(Register(reg)) => {
                let value = self.stack.pop().expect("Pop on empty stack");
                self.frame.registers[reg as usize] = value;
            }
            Instruction::InstallHandler(id, idx) => {
                let stack = self.handlers.entry(id).or_default();
                stack.push(Handler::Fn {
                    idx: idx as usize,
                    frame_level: self.frames.len(),
                });
            }
            Instruction::UninstallHandler(id) => {
                let stack = self.handlers.get_mut(&id).unwrap();
                stack.pop();
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
