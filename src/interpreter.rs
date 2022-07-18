mod builtin;
mod standard;
mod test;

use core::panic;
use std::{collections::HashMap, io::Write, rc::Rc};

use chumsky::prelude::Simple;
use lasso::{Rodeo, Spur};

use derive_more::From;

use crate::{
    compile::{Builder, CompileError},
    lexer::{LexError, Lexer, Token},
    lower::{Instruction, LowerError, Lowerer, Value},
    parser::parse_tokens,
};

use self::builtin::{BuiltinAdapter, BuiltinFunction, ErasedBuiltin};

pub trait Ports {
    type Stdin: 'static;
    type Stdout: Write + 'static;
}

#[derive(Default)]
struct CallFrame {
    return_addr: Option<usize>,
}

pub struct Interpreter<P: Ports> {
    insts: Vec<Instruction>,
    globals: HashMap<Spur, Value>,
    interner: Rodeo,
    ip: usize,
    stdout: Option<P::Stdout>,
    frame: CallFrame,
    executing: bool,
    stack: Vec<Value>,
    builtins: Vec<Rc<dyn ErasedBuiltin<P>>>,
}

#[derive(Debug, From)]
pub enum BuildError {
    Lex(LexError),
    Parse(Vec<Simple<Token>>),
    Compile(CompileError),
    Lower(LowerError),
}

#[derive(Debug)]
pub enum ExecuteError {}

type ExecuteResult<T = (), E = ExecuteError> = Result<T, E>;

impl<P: Ports> Interpreter<P> {
    pub fn new() -> Self {
        Interpreter {
            insts: Vec::new(),
            globals: HashMap::new(),
            interner: Rodeo::default(),
            ip: 0,
            stdout: None,
            frame: CallFrame::default(),
            executing: false,
            stack: Vec::new(),
            builtins: Vec::new(),
        }
    }

    pub fn with_stdout(mut self, stdout: P::Stdout) -> Self {
        self.stdout = Some(stdout);
        self
    }

    pub fn load_source(&mut self, source: &str) -> Result<(), BuildError> {
        let tokens = Lexer::new(source).collect(&mut self.interner)?;
        let ast = parse_tokens(tokens)?;

        let mut builder = Builder::default();
        builder.read_top_nodes(&ast)?;

        let mut lowerer = Lowerer::default();
        lowerer.read_functions(&builder.functions)?;

        self.insts = lowerer.insts;
        self.globals.extend(lowerer.globals);

        Ok(())
    }

    pub fn load_builtin<F, I>(&mut self, name: &str, f: F)
    where
        F: BuiltinFunction<P, I> + 'static,
        P: 'static,
        I: 'static,
    {
        let argc = f.arg_count() as u32;
        let key = self.interner.get_or_intern(name);

        self.builtins.push(Rc::new(BuiltinAdapter::new(f)));

        let id = self.builtins.len() - 1;
        self.globals.insert(key, Value::Builtin(id, argc));
    }

    fn begin(&mut self) {
        // It is assumed `main` exists.
        self.ip = self.find_entry_point().unwrap();
        self.executing = true;
    }

    fn find_entry_point(&self) -> Option<usize> {
        let key = self.interner.get("main")?;
        let main = self.globals.get(&key)?;
        match main {
            Value::Function(pos, _) => Some(*pos),
            _ => panic!("main is not a function"),
        }
    }

    pub fn run(&mut self) -> ExecuteResult {
        self.begin();

        while self.executing {
            self.step()?;
        }

        Ok(())
    }

    fn step(&mut self) -> ExecuteResult {
        let inst = &self.insts[self.ip];
        self.ip += 1;

        match inst {
            Instruction::PushString(key) => self.push_string(*key)?,
            Instruction::Call(argc) => self.call_function(*argc)?,
            Instruction::LoadLocal(key) => self.load_local(*key)?,
            Instruction::Return => self.executing = false,
        }

        Ok(())
    }

    fn load_local(&mut self, key: Spur) -> ExecuteResult {
        let value = self.globals.get(&key).unwrap();
        self.stack.push(value.clone());
        Ok(())
    }

    fn push_string(&mut self, key: Spur) -> ExecuteResult {
        let string = self.interner.resolve(&key).to_owned();
        self.stack.push(Value::String(string));
        Ok(())
    }

    fn call_function(&mut self, arg_count: u32) -> ExecuteResult {
        match &self.stack[self.stack.len() - arg_count as usize - 1] {
            Value::Builtin(id, argc) => {
                if *argc != arg_count {
                    panic!("Calling a nuiltin with incorrect arg count");
                }
                let func = self.builtins.get(*id).unwrap().clone();
                func.call(self).unwrap();
                self.stack.pop();

                Ok(())
            }
            Value::Function(_, _) => todo!(),
            x => panic!("Calling a non-function: {:?}", x),
        }
    }
}
