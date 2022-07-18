use std::collections::HashMap;

use lasso::Spur;

use crate::compile::{Function, HLInstruction};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Function(usize, u32),
    Builtin(usize, u32),
}

pub type Instruction = HLInstruction;

// This needs a better name.
#[derive(Default)]
pub struct Lowerer {
    pub insts: Vec<Instruction>,
    pub globals: HashMap<Spur, Value>,
}

#[derive(Debug)]
pub enum LowerError {}

type LowerResult<T = (), E = LowerError> = Result<T, E>;

impl Lowerer {
    pub fn read_functions(&mut self, functions: &[Function]) -> LowerResult {
        for function in functions {
            self.read_function(function)?;
        }
        Ok(())
    }

    fn read_function(&mut self, function: &Function) -> LowerResult {
        let begin = self.insts.len();

        for inst in &function.body {
            self.insts.push(inst.clone());
        }

        self.insts.push(Instruction::Return);

        if let Some(name) = function.name {
            self.globals
                .insert(name, Value::Function(begin, function.argc as u32));
        }

        Ok(())
    }
}
