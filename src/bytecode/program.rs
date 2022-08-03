use std::collections::HashMap;

use dbg_pls::DebugPls;

use crate::{
    hlir::{FnDef, FunctionId, Hlir, Module},
    intern::resolve_symbol,
};

use super::{
    optimize::simplify_function, print_inst, Function, FunctionBuilder, FunctionBuilderCtx,
    Instruction, Register, Value,
};

#[derive(Debug, DebugPls)]
pub struct Program {
    pub insts: Vec<Instruction<u8>>,
    pub constants: Vec<Value>,
    pub entrypoint: usize,
}

#[derive(Default)]
struct ProgramBuilder {
    insts: Vec<Instruction<usize>>,
    ctx: FunctionBuilderCtx,
    functions: HashMap<FunctionId, usize>,
    entrypoint: usize,
}

impl Program {
    pub fn from_hlir(hlir: &Hlir) -> Self {
        let mut builder = ProgramBuilder::default();
        builder.load_hlir(hlir);
        let ProgramBuilder {
            insts,
            ctx: FunctionBuilderCtx { mut constants },
            functions,
            entrypoint,
        } = builder;

        for constant in &mut constants {
            match constant {
                Value::Function(id) => {
                    *id = *functions.get(&FunctionId(*id as usize)).unwrap() as u32;
                }
                _ => {}
            }
        }

        let insts = insts
            .into_iter()
            .map(|x| x.map_reg(|r| Register(r.0.try_into().unwrap())))
            .collect();

        Self {
            insts,
            constants,
            entrypoint,
        }
    }

    pub fn print(&self) {
        for (idx, inst) in self.insts.iter().enumerate() {
            print!("  #{idx:<3}");
            let inst = inst.map_reg(|r| Register(r.0 as _));
            print_inst(&inst, &self.constants);
        }
    }
}

impl ProgramBuilder {
    fn load_hlir(&mut self, hlir: &Hlir) {
        for module in hlir.modules.values() {
            self.load_module(module);
        }
    }

    fn load_module(&mut self, module: &Module) {
        for function in module.functions.values() {
            self.load_function(function);
        }
    }

    fn load_function(&mut self, def: &FnDef) {
        let mut func = Function::default();
        FunctionBuilder::new(&mut func, &mut self.ctx).build_fndef(def);

        simplify_function(&mut func);

        let begin = self.insts.len();
        let mut block_indices = vec![];
        for (idx, block) in func.blocks.iter().enumerate() {
            block_indices.push(self.insts.len() as u32);
            for inst in &block.insts {
                // Drop unnecessary jump.
                if let Instruction::Jump(target) = inst && *target == idx as u32 + 1 {
                    continue;
                }
                self.insts.push(*inst);
            }
        }

        // Rewrite jumps from block index to global instruction index.
        for inst in &mut self.insts[begin..] {
            match inst {
                Instruction::Jump(idx) => {
                    *idx = block_indices[*idx as usize];
                }
                Instruction::Branch(idx1, idx2, _) => {
                    *idx1 = block_indices[*idx1 as usize];
                    *idx2 = block_indices[*idx2 as usize];
                }
                _ => {}
            }
        }

        if let Some(name) = def.name && resolve_symbol(name) == "main" {
            self.entrypoint = begin;
        }

        self.functions.insert(def.id, begin);
    }
}
