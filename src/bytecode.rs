pub mod optimize;

use std::{cmp::Ordering, collections::HashSet, fmt::Display, rc::Rc};

use dbg_pls::DebugPls;
use lasso::Spur;

use crate::{
    hlir::{
        self,
        visitor::{HlirVisitorImmut, VisitAction},
        FnDef, Node, NodeKind, Type,
    },
    intern::resolve_symbol,
    parser::BinopKind,
};

use self::optimize::describe_inst;

pub enum OldBadInstruction {
    PushString(Spur),
    LoadLocal(Spur),
    Call(u32),
    Return,
}

#[derive(Clone, Debug)]
pub enum OldBadValue {
    Builtin(usize, u32),
    Function(usize, u32),
    String(String),
}

#[derive(Clone, Debug, DebugPls, PartialEq)]
pub enum Value {
    Builtin(usize),
    Function(usize),
    String(Rc<String>),
    Int(i64),
    Bool(bool),
}

/// Represents register indices.
/// Register 0 is the accumulator, which is alwayx the result of the previous operation.
#[derive(Clone, Copy, Debug, DebugPls, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[repr(transparent)]
pub struct Register(u8);

impl Register {
    const ACC: Register = Register(0);
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, DebugPls, PartialEq)]
pub enum Instruction {
    Copy(Register, Register),
    /// Loads a constant at idx to register.
    LoadConstant(u32, Register),
    /// Loads a local at idx to register.
    LoadLocal(u32, Register),
    /// Stores a local from register to idx.
    StoreLocal(u32, Register),
    /// Loads a builtin at idx to register.
    LoadBuiltin(u32, Register),
    /// Compares registers and sets the comparison flag accordingly,
    IntCmp(Register, Register),
    /// Set the accumulator to true if the comparison flag is Equal.
    Equals,
    /// Unconditional jump to index.
    Jump(u32),
    /// Jump left if the register is true and otherwise jump right.
    Branch(u32, u32, Register),
    /// Calls the function loaded in register.
    Call(Register),
    Return,
    Push(Register),
    Pop(Register),
}

/// Represents a linear sequence of operations. Can only have exactly one jump, at the end of the
/// block.
#[derive(Debug, DebugPls, Default, PartialEq)]
pub struct Block {
    insts: Vec<Instruction>,
    inputs: Vec<u32>,
    outputs: Vec<u32>,
}

#[derive(Debug, DebugPls, Default, PartialEq)]
pub struct Function {
    blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    func: &'a mut Function,
    ctx: &'a mut FunctionBuilderCtx,
    current_block: usize,
    out_registers: Vec<Register>,
    occupied_registers: Vec<Register>,
    locals: Vec<Spur>,
}

#[derive(Debug, DebugPls, Default, PartialEq)]
pub struct FunctionBuilderCtx {
    constants: Vec<Value>,
}

impl Block {
    fn resolve_inputs(&self) -> Vec<u32> {
        let mut written = HashSet::new();
        let mut unbound = HashSet::new();
        for inst in self.insts.iter() {
            match inst {
                Instruction::LoadLocal(var, _) => {
                    if !written.contains(var) {
                        unbound.insert(*var);
                    }
                }
                Instruction::StoreLocal(var, _) => {
                    written.insert(*var);
                }
                _ => {}
            }
        }
        unbound.into_iter().collect()
    }
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(func: &'a mut Function, ctx: &'a mut FunctionBuilderCtx) -> Self {
        FunctionBuilder {
            func,
            ctx,
            current_block: 0,
            out_registers: Default::default(),
            occupied_registers: Default::default(),
            locals: Default::default(),
        }
    }

    pub fn build_fndef(&mut self, def: &FnDef) {
        let block = self.new_block();
        self.switch_block(block);
        self.walk_function(def);
        self.inst(Instruction::Return);

        let mut block_inputs = vec![];
        for block in self.func.blocks.iter_mut() {
            let inputs = block.resolve_inputs();
            block_inputs.push(inputs.clone());
            block.inputs = inputs;
        }

        for block in self.func.blocks.iter_mut() {
            match block.insts.last() {
                Some(Instruction::Jump(idx)) => {
                    block
                        .outputs
                        .extend_from_slice(&block_inputs[*idx as usize]);
                }
                Some(Instruction::Branch(idx1, idx2, _)) => {
                    block
                        .outputs
                        .extend_from_slice(&block_inputs[*idx1 as usize]);
                    block
                        .outputs
                        .extend_from_slice(&block_inputs[*idx2 as usize]);
                }
                Some(Instruction::Return) => {}
                x => panic!("Last instruction of block isn't a branch: {:?}", x),
            }
        }

        for block in self.func.blocks.iter_mut() {
            block.outputs = block
                .outputs
                .iter()
                .copied()
                .collect::<HashSet<_>>()
                .into_iter()
                .collect();
        }
    }

    fn new_block(&mut self) -> u32 {
        let func = &mut *self.func;
        func.blocks.push(Block::default());
        let idx = func.blocks.len() - 1;
        idx as u32
    }

    fn switch_block(&mut self, block: u32) {
        self.current_block = block as usize;
    }

    fn inst(&mut self, inst: Instruction) {
        self.func.blocks[self.current_block].insts.push(inst);
    }

    fn create_constant(&mut self, value: Value) -> u32 {
        if let Some(idx) = self.ctx.constants.iter().position(|x| x == &value) {
            return idx as u32;
        }

        self.ctx.constants.push(value);
        self.ctx.constants.len() as u32 - 1
    }

    fn set_out_reg(&mut self, reg: Register) -> Register {
        self.out_registers.push(reg);
        reg
    }

    fn pop_out_reg(&mut self) {
        self.out_registers.pop().expect("No out registers set");
    }

    fn out_reg(&self) -> Register {
        *self.out_registers.last().expect("No out registers set")
    }

    fn next_reg(&mut self) -> Register {
        let mut reg = Register(1);
        while self.occupied_registers.contains(&reg) {
            reg.0 += 1;
        }
        self.occupied_registers.push(reg);
        reg
    }

    fn release_reg(&mut self, reg: Register) {
        self.occupied_registers.retain(|x| x != &reg);
    }

    fn with_reg(&mut self, f: impl FnOnce(&mut Self, Register)) {
        let reg = self.next_reg();
        f(self, reg);
        self.release_reg(reg);
    }

    fn walk_with_out(&mut self, out: Register, node: &Node) {
        self.set_out_reg(out);
        self.walk_node(node);
        self.pop_out_reg();
    }
}

impl HlirVisitorImmut for FunctionBuilder<'_> {
    fn visit_node(&mut self, node: &Node) -> VisitAction {
        match &node.kind {
            NodeKind::Let { name, value, expr } => {
                let reg = self.next_reg();
                self.walk_with_out(reg, value);

                let idx = self.locals.len();
                self.inst(Instruction::StoreLocal(idx as _, reg));
                self.release_reg(reg);

                self.locals.push(*name);
                self.walk_node(expr);
                self.locals.pop();

                VisitAction::Nothing
            }
            NodeKind::Binop { op, left, right } => {
                let left_reg = self.next_reg();
                self.walk_with_out(left_reg, left);

                let right_reg = self.next_reg();
                self.walk_with_out(right_reg, right);

                let out = self.out_reg();
                self.inst(Instruction::IntCmp(left_reg, right_reg));
                match op {
                    BinopKind::Equals => self.inst(Instruction::Equals),
                }
                self.inst(Instruction::Copy(Register::ACC, out));

                self.release_reg(right_reg);
                self.release_reg(left_reg);

                VisitAction::Nothing
            }
            NodeKind::Call { callee, args } => {
                let func = self.next_reg();
                self.walk_with_out(func, callee);

                let reg = self.next_reg();
                self.set_out_reg(reg);
                for arg in args {
                    self.walk_node(arg);
                    self.inst(Instruction::Push(reg));
                }
                self.pop_out_reg();
                self.release_reg(reg);

                self.inst(Instruction::Call(func));
                self.release_reg(func);

                match callee.ty {
                    Type::Function {
                        output: box Type::Unit,
                        ..
                    } => {}
                    Type::Function { .. } => {
                        let out = self.out_reg();
                        self.inst(Instruction::Pop(out));
                    }
                    _ => todo!(),
                }

                VisitAction::Nothing
            }
            NodeKind::If {
                cond,
                if_true,
                if_false,
            } => {
                let true_block = self.new_block();
                let end_block = self.new_block();

                let cond_reg = self.next_reg();
                self.walk_with_out(cond_reg, cond);
                self.inst(Instruction::Branch(true_block, end_block, cond_reg));
                self.release_reg(cond_reg);

                self.switch_block(true_block);
                self.walk_node(if_true);
                self.inst(Instruction::Jump(end_block));

                // TODO: handle if_false

                self.switch_block(end_block);

                VisitAction::Nothing
            }
            NodeKind::Block(_) => VisitAction::Recurse,
            NodeKind::Literal(lit) => {
                let val = match lit {
                    hlir::Literal::String(key) => {
                        Value::String(resolve_symbol(*key).to_owned().into())
                    }
                    hlir::Literal::Int(v) => Value::Int(*v),
                };

                let constant = self.create_constant(val);
                let reg = self.out_reg();
                self.inst(Instruction::LoadConstant(constant, reg));

                VisitAction::Nothing
            }
            NodeKind::Name(name) => {
                let idx = self
                    .locals
                    .iter()
                    .rev()
                    .position(|x| x == name)
                    .expect("Name not found");

                let out = self.out_reg();
                self.inst(Instruction::LoadLocal(idx as _, out));

                VisitAction::Nothing
            }
            NodeKind::Builtin(idx) => {
                let reg = self.out_reg();
                self.inst(Instruction::LoadBuiltin(*idx as u32, reg));
                VisitAction::Nothing
            }
        }
    }
}

fn print_inst(inst: &Instruction, consts: &Vec<Value>) {
    print!("  ");
    match inst {
        Instruction::Copy(from, to) => println!("{to} = {from}"),
        Instruction::LoadConstant(idx, reg) => {
            println!(
                "{reg} = const {idx} ({})",
                match &consts[*idx as usize] {
                    Value::Builtin(_) => todo!(),
                    Value::Function(_) => todo!(),
                    Value::String(v) => format!(r#""{v}""#),
                    Value::Int(v) => v.to_string(),
                    Value::Bool(v) => v.to_string(),
                }
            );
        }
        Instruction::LoadLocal(idx, reg) => println!("{reg} = local {idx}"),
        Instruction::StoreLocal(idx, reg) => println!("local {idx} = {reg}"),
        Instruction::LoadBuiltin(idx, reg) => println!("{reg} = builtin {idx}"),
        Instruction::IntCmp(a, b) => println!("{} = cmp {a}, {b}", Register::ACC),
        Instruction::Equals => println!("{} = equals", Register::ACC),
        Instruction::Jump(addr) => println!("jump #{addr}"),
        Instruction::Branch(true_addr, false_addr, reg) => {
            println!("branch #{true_addr}, #{false_addr} on {reg}");
        }
        Instruction::Call(reg) => println!("call {reg}"),
        Instruction::Return => println!("ret"),
        Instruction::Push(reg) => println!("push {reg}"),
        Instruction::Pop(reg) => println!("{reg} = pop"),
    }
}

pub fn print_function(func: &Function, ctx: &FunctionBuilderCtx) {
    for (idx, block) in func.blocks.iter().enumerate() {
        println!(
            "block {idx} ({})",
            block
                .inputs
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        for inst in block.insts.iter() {
            print_inst(inst, &ctx.constants);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        bytecode::optimize::simplify_function,
        hlir::{
            name_resolve::NameResolver, simplify::Simplifier, visitor::HlirVisitor, Builtins,
            FileId, HlirBuilder, ModuleId,
        },
        interpreter::standard::{load_standard_builtins, StandardPorts},
        parser::parse,
    };
    use pretty_assertions::assert_eq;
    use unindent::unindent;

    #[test]
    fn cond() {
        let source = unindent(
            r#"
            fn main() {
                let x = 0;
                if (x == 0) {
                    print("hello");
                }
            }
            "#,
        );
        let ast = parse(&source).unwrap();

        let mut builder = HlirBuilder::default();
        builder.read_module(FileId(0), ast).unwrap();

        let builtins = Builtins::load(|l| load_standard_builtins::<StandardPorts>(l));

        Simplifier.walk_hlir(&mut builder.hlir);
        NameResolver::new(&builtins).walk_hlir(&mut builder.hlir);

        let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();
        let fndef = module.functions.get(&hlir::FunctionId(0)).unwrap();

        let mut ctx = FunctionBuilderCtx::default();

        let mut func = Function::default();
        FunctionBuilder::new(&mut func, &mut ctx).build_fndef(fndef);

        println!("Source:\n{source}");
        println!("Bytecode (raw):");
        print_function(&func, &ctx);

        simplify_function(&mut func);

        println!("\nBytecode (simplified):");
        print_function(&func, &ctx);
        println!("{}", dbg_pls::pretty(&func));

        use Instruction::*;
        assert_eq!(
            func,
            Function {
                blocks: vec![
                    Block {
                        insts: vec![
                            LoadConstant(0, Register(1)),
                            LoadConstant(0, Register(3)),
                            IntCmp(Register(1), Register(3)),
                            Equals,
                            Branch(1, 2, Register(0))
                        ],
                        outputs: vec![],
                        inputs: vec![],
                    },
                    Block {
                        insts: vec![
                            LoadBuiltin(0, Register(1)),
                            LoadConstant(1, Register(2)),
                            Push(Register(2)),
                            Call(Register(1)),
                            Jump(2)
                        ],
                        outputs: vec![],
                        inputs: vec![],
                    },
                    Block {
                        insts: vec![Return],
                        outputs: vec![],
                        inputs: vec![],
                    }
                ],
            }
        );
        assert_eq!(
            ctx,
            FunctionBuilderCtx {
                constants: vec![Value::Int(0), Value::String("hello".to_owned().into())]
            }
        );
    }
}
