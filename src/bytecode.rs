/// Generates bytecode directly from HLIR. This is.. tricky. We really want some kind of CFG before
/// this.
///
/// Currently it is assumed virtual registers do not escape blocks, but this is plain not true for
/// conditions. Thus we need a way to solve which registers are inputs to blocks and respect that
/// in optimizations.
pub mod optimize;
pub mod program;

use std::{collections::HashSet, fmt::Display, rc::Rc};

use dbg_pls::DebugPls;
use lasso::Spur;

use crate::{
    hlir::{
        self,
        index::{Header, Index},
        visitor::{HlirVisitorImmut, VisitAction},
        FnDef, Hlir, Node, NodeKind,
    },
    intern::resolve_symbol,
    parser::BinopKind,
    typecheck::{ConcreteType, TypeStore},
};

#[derive(Clone, Debug, DebugPls, PartialEq)]
pub enum Value {
    Builtin(u32),
    Function(u32),
    Effect(u32),
    String(Rc<String>),
    Int(i64),
    Bool(bool),
    Nothing,
}

impl Default for Value {
    fn default() -> Self {
        Self::Nothing
    }
}

type DefaultReg = usize;

/// Represents register indices.
/// Register 0 is the accumulator, which is alwayx the result of the previous operation.
#[derive(Clone, Copy, Debug, DebugPls, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[repr(transparent)]
pub struct Register<T = DefaultReg>(pub T);

impl<T: Display> Display for Register<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("%{}", self.0))
    }
}

#[derive(Clone, Copy, Debug, DebugPls, PartialEq)]
pub enum Instruction<T = DefaultReg> {
    #[allow(unused)]
    Copy(Register<T>, Register<T>),
    /// Loads a constant at idx to register.
    LoadConstant(u32, Register<T>),
    /// Loads a local at idx to register.
    LoadLocal(u32, Register<T>),
    /// Stores a local from register to idx.
    StoreLocal(u32, Register<T>),
    /// Loads a builtin at idx to register.
    LoadBuiltin(u32, Register<T>),
    Add(Register<T>, Register<T>, Register<T>),
    /// Compares registers and sets the comparison flag accordingly,
    IntCmp(Register<T>, Register<T>),
    /// Set the accumulator to true if the comparison flag is Equal.
    Equals(Register<T>),
    Less(Register<T>),
    Greater(Register<T>),
    /// Unconditional jump to index.
    Jump(u32),
    /// Jump left if the register is true and otherwise jump right.
    Branch(u32, u32, Register<T>),
    /// Calls the function loaded in register.
    Call(Register<T>),
    Return,
    Resume,
    Push(Register<T>),
    Pop(Register<T>),
    /// Install a handler for an effect function at bytecode index.
    InstallHandler(u32, u32),
    UninstallHandler(u32),
}

/// Represents a linear sequence of operations. Can only have exactly one jump, at the end of the
/// block.
#[derive(Debug, DebugPls, Default, PartialEq)]
pub struct Block<T = DefaultReg> {
    insts: Vec<Instruction<T>>,
    inputs: Vec<u32>,
    outputs: Vec<u32>,
}

#[derive(Debug, DebugPls, Default, PartialEq)]
pub struct Function<T> {
    blocks: Vec<Block<T>>,
}

#[derive(Debug)]
pub struct FunctionBuilder<'a, T> {
    func: &'a mut Function<T>,
    ctx: &'a mut FunctionBuilderCtx,
    current_block: usize,
    out_registers: Vec<Register<T>>,
    register_counter: usize,
    locals: Vec<Spur>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionBuilderCtx {
    constants: Vec<Value>,
    types: TypeStore,
    index: Index,
}

impl FunctionBuilderCtx {
    pub fn new(hlir: &Hlir) -> Self {
        Self {
            constants: Default::default(),
            types: hlir.types.clone(),
            index: hlir.construct_index(),
        }
    }
}

impl<T> Instruction<T> {
    pub fn map_reg<U>(self, mut f: impl FnMut(Register<T>) -> Register<U>) -> Instruction<U> {
        use Instruction::*;
        match self {
            Copy(a, b) => Copy(f(a), f(b)),
            LoadConstant(i, r) => LoadConstant(i, f(r)),
            LoadLocal(i, r) => LoadLocal(i, f(r)),
            StoreLocal(i, r) => StoreLocal(i, f(r)),
            LoadBuiltin(i, r) => LoadBuiltin(i, f(r)),
            Add(a, b, c) => Add(f(a), f(b), f(c)),
            IntCmp(a, b) => IntCmp(f(a), f(b)),
            Equals(r) => Equals(f(r)),
            Less(r) => Less(f(r)),
            Greater(r) => Greater(f(r)),
            Jump(i) => Jump(i),
            Branch(a, b, r) => Branch(a, b, f(r)),
            Call(r) => Call(f(r)),
            Return => Return,
            Resume => Resume,
            Push(r) => Push(f(r)),
            Pop(r) => Pop(f(r)),
            InstallHandler(a, b) => InstallHandler(a, b),
            UninstallHandler(a) => UninstallHandler(a),
        }
    }
}

impl<T> Block<T> {
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

impl<'a> FunctionBuilder<'a, usize> {
    pub fn new(func: &'a mut Function<usize>, ctx: &'a mut FunctionBuilderCtx) -> Self {
        FunctionBuilder {
            func,
            ctx,
            current_block: 0,
            out_registers: Default::default(),
            register_counter: 0,
            locals: Default::default(),
        }
    }

    pub fn build_fndef(&mut self, def: &FnDef) {
        let block = self.new_block();
        self.switch_block(block);
        self.walk_function(def);

        if self.func.blocks.last().unwrap().insts.last() != Some(&Instruction::Return) {
            self.inst(Instruction::Return);
        }

        let mut block_inputs = vec![];
        for block in self.func.blocks.iter_mut() {
            let inputs = block.resolve_inputs();
            block_inputs.push(inputs.clone());
            block.inputs = inputs;
        }

        // Resolve block outputs
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
                Some(Instruction::Resume) => {}
                None => {}
                x => panic!("Last instruction of block isn't a branch: {:?}", x),
            }
        }

        // Deduplicate outputs.
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

    fn inst(&mut self, inst: Instruction<usize>) {
        self.func.blocks[self.current_block].insts.push(inst);
    }

    fn create_constant(&mut self, value: Value) -> u32 {
        if let Some(idx) = self.ctx.constants.iter().position(|x| x == &value) {
            return idx as u32;
        }

        self.ctx.constants.push(value);
        self.ctx.constants.len() as u32 - 1
    }

    fn set_out_reg(&mut self, reg: Register<usize>) {
        self.out_registers.push(reg);
    }

    fn pop_out_reg(&mut self) {
        self.out_registers.pop().expect("No out registers set");
    }

    fn out_reg(&self) -> Register<usize> {
        *self.out_registers.last().expect("No out registers set")
    }

    fn next_reg(&mut self) -> Register<usize> {
        self.register_counter += 1;
        Register(self.register_counter)
    }

    fn walk_with_out(&mut self, out: Register<usize>, node: &Node) {
        self.set_out_reg(out);
        self.walk_node(node);
        self.pop_out_reg();
    }
}

impl HlirVisitorImmut for FunctionBuilder<'_, usize> {
    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        // Collect arguments in reverse order, as the last arg is on top.
        for arg in function.arguments.iter().rev() {
            let reg = self.next_reg();
            self.inst(Instruction::Pop(reg));

            let idx = self.locals.len();
            self.inst(Instruction::StoreLocal(idx as _, reg));

            self.locals.push(arg.name);
        }

        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &Node) -> VisitAction {
        match &node.kind {
            NodeKind::Let { name, value, expr } => {
                let reg = self.next_reg();
                self.walk_with_out(reg, value);

                let idx = self.locals.len();
                self.inst(Instruction::StoreLocal(idx as _, reg));

                self.locals.push(*name);
                self.walk_node(expr);
                self.locals.pop();

                VisitAction::Nothing
            }
            NodeKind::Assign { name, value } => {
                let reg = self.next_reg();
                self.walk_with_out(reg, value);

                let idx = self
                    .locals
                    .iter()
                    .rev()
                    .position(|x| x == name)
                    .expect("Name not found");
                let idx = self.locals.len() - idx - 1;
                self.inst(Instruction::StoreLocal(idx as _, reg));

                VisitAction::Nothing
            }
            NodeKind::Binop { op, left, right } => {
                let left_reg = self.next_reg();
                self.walk_with_out(left_reg, left);

                let right_reg = self.next_reg();
                self.walk_with_out(right_reg, right);

                let out = self.out_reg();
                match op {
                    BinopKind::Equals => {
                        self.inst(Instruction::IntCmp(left_reg, right_reg));
                        self.inst(Instruction::Equals(out));
                    }
                    BinopKind::Less => {
                        self.inst(Instruction::IntCmp(left_reg, right_reg));
                        self.inst(Instruction::Less(out));
                    }
                    BinopKind::Greater => {
                        self.inst(Instruction::IntCmp(left_reg, right_reg));
                        self.inst(Instruction::Greater(out));
                    }
                    BinopKind::Add => {
                        self.inst(Instruction::Add(left_reg, right_reg, out));
                    }
                }

                VisitAction::Nothing
            }
            NodeKind::Call { callee, args } => {
                let func = self.next_reg();
                self.walk_with_out(func, callee);

                for arg in args {
                    let reg = self.next_reg();
                    self.walk_with_out(reg, arg);
                    self.inst(Instruction::Push(reg));
                }

                self.inst(Instruction::Call(func));

                match self.ctx.types.get_concrete(callee.ty) {
                    ConcreteType::Function {
                        output: box ConcreteType::Unit,
                        ..
                    } => {}
                    ConcreteType::Function { .. } => {
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
                let false_block = self.new_block();
                let end_block = if if_false.is_none() {
                    false_block
                } else {
                    self.new_block()
                };

                let cond_reg = self.next_reg();
                self.walk_with_out(cond_reg, cond);
                self.inst(Instruction::Branch(true_block, false_block, cond_reg));

                self.switch_block(true_block);
                self.walk_node(if_true);
                self.inst(Instruction::Jump(end_block));

                if let Some(if_false) = if_false {
                    self.switch_block(false_block);
                    self.walk_node(if_false);
                    self.inst(Instruction::Jump(end_block));
                }

                self.switch_block(end_block);

                VisitAction::Nothing
            }
            NodeKind::While { cond, body } => {
                let cond_block = self.new_block();
                let body_block = self.new_block();
                let end_block = self.new_block();

                self.inst(Instruction::Jump(cond_block));
                self.switch_block(cond_block);

                let cond_reg = self.next_reg();
                self.walk_with_out(cond_reg, cond);
                self.inst(Instruction::Branch(body_block, end_block, cond_reg));

                self.switch_block(body_block);
                self.walk_node(body);
                self.inst(Instruction::Jump(cond_block));

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
                    hlir::Literal::Bool(v) => Value::Bool(*v),
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
                let idx = self.locals.len() - idx - 1;

                let out = self.out_reg();
                self.inst(Instruction::LoadLocal(idx as _, out));

                VisitAction::Nothing
            }
            NodeKind::Builtin(idx) => {
                let reg = self.out_reg();
                self.inst(Instruction::LoadBuiltin(*idx as u32, reg));
                VisitAction::Nothing
            }
            NodeKind::Function(id) => {
                let header = self.ctx.index.functions.get(id).unwrap();
                let val = match header {
                    Header::Fn(_) => Value::Function(id.0 as u32),
                    Header::Effect(_) => Value::Effect(id.0 as u32),
                };

                let constant = self.create_constant(val);
                let reg = self.out_reg();
                self.inst(Instruction::LoadConstant(constant, reg));

                VisitAction::Nothing
            }
            NodeKind::Return(value) => {
                if let Some(value) = value {
                    let reg = self.next_reg();
                    self.walk_with_out(reg, value);
                    self.inst(Instruction::Push(reg));
                }

                self.inst(Instruction::Return);

                let block = self.new_block();
                self.switch_block(block);

                VisitAction::Nothing
            }
            NodeKind::Resume { arg } => {
                let reg = self.next_reg();
                self.walk_with_out(reg, arg);
                self.inst(Instruction::Push(reg));

                self.inst(Instruction::Resume);

                let block = self.new_block();
                self.switch_block(block);

                VisitAction::Nothing
            }
            NodeKind::Handle { handlers, expr, .. } => {
                let block = self.current_block as u32;
                let mut blocks = vec![];
                for handler in handlers {
                    let block = self.new_block();
                    self.switch_block(block);

                    for arg in handler.arguments.iter().rev() {
                        let reg = self.next_reg();
                        self.inst(Instruction::Pop(reg));

                        let idx = self.locals.len();
                        self.inst(Instruction::StoreLocal(idx as _, reg));

                        self.locals.push(*arg);
                    }

                    self.walk_node(&handler.body);

                    for _ in handler.arguments.iter().rev() {
                        self.locals.pop();
                    }

                    blocks.push((handler.effect_id, block));
                }

                self.switch_block(block);
                for (id, block) in &blocks {
                    self.inst(Instruction::InstallHandler(id.0 as u32, *block));
                }

                self.walk_node(expr);

                for (id, _) in &blocks {
                    self.inst(Instruction::UninstallHandler(id.0 as u32));
                }

                VisitAction::Nothing
            }
        }
    }
}

#[allow(unused)]
fn print_inst(inst: &Instruction<usize>, consts: &Vec<Value>) {
    let pad: usize = 4;
    let spad = pad + 3;
    match inst {
        Instruction::Copy(from, to) => println!("{to:>pad$} = {from}"),
        Instruction::LoadConstant(idx, reg) => {
            println!(
                "{reg:>pad$} = const {idx} ({})",
                match &consts[*idx as usize] {
                    Value::Builtin(_) => todo!(),
                    Value::Function(id) => format!("fn {}", id),
                    Value::Effect(id) => format!("eff fn {}", id),
                    Value::String(v) => format!(r#""{v}""#),
                    Value::Int(v) => v.to_string(),
                    Value::Bool(v) => v.to_string(),
                    Value::Nothing => todo!(),
                }
            );
        }
        Instruction::LoadLocal(idx, reg) => println!("{reg:>pad$} = local {idx}"),
        Instruction::StoreLocal(idx, reg) => println!("{:>spad$}store {idx}, {reg}", ""),
        Instruction::LoadBuiltin(idx, reg) => println!("{reg:>pad$} = builtin {idx}"),
        Instruction::Equals(reg) => println!("{reg:>pad$} = equals"),
        Instruction::Less(reg) => println!("{reg:>pad$} = less"),
        Instruction::Greater(reg) => println!("{reg:>pad$} = greater"),
        Instruction::Add(a, b, reg) => println!("{reg:>pad$} = add {a}, {b}"),
        Instruction::IntCmp(a, b) => println!("{:>spad$}cmp {a}, {b}", ""),
        Instruction::Jump(addr) => println!("{:>spad$}jump #{addr}", ""),
        Instruction::Branch(true_addr, false_addr, reg) => {
            println!("{:>spad$}branch #{true_addr}, #{false_addr} on {reg}", "");
        }
        Instruction::Call(reg) => println!("{:>spad$}call {reg}", ""),
        Instruction::Return => println!("{:>spad$}ret", ""),
        Instruction::Resume => println!("{:>spad$}resume", ""),
        Instruction::Push(reg) => println!("{:>spad$}push {reg}", ""),
        Instruction::Pop(reg) => println!("{reg:>pad$} = pop"),
        Instruction::InstallHandler(func, idx) => println!("{:>spad$}install {func}, {idx}", ""),
        Instruction::UninstallHandler(func) => println!("{:>spad$}uninstall {func}", ""),
    }
}

#[allow(unused)]
pub fn print_function(func: &Function<usize>, ctx: &FunctionBuilderCtx) {
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
            name_resolve::NameResolver, simplify::Simplifier, visitor::HlirVisitor, FileId,
            HlirBuilder, ModuleId,
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

        builder.load_builtins(|l| load_standard_builtins::<StandardPorts>(l));

        Simplifier.walk_hlir(&mut builder.hlir);
        NameResolver::new().walk_hlir(&mut builder.hlir);

        let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();
        let fndef = module.functions.get(&hlir::FunctionId(0)).unwrap();

        let mut ctx = FunctionBuilderCtx::new(&builder.hlir);

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
                            StoreLocal(0, Register(1)),
                            LoadLocal(0, Register(1)),
                            LoadConstant(0, Register(2)),
                            IntCmp(Register(1), Register(2)),
                            Equals(Register(1)),
                            Branch(1, 2, Register(1))
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
                constants: vec![Value::Int(0), Value::String("hello".to_owned().into())],
                types: builder.hlir.types.clone(),
                index: builder.hlir.construct_index()
            }
        );
    }
}
