use std::collections::HashSet;

use tinyvec::ArrayVec;

use super::{Block, Function, Instruction, Register};

#[derive(Debug)]
pub struct InstDesc<T: Default> {
    pub reads: ArrayVec<[Register<T>; 2]>,
    pub writes: ArrayVec<[Register<T>; 2]>,
}

pub fn simplify_function(func: &mut Function<usize>) {
    for block in func.blocks.iter_mut() {
        replace_load_with_copy(block);
        remove_unnecessary_write(block);
        remove_unnecessary_load(block);
        simplify_registers(block);
        compact_registers(block);
    }
}

fn remove_unnecessary_write(block: &mut Block<usize>) {
    enum Rewrite {
        Remove(usize),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate().rev() {
        let (local, var) = match inst {
            Instruction::StoreLocal(idx, var) => (*idx, *var),
            _ => continue,
        };

        if block.outputs.contains(&local) {
            continue;
        }

        let start = idx + 1;
        let scope =
            resolve_local_write_scope(local, &block.insts, start).unwrap_or(block.insts.len() - 1);

        let mut read = false;
        for inst in block.insts[start..=scope].iter() {
            match inst {
                Instruction::LoadLocal(loc, _) if *loc == local => {
                    read = true;
                    break;
                }
                Instruction::StoreLocal(loc, _) if *loc == local => unreachable!(),
                _ => continue,
            }
        }

        if !read {
            rewrites.push(Remove(idx));
        }
    }

    for rewrite in rewrites {
        match rewrite {
            Remove(idx) => {
                block.insts.remove(idx);
            }
        }
    }
}

fn replace_load_with_copy(block: &mut Block<usize>) {
    enum Rewrite {
        Substitute(usize, Instruction<usize>),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        let (local, var) = match inst {
            Instruction::StoreLocal(idx, var) => (*idx, *var),
            _ => continue,
        };

        let start = idx + 1;
        let scope = resolve_write_scope(&var, &block.insts, start).unwrap_or(block.insts.len() - 1);

        for (idx, inst) in block.insts[start..=scope].iter().enumerate() {
            match inst {
                Instruction::LoadLocal(loc, reg) if *loc == local => {
                    rewrites.push(Substitute(idx + start, Instruction::Copy(var, *reg)));
                }
                Instruction::StoreLocal(loc, _) if *loc == local => unreachable!(),
                _ => continue,
            }
        }
    }

    for rewrite in rewrites {
        match rewrite {
            Substitute(idx, inst) => {
                block.insts[idx] = inst;
            }
        }
    }
}

fn remove_unnecessary_load(block: &mut Block<usize>) {
    enum Rewrite {
        Remove(usize),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        let var = match inst {
            Instruction::LoadLocal(_, var) => *var,
            _ => continue,
        };

        let start = idx + 1;
        let scope = resolve_scope(&var, &block.insts, start);
        if scope.is_none() {
            rewrites.push(Remove(idx));
        }
    }

    for rewrite in rewrites.into_iter().rev() {
        match rewrite {
            Remove(idx) => {
                block.insts.remove(idx);
            }
        }
    }
}

fn compact_registers(block: &mut Block<usize>) {
    enum Rewrite {
        Substitute(usize, Register<usize>, Register<usize>),
    }
    use Rewrite::*;

    let mut used_registers = HashSet::new();
    let mut remove_at = vec![];

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        for (remove_at, var) in &remove_at {
            if idx == *remove_at {
                used_registers.remove(var);
                break;
            }
        }

        let desc = describe_inst(inst);
        let var = match desc.writes.last() {
            Some(w) => *w,
            None => continue,
        };

        let start = idx + 1;
        let scope = resolve_scope(&var, &block.insts, start).unwrap_or(start);

        let mut minimal = Register(1);
        while used_registers.contains(&minimal) {
            minimal.0 += 1;
        }

        rewrites.push(Substitute(idx, var, minimal));

        for (idx, inst) in block.insts.iter().enumerate().skip(start).take(scope - idx) {
            let desc = describe_inst(inst);
            if desc.reads.contains(&var) {
                rewrites.push(Substitute(idx, var, minimal));
            }
        }

        used_registers.insert(minimal);
        remove_at.push((scope, minimal));
    }

    for rewrite in rewrites.into_iter().rev() {
        match rewrite {
            Substitute(idx, orig, new) => {
                let inst = &mut block.insts[idx];
                replace_regiater(inst, orig, new);
            }
        }
    }
}

fn simplify_registers(block: &mut Block<usize>) {
    enum Rewrite {
        Substitute(usize, Register<usize>, Register<usize>),
        Remove(usize),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate().rev() {
        let desc = describe_inst(inst);
        let var = match desc.writes.last() {
            Some(w) => *w,
            None => continue,
        };

        let start = idx + 1;
        let scope = resolve_scope(&var, &block.insts, start);
        match scope {
            Some(scope) => {
                let mut uses = block.insts[start..=scope]
                    .iter()
                    .enumerate()
                    .filter(|x| describe_inst(&x.1).reads.contains(&var));

                let first = uses.next().unwrap();
                let first_idx = start + first.0;

                if uses.next().is_some() {
                    continue;
                }

                match inst {
                    Instruction::Copy(from, to) => {
                        rewrites.push(Substitute(first_idx, *to, *from));
                        rewrites.push(Remove(idx));
                    }
                    _ => {}
                }
            }
            None => {}
        }
    }

    for rewrite in rewrites {
        match rewrite {
            Substitute(idx, orig, new) => {
                let inst = &mut block.insts[idx];
                replace_regiater(inst, orig, new);
            }
            Remove(idx) => {
                block.insts.remove(idx);
            }
        }
    }
}

fn replace_regiater(inst: &mut Instruction<usize>, orig: Register<usize>, new: Register<usize>) {
    use Instruction::*;
    match inst {
        Branch(_, _, reg)
        | LoadConstant(_, reg)
        | LoadBuiltin(_, reg)
        | StoreLocal(_, reg)
        | LoadLocal(_, reg)
        | Push(reg)
        | Call(reg)
        | Equals(reg) => {
            *reg = new;
        }
        IntCmp(a, b) => {
            if *a == orig {
                *a = new;
            }
            if *b == orig {
                *b = new;
            }
        }
        _ => todo!("replace_regiater {:?}", inst),
    }
}

/// Finds the last index that uses this variable.
/// Returns `None` if the variable is written to before it's used.
fn resolve_scope(var: &Register, insts: &[Instruction], start: usize) -> Option<usize> {
    let mut last = None;
    for (idx, inst) in insts.iter().enumerate().skip(start) {
        let desc = describe_inst(inst);
        if desc.writes.contains(var) {
            break;
        }
        if desc.reads.contains(var) {
            last = Some(idx);
        }
    }
    last
}

/// Finds the index where this variable is replaced.
/// Returns `None` if the variable is never written to.
fn resolve_write_scope(var: &Register, insts: &[Instruction], start: usize) -> Option<usize> {
    for (idx, inst) in insts.iter().enumerate().skip(start) {
        let desc = describe_inst(inst);
        if desc.writes.contains(var) {
            return Some(idx - 1);
        }
    }
    None
}

/// Finds the index where this local is replaced.
/// Returns `None` if the local is never written to.
fn resolve_local_write_scope<T>(
    local: u32,
    insts: &[Instruction<T>],
    start: usize,
) -> Option<usize> {
    for (idx, inst) in insts.iter().enumerate().skip(start) {
        match inst {
            Instruction::StoreLocal(loc, _) if *loc == local => return Some(idx - 1),
            _ => {}
        }
    }
    None
}

pub fn describe_inst<T: Default + Copy>(inst: &Instruction<T>) -> InstDesc<T> {
    let mut reads = ArrayVec::default();
    let mut writes = ArrayVec::default();

    match inst {
        Instruction::Copy(from, to) => {
            reads.push(*from);
            writes.push(*to);
        }
        Instruction::LoadConstant(_, to) => {
            writes.push(*to);
        }
        Instruction::LoadLocal(_, to) => {
            writes.push(*to);
        }
        Instruction::StoreLocal(_, from) => {
            reads.push(*from);
        }
        Instruction::LoadBuiltin(_, to) => {
            writes.push(*to);
        }
        Instruction::IntCmp(a, b) => {
            reads.push(*a);
            reads.push(*b);
        }
        Instruction::Equals(reg) => {
            writes.push(*reg);
        }
        Instruction::Jump(_) => {}
        Instruction::Branch(_, _, reg) => {
            reads.push(*reg);
        }
        Instruction::Call(reg) => {
            reads.push(*reg);
        }
        Instruction::Return => {}
        Instruction::Push(reg) => {
            reads.push(*reg);
        }
        Instruction::Pop(reg) => {
            writes.push(*reg);
        }
    }

    InstDesc { reads, writes }
}
