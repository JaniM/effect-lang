use tinyvec::ArrayVec;

use super::{Block, Function, Instruction, Register};

#[derive(Debug)]
pub struct InstDesc {
    pub reads: ArrayVec<[Register; 2]>,
    pub writes: ArrayVec<[Register; 2]>,
}

pub fn simplify_function(func: &mut Function) {
    for block in func.blocks.iter_mut() {
        replace_load_with_copy(block);
        remove_unnecessary_write(block);
        remove_unnecessary_load(block);
        simplify_registers(block);
    }
}

fn remove_unnecessary_write(block: &mut Block) {
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
        let scope = resolve_write_scope(&var, &block.insts, start).unwrap_or(block.insts.len());

        let mut read = false;
        for inst in block.insts[start..=scope].iter() {
            match inst {
                Instruction::LoadLocal(loc, _) if *loc == local => {
                    read = true;
                    break;
                }
                Instruction::StoreLocal(_, _) => unreachable!(),
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

fn replace_load_with_copy(block: &mut Block) {
    enum Rewrite {
        Substitute(usize, Instruction),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        let (local, var) = match inst {
            Instruction::StoreLocal(idx, var) => (*idx, *var),
            _ => continue,
        };

        let start = idx + 1;
        let scope = resolve_write_scope(&var, &block.insts, start).unwrap_or(block.insts.len());

        for (idx, inst) in block.insts[start..=scope].iter().enumerate() {
            match inst {
                Instruction::LoadLocal(loc, reg) if *loc == local => {
                    rewrites.push(Substitute(idx + start, Instruction::Copy(var, *reg)));
                }
                Instruction::StoreLocal(_, _) => unreachable!(),
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

fn remove_unnecessary_load(block: &mut Block) {
    enum Rewrite {
        Remove(usize),
    }
    use Rewrite::*;

    let mut rewrites = Vec::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        let (local, var) = match inst {
            Instruction::LoadLocal(idx, var) => (*idx, *var),
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

fn simplify_registers(block: &mut Block) {
    enum Rewrite {
        Substitute(usize, Register, Register),
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

fn replace_regiater(inst: &mut Instruction, orig: Register, new: Register) {
    match inst {
        Instruction::Branch(_, _, reg) => {
            *reg = new;
        }
        Instruction::IntCmp(a, b) => {
            if *a == orig {
                *a = new;
            }
            if *b == orig {
                *b = new;
            }
        }
        _ => todo!(),
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
/// Returns `None` if the variable is written to before it's used.
fn resolve_write_scope(var: &Register, insts: &[Instruction], start: usize) -> Option<usize> {
    for (idx, inst) in insts.iter().enumerate().skip(start) {
        let desc = describe_inst(inst);
        if desc.writes.contains(var) {
            return Some(idx - 1);
        }
    }
    None
}

pub fn describe_inst(inst: &Instruction) -> InstDesc {
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
        Instruction::Equals => {}
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
