use lasso::Spur;

use super::{Block, Builtins, Function, Node, NodeKind, IR};

pub struct IRWalker<'a> {
    ir: &'a mut IR,
    function: Option<usize>,
    block: Option<usize>,
    node: Option<usize>,
}

impl<'a> IRWalker<'a> {
    pub fn new(ir: &'a mut IR) -> Self {
        IRWalker {
            ir,
            function: None,
            block: None,
            node: None,
        }
    }

    fn next_function(&mut self) -> Option<usize> {
        let func = self.function.map_or(0, |x| x + 1);
        if func >= self.ir.functions.len() {
            return None;
        }
        self.function = Some(func);
        self.block = None;
        self.function
    }

    fn next_block(&mut self) -> Option<usize> {
        let function = self.get_function()?;
        let block = self.block.map_or(0, |x| x + 1);
        if block >= function.blocks.len() {
            return None;
        }
        self.block = Some(block);
        self.node = None;
        self.block
    }

    fn next_node(&mut self) -> Option<usize> {
        let block = self.get_block()?;
        let node = self.node.map_or(0, |x| x + 1);
        if node >= block.nodes.len() {
            return None;
        }
        self.node = Some(node);
        self.node
    }

    fn step_nodes(&mut self) -> Option<usize> {
        loop {
            if let Some(node) = self.next_node() {
                return Some(node);
            }
            if self.next_block().is_some() {
                continue;
            }
            if self.next_function().is_some() {
                self.next_block();
                continue;
            }
            return None;
        }
    }

    fn get_function(&self) -> Option<&Function> {
        Some(&self.ir.functions[self.function?])
    }

    fn get_block(&self) -> Option<&Block> {
        let function = self.get_function()?;
        Some(&function.blocks[self.block?])
    }

    fn get_node(&self) -> Option<&Node> {
        let block = self.get_block()?;
        Some(&block.nodes[self.node?])
    }

    fn get_function_mut(&mut self) -> Option<&mut Function> {
        Some(&mut self.ir.functions[self.function?])
    }

    fn get_block_mut(&mut self) -> Option<&mut Block> {
        let block = self.block?;
        let function = self.get_function_mut()?;
        Some(&mut function.blocks[block])
    }

    fn get_node_mut(&mut self) -> Option<&mut Node> {
        let node = self.node?;
        let block = self.get_block_mut()?;
        Some(&mut block.nodes[node])
    }
}

#[derive(Debug)]
pub enum ResolveError {
    UnknownName(Spur),
}

type ResolveResult<T = (), E = ResolveError> = Result<T, E>;

pub fn resolve_names(builtins: &Builtins, ir: &mut IR) -> ResolveResult {
    let mut walker = IRWalker::new(ir);
    while walker.step_nodes().is_some() {
        let node = walker.get_node_mut().unwrap();
        match &node.kind {
            &NodeKind::Symbol(name) => {
                if let Some(builtin) = builtins.builtins.get(&name) {
                    node.kind = NodeKind::Builtin {
                        name,
                        idx: builtin.idx,
                    };
                    node.ty = builtin.ty.clone();
                } else {
                    return Err(ResolveError::UnknownName(name));
                }
            }
            _ => {}
        }
    }
    Ok(())
}
