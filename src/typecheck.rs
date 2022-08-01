use lasso::{Rodeo, Spur};

use crate::hlir::Builtins;
use crate::ir::{Literal, Node, NodeKind, Type, IR};

pub struct Typechecker {
    func_types: Vec<Type>,
}

#[derive(Debug)]
pub enum TypeError {}

pub type TypeResult<T = (), E = TypeError> = Result<T, E>;

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            func_types: Vec::new(),
        }
    }

    pub fn infer(&mut self, ir: &mut IR) -> TypeResult {
        self.func_types = ir.functions.iter().map(|f| f.ty.clone()).collect();
        let mut prev_nodes = Vec::new();

        for function in &mut ir.functions {
            for block in &mut function.blocks {
                prev_nodes.clear();
                for node in &mut block.nodes {
                    self.infer_node(&prev_nodes, node)?;
                    prev_nodes.push(node.clone());
                }
            }
        }
        Ok(())
    }

    fn infer_node(&mut self, prev_nodes: &[Node], node: &mut Node) -> TypeResult {
        let ty = match &node.kind {
            NodeKind::Symbol(_name) => todo!(),
            NodeKind::Builtin { .. } => None,
            NodeKind::Literal(Literal::String(_)) => Some(Type::String),
            NodeKind::Call(idx, _) => self.infer_call(prev_nodes, *idx)?,
        };
        if let Some(ty) = ty {
            node.ty = ty;
        }
        Ok(())
    }

    fn infer_call(&mut self, prev_nodes: &[Node], idx: usize) -> TypeResult<Option<Type>> {
        let node = &prev_nodes[idx];
        let ty = match &node.ty {
            Type::Function { output, .. } => Some((&**output).clone()),
            _ => None,
        };
        Ok(ty)
    }
}
