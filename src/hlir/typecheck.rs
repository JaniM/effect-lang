use lasso::Spur;

use crate::{
    extract,
    hlir::{visitor::VisitAction, Literal},
};

use super::{visitor::HlirVisitor, Hlir, Node, NodeKind, Type};

#[derive(Default)]
pub struct Typechecker {
    names: Vec<(Spur, Type)>,
}

impl HlirVisitor for Typechecker {
    fn visit_let(&mut self, node: &mut Node) -> super::visitor::VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Let { name, value, expr });

        node.ty = Type::Unit;
        self.walk_node(value);
        self.names.push((*name, value.ty.clone()));
        self.walk_node(expr);
        self.names.pop();

        VisitAction::Nothing
    }

    fn visit_name(&mut self, node: &mut Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Name(name));

        if let Some((_, ty)) = self.names.iter().rev().find(|x| x.0 == *name) {
            node.ty = ty.clone();
        }

        VisitAction::Nothing
    }

    fn visit_literal(&mut self, node: &mut Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Literal(lit));
        match lit {
            Literal::String(_) => {
                node.ty = Type::String;
            }
            Literal::Int(_) => node.ty = Type::Int,
        }
        VisitAction::Nothing
    }

    fn visit_call(&mut self, node: &mut Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Call { callee, args });

        self.walk_node(callee);
        for arg in args {
            self.walk_node(arg);
        }
        node.ty = match &callee.ty {
            Type::Function { output, .. } => (&**output).clone(),
            _ => panic!("Called a non-function"),
        };

        VisitAction::Nothing
    }

    fn visit_if(&mut self, node: &mut Node) -> VisitAction<Self> {
        extract!(
            &mut node.kind,
            NodeKind::If {
                cond,
                if_true,
                if_false
            }
        );

        self.walk_node(cond);
        self.walk_node(if_true);
        if let Some(if_false) = if_false {
            self.walk_node(if_false);
            node.ty = if_false.ty.clone();
        } else {
            node.ty = Type::Unit;
        }

        VisitAction::Nothing
    }

    fn visit_binop(&mut self, node: &mut Node) -> VisitAction<Self> {
        node.ty = Type::Bool;

        VisitAction::Recurse
    }
}

pub fn report_unknown_types(hlir: &mut Hlir) {
    struct Visitor;

    impl HlirVisitor for Visitor {
        fn visit_node(&mut self, node: &mut Node) {
            match &node.ty {
                Type::Unknown(id) => {
                    println!("Unknown type: ?{}", id);
                    println!("{:?}", node);
                }
                _ => {}
            }
        }
    }

    Visitor.walk_hlir(hlir);
}
