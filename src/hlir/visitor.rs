use super::{FnDef, Hlir, Module, Node, NodeKind};

pub enum VisitAction<T> {
    OnExit(Box<dyn FnOnce(&mut T, &mut Node)>),
    Recurse,
    Nothing,
}

impl<T> VisitAction<T> {
    pub fn on_exit(f: impl FnOnce(&mut T, &mut Node) + 'static) -> VisitAction<T> {
        Self::OnExit(Box::new(f) as _)
    }
}

#[allow(unused_variables)]
pub trait HlirVisitor: Sized {
    fn walk_hlir(&mut self, node: &mut Hlir) {
        for module in node.modules.values_mut() {
            self.walk_module(module);
        }
    }

    fn walk_module(&mut self, node: &mut Module) {
        let action = self.visit_module(node);

        match action {
            VisitAction::OnExit(_) => todo!(),
            VisitAction::Recurse => {
                for function in node.functions.values_mut() {
                    self.walk_function(function);
                }
            }
            VisitAction::Nothing => {}
        }
    }

    fn walk_function(&mut self, node: &mut FnDef) {
        let action = self.visit_function(node);

        match action {
            VisitAction::OnExit(_) => todo!(),
            VisitAction::Recurse => {
                self.walk_node(&mut node.body);
            }
            VisitAction::Nothing => {}
        }
    }

    fn walk_node(&mut self, node: &mut Node) {
        self.visit_node(node);

        let mut action = match &node.kind {
            NodeKind::Let { .. } => self.visit_let(node),
            NodeKind::Binop { .. } => self.visit_binop(node),
            NodeKind::Call { .. } => self.visit_call(node),
            NodeKind::If { .. } => self.visit_if(node),
            NodeKind::Block(_) => self.visit_block(node),
            NodeKind::Literal(_) => self.visit_literal(node),
            NodeKind::Name(_) => self.visit_name(node),
            NodeKind::Builtin(_) => self.visit_builtin(node),
        };

        let on_exit = match action {
            VisitAction::OnExit(c) => {
                action = VisitAction::Recurse;
                Some(c)
            }
            VisitAction::Recurse => None,
            VisitAction::Nothing => None,
        };

        match action {
            VisitAction::Recurse => match &mut node.kind {
                NodeKind::Let { value, expr, .. } => {
                    self.walk_node(value);
                    self.walk_node(expr);
                }
                NodeKind::Binop { left, right, .. } => {
                    self.walk_node(left);
                    self.walk_node(right);
                }
                NodeKind::Call { callee, args } => {
                    self.walk_node(callee);
                    for arg in args {
                        self.walk_node(arg);
                    }
                }
                NodeKind::If {
                    cond,
                    if_true,
                    if_false,
                } => {
                    self.walk_node(cond);
                    self.walk_node(if_true);
                    if let Some(if_false) = if_false {
                        self.walk_node(if_false);
                    }
                }
                NodeKind::Block(nodes) => {
                    for node in nodes {
                        self.walk_node(node);
                    }
                }
                NodeKind::Literal(_) => {}
                NodeKind::Name(_) => {}
                NodeKind::Builtin(_) => {}
            },
            VisitAction::Nothing => {}
            VisitAction::OnExit(_) => unreachable!(),
        }

        if let Some(on_exit) = on_exit {
            on_exit(self, node);
        }
    }

    fn visit_module(&mut self, module: &mut Module) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &mut FnDef) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &mut Node) {}

    fn visit_let(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_binop(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_call(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_if(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_block(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_literal(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_name(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }

    fn visit_builtin(&mut self, node: &mut Node) -> VisitAction<Self> {
        VisitAction::Recurse
    }
}
