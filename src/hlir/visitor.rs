use super::{FnDef, Hlir, Module, Node, NodeKind};

pub enum VisitAction {
    Recurse,
    Nothing,
}

#[allow(unused_variables)]
pub trait HlirVisitor: Sized {
    fn walk_hlir(&mut self, node: &mut Hlir) {
        self.visit_hlir(node);
        for module in node.modules.values_mut() {
            self.walk_module(module);
        }
    }

    fn walk_module(&mut self, node: &mut Module) {
        let action = self.visit_module(node);

        match action {
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
            VisitAction::Recurse => {
                self.walk_node(&mut node.body);
            }
            VisitAction::Nothing => {}
        }
    }

    fn walk_node(&mut self, node: &mut Node) {
        let action = self.visit_node(node);

        match action {
            VisitAction::Recurse => match &mut node.kind {
                NodeKind::Let { value, expr, .. } => {
                    self.walk_node(value);
                    self.walk_node(expr);
                }
                NodeKind::Assign { value, .. } => {
                    self.walk_node(value);
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
                NodeKind::While { cond, body } => {
                    self.walk_node(cond);
                    self.walk_node(body);
                }
                NodeKind::Block(nodes) => {
                    for node in nodes {
                        self.walk_node(node);
                    }
                }
                NodeKind::Return(v) => self.walk_node(v),
                NodeKind::Literal(_) => {}
                NodeKind::Name(_) => {}
                NodeKind::Builtin(_) => {}
                NodeKind::Function(_) => {}
            },
            VisitAction::Nothing => {}
        }
    }

    fn visit_hlir(&mut self, hlir: &mut Hlir) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &mut Module) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &mut FnDef) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &mut Node) -> VisitAction {
        VisitAction::Recurse
    }
}

#[allow(unused_variables)]
pub trait HlirVisitorImmut: Sized {
    fn walk_hlir(&mut self, node: &Hlir) {
        self.visit_hlir(node);
        for module in node.modules.values() {
            self.walk_module(module);
        }
    }

    fn walk_module(&mut self, node: &Module) {
        let action = self.visit_module(node);

        match action {
            VisitAction::Recurse => {
                for function in node.functions.values() {
                    self.walk_function(function);
                }
            }
            VisitAction::Nothing => {}
        }
    }

    fn walk_function(&mut self, node: &FnDef) {
        let action = self.visit_function(node);

        match action {
            VisitAction::Recurse => {
                self.walk_node(&node.body);
            }
            VisitAction::Nothing => {}
        }
    }

    fn walk_node(&mut self, node: &Node) {
        let action = self.visit_node(node);

        match action {
            VisitAction::Recurse => match &node.kind {
                NodeKind::Let { value, expr, .. } => {
                    self.walk_node(value);
                    self.walk_node(expr);
                }
                NodeKind::Assign { value, .. } => {
                    self.walk_node(value);
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
                NodeKind::While { cond, body } => {
                    self.walk_node(cond);
                    self.walk_node(body);
                }
                NodeKind::Block(nodes) => {
                    for node in nodes {
                        self.walk_node(node);
                    }
                }
                NodeKind::Return(v) => self.walk_node(v),
                NodeKind::Literal(_) => {}
                NodeKind::Name(_) => {}
                NodeKind::Builtin(_) => {}
                NodeKind::Function(_) => {}
            },
            VisitAction::Nothing => {}
        }
    }

    fn visit_hlir(&mut self, hlir: &Hlir) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &Module) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        VisitAction::Recurse
    }

    fn visit_node(&mut self, node: &Node) -> VisitAction {
        VisitAction::Recurse
    }
}

impl<T: HlirVisitorImmut> HlirVisitor for T {
    fn walk_hlir(&mut self, node: &mut Hlir) {
        HlirVisitorImmut::walk_hlir(self, &*node)
    }

    fn walk_module(&mut self, node: &mut Module) {
        HlirVisitorImmut::walk_module(self, node)
    }

    fn walk_function(&mut self, node: &mut FnDef) {
        HlirVisitorImmut::walk_function(self, node)
    }

    fn walk_node(&mut self, node: &mut Node) {
        HlirVisitorImmut::walk_node(self, node)
    }
}
