use std::borrow::Cow;

use lasso::Rodeo;

use crate::{
    extract,
    hlir::{visitor::VisitAction, Literal},
    parser::BinopKind,
};

use super::{visitor::HlirVisitor, FnDef, Hlir, NodeKind, Type};

#[derive(Debug)]
pub enum Fragment<'a> {
    Text(Cow<'a, str>),
    Indent(i32),
    HardBreak,
}

pub struct PrettyPrint<'a> {
    pub interner: &'a Rodeo,
    pub fragments: Vec<Fragment<'a>>,
}

impl<'a> PrettyPrint<'a> {
    pub fn new(interner: &'a Rodeo) -> Self {
        Self {
            interner,
            fragments: Vec::new(),
        }
    }

    fn text(&mut self, x: impl Into<Cow<'a, str>>) {
        self.fragments.push(Fragment::Text(x.into()));
    }

    fn hard_break(&mut self) {
        self.fragments.push(Fragment::HardBreak);
    }

    fn indent(&mut self) {
        self.fragments.push(Fragment::Indent(2));
    }

    fn dedent(&mut self) {
        self.fragments.push(Fragment::Indent(-2));
    }

    fn format_type(&mut self, ty: &Type) {
        match ty {
            Type::Unknown(id) => {
                self.text(format!("?{}", id));
            }
            Type::Function { inputs, output } => {
                self.text("(");
                for (i, input) in inputs.iter().enumerate() {
                    self.format_type(input);
                    if i != inputs.len() - 1 {
                        self.text(", ");
                    }
                }
                self.text(") -> ");
                self.format_type(output);
            }
            Type::String => {
                self.text("string");
            }
            Type::Unit => {
                self.text("unit");
            }
            Type::Int => {
                self.text("int");
            }
            Type::Bool => {
                self.text("bool");
            }
        }
    }
}

impl HlirVisitor for PrettyPrint<'_> {
    fn visit_module(&mut self, module: &mut super::Module) -> super::visitor::VisitAction<Self> {
        self.text(format!("Module #{}", module.id.0));
        self.hard_break();
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &mut FnDef) -> VisitAction<Self> {
        let FnDef {
            id,
            name,
            return_ty,
            body,
            ..
        } = function;

        let name = name.map_or("<unnamed>", |x| self.interner.resolve(&x));
        self.text(format!("fn {} #{} () -> ", name, id.0));
        self.format_type(return_ty);
        self.hard_break();

        self.indent();
        self.walk_node(body);
        self.dedent();

        VisitAction::Nothing
    }

    fn visit_call(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Call { callee, args });
        self.walk_node(callee);
        self.text("(");
        let len = args.len();
        for (i, arg) in args.iter_mut().enumerate() {
            self.walk_node(arg);
            if i < len - 1 {
                self.text(", ");
            }
        }
        self.text(")");
        VisitAction::Nothing
    }

    fn visit_let(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Let { name, value, expr });

        self.text("let ");
        self.text(self.interner.resolve(name));
        self.text(": ");
        self.format_type(&value.ty);
        self.text(" = ");
        self.walk_node(value);
        self.text(" in");
        self.indent();
        self.hard_break();
        self.walk_node(expr);
        self.dedent();

        VisitAction::Nothing
    }

    fn visit_if(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(
            &mut node.kind,
            NodeKind::If {
                cond,
                if_true,
                if_false
            }
        );

        self.text("if: ");
        self.format_type(&node.ty);
        self.hard_break();
        self.indent();

        self.text("cond:  ");
        self.indent();
        self.walk_node(cond);
        self.dedent();
        self.hard_break();

        self.text("true:  ");
        self.walk_node(if_true);
        self.hard_break();

        if let Some(if_false) = if_false {
            self.text("false: ");
            self.walk_node(if_false);
            self.hard_break();
        }

        self.dedent();

        VisitAction::Nothing
    }

    fn visit_binop(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Binop { op, left, right });

        self.text("(");
        self.indent();
        self.walk_node(left);

        match op {
            BinopKind::Equals => {
                self.text(" == ");
            }
        }

        self.walk_node(right);
        self.dedent();
        self.text("): ");
        self.format_type(&node.ty);

        VisitAction::Nothing
    }

    fn visit_builtin(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Builtin(key));
        let name = self.interner.resolve(key);
        self.text("(");
        self.text(format!("{name}"));
        self.text(": ");
        self.format_type(&node.ty);
        self.text(")");
        VisitAction::Recurse
    }

    fn visit_name(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Name(key));
        let name = self.interner.resolve(key);
        self.text(format!("{name}"));
        self.text(": ");
        self.format_type(&node.ty);
        VisitAction::Recurse
    }

    fn visit_block(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&mut node.kind, NodeKind::Block(nodes));
        self.text("{");
        self.indent();
        self.hard_break();
        for node in nodes {
            self.walk_node(node);
            self.hard_break();
        }
        self.dedent();
        self.text("}");
        self.hard_break();
        VisitAction::Nothing
    }

    fn visit_literal(&mut self, node: &mut super::Node) -> VisitAction<Self> {
        extract!(&node.kind, NodeKind::Literal(lit));
        match lit {
            Literal::String(key) => {
                let name = self.interner.resolve(key);
                self.text(format!(r#""{name}""#));
            }
            Literal::Int(v) => self.text(format!("{v}")),
        }
        VisitAction::Recurse
    }
}

pub fn print_fragments(fragments: &[Fragment]) {
    let max_width = 80;
    let mut indent = 0;
    let mut column = 0;
    for fragment in fragments {
        match fragment {
            Fragment::Text(text) => {
                // TODO: use unicode segmentation
                let len = text.len();

                if column + len > max_width {
                    println!();
                    column = 0;
                }

                if column == 0 && indent > 0 {
                    column += indent as usize;
                    print!("{}", " ".repeat(indent));
                }

                column += len;
                print!("{}", text);
            }
            Fragment::Indent(change) => {
                indent = (indent as i32 + change).clamp(0, i32::MAX) as usize;
            }
            Fragment::HardBreak => {
                if column > 0 {
                    println!();
                    column = 0;
                }
            }
        }
    }
}
