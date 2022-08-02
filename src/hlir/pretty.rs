use std::borrow::Cow;

use crate::{
    hlir::{visitor::VisitAction, Literal},
    intern::resolve_symbol,
    parser::BinopKind,
};

use super::{visitor::HlirVisitorImmut, Builtins, FnDef, NodeKind, Type};

#[derive(Debug)]
pub enum Fragment<'a> {
    Text(Cow<'a, str>),
    Indent(i32),
    HardBreak,
}

#[derive(Debug)]
pub struct PrettyPrint<'s, 'b> {
    pub builtins: &'b Builtins,
    pub fragments: Vec<Fragment<'s>>,
}

impl<'s, 'b> PrettyPrint<'s, 'b> {
    pub fn new(builtins: &'b Builtins) -> Self {
        Self {
            builtins,
            fragments: Default::default(),
        }
    }

    fn text(&mut self, x: impl Into<Cow<'s, str>>) {
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

impl HlirVisitorImmut for PrettyPrint<'_, '_> {
    fn visit_module(&mut self, module: &super::Module) -> VisitAction {
        self.text(format!("Module #{}", module.id.0));
        self.hard_break();
        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        let FnDef {
            id,
            name,
            return_ty,
            body,
            ..
        } = function;

        let name = name.map_or("<unnamed>", resolve_symbol);
        self.text(format!("fn {} #{} () -> ", name, id.0));
        self.format_type(return_ty);
        self.hard_break();

        self.indent();
        self.walk_node(body);
        self.dedent();

        VisitAction::Nothing
    }

    fn visit_node(&mut self, node: &super::Node) -> VisitAction {
        match &node.kind {
            NodeKind::Let { name, value, expr } => {
                self.text("let ");
                self.text(resolve_symbol(*name));
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
            NodeKind::Binop { op, left, right } => {
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
            NodeKind::Call { callee, args } => {
                self.walk_node(callee);
                self.text("(");
                let len = args.len();
                for (i, arg) in args.iter().enumerate() {
                    self.walk_node(arg);
                    if i < len - 1 {
                        self.text(", ");
                    }
                }
                self.text(")");
                VisitAction::Nothing
            }
            NodeKind::If {
                cond,
                if_true,
                if_false,
            } => {
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
            NodeKind::Block(nodes) => {
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
            NodeKind::Literal(lit) => {
                match lit {
                    Literal::String(key) => {
                        let name = resolve_symbol(*key);
                        self.text(format!(r#""{name}""#));
                    }
                    Literal::Int(v) => self.text(format!("{v}")),
                }
                VisitAction::Recurse
            }
            NodeKind::Name(key) => {
                let name = resolve_symbol(*key);
                self.text(format!("{name}"));
                self.text(": ");
                self.format_type(&node.ty);
                VisitAction::Recurse
            }
            NodeKind::Builtin(idx) => {
                let builtin = &self.builtins.builtins_ord[*idx];
                let name = resolve_symbol(builtin.name);
                self.text("(");
                self.text(name);
                self.text(": ");
                self.format_type(&node.ty);
                self.text(")");
                VisitAction::Recurse
            }
        }
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
