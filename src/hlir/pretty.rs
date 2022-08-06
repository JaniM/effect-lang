use std::borrow::Cow;

use crate::{
    extract,
    hlir::{visitor::VisitAction, Literal},
    intern::resolve_symbol,
    parser::{BinopKind, EffectKind},
    typecheck::TypeStore,
};

use super::{
    index::{Header, Index},
    visitor::HlirVisitorImmut,
    EffectDef, EffectHeader, FnDef, FnHeader, NodeKind, Type, TypeId,
};

#[derive(Debug)]
pub enum Fragment<'a> {
    Text(Cow<'a, str>),
    Indent(i32),
    HardBreak,
}

#[derive(Debug, Default)]
pub struct PrettyPrint<'s> {
    pub fragments: Vec<Fragment<'s>>,
    index: Index,
    types: TypeStore,
}

impl<'s> PrettyPrint<'s> {
    pub fn new() -> Self {
        PrettyPrint::default()
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

    fn format_type(&mut self, id: &TypeId) {
        let ty = self.types.get(*id);
        match ty {
            Type::Unknown => {
                self.text(format!("?{}", id.0));
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
                self.format_type(&output);
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
            Type::Name(key) => {
                self.text("?");
                self.text(resolve_symbol(key));
            }
        }
    }
}

impl HlirVisitorImmut for PrettyPrint<'_> {
    fn visit_hlir(&mut self, hlir: &super::Hlir) -> VisitAction {
        self.types = hlir.types.clone();
        self.index = hlir.construct_index();
        VisitAction::Recurse
    }

    fn visit_module(&mut self, module: &super::Module) -> VisitAction {
        self.text(format!("Module #{}", module.id.0));
        self.hard_break();

        for group in module.effect_groups.values() {
            self.text(format!(
                "effect {} #{}",
                resolve_symbol(group.name),
                group.id.0
            ));
            self.indent();
            self.hard_break();

            for effect in &group.effects {
                let EffectDef {
                    header: EffectHeader { kind, id, name, .. },
                    return_ty,
                    arguments,
                } = effect;

                let name = resolve_symbol(*name);
                self.text(match kind {
                    EffectKind::Function => "fn",
                    EffectKind::Control => "ctl",
                });
                self.text(format!(" {} #{} (", name, id.0));

                for (i, arg) in arguments.iter().enumerate() {
                    self.text(resolve_symbol(arg.name));
                    self.text(": ");
                    self.format_type(&arg.ty);
                    if i < arguments.len() - 1 {
                        self.text(", ");
                    }
                }

                self.text(") -> ");
                self.format_type(return_ty);
                self.hard_break();
            }

            self.dedent();
        }

        VisitAction::Recurse
    }

    fn visit_function(&mut self, function: &FnDef) -> VisitAction {
        let FnDef {
            header: FnHeader { id, name, .. },
            return_ty,
            body,
            arguments,
        } = function;

        let name = name.map_or("<unnamed>", resolve_symbol);
        self.text(format!("fn {} #{} (", name, id.0));

        for (i, arg) in arguments.iter().enumerate() {
            self.text(resolve_symbol(arg.name));
            self.text(": ");
            self.format_type(&arg.ty);
            if i < arguments.len() - 1 {
                self.text(", ");
            }
        }

        self.text(") -> ");
        self.format_type(return_ty);
        self.hard_break();

        self.indent();
        self.walk_node(body);
        self.dedent();
        self.hard_break();

        VisitAction::Nothing
    }

    fn visit_node(&mut self, node: &super::Node) -> VisitAction {
        match &node.kind {
            NodeKind::Handle {
                name,
                handlers,
                expr,
            } => {
                self.text("handle ");
                self.text(resolve_symbol(*name));
                self.indent();
                self.hard_break();

                for handler in handlers {
                    extract!(
                        self.types.get(handler.ty),
                        Type::Function { inputs, output }
                    );

                    self.text(resolve_symbol(handler.name));
                    self.text("(");
                    for (i, (arg, ty)) in std::iter::zip(&handler.arguments, &inputs).enumerate() {
                        self.text(resolve_symbol(*arg));
                        self.text(": ");
                        self.format_type(ty);
                        if i < handler.arguments.len() - 1 {
                            self.text(", ");
                        }
                    }
                    self.text(") -> ");
                    self.format_type(&output);
                    self.hard_break();
                    self.indent();
                    self.walk_node(&handler.body);
                    self.dedent();
                    self.hard_break();
                }

                self.dedent();
                self.text("in ");
                self.indent();
                self.hard_break();

                self.walk_node(expr);

                self.dedent();

                VisitAction::Nothing
            }
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
            NodeKind::Assign { name, value } => {
                self.text(resolve_symbol(*name));
                self.text(": ");
                self.format_type(&value.ty);
                self.text(" = ");
                self.indent();
                self.walk_node(value);
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
                    BinopKind::Less => {
                        self.text(" < ");
                    }
                    BinopKind::Add => {
                        self.text(" + ");
                    }
                }

                self.walk_node(right);
                self.dedent();
                self.text("): ");
                self.format_type(&node.ty);

                VisitAction::Nothing
            }
            NodeKind::Call { callee, args } => {
                self.text("(");
                self.walk_node(callee);
                self.text(")");
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
            NodeKind::Resume { arg } => {
                self.text("resume(");
                self.walk_node(arg);
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
            NodeKind::While { cond, body } => {
                self.text("while: ");
                self.format_type(&node.ty);
                self.hard_break();
                self.indent();

                self.text("cond:  ");
                self.indent();
                self.walk_node(cond);
                self.dedent();
                self.hard_break();

                self.text("true:  ");
                self.walk_node(body);
                self.hard_break();

                self.dedent();

                VisitAction::Nothing
            }
            NodeKind::Block(nodes) => {
                self.hard_break();
                for node in nodes {
                    self.walk_node(node);
                    self.hard_break();
                }
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
                    Literal::Bool(true) => self.text("true"),
                    Literal::Bool(false) => self.text("false"),
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
                let builtin = &self.index.builtins.builtins_ord[*idx];
                let name = resolve_symbol(builtin.name);
                self.text(name);
                self.text(": ");
                self.format_type(&node.ty);
                VisitAction::Recurse
            }
            NodeKind::Function(id) => {
                let header = self.index.functions.get(id).unwrap();
                match header {
                    Header::Fn(header) => {
                        let name = header.name.map_or("", resolve_symbol);
                        self.text(name);
                    }
                    Header::Effect(header) => {
                        let name = resolve_symbol(header.name);
                        let kind = match header.kind {
                            EffectKind::Function => "eff fn ",
                            EffectKind::Control => "eff ctl ",
                        };
                        self.text(kind);
                        self.text(name);
                    }
                }
                self.text(": ");
                self.format_type(&node.ty);
                VisitAction::Recurse
            }
            NodeKind::Return(value) => {
                self.text("return");
                if let Some(value) = value {
                    self.text(" ");
                    self.indent();
                    self.walk_node(value);
                    self.dedent();
                }

                VisitAction::Nothing
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
