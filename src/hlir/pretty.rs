use std::borrow::Cow;

use crate::{
    extract,
    hlir::{visitor::VisitAction, Literal},
    intern::{resolve_symbol, INTERNER},
    parser::{BinopKind, EffectKind, Generic},
    typecheck::{EffectSet, Type, TypeId, TypeStore},
};

use super::{
    index::{Header, Index},
    visitor::HlirVisitorImmut,
    EffectDef, EffectHeader, FnDef, FnHeader, NodeKind,
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
    generics: Vec<Vec<Generic>>,
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
        let default_params = || {
            vec![Generic {
                name: INTERNER.get_or_intern_static("a"),
            }]
        };
        match ty {
            Type::Ref(_) => todo!(),
            Type::Unknown(id) => {
                self.text(format!("?{id}"));
            }
            Type::Function {
                inputs,
                output,
                effects,
            } => {
                self.text("(");
                for (i, input) in inputs.iter().enumerate() {
                    self.format_type(input);
                    if i != inputs.len() - 1 {
                        self.text(", ");
                    }
                }
                self.text(") -> ");
                self.format_type(&output);
                let names = match effects {
                    EffectSet::Solved { effects, .. } => effects
                        .iter()
                        .map(|x| self.index.effect_groups.get(x).unwrap().name)
                        .collect(),
                    EffectSet::Unsolved { names, .. } => names,
                };
                let names_len = names.len();
                if names_len > 0 {
                    self.text(" with ");
                }
                for (i, eff) in names.into_iter().enumerate() {
                    self.text(resolve_symbol(eff));
                    if i != names_len - 1 {
                        self.text("+");
                    }
                }
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
            Type::Forall(x, ty) => {
                self.generics.push(default_params());
                let x = x as usize;
                let param_names = self.generics.last().cloned().unwrap_or_else(default_params);
                self.text(format!("for {}. ", resolve_symbol(param_names[x].name)));
                self.format_type(&ty);
                self.generics.pop();
            }
            Type::Parameter(x) => {
                let x = x as usize;
                let param_names = self.generics.last().cloned().unwrap_or_else(default_params);
                self.text(resolve_symbol(param_names[x].name));
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
            header: FnHeader {
                name, generics, ty, ..
            },
            return_ty,
            body,
            arguments,
        } = function;

        self.generics.push(generics.clone());

        let name = name.map_or("<unnamed>", resolve_symbol);
        self.text(format!("fn {}", name));

        if generics.len() > 0 {
            self.text("<");
            for (i, arg) in generics.iter().enumerate() {
                self.text(resolve_symbol(arg.name));
                if i < generics.len() - 1 {
                    self.text(", ");
                }
            }
            self.text(">");
        }

        self.text("(");
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

        let mut ty = self.types.get(*ty);
        let effects = loop {
            match ty {
                Type::Function { effects, .. } => break effects,
                Type::Forall(_, t) => ty = self.types.get(t),
                _ => todo!(),
            }
        };

        let names = match effects {
            EffectSet::Solved { effects, .. } => effects
                .iter()
                .map(|x| self.index.effect_groups.get(x).unwrap().name)
                .collect(),
            EffectSet::Unsolved { names, .. } => names,
        };
        let names_len = names.len();
        if names_len > 0 {
            self.text(" with ");
        }
        for (i, eff) in names.into_iter().enumerate() {
            self.text(resolve_symbol(eff));
            if i != names_len - 1 {
                self.text("+");
            }
        }

        self.hard_break();

        self.indent();
        self.walk_node(body);
        self.dedent();
        self.hard_break();

        self.generics.pop();

        VisitAction::Nothing
    }

    fn visit_node(&mut self, node: &super::Node) -> VisitAction {
        match &node.kind {
            NodeKind::Handle {
                group_id,
                name,
                handlers,
                expr,
            } => {
                self.text(format!("handle {} #{}", resolve_symbol(*name), group_id.0));
                self.indent();
                self.hard_break();

                for handler in handlers {
                    extract!(
                        self.types.get(handler.ty),
                        Type::Function { inputs, output }
                    );

                    self.text(format!(
                        "{} #{} (",
                        resolve_symbol(handler.name),
                        handler.effect_id.0,
                    ));
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
                    BinopKind::Greater => {
                        self.text(" > ");
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
                if let Some(arg) = arg {
                    self.walk_node(arg);
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
                self.indent();
                self.hard_break();
                self.walk_node(if_true);
                self.dedent();
                self.hard_break();

                if let Some(if_false) = if_false {
                    self.text("false: ");
                    self.indent();
                    self.hard_break();
                    self.walk_node(if_false);
                    self.dedent();
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

                self.walk_node(body);

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
            NodeKind::ApplyType { expr, ty } => {
                self.text("(");
                self.walk_node(expr);
                self.text(") with(");
                self.format_type(ty);
                self.text("): ");
                self.format_type(&node.ty);

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
