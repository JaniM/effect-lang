use std::collections::HashMap;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::{
    hlir::{index::Index, pretty::format_type, FileId, Span, Spanned},
    typecheck::{Constraint, Error, Type},
};

pub struct ErrorContext {
    cache: SimpleFiles<FileId, String>,
    file_ids: HashMap<FileId, usize>,
    index: Index,
}

impl ErrorContext {
    pub fn new(sourcea: HashMap<FileId, String>, index: Index) -> Self {
        let mut cache = SimpleFiles::new();
        let mut file_ids = HashMap::new();
        for (id, source) in sourcea {
            let key = cache.add(id, source);
            file_ids.insert(id, key);
        }
        Self {
            cache,
            file_ids,
            index,
        }
    }

    fn file(&self, span: Span) -> usize {
        self.file_ids[&span.0]
    }

    pub fn report_type_error(&mut self, error: &Error) {
        let types = &self.index.types;
        let mut diags = vec![];
        for constraint in &error.constraints {
            match constraint {
                &Constraint::Equal(left, right) => {
                    let Spanned(_, left_span) = types.get_spanned_noderef(left);
                    let Spanned(_, right_span) = types.get_spanned_noderef(right);

                    let report = Diagnostic::error()
                        .with_message("Conflicting requirements")
                        .with_labels(vec![
                            Label::secondary(self.file(left_span), left_span.1..left_span.2)
                                .with_message(format!(
                                    "This is `{}`",
                                    format_type(&self.index, left)
                                )),
                            Label::primary(self.file(right_span), right_span.1..right_span.2)
                                .with_message(format!(
                                    "This is `{}`, but we required `{}`",
                                    format_type(&self.index, right),
                                    format_type(&self.index, left)
                                )),
                        ]);
                    diags.push(report);
                }
                Constraint::LeftEqual(_, _) => todo!(),
                &Constraint::ArgumentOf(func, ty, idx) => {
                    let input = match types.walk_forall(func) {
                        Type::Function { inputs, .. } => inputs[idx],
                        _ => unreachable!(),
                    };
                    let Spanned(_, left_span) = types.get_spanned_noderef(input);
                    let Spanned(_, right_span) = types.get_spanned_noderef(ty);

                    let report = Diagnostic::error()
                        .with_message("Invalid argument")
                        .with_labels(vec![
                            Label::secondary(self.file(left_span), left_span.1..left_span.2)
                                .with_message(format!("Function defined here",)),
                            Label::primary(self.file(right_span), right_span.1..right_span.2)
                                .with_message(format!(
                                    "Required `{}` but got `{}`",
                                    format_type(&self.index, input),
                                    format_type(&self.index, ty)
                                )),
                        ]);
                    diags.push(report);
                }
                &Constraint::ResultOf(func, ty) => {
                    let func_ty = match types.get(func) {
                        Type::Function { output, .. } => output,
                        _ => todo!(),
                    };
                    let Spanned(_, right_span) = types.get_spanned_noderef(ty);
                    let Spanned(_, mut ret_span) = types.get_spanned_noderef(func_ty);
                    let mut ret_id = func_ty;
                    while let Spanned(Type::Ref(new_ret_id), new_ret_span) =
                        types.get_spanned_noderef(ret_id)
                    {
                        ret_span = new_ret_span;
                        ret_id = new_ret_id;
                    }

                    let report = Diagnostic::error()
                        .with_message("Conflicting requirements for return type")
                        .with_labels(vec![
                            Label::primary(self.file(right_span), right_span.1..right_span.2)
                                .with_message(format!(
                                    "Required `{}` but got `{}`",
                                    format_type(&self.index, ty),
                                    format_type(&self.index, func_ty)
                                )),
                            Label::secondary(self.file(ret_span), ret_span.1..ret_span.2)
                                .with_message("Return type defined here"),
                        ]);

                    diags.push(report);
                }
                Constraint::Call(_, _, _) => todo!(),
                Constraint::Apply(_, _) => todo!(),
            }
        }

        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        for diag in diags {
            codespan_reporting::term::emit(&mut writer.lock(), &config, &self.cache, &diag)
                .unwrap();
        }
    }
}
