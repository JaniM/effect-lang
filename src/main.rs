mod compile;
mod interpreter;
mod ir;
mod lexer;
mod lower;
mod parser;
mod typecheck;

use crate::{
    interpreter::standard::{load_standard_builtins, StandardPorts},
    ir::{resolve_names, Builder, Builtins, IRWriteCtx},
    parser::parse,
    typecheck::Typechecker,
};
use lasso::Rodeo;

fn main() {
    let source = r#"fn main() { print("hello"); }"#;

    println!("Source:\n  {}\n", source);

    let mut interner = Rodeo::default();
    let ast = parse(source, &mut interner).unwrap();
    let mut builder = Builder::default();
    builder.read_top_nodes(&ast).unwrap();

    let mut ir = builder.to_ir();

    println!("{}", ir.to_string(&IRWriteCtx::new(&interner, source)));

    let builtins = Builtins::load(&mut interner, |l| {
        load_standard_builtins::<StandardPorts>(l)
    });

    resolve_names(&builtins, &mut ir).unwrap();

    let mut typecheck = Typechecker::new(&mut interner, &builtins);
    typecheck.infer(&mut ir).unwrap();

    println!("{}", ir.to_string(&IRWriteCtx::new(&interner, source)));
}
