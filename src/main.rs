mod compile;
mod hlir;
mod interpreter;
mod ir;
mod lexer;
mod lower;
mod parser;
mod typecheck;

use crate::{
    hlir::{name_resolve::NameResolver, pretty::PrettyPrint, simplify::Simplifier, HlirBuilder},
    hlir::{
        pretty::print_fragments,
        typecheck::{report_unknown_types, Typechecker},
        visitor::HlirVisitor,
        Builtins, FileId,
    },
    interpreter::standard::{load_standard_builtins, StandardPorts},
    parser::parse,
};
use lasso::Rodeo;
use unindent::unindent;

#[macro_export]
macro_rules! extract {
    ($v:expr, $($path:ident)::+ ( $($x:ident),+ )) => {
        #[allow(unused_parens)]
        let ( $($x),+ ) = match $v {
            $($path)::+ ( $($x),+ ) => ( $($x),+ ),
            _ => unreachable!("invalid extract"),
        };
    };
    ($v:expr, $name:ident $(:: $path:ident)* { $($x:ident),+ }) => {
        #[allow(unused_parens)]
        let ( $($x),+ ) = match $v {
            $name $(:: $path)* { $($x),+, .. } => ( $($x),+ ),
            _ => unreachable!("invalid extract"),
        };
    };
}

#[macro_export]
macro_rules! inc {
    ($x:expr) => {{
        let v = $x;
        $x += 1;
        v
    }};
}

fn main() {
    let source = unindent(
        r#"
        fn main() {
            let x = 0;
            if (x == 0) {
                print("hello");
            }
        }
        "#,
    );

    println!("Source:\n{}\n", source);

    let mut interner = Rodeo::default();
    let ast = parse(&source, &mut interner).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();
    let mut hlir = builder.hlir;

    let builtins = Builtins::load(&mut interner, |l| {
        load_standard_builtins::<StandardPorts>(l)
    });

    Simplifier.walk_hlir(&mut hlir);
    NameResolver::new(&builtins).walk_hlir(&mut hlir);
    Typechecker::default().walk_hlir(&mut hlir);

    let mut pretty = PrettyPrint::new(&interner);
    pretty.walk_hlir(&mut hlir);
    println!("HIR:");
    print_fragments(&pretty.fragments);

    report_unknown_types(&mut hlir);
}
