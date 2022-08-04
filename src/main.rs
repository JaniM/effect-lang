#![feature(box_patterns)]
#![feature(int_log)]

mod bytecode;
mod hlir;
mod intern;
mod interpreter;
mod lexer;
mod parser;

use crate::{
    bytecode::{
        optimize::simplify_function, print_function, program::Program, Function, FunctionBuilder,
        FunctionBuilderCtx,
    },
    hlir::{name_resolve::NameResolver, pretty::PrettyPrint, simplify::Simplifier, HlirBuilder},
    hlir::{
        pretty::print_fragments,
        typecheck::{report_unknown_types, Typechecker},
        visitor::HlirVisitor,
        Builtins, FileId,
    },
    interpreter::{
        standard::{load_standard_builtins, StandardPorts},
        Interpreter,
    },
    parser::parse,
};
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
            wow("first", "second");
        }
        fn wow(a, b) {
            print(a);
            print(b);
        }
        "#,
    );

    println!("Source:\n{}\n", source);

    let ast = parse(&source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();
    let mut hlir = builder.hlir;

    let builtins = Builtins::load(|l| load_standard_builtins::<StandardPorts>(l));

    Simplifier.walk_hlir(&mut hlir);
    NameResolver::new(&builtins).walk_hlir(&mut hlir);
    Typechecker::default().walk_hlir(&mut hlir);

    {
        let mut pretty = PrettyPrint::new(&builtins);
        pretty.walk_hlir(&mut hlir);
        println!("HIR:");
        print_fragments(&pretty.fragments);
        println!();
    }

    report_unknown_types(&mut hlir);

    let module = hlir.modules.get(&hlir::ModuleId(0)).unwrap();
    let mut ctx = FunctionBuilderCtx::default();
    for fndef in module.functions.values() {
        let mut func = Function::default();
        FunctionBuilder::new(&mut func, &mut ctx).build_fndef(fndef);
        print_function(&func, &ctx);
        simplify_function(&mut func);
        // print_function(&func, &ctx);
    }

    println!("\nBytecode: ");
    let program = Program::from_hlir(&hlir);
    program.print();

    let mut interpreter = Interpreter::<StandardPorts>::new(program).with_stdout(std::io::stdout());
    load_standard_builtins(&mut interpreter);
    println!("\nOutput:");
    interpreter.run().unwrap();
}
