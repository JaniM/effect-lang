#![feature(box_patterns)]
#![feature(int_log)]

mod bytecode;
mod hlir;
mod intern;
mod interpreter;
mod lexer;
mod parser;
mod typecheck;

use crate::{
    bytecode::{
        optimize::simplify_function, program::Program, Function, FunctionBuilder,
        FunctionBuilderCtx,
    },
    hlir::{name_resolve::NameResolver, pretty::PrettyPrint, simplify::Simplifier, HlirBuilder},
    hlir::{pretty::print_fragments, visitor::HlirVisitor, Builtins, FileId},
    interpreter::{
        standard::{load_standard_builtins, StandardPorts},
        Interpreter,
    },
    parser::parse,
    typecheck::{report_unknown_types, TypecheckContext},
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
            let x = foo(0);
            print_int(x);
        }
        fn foo(c: int) -> int {
            if (c < 5) {
                return foo(c+1);
            }
            return c;
        }
        "#,
    );

    println!("Source:\n{}\n", source);

    let ast = parse(&source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();
    let mut hlir = builder.hlir;

    let builtins = Builtins::load(&hlir.types, |l| load_standard_builtins::<StandardPorts>(l));

    Simplifier.walk_hlir(&mut hlir);
    NameResolver::new(&builtins).walk_hlir(&mut hlir);

    let mut typecheck = TypecheckContext::new(&hlir.types);
    typecheck.walk_hlir(&mut hlir);
    typecheck.apply_constraints();

    {
        let mut pretty = PrettyPrint::new(&builtins);
        pretty.walk_hlir(&mut hlir);
        println!("HIR:");
        print_fragments(&pretty.fragments);
        println!();
    }

    report_unknown_types(&mut hlir);

    let module = hlir.modules.get(&hlir::ModuleId(0)).unwrap();
    let mut ctx = FunctionBuilderCtx::new(&hlir.types);
    for fndef in module.functions.values() {
        let mut func = Function::default();
        FunctionBuilder::new(&mut func, &mut ctx).build_fndef(fndef);
        bytecode::print_function(&func, &ctx);
        simplify_function(&mut func);
    }

    let program = Program::from_hlir(&hlir);
    println!("\nBytecode: ");
    program.print();

    let mut interpreter = Interpreter::<StandardPorts>::new(program).with_stdout(std::io::stdout());
    load_standard_builtins(&mut interpreter);
    println!("\nOutput:");
    interpreter.run().unwrap();
}
