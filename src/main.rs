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
    hlir::{pretty::print_fragments, visitor::HlirVisitor, FileId},
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
        effect foo {
            fn get_number() -> int;
            ctl yeet(error: string);
        }
        fn main() {
            handle foo {
                get_number() {
                    resume(0);
                }
                yeet(error) {
                    print(error);
                    return;
                }
            }
            print_int(wow());
        }
        fn wow() -> int {
            get_number();
        }
        "#,
    );

    println!("Source:\n{}\n", source);

    let ast = parse(&source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();
    builder.load_builtins(|l| load_standard_builtins::<StandardPorts>(l));
    let mut hlir = builder.hlir;

    Simplifier.walk_hlir(&mut hlir);
    NameResolver::new().walk_hlir(&mut hlir);

    let mut typecheck = TypecheckContext::new();
    typecheck.walk_hlir(&mut hlir);
    typecheck.apply_constraints();

    {
        let mut pretty = PrettyPrint::new();
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
