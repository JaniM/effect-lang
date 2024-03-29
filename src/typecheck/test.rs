#![cfg(test)]

use lasso::Spur;

use crate::{
    hlir::{
        name_resolve::NameResolver,
        pretty::{print_fragments, PrettyPrint},
        simplify::Simplifier,
        visitor::HlirVisitor,
        FileId, HlirBuilder,
    },
    parser::parse,
    typecheck::TypecheckContext,
};

fn typecheck_test(source: &str) -> TypecheckContext {
    let ast = parse(&source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(Spur::default()), ast).unwrap();
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

    typecheck
}

#[test]
fn generic_inference() {
    let source = unindent::unindent(
        r#"
        fn main() {
          let v = pass(manifest());
          takes_int(v);
        }

        fn manifest<a>() -> a { return manifest(); }
        fn pass<a>(x: a) -> a { return x; }
        fn takes_int(x: int) {}
        "#,
    );

    let typecheck = typecheck_test(&source);
    assert_eq!(typecheck.errors, vec![]);
}

/// Generic functions should be instantiated immediately when their name is used. Hence, in this
/// example `f` gets a concrete type of `() -> ?0` which unifies to `() -> string`. This should
/// error when attempting to unify with int.
#[test]
fn instantiate_functions_early() {
    let source = unindent::unindent(
        r#"
        fn main() {
          let f = manifest;
          takes_string(f());
          takes_int(f());
        }

        fn manifest<a>() -> a { return manifest(); }
        fn takes_int(x: int) {}
        fn takes_string(x: string) {}
        "#,
    );

    let typecheck = typecheck_test(&source);
    assert!(typecheck.errors.len() > 0);
}

/// If we attempt to solve main before `takes_int`, we won't yet know what type `takes_int` should
/// return. We should always expect ambiguous definition to be solved first.
#[test]
fn solve_ambiguous_functions_first() {
    let source = unindent::unindent(
        r#"
        fn main() {
          takes_string(pass(takes_int, 0));
        }

        fn pass<a, b>(f: (a) -> b, x: a) -> b { return f(x); }
        fn takes_int(x: int) -> _ { return ""; }
        fn takes_string(x: string) { }
        "#,
    );

    let typecheck = typecheck_test(&source);
    assert_eq!(typecheck.errors, vec![]);
}
