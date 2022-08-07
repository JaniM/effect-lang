#![cfg(test)]

use super::*;
use crate::{
    hlir::{name_resolve::NameResolver, simplify::Simplifier, visitor::HlirVisitor},
    interpreter::standard::{load_standard_builtins, StandardPorts},
    parser::parse,
};
use pretty_assertions::assert_eq;

#[test]
fn simple() {
    let source = r#"fn main() { print("hello"); }"#;
    let ast = parse(source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();

    builder.load_builtins(|l| load_standard_builtins::<StandardPorts>(l));

    Simplifier.walk_hlir(&mut builder.hlir);
    NameResolver::new().walk_hlir(&mut builder.hlir);

    let key = |v| INTERNER.get(v).unwrap();

    let types = builder.hlir.types;
    let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();

    use NodeKind::*;

    let unit = types.unit();

    let func = FnDef {
        header: FnHeader {
            id: FunctionId(0),
            name: Some(key("main")),
            ty: types.insert(Type::Function {
                inputs: vec![],
                output: unit,
            }),
        },
        arguments: vec![],
        return_ty: unit,
        body: Node {
            kind: Call {
                callee: Node {
                    kind: Builtin(0),
                    ty: types.insert(Type::Function {
                        inputs: vec![types.string()],
                        output: unit,
                    }),
                    source_span: Span(FileId(0), 12, 17),
                }
                .into(),
                args: vec![Node {
                    kind: Literal(self::Literal::String(key("hello"))),
                    ty: TypeId::new(3),
                    source_span: Span(FileId(0), 18, 25),
                }],
            },
            ty: TypeId::new(4),
            source_span: Span(FileId(0), 12, 26),
        },
    };
    let functions = HashMap::from([(FunctionId(0), func)]);
    assert_eq!(
        module,
        &Module {
            file: FileId(0),
            id: ModuleId(0),
            name: None,
            functions,
            effect_groups: Default::default(),
        }
    );
}

#[test]
fn let_fold() {
    let source = r#"fn main() { let a = 1; a; }"#;
    let ast = parse(source).unwrap();

    let mut builder = HlirBuilder::default();
    builder.read_module(FileId(0), ast).unwrap();

    let builtins = Builtins::load(&builder.hlir.types, |l| {
        load_standard_builtins::<StandardPorts>(l)
    });

    Simplifier.walk_hlir(&mut builder.hlir);
    NameResolver::new().walk_hlir(&mut builder.hlir);

    let key = |v: &str| INTERNER.get(v).unwrap();

    let types = &builder.hlir.types;
    let module = builder.hlir.modules.get(&ModuleId(0)).unwrap();

    use NodeKind::*;

    let unit = types.unit();

    let func = FnDef {
        header: FnHeader {
            id: FunctionId(0),
            name: Some(key("main")),
            ty: types.insert(Type::Function {
                inputs: vec![],
                output: unit,
            }),
        },
        arguments: vec![],
        return_ty: unit,
        body: Node {
            kind: Let {
                name: key("a"),
                value: Node {
                    kind: Literal(self::Literal::Int(1)),
                    ty: TypeId::new(2),
                    source_span: Span(FileId(0), 20, 21),
                }
                .into(),
                expr: Node {
                    kind: Name(key("a")),
                    ty: TypeId::new(3),
                    source_span: Span(FileId(0), 23, 24),
                }
                .into(),
            },
            ty: unit,
            source_span: Span(FileId(0), 12, 22),
        },
    };

    println!("{:?}", builder.hlir);
    let functions = HashMap::from([(FunctionId(0), func)]);
    assert_eq!(
        module,
        &Module {
            file: FileId(0),
            id: ModuleId(0),
            name: None,
            functions,
            effect_groups: Default::default(),
        }
    );
}
