#![cfg(test)]

use std::io::Cursor;

use unindent::unindent;

use super::{standard::load_standard_builtins, *};

struct TestPorts;
impl Ports for TestPorts {
    type Stdin = ();
    type Stdout = Cursor<Vec<u8>>;
}

fn test_interpreter(source: &str) -> Interpreter<TestPorts> {
    let cursor = Cursor::new(Vec::new());
    let mut interpreter = Interpreter::load_source(source)
        .unwrap()
        .with_stdout(cursor);
    load_standard_builtins(&mut interpreter);
    interpreter
}

#[test]
fn hello_world() {
    let source = r#"fn main() { print("hello"); }"#;

    let mut interpreter = test_interpreter(source);
    interpreter.program.print();
    interpreter.run().unwrap();

    assert_eq!(interpreter.stdout.unwrap().get_ref(), b"hello\n");
}

#[test]
fn unit_fn_call() {
    let source = unindent(
        r#"
        fn main() {
            func("a", "b");
            func("c", "d");
        }
        fn func(a: string, b: string) {
            print(a);
            print(b);
        }"#,
    );

    let mut interpreter = test_interpreter(&source);
    interpreter.program.print();
    interpreter.run().unwrap();

    assert_eq!(interpreter.stdout.unwrap().get_ref(), b"a\nb\nc\nd\n");
}

#[test]
fn while_loop() {
    let source = unindent(
        r#"
        fn main() {
            let x = 0;
            while (x < 5) {
                x = x + 1;
                print_int(x);
            }
        }"#,
    );

    let mut interpreter = test_interpreter(&source);
    interpreter.program.print();
    interpreter.run().unwrap();

    assert_eq!(interpreter.stdout.unwrap().get_ref(), b"1\n2\n3\n4\n5\n");
}

#[test]
fn recurse() {
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
        }"#,
    );

    let mut interpreter = test_interpreter(&source);
    interpreter.program.print();
    interpreter.run().unwrap();

    assert_eq!(interpreter.stdout.unwrap().get_ref(), b"5\n");
}

#[test]
fn simple_function_effects() {
    let source = unindent(
        r#"
        effect foo {
            fn get_number() -> int;
        }
        fn main() {
            handle foo {
                get_number() {
                    resume(1);
                }
            }
            wow();
            print_int(get_number());
        }
        fn wow() {
            handle foo {
                get_number() {
                    resume(2);
                }
            }
            print_int(get_number());
        }"#,
    );

    let mut interpreter = test_interpreter(&source);
    interpreter.program.print();
    interpreter.run().unwrap();

    assert_eq!(interpreter.stdout.unwrap().get_ref(), b"2\n1\n");
}
