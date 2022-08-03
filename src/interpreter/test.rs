#![cfg(test)]

use std::io::Cursor;

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
