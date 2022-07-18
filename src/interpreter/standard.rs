use std::io::Write;

use super::{Interpreter, Ports};

pub struct StandardPorts;

impl Ports for StandardPorts {
    type Stdin = ();
    type Stdout = std::io::Stdout;
}

fn print<P: Ports>(interpreter: &mut Interpreter<P>, text: String) {
    let out = interpreter.stdout.as_mut().unwrap();
    write!(out, "{}\n", text).unwrap();
}

pub fn load_standard_builtins<P: Ports + 'static>(interpreter: &mut Interpreter<P>) {
    interpreter.load_builtin("print", print);
}
