use std::{io::Write, rc::Rc};

use super::{builtin::LoadBuiltin, Interpreter, Ports};

pub struct StandardPorts;

impl Ports for StandardPorts {
    type Stdin = ();
    type Stdout = std::io::Stdout;
}

fn print<P: Ports>(interpreter: &mut Interpreter<P>, text: Rc<String>) {
    let out = interpreter.stdout.as_mut().unwrap();
    write!(out, "{}\n", text).unwrap();
}

fn print_int<P: Ports>(interpreter: &mut Interpreter<P>, num: i64) {
    let out = interpreter.stdout.as_mut().unwrap();
    write!(out, "{}\n", num).unwrap();
}

pub fn load_standard_builtins<P: Ports + 'static>(loader: &mut impl LoadBuiltin<P>) {
    loader.load_builtin("print", print);
    loader.load_builtin("print_int", print_int);
}
