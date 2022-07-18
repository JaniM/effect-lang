use crate::lower::Value;

use super::{Interpreter, Ports};

trait Extract: Sized {
    fn extract<P: Ports>(interpreter: &mut Interpreter<P>) -> Option<Self>;
}

impl Extract for String {
    fn extract<P: Ports>(interpreter: &mut Interpreter<P>) -> Option<Self> {
        match interpreter.stack.pop().unwrap() {
            Value::String(x) => Some(x),
            x => {
                interpreter.stack.push(x);
                None
            }
        }
    }
}

pub trait BuiltinFunction<P: Ports, I> {
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()>;
    fn arg_count(&self) -> usize;
}

impl<F, P> BuiltinFunction<P, ()> for F
where
    F: Fn(&mut Interpreter<P>),
    P: Ports,
{
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()> {
        Some(self(interpreter))
    }

    fn arg_count(&self) -> usize {
        0
    }
}

impl<F, P, A> BuiltinFunction<P, (A,)> for F
where
    F: Fn(&mut Interpreter<P>, A),
    A: Extract,
    P: Ports,
{
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()> {
        let a1 = A::extract(interpreter)?;
        Some(self(interpreter, a1))
    }

    fn arg_count(&self) -> usize {
        1
    }
}

pub trait ErasedBuiltin<P: Ports> {
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()>;
    fn arg_count(&self) -> usize;
}

impl<I, P> ErasedBuiltin<P> for Box<dyn BuiltinFunction<P, I>>
where
    P: Ports,
{
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()> {
        BuiltinFunction::call(&**self, interpreter)
    }

    fn arg_count(&self) -> usize {
        BuiltinFunction::arg_count(&**self)
    }
}
