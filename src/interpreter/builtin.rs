use std::marker::PhantomData;

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

pub struct BuiltinAdapter<F, P, I>
{
    builtin: F,
    _phantom: PhantomData<fn() -> (P, I)>,
}

impl<F, P, I> BuiltinAdapter<F, P, I>
where
    F: BuiltinFunction<P, I>,
    P: Ports,
{
    pub fn new(f: F) -> Self {
        Self {
            builtin: f,
            _phantom: PhantomData,
        }
    }
}

pub trait ErasedBuiltin<P: Ports> {
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()>;
    fn arg_count(&self) -> usize;
}

impl<F, P, I> ErasedBuiltin<P> for BuiltinAdapter<F, P, I>
where
    F: BuiltinFunction<P, I>,
    P: Ports,
{
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()> {
        BuiltinFunction::call(&self.builtin, interpreter)
    }

    fn arg_count(&self) -> usize {
        BuiltinFunction::arg_count(&self.builtin)
    }
}
