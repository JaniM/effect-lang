use std::{marker::PhantomData, rc::Rc};

use crate::{
    bytecode::Value,
    hlir::{Type, TypeId},
    typecheck::TypeStore,
};

use super::{Interpreter, Ports};

trait Extract: Sized {
    fn extract<P: Ports>(interpreter: &mut Interpreter<P>) -> Option<Self>;
    fn extract_type(types: &TypeStore) -> TypeId;
}

impl Extract for Rc<String> {
    fn extract<P: Ports>(interpreter: &mut Interpreter<P>) -> Option<Self> {
        match interpreter.stack.pop().unwrap() {
            Value::String(x) => Some(x),
            x => {
                interpreter.stack.push(x);
                None
            }
        }
    }

    fn extract_type(types: &TypeStore) -> TypeId {
        types.string()
    }
}

impl Extract for i64 {
    fn extract<P: Ports>(interpreter: &mut Interpreter<P>) -> Option<Self> {
        match interpreter.stack.pop().unwrap() {
            Value::Int(x) => Some(x),
            x => {
                interpreter.stack.push(x);
                None
            }
        }
    }

    fn extract_type(types: &TypeStore) -> TypeId {
        types.int()
    }
}

pub trait BuiltinFunction<P: Ports, I> {
    fn call(&self, interpreter: &mut Interpreter<P>) -> Option<()>;
    fn arg_count(&self) -> usize;
    fn extract_type(&self, types: &TypeStore) -> TypeId;
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

    fn extract_type(&self, types: &TypeStore) -> TypeId {
        types.insert(Type::Function {
            inputs: vec![],
            output: types.unit(),
        })
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

    fn extract_type(&self, types: &TypeStore) -> TypeId {
        types.insert(Type::Function {
            inputs: vec![A::extract_type(types)],
            output: types.unit(),
        })
    }
}

pub struct BuiltinAdapter<F, P, I> {
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
    fn extract_type(&self, types: &TypeStore) -> TypeId;
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

    fn extract_type(&self, types: &TypeStore) -> TypeId {
        BuiltinFunction::extract_type(&self.builtin, types)
    }
}

pub trait LoadBuiltin<P: Ports> {
    fn load_builtin<F, I>(&mut self, name: &str, f: F)
    where
        F: BuiltinFunction<P, I> + 'static,
        P: 'static,
        I: 'static;
}
