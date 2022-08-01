use lasso::Spur;

pub enum Instruction {
    PushString(Spur),
    LoadLocal(Spur),
    Call(u32),
    Return,
}

#[derive(Clone, Debug)]
pub enum Value {
    Builtin(usize, u32),
    Function(usize, u32),
    String(String),
}
