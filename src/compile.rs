use lasso::Spur;

use crate::parser::{Call, FnDef, Node, RawNode};

#[derive(Clone, Debug, PartialEq)]
pub enum HLInstruction {
    PushString(Spur),
    /// arg count
    Call(u32),
    LoadLocal(Spur),
    Return,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Option<Spur>,
    pub argc: usize,
    pub body: Vec<HLInstruction>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Builder {
    current: Option<Function>,
    pub functions: Vec<Function>,
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    InvalidTopLevel,
    NestedFn,
}

impl Function {
    fn new(def: &FnDef) -> Self {
        Function {
            name: def.name,
            argc: 0,
            body: Vec::new(),
        }
    }
}

type BuildResult<T = (), E = CompileError> = Result<T, E>;

impl Builder {
    pub fn read_top_nodes(&mut self, nodes: &[Node]) -> BuildResult {
        for item in nodes {
            self.read_top_node(item)?;
        }
        Ok(())
    }

    fn read_top_node(&mut self, node: &Node) -> BuildResult {
        match &node.0 {
            RawNode::FnDef(def) => self.read_function(def),
            _ => Err(CompileError::InvalidTopLevel),
        }
    }

    fn read_function(&mut self, def: &FnDef) -> BuildResult {
        self.begin_function(Function::new(def))?;

        for item in def.body.0.stmts.iter() {
            self.read_expression(item)?;
        }

        self.end_function()?;
        Ok(())
    }

    fn read_expression(&mut self, node: &Node) -> BuildResult {
        match &node.0 {
            RawNode::FnDef(_) => todo!(),
            RawNode::If(_) => todo!(),
            RawNode::Call(call) => self.read_call(call),
            RawNode::Name(name) => self.read_name(*name),
            RawNode::String(string) => self.read_string(*string),
            RawNode::Binop(_) => todo!(),
            RawNode::Let(_) => todo!(),
            RawNode::Number(_) => todo!(),
        }
    }

    fn read_call(&mut self, call: &Call) -> BuildResult {
        self.read_expression(&call.callee)?;
        for item in call.args.iter() {
            self.read_expression(item)?;
        }
        self.push(HLInstruction::Call(call.args.len() as u32));
        Ok(())
    }

    fn read_name(&mut self, name: Spur) -> BuildResult {
        self.push(HLInstruction::LoadLocal(name));
        Ok(())
    }

    fn read_string(&mut self, string: Spur) -> BuildResult {
        self.push(HLInstruction::PushString(string));
        Ok(())
    }

    fn begin_function(&mut self, function: Function) -> BuildResult {
        if self.current.is_some() {
            return Err(CompileError::NestedFn);
        }
        self.current = Some(function);
        Ok(())
    }

    fn end_function(&mut self) -> BuildResult {
        let func = self.current.take().unwrap();
        self.functions.push(func);
        Ok(())
    }

    fn current(&mut self) -> &mut Function {
        self.current.as_mut().unwrap()
    }

    fn push(&mut self, inst: HLInstruction) {
        self.current().body.push(inst);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{intern::INTERNER, parser::parse};

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;

        let ast = parse(source).unwrap();
        let mut builder = Builder::default();
        builder.read_top_nodes(&ast).unwrap();

        let key = |v| INTERNER.get(v).unwrap();

        use HLInstruction::*;
        assert_eq!(
            &builder.functions,
            &vec![Function {
                name: Some(key("main")),
                argc: 0,
                body: vec![LoadLocal(key("print")), PushString(key("hello")), Call(1)],
            }]
        );
    }
}
