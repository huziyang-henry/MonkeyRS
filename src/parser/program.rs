use std::fmt::{Display, Formatter};
use crate::object::Object;
use crate::parser::node::NodeOp;
use crate::parser::statement::{Statement};

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?
        }

        Ok(())
    }
}

impl NodeOp for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].to_string()
        } else {
            String::default()
        }
    }

    fn eval(&self) -> Object {
        let mut result = Object::Null;
        for stmt in &self.statements {
            result = stmt.eval();

            if let Object::Return(r) = result {
                return *r;
            }
        }

        result
    }
}
