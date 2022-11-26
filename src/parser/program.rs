use std::fmt::{Display, Formatter};
use crate::object::Object;
use crate::evaluator::Evaluator;
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

impl Evaluator for Program {
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
