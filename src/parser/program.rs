use crate::evaluator::Evaluator;
use crate::object::environment::Environment;
use crate::object::{Object, ObjectError};
use crate::parser::statement::Statement;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let mut result = Object::Null;
        for stmt in &self.statements {
            result = stmt.eval(Rc::clone(&env))?;

            if let Object::Return(r) = result {
                return Ok(*r);
            }
        }

        Ok(result)
    }
}
