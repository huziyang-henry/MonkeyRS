use std::fmt::{Display, Formatter};
use crate::parser::node::Node;
use crate::parser::statement::Statement;

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement.to_string())?
        }

        Ok(())
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::default()
        }
    }
}