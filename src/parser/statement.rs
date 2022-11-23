use std::fmt::{Display, Formatter};
use crate::lexer::token::Token;
use crate::parser::expression::{Expression, Identifier};
use crate::parser::node::Node;

pub trait Statement: Node {
    fn statement_node(&self) -> Box<dyn Node>;
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{} {};", self.token.literal, self.name) }
            Some(e) => { write!(f, "{} {} = {};", self.token.literal, self.name, e.to_string()) }
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{};", self.token.literal) }
            Some(e) => { write!(f, "{} {};", self.token.literal, e.to_string()) }
        }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}


pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            None => { write!(f, "") }
            Some(e) => { write!(f, "{}", e.to_string()) }
        }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement.to_string())?
        }
        Ok(())
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}
