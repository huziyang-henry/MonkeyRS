use std::fmt::{Display, Formatter};
use crate::lexer::token::Token;
use crate::parser::expression::{Expression, Identifier};
use crate::parser::node::NodeOp;

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(s) => { s.fmt(f) }
            Statement::ReturnStatement(s) => { s.fmt(f) }
            Statement::ExpressionStatement(s) => { s.fmt(f) }
            Statement::BlockStatement(s) => { s.fmt(f) }
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<Expression>>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{} {};", self.token.literal, self.name) }
            Some(e) => { write!(f, "{} {} = {};", self.token.literal, self.name, e.to_string()) }
        }
    }
}

impl NodeOp for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{};", self.token.literal) }
            Some(e) => { write!(f, "{} {};", self.token.literal, e.to_string()) }
        }
    }
}

impl NodeOp for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}


pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<Expression>>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            None => { write!(f, "") }
            Some(e) => { write!(f, "{}", e.as_ref().to_string()) }
        }
    }
}

impl NodeOp for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl NodeOp for BlockStatement {
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
