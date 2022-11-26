use std::fmt::{Display, Formatter};
use crate::token::Token;
use crate::object::Object;
use crate::parser::expression::{Expression, Identifier};
use crate::evaluator::Evaluator;

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

impl Evaluator for Statement {
    fn eval(&self) -> Object {
        match self {
            Statement::LetStatement(s) => { s.eval() }
            Statement::ReturnStatement(s) => { s.eval() }
            Statement::ExpressionStatement(s) => { s.eval() }
            Statement::BlockStatement(s) => { s.eval() }
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Option<Box<Expression>>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{} {};", self.token, self.identifier) }
            Some(e) => { write!(f, "{} {} = {};", self.token, self.identifier, e) }
        }
    }
}

impl Evaluator for LetStatement {
    fn eval(&self) -> Object {
        todo!()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{};", self.token) }
            Some(e) => { write!(f, "{} {};", self.token, e) }
        }
    }
}

impl Evaluator for ReturnStatement {
    fn eval(&self) -> Object {
        let v = match &self.value {
            None => { Object::Null }
            Some(v) => { v.eval() }
        };

        Object::Return(Box::new(v))
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

impl Evaluator for ExpressionStatement {
    fn eval(&self) -> Object {
        match &self.expression {
            None => { Object::Null }
            Some(e) => { e.eval() }
        }
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Evaluator for BlockStatement {
    fn eval(&self) -> Object {
        let mut result = Object::Null;

        for stmt in &self.statements {
            result = stmt.eval();

            if let Object::Return(r) = result {
                return Object::Return(r);
            }
        }

        result
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
