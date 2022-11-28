use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::token::Token;
use crate::object::{Object, ObjectError};
use crate::parser::expression::{Expression, Identifier};
use crate::evaluator::Evaluator;
use crate::object::environment::Environment;

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        match self {
            Statement::LetStatement(s) => { s.eval(env) }
            Statement::ReturnStatement(s) => { s.eval(env) }
            Statement::ExpressionStatement(s) => { s.eval(env) }
            Statement::BlockStatement(s) => { s.eval(env) }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        if self.value.is_none() {
            return Err(ObjectError::new(format!("identifier not found: {}", self.identifier)));
        }

        let e = self.value.as_ref().unwrap();
        let result = e.eval(Rc::clone(&env))?;
        env.borrow_mut().set(self.identifier.token.literal(), result);

        Ok(Object::Null)
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let v = match &self.value {
            None => { Ok(Object::Null) }
            Some(v) => { v.eval(env) }
        }?;

        Ok(Object::Return(Box::new(v)))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<Expression>>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            None => { write!(f, "") }
            Some(e) => { write!(f, "{}", e) }
        }
    }
}

impl Evaluator for ExpressionStatement {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        Ok(match &self.expression {
            None => { Object::Null }
            Some(e) => { e.eval(env)? }
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Evaluator for BlockStatement {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let mut result = Object::Null;

        for stmt in &self.statements {
            result = stmt.eval(Rc::clone(&env))?;

            if matches!(result, Object::Return(_)) {
                return Ok(result);
            }
        }

        Ok(result)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?
        }
        Ok(())
    }
}
